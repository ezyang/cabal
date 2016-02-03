{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}
module Distribution.Backpack where

import Prelude hiding (mod)
import qualified Distribution.Utils.UnionFind as UnionFind
import Distribution.ModuleName
import Distribution.Package
import Distribution.PackageDescription

import Data.STRef
import Control.Monad.ST
import Control.Monad
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Foldable as F
import qualified Data.Traversable as T

-----------------------------------------------------------------------
-- The workhorse

-- TODO: Need to be a little more sophisticated here: a conflict
-- should only be an error if the site is a requirement, because
-- Haskell people do sometimes include ambiguous sets.
data ModuleShape = ModuleShape {
    modShapeProvides :: ModuleProvides,
    modShapeRequires :: ModuleRequires
    }


moduleFreeHoles :: Module -> Set ModuleName
moduleFreeHoles (ModuleVar mod_name) = Set.singleton mod_name
moduleFreeHoles (Module uid n) = unitIdFreeHoles uid

unitIdFreeHoles :: UnitId -> Set ModuleName
unitIdFreeHoles (UnitId _ insts) = substFreeHoles insts
unitIdFreeHoles _ = Set.empty

generalizeUnitId :: UnitId -> UnitId
generalizeUnitId (UnitId cid insts) = UnitId cid (Map.mapWithKey (\k _ -> ModuleVar k) insts)
generalizeUnitId uid = uid

substFreeHoles :: ModuleSubst -> Set ModuleName
substFreeHoles insts = Set.unions (map moduleFreeHoles (Map.elems insts))

instance ModSubst ModuleShape where
    modSubst subst (ModuleShape provs reqs)
        = ModuleShape (modSubst subst provs) (modSubst subst reqs)

instance ModSubst (Set ModuleName) where
    modSubst subst reqs
        = Set.union (Set.difference reqs (Map.keysSet subst))
                    (substFreeHoles subst)

type ModuleProvides = ModuleSubst
type ModuleRequires = Set ModuleName

emptyModuleShape :: ModuleShape
emptyModuleShape = ModuleShape Map.empty Set.empty

emptyModuleShapeU :: ModuleShapeU s
emptyModuleShapeU = (Map.empty, Map.empty)

-----------------------------------------------------------------------
-- Substitutions

class ModSubst a where
    -- In notation, substitution is postfix, which implies
    -- putting it on the right hand side, but for partial
    -- application it's more convenient to have it on the left
    -- hand side.
    modSubst :: ModuleSubst -> a -> a

-- NB: not composition!
instance ModSubst a => ModSubst (Map ModuleName a) where
    modSubst subst = fmap (modSubst subst)

instance ModSubst Module where
    modSubst subst (Module cid mod_name) = Module (modSubst subst cid) mod_name
    modSubst subst mod@(ModuleVar mod_name)
        | Just mod' <- Map.lookup mod_name subst = mod'
        | otherwise = mod

instance ModSubst UnitId where
    modSubst subst (UnitId cid insts) = UnitId cid (modSubst subst insts)
    modSubst _subst uid = uid

-----------------------------------------------------------------------
-- Linking

type ModuleShapeU s = (ModuleProvidesU s, ModuleRequiresU s)
type ModuleSubstU s = Map ModuleName (ModuleU s)
type ModuleProvidesU s = ModuleSubstU s
type ModuleRequiresU s = ModuleSubstU s -- more convenient!!

convertModuleShape :: (ModuleRenaming, ModuleRenaming)
                   -> ModuleShape -> UnifyM s (ModuleShapeU s)
convertModuleShape (prov_rns, req_rns) (ModuleShape provs reqs) = do
    -- NOTICE!  prov_subst is POST-COMPOSED
    provs_u <- convertModuleSubst (modSubst req_subst (modSubst provs prov_subst))
    reqs_u  <- convertModuleSubst (Map.fromSet ModuleVar (Set.map req_rename reqs))
    return (provs_u, reqs_u)
  where prov_subst = renameProvSubst provs prov_rns
        req_rename = renameReqRename req_rns
        req_subst = renameReqSubst req_rns

renameReqRename :: ModuleRenaming -> ModuleName -> ModuleName
renameReqRename (ModuleRenaming _ xs) =
    let m = Map.fromList xs
    in \mod_name -> fromMaybe mod_name (Map.lookup mod_name m)

renameReqSubst :: ModuleRenaming -> ModuleSubst
renameReqSubst (ModuleRenaming _ xs) = Map.fromList [ (x0, ModuleVar x1) | (x0, x1) <- xs ]

renameProvSubst :: ModuleSubst -> ModuleRenaming -> ModuleSubst
renameProvSubst provs (ModuleRenaming b xs) =
    Map.fromList $ [ (x0, ModuleVar x1) | (x0,x1) <- xs ] ++
                   if b
                    then [ (k, ModuleVar k) | k <- Map.keys provs ]
                    else []

convertModuleShapeU :: ModuleShapeU s -> UnifyM s ModuleShape
convertModuleShapeU (provs_u, reqs_u) = do
    provs <- convertModuleSubstU provs_u
    reqs  <- convertModuleSubstU reqs_u
    return (ModuleShape provs (Map.keysSet reqs))

convertModuleSubst :: Map ModuleName Module -> UnifyM s (Map ModuleName (ModuleU s))
convertModuleSubst = T.mapM convertModule

convertModuleSubstU :: ModuleSubstU s -> UnifyM s ModuleSubst
convertModuleSubstU = T.mapM convertModuleU

mixLink :: ModuleShapeU s -> ModuleShapeU s -> UnifyM s (ModuleShapeU s)
mixLink (provs1, reqs1) (provs2, reqs2) = do
    -- Fill every requirement of 2 with the provisions of 1, and vice versa
    F.sequenceA_ (Map.intersectionWith unifyModule provs1 reqs2)
    F.sequenceA_ (Map.intersectionWith unifyModule provs2 reqs1)
    -- This is a bit tricky.  What we should ACTUALLY do is collect
    -- all of the provisions at a name, and then at the end make
    -- sure they're all actually the same, LITERALLY.
    -- F.sequenceA_ (Map.intersectionWith unifyModule provs1 provs2)
    -- Assert all the the requirements match (NB: should not be necessary)
    -- F.sequenceA_ (Map.intersectionWith unifyModule reqs1 reqs2)
    return (Map.union provs1 provs2,
            -- NB: NOT the difference of the unions.  That implies
            -- self-unification not allowed.  (But maybe requirement prov is disjoint
            -- from reqs makes this a moot point?)
            Map.union (Map.difference reqs1 provs2)
                      (Map.difference reqs2 provs1))

-----------------------------------------------------------------------
-- The "unifiable" variants of the data types
--
-- In order to properly do unification over infinite trees, we
-- need to union find over 'Module's and 'UnitId's.  The pure
-- representation is ill-equipped to do this, so we convert
-- from the pure representation into one which is indirected
-- through union-find.  'ModuleU' handles hole variables;
-- 'UnitIdU' handles mu-binders.

data ModuleU' s
    = ModuleU (UnitIdU s) ModuleName
    | ModuleVarU ModuleName
data UnitIdU' s
    = UnitIdU UnitIdUnique ComponentId (Map ModuleName (ModuleU s))
type ModuleU s = UnionFind.Point s (ModuleU' s)
type UnitIdU s = UnionFind.Point s (UnitIdU' s)

-- | An integer for uniquely labeling 'UnitIdU' nodes.  We need
-- these labels in order to efficiently serialize 'UnitIdU's into
-- 'UnitId's (we use the label to check if any parent is the
-- node in question, and if so insert a deBruijn index instead.)
-- These labels must be unique across all 'UnitId's/'Module's which
-- participate in unification!
type UnitIdUnique = Int

-----------------------------------------------------------------------
-- Unification monad

-- ERROR REPORTING?
--      Strongly want to know where variables are bound.
--      Assume that the inputs are located, then we can build an
--      error location this way.

newtype UnifyM s a = UnifyM { unUnifyM :: UnifEnv s -> ST s a }

-- NB: GHC 7.6 throws a hissy fit if you pattern match on 'm'.
runUnifyM :: (forall s. UnifyM s a) -> a
runUnifyM m = runST $ do i    <- newSTRef 0
                         hmap <- newSTRef Map.empty
                         unUnifyM m (UnifEnv i hmap)

data UnifEnv s = UnifEnv {
        unify_uniq :: UnifRef s UnitIdUnique,
        unify_reqs :: UnifRef s (Map ModuleName (ModuleU s))
    }
instance Functor (UnifyM s) where
    fmap f (UnifyM m) = UnifyM (fmap (fmap f) m)
instance Applicative (UnifyM s) where
    pure = UnifyM . pure . pure
    UnifyM f <*> UnifyM x = UnifyM $ \r -> f r <*> x r
instance Monad (UnifyM s) where
    return = UnifyM . return . return
    UnifyM m >>= f = UnifyM $ \r -> m r >>= \x -> unUnifyM (f x) r

liftST :: ST s a -> UnifyM s a
liftST m = UnifyM $ \_ -> m

type UnifRef s a = STRef s a

newUnifRef :: a -> UnifyM s (UnifRef s a)
newUnifRef = liftST . newSTRef

readUnifRef :: UnifRef s a -> UnifyM s a
readUnifRef = liftST . readSTRef

writeUnifRef :: UnifRef s a -> a -> UnifyM s ()
writeUnifRef x = liftST . writeSTRef x

getUnifEnv :: UnifyM s (UnifEnv s)
getUnifEnv = UnifyM $ \r -> return r

-----------------------------------------------------------------------
-- Conversion to the unifiable data types

-- An environment for tracking the mu-bindings in scope.
-- The invariant for a state @(m, i)@ is that [0..i] are
-- keys of @m@; in fact, the @i-k@th entry is the @k@th
-- de Bruijn index (this saves us from having to shift as
-- we enter mu-binders.)
type MuEnv s = (IntMap (UnitIdU s), Int)

extendMuEnv :: MuEnv s -> UnitIdU s -> MuEnv s
extendMuEnv (m, i) x =
    (IntMap.insert (i + 1) x m, i + 1)

lookupMuEnv :: MuEnv s -> Int {- de Bruijn index -} -> UnitIdU s
lookupMuEnv (m, i) k =
    case IntMap.lookup (i - k) m of
        Nothing -> error "lookupMuEnv: out of bounds"
        Just v -> v

emptyMuEnv :: MuEnv s
emptyMuEnv = (IntMap.empty, -1)

-- The workhorse functions.  These share an environment:
--   * @UnifRef s UnitIdUnique@ - the unique label supply for 'UnitIdU' nodes
--   * @UnifRef s (Map ModuleName moduleU)@ - the (lazily initialized)
--     environment containing the implicitly universally quantified
--     @hole:A@ binders.
--   * @MuEnv@ - the environment for mu-binders.

convertUnitId' :: MuEnv s
               -> UnitId
               -> UnifyM s (UnitIdU s)
convertUnitId' stk (SimpleUnitId cid) = convertUnitId' stk (UnitId cid Map.empty)
convertUnitId' stk (UnitId cid insts) = do
    UnifEnv fs _ <- getUnifEnv
    x <- liftST $ UnionFind.fresh (error "convertUnitId") -- tie the knot later
    insts_u <- T.forM insts $ convertModule' (extendMuEnv stk x)
    u <- readUnifRef fs
    writeUnifRef fs (u+1)
    y <- liftST $ UnionFind.fresh (UnitIdU u cid insts_u)
    liftST $ UnionFind.union x y
    return y
convertUnitId' stk (UnitIdVar i) = return (lookupMuEnv stk i)

convertModule' :: MuEnv s
               -> Module -> UnifyM s (ModuleU s)
convertModule' _stk (ModuleVar mod_name) = do
    UnifEnv _fs hmap <- getUnifEnv
    hm <- readUnifRef hmap
    case Map.lookup mod_name hm of
        Nothing -> do mod <- liftST $ UnionFind.fresh (ModuleVarU mod_name)
                      writeUnifRef hmap (Map.insert mod_name mod hm)
                      return mod
        Just mod -> return mod
convertModule' stk (Module uid mod_name) = do
    uid_u <- convertUnitId' stk uid
    liftST $ UnionFind.fresh (ModuleU uid_u mod_name)

convertUnitId :: UnitId -> UnifyM s (UnitIdU s)
convertUnitId = convertUnitId' emptyMuEnv

convertModule :: Module -> UnifyM s (ModuleU s)
convertModule = convertModule' emptyMuEnv

-----------------------------------------------------------------------
-- Conversion from the unifiable data types

-- An environment for tracking candidates for adding a mu-binding.
-- The invariant for a state @(m, i)@, is that if we encounter a node
-- labeled @k@ such that @m[k -> v]@, then we can replace this
-- node with the de Bruijn index @i-v@ referring to an enclosing
-- mu-binder; furthermore, @range(m) = [0..i]@.
type MooEnv = (IntMap Int, Int)

emptyMooEnv :: MooEnv
emptyMooEnv = (IntMap.empty, -1)

extendMooEnv :: MooEnv -> UnitIdUnique -> MooEnv
extendMooEnv (m, i) k = (IntMap.insert k (i + 1) m, i + 1)

lookupMooEnv :: MooEnv -> UnitIdUnique -> Maybe Int
lookupMooEnv (m, i) k =
    case IntMap.lookup k m of
        Nothing -> Nothing
        Just v -> Just (i-v) -- de Bruijn indexize

-- The workhorse functions

convertUnitIdU' :: MooEnv -> UnitIdU s -> UnifyM s UnitId
convertUnitIdU' stk uid_u = do
    UnitIdU u cid insts_u <- liftST $ UnionFind.find uid_u
    case lookupMooEnv stk u of
        Just i -> return (UnitIdVar i)
        Nothing -> do
            insts <- T.forM insts_u $ convertModuleU' (extendMooEnv stk u)
            if Map.null insts
                then return (SimpleUnitId cid)
                else return (UnitId cid insts)


convertModuleU' :: MooEnv -> ModuleU s -> UnifyM s Module
convertModuleU' stk mod_u = do
    mod <- liftST $ UnionFind.find mod_u
    case mod of
        ModuleVarU mod_name -> return (ModuleVar mod_name)
        ModuleU uid_u mod_name -> do
            uid <- convertUnitIdU' stk uid_u
            return (Module uid mod_name)

-- Helper functions

convertUnitIdU :: UnitIdU s -> UnifyM s UnitId
convertUnitIdU = convertUnitIdU' emptyMooEnv

convertModuleU :: ModuleU s -> UnifyM s Module
convertModuleU = convertModuleU' emptyMooEnv

-----------------------------------------------------------------------
-- The unification algorithm

-- This is based off of https://gist.github.com/amnn/559551517d020dbb6588
-- which is a translation from Huet's thesis.

unifyUnitId :: UnitIdU s -> UnitIdU s -> UnifyM s ()
unifyUnitId uid1_u uid2_u
    | uid1_u == uid2_u = return ()
    | otherwise = do
        UnitIdU _ cid1 insts1 <- liftST $ UnionFind.find uid1_u
        UnitIdU _ cid2 insts2 <- liftST $ UnionFind.find uid2_u
        when (cid1 /= cid2) $ error "unifyUnitId: cid failed"
        -- The KEY STEP which makes this a Huet-style unification
        -- algorithm.  (Also a payoff of using union-find.)
        liftST $ UnionFind.union uid1_u uid2_u
        F.sequenceA_ $ Map.intersectionWith unifyModule insts1 insts2

unifyModule :: ModuleU s -> ModuleU s -> UnifyM s ()
unifyModule mod1_u mod2_u
    | mod1_u == mod2_u = return ()
    | otherwise = do
        mod1 <- liftST $ UnionFind.find mod1_u
        mod2 <- liftST $ UnionFind.find mod2_u
        case (mod1, mod2) of
            (ModuleVarU _, _) -> liftST $ UnionFind.union mod1_u mod2_u
            (_, ModuleVarU _) -> liftST $ UnionFind.union mod2_u mod1_u
            (ModuleU uid1 mod_name1, ModuleU uid2 mod_name2) -> do
                when (mod_name1 /= mod_name2) $ error "unifyModule: mod_name failed"
                -- NB: this is not actually necessary (because we'll
                -- detect loops eventually in 'unifyUnitId'), but it
                -- seems harmless enough
                liftST $ UnionFind.union mod1_u mod2_u
                unifyUnitId uid1 uid2

