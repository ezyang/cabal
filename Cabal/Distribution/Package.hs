{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Package
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Defines a package identifier along with a parser and pretty printer for it.
-- 'PackageIdentifier's consist of a name and an exact version. It also defines
-- a 'Dependency' data type. A dependency is a package name and a version
-- range, like @\"foo >= 1.2 && < 2\"@.

module Distribution.Package (
        -- * Package ids
        PackageName(..),
        PackageIdentifier(..),
        PackageId,

        -- * Package keys/installed package IDs (used for linker symbols)
        ComponentId(..),
        UnitId(..),
        hashUnitId,
        mkUnitId,
        mkLegacyUnitId,
        getHSLibraryName,
        InstalledPackageId, -- backwards compat

        -- * Modules
        Module(..),
        ModuleSubst,
        modSubstToList,
        mkModSubst,
        dispModSubst,

        -- * ABI hash
        AbiHash(..),

        -- * Package source dependencies
        Dependency(..),
        thisPackageVersion,
        notThisPackageVersion,
        simplifyDependency,

        -- * Package classes
        Package(..), packageName, packageVersion,
        HasUnitId(..),
        installedPackageId,
        PackageInstalled(..),
  ) where

import Distribution.Version
         ( Version(..), VersionRange, anyVersion, thisVersion
         , notThisVersion, simplifyVersionRange )

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Distribution.Compat.ReadP
import Distribution.Compat.Binary
import Distribution.Text
import Distribution.ModuleName
import Distribution.Utils.Base62

import Control.DeepSeq (NFData(..))
import qualified Data.Char as Char
    ( isDigit, isAlphaNum, )
import Data.Data ( Data )
import Data.List ( intercalate )
import Data.Typeable ( Typeable )
import Data.Map ( Map )
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Text.PrettyPrint ((<>), (<+>), text)

newtype PackageName = PackageName { unPackageName :: String }
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary PackageName

instance Text PackageName where
  disp (PackageName n) = Disp.text n
  parse = do
    ns <- Parse.sepBy1 component (Parse.char '-')
    return (PackageName (intercalate "-" ns))
    where
      component = do
        cs <- Parse.munch1 Char.isAlphaNum
        if all Char.isDigit cs then Parse.pfail else return cs
        -- each component must contain an alphabetic character, to avoid
        -- ambiguity in identifiers like foo-1 (the 1 is the version number).

instance NFData PackageName where
    rnf (PackageName pkg) = rnf pkg

-- | Type alias so we can use the shorter name PackageId.
type PackageId = PackageIdentifier

-- | The name and version of a package.
data PackageIdentifier
    = PackageIdentifier {
        pkgName    :: PackageName, -- ^The name of this package, eg. foo
        pkgVersion :: Version -- ^the version of this package, eg 1.2
     }
     deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary PackageIdentifier

instance Text PackageIdentifier where
  disp (PackageIdentifier n v) = case v of
    Version [] _ -> disp n -- if no version, don't show version.
    _            -> disp n <> Disp.char '-' <> disp v

  parse = do
    n <- parse
    v <- (Parse.char '-' >> parse) <++ return (Version [] [])
    return (PackageIdentifier n v)

instance NFData PackageIdentifier where
    rnf (PackageIdentifier name version) = rnf name `seq` rnf version

-- | A module identity uniquely identifies a Haskell module by
-- qualifying a 'ModuleName' with the 'UnitId' which defined
-- it.  This type distinguishes between two packages
-- which provide a module with the same name, or a module
-- from the same package compiled with different dependencies.
-- There are a few cases where Cabal needs to know about
-- module identities, e.g., when writing out reexported modules in
-- the 'InstalledPackageInfo'.
--
-- In GHC, this is just a 'Module' constructor for backwards
-- compatibility reasons, but in Cabal we can do it properly.
data Module
    = Module UnitId ModuleName
    | ModuleVar ModuleName
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary Module

instance Text Module where
    disp (Module uid mod_name) =
        disp uid <> Disp.text ":" <> disp mod_name
    disp (ModuleVar mod_name) =
        Disp.text "hole:" <> disp mod_name
    parse = do
        mb_uid <- ((Parse.string "hole" >> return Nothing)
                    <++
                   (fmap Just parse))
        _ <- Parse.char ':'
        mod_name <- parse
        return $ case mb_uid of
            Nothing ->  ModuleVar mod_name
            Just uid -> Module uid mod_name

instance NFData Module where
    rnf (Module uid mod_name) = rnf uid `seq` rnf mod_name
    rnf (ModuleVar mod_name) = rnf mod_name

-- | A 'ComponentId' uniquely identifies the transitive source
-- code closure of a component.  For non-Backpack components, it also
-- serves as the basis for install paths, symbols, etc.
--
data ComponentId
    = ComponentId String
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

{-# DEPRECATED InstalledPackageId "Use UnitId instead" #-}
type InstalledPackageId = UnitId

instance Binary ComponentId

instance Text ComponentId where
  disp (ComponentId str) = text str

  parse = ComponentId `fmap` Parse.munch1 abi_char
   where abi_char c = Char.isAlphaNum c || c `elem` "-_."

instance NFData ComponentId where
    rnf (ComponentId pk) = rnf pk

-- | Returns library name prefixed with HS, suitable for filenames
getHSLibraryName :: UnitId -> String
getHSLibraryName (UnitIdVar _) = error "getHSLibraryName: unbound variable"
getHSLibraryName uid = "HS" ++ hashUnitId uid

-- | An explicit substitution on modules.  More functions dealing
-- with this are in 'Backpack'.
--
-- NB: These substitutions are NOT idempotent, for example, a
-- valid substitution is (A -> B, B -> A).
type ModuleSubst = Map ModuleName Module

mkModSubst :: [(ModuleName, Module)] -> ModuleSubst
mkModSubst = Map.fromList

modSubstToList :: ModuleSubst -> [(ModuleName, Module)]
modSubstToList = Map.toAscList

dispModSubst :: ModuleSubst -> Disp.Doc
dispModSubst subst
    = Disp.brackets . Disp.hcat
    . Disp.punctuate Disp.comma
    $ [ disp k <> Disp.char '=' <> disp v | (k,v) <- Map.toAscList subst ]

parseModSubst :: ReadP r ModuleSubst
parseModSubst = Parse.between (Parse.char '[') (Parse.char ']')
      . fmap mkModSubst
      . flip Parse.sepBy (Parse.char ',')
      $ do k <- parse
           _ <- Parse.char '='
           v <- parse
           return (k, v)

-- | For now, there is no distinction between component IDs
-- and unit IDs in Cabal.
data UnitId = SimpleUnitId ComponentId
            -- ^ Equivalent to @'UnitId' cid Map.empty@
            | UnitId ComponentId ModuleSubst
            | UnitIdVar !Int -- de Bruijn indexed
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

-- Here because we need it for getHSLibraryName
-- TODO: this is pretty inefficient!
-- TODO: write a spec for this, because we want this synchronized
-- with GHC
hashUnitId :: UnitId -> String
hashUnitId (SimpleUnitId cid) = display cid
hashUnitId (UnitIdVar i)      = show i -- this is never bare
hashUnitId (UnitId cid subst)
  | all (\(k, v) -> v == ModuleVar k) (modSubstToList subst)
     = display cid
  | otherwise
     = (\h -> display cid ++ "-" ++ h)
     . hashToBase62 $
        -- Similar to 'computeComponentId', it is safest to
        -- include the cid in the hash
           display cid ++ "\n" ++
           concat [ display mod_name ++ "=" ++ hashUnitId uid ++ ":" ++ display m  ++ "\n"
                  | (mod_name, Module uid m) <- modSubstToList subst]

instance Binary UnitId

instance NFData UnitId where
    rnf (SimpleUnitId cid) = rnf cid
    rnf (UnitId cid insts) = rnf cid `seq` rnf insts
    rnf (UnitIdVar i) = rnf i

instance Text UnitId where
    disp (SimpleUnitId cid) = disp cid
    disp (UnitIdVar i) = Disp.char '?' <> Disp.int i
    disp (UnitId cid insts) = disp cid <> dispModSubst insts

    parse = parseUnitIdVar <++ parseUnitId <++ parseSimpleUnitId
      where
        parseUnitIdVar = do _ <- Parse.char '?'
                            fmap UnitIdVar (readS_to_P reads)
        parseUnitId = do cid <- parse
                         insts <- parseModSubst
                         return (UnitId cid insts)
        parseSimpleUnitId = fmap SimpleUnitId parse

-- | Makes a simple-style UnitId from a string.
mkUnitId :: String -> UnitId
mkUnitId = SimpleUnitId . ComponentId

-- | Make an old-style UnitId from a package identifier
mkLegacyUnitId :: PackageId -> UnitId
mkLegacyUnitId = SimpleUnitId . ComponentId . display

-- ------------------------------------------------------------
-- * Package source dependencies
-- ------------------------------------------------------------

-- | Describes a dependency on a source package (API)
--
data Dependency = Dependency PackageName VersionRange
                  deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Binary Dependency

instance Text Dependency where
  disp (Dependency name ver) =
    disp name <+> disp ver

  parse = do name <- parse
             Parse.skipSpaces
             ver <- parse <++ return anyVersion
             Parse.skipSpaces
             return (Dependency name ver)

thisPackageVersion :: PackageIdentifier -> Dependency
thisPackageVersion (PackageIdentifier n v) =
  Dependency n (thisVersion v)

notThisPackageVersion :: PackageIdentifier -> Dependency
notThisPackageVersion (PackageIdentifier n v) =
  Dependency n (notThisVersion v)

-- | Simplify the 'VersionRange' expression in a 'Dependency'.
-- See 'simplifyVersionRange'.
--
simplifyDependency :: Dependency -> Dependency
simplifyDependency (Dependency name range) =
  Dependency name (simplifyVersionRange range)

-- | Class of things that have a 'PackageIdentifier'
--
-- Types in this class are all notions of a package. This allows us to have
-- different types for the different phases that packages go though, from
-- simple name\/id, package description, configured or installed packages.
--
-- Not all kinds of packages can be uniquely identified by a
-- 'PackageIdentifier'. In particular, installed packages cannot, there may be
-- many installed instances of the same source package.
--
class Package pkg where
  packageId :: pkg -> PackageIdentifier

packageName    :: Package pkg => pkg -> PackageName
packageName     = pkgName    . packageId

packageVersion :: Package pkg => pkg -> Version
packageVersion  = pkgVersion . packageId

instance Package PackageIdentifier where
  packageId = id

-- | Packages that have an installed package ID
class Package pkg => HasUnitId pkg where
  installedUnitId :: pkg -> UnitId

{-# DEPRECATED installedPackageId "Use installedUnitId instead" #-}
-- | Compatibility wrapper for Cabal pre-1.24.
installedPackageId :: HasUnitId pkg => pkg -> UnitId
installedPackageId = installedUnitId

-- | Class of installed packages.
--
-- The primary data type which is an instance of this package is
-- 'InstalledPackageInfo', but when we are doing install plans in Cabal install
-- we may have other, installed package-like things which contain more metadata.
-- Installed packages have exact dependencies 'installedDepends'.
class (HasUnitId pkg) => PackageInstalled pkg where
  installedDepends :: pkg -> [UnitId]

-- -----------------------------------------------------------------------------
-- ABI hash

newtype AbiHash = AbiHash String
    deriving (Eq, Show, Read, Generic)
instance Binary AbiHash

instance Text AbiHash where
    disp (AbiHash abi) = Disp.text abi
    parse = fmap AbiHash (Parse.munch Char.isAlphaNum)
