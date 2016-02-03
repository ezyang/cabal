{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
module Distribution.Utils.UnionFind (
    Point,
    fresh,
    find,
    union,
    equivalent,
) where

import Data.STRef
import Control.Monad
import Control.Monad.ST

-- Based off of the implementation in "The Essence of ML Type Inference".
-- (N.B. union-find is also based off of this.)

newtype Point s a = Point (STRef s (Link s a))
    deriving (Eq)

writePoint :: Point s a -> Link s a -> ST s ()
writePoint (Point v) = writeSTRef v

readPoint :: Point s a -> ST s (Link s a)
readPoint (Point v) = readSTRef v

data Link s a
    -- NB: it is too bad we can't say STRef Int#; the weights remain boxed
    = Info {-# UNPACK #-} !(STRef s Int) {-# UNPACK #-} !(STRef s a)
    | Link {-# UNPACK #-} !(Point s a)

fresh :: a -> ST s (Point s a)
fresh desc = do
    weight <- newSTRef 1
    descriptor <- newSTRef desc
    Point `fmap` newSTRef (Info weight descriptor)

repr :: Point s a -> ST s (Point s a)
repr point = readPoint point >>= \case
    Link point' -> do
        point'' <- repr point'
        when (point'' /= point') $ do
            writePoint point =<< readPoint point'
        return point''
    Info _ _ -> return point

find :: Point s a -> ST s a
find point =
    -- Optimize length 0 and 1 case at expense of
    -- general case
    readPoint point >>= \case
        Info _ d_ref -> readSTRef d_ref
        Link point' -> readPoint point' >>= \case
            Info _ d_ref -> readSTRef d_ref
            Link _ -> repr point >>= find

-- Keeps the descriptor of point2
union :: Point s a -> Point s a -> ST s ()
union point1 point2 = do
    point1 <- repr point1
    point2 <- repr point2
    when (point1 /= point2) $ do
    l1 <- readPoint point1
    l2 <- readPoint point2
    case (l1, l2) of
        (Info wref1 dref1, Info wref2 dref2) -> do
            weight1 <- readSTRef wref1
            weight2 <- readSTRef wref2
            -- Should be able to optimize the == case separately
            if weight1 >= weight2
                then do
                    writePoint point2 (Link point1)
                    -- The weight calculation here seems a bit dodgy
                    writeSTRef wref1 (weight1 + weight2)
                    writeSTRef dref1 =<< readSTRef dref2
                else do
                    writePoint point1 (Link point2)
                    writeSTRef wref2 (weight1 + weight2)
        _ -> error "UnionFind.union: repr invariant broken"

equivalent :: Point s a -> Point s a -> ST s Bool
equivalent point1 point2 = liftM2 (==) (repr point1) (repr point2)
