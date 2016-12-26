module GridCoord where

import           GHC.Arr
import           Data.Foldable
import           Control.Monad
import qualified Data.Array as Array
import           Data.Array as Array

data Coord = Coord !Int !Int
  deriving (Read, Show, Ord, Eq)

cardinalNeighbors :: Coord -> [Coord]
cardinalNeighbors (Coord x y) =
  [ Coord (x+1) y
  , Coord (x-1) y
  , Coord x (y+1)
  , Coord x (y-1)
  ]

manhattanDistance :: Coord -> Int
manhattanDistance (Coord x y) = abs x + abs y
{-# INLINE manhattanDistance #-}

instance Ix Coord where
  range (Coord loX loY, Coord hiX hiY)
     = [ Coord x y | x <- [loX..hiX], y <- [loY..hiY] ]
  inRange (Coord loX loY, Coord hiX hiY) (Coord x y)
     = loX <= x && x <= hiX && loY <= y && y <= hiY
  unsafeIndex (Coord loX loY, Coord hiX hiY) (Coord x y)
     = x + y * (hiX - loX + 1)

coordArray :: a -> [(Coord, a)] -> Array Coord a
coordArray def xs =
  accumArray (\_ new -> new) def (minB, maxB) xs
  where
    minB = foldl' (liftCoord2 min) (Coord 0 0)       (map fst xs)
    maxB = foldl' (liftCoord2 max) (Coord (-1) (-1)) (map fst xs)

indexArray :: (Ix i, MonadPlus m) => Array i a -> i -> m a
indexArray a i
  | inRange (bounds a) i = return $! unsafeAt a (unsafeIndex (bounds a) i)
  | otherwise            = mzero
{-# INLINE indexArray #-}

liftCoord2 :: (Int -> Int -> Int) -> Coord -> Coord -> Coord
liftCoord2 f (Coord x y) (Coord u v) = Coord (f x u) (f y v)
