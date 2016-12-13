module GridCoord where

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
