{-# Language TransformListComp #-}
module Main where

import Data.Bits
import Data.List
import GridCoord
import Search (bfsOn)

myInput :: Int
myInput = 1350

data Entry = Entry { entrySteps :: !Int, entryCoord :: Coord }
  deriving (Eq, Show)

main :: IO ()
main =
  do let entries = bfsOn entryCoord nextEntries initialEntry
     print $ head [ steps | Entry steps (Coord 31 39) <- entries ]
     print $ length $ nub [ coord | Entry steps coord <- entries
                                  , then takeWhile by steps <= 50 ]

initialEntry :: Entry
initialEntry = Entry 0 (Coord 1 1)

isValidCoord :: Coord -> Bool
isValidCoord (Coord x y) =
  x >= 0 && y >= 0 &&
  even (popCount (x*x + 3*x + 2*x*y + y + y*y + myInput))

nextEntries :: Entry -> [Entry]
nextEntries (Entry steps coord)
  = [ Entry (steps+1) c | c <- cardinalNeighbors coord, isValidCoord c ]
