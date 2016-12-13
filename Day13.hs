{-# Language TransformListComp #-}
module Main where

import Data.Bits
import Data.List
import Search (bfs)

myInput :: Int
myInput = 1350

data Entry = Entry !Int !Int !Int
  deriving (Eq, Ord, Show)

main :: IO ()
main =
  do print $ head [ steps | Entry steps 31 39 <- bfs nextEntries initialEntry ]
     print $ length $ nub [ (x,y) | Entry steps x y <- bfs nextEntries initialEntry
                                  , then takeWhile by steps <= 50 ]

initialEntry :: Entry
initialEntry = Entry 0 1 1

isValidEntry :: Entry -> Bool
isValidEntry (Entry _ x y) =
  x >= 0 && y >= 0 &&
  even (popCount ( x*x + 3*x + 2*x*y + y + y*y + myInput ))

nextEntries :: Entry -> [Entry]
nextEntries (Entry steps x y)
  = filter isValidEntry
   [ Entry (steps+1) (x+1) y
   , Entry (steps+1) (x-1) y
   , Entry (steps+1) x (y+1)
   , Entry (steps+1) x (y-1)
   ]
