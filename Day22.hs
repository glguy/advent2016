{-# Language RecordWildCards #-}
module Main where

import Common
import Control.Monad
import GridCoord
import Search
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Array (Array)
import qualified Data.Array as Array

data Node = Node { nodeSize, nodeUsed :: !Int }
  deriving (Show)

parseNode :: Parser (Coord, Node)
parseNode =
  do x <- string "/dev/grid/node-x" *> number
     y <- string "-y" *> number <* space
     nodeSize <- number <* char 'T' <* space
     nodeUsed <- number <* char 'T' <* space
     _avail   <- number <* char 'T' <* space
     _percent <- number <* char '%'
     return (Coord x y, Node{..})

parseInput :: String -> Array Coord Node
parseInput
  = coordArray (error "hole in grid") . parseLines parseNode
  . unlines . drop 2 . lines -- header lines

-- Hardcode a constant I computed for my particular input.
-- This problem doesn't favor a general solution, so I didn't
-- bother encoding my analysis of what constitutes an immobile
-- node into my program
cutoff = 100

main :: IO ()
main =
  do grid <- parseInput <$> readInputFile 22
     let start = findStart grid
         hole  = findHole grid
     print $ viable grid
     print $ head
             [ cost | (ss, cost) <- astar (next grid) (SearchState start hole)
                    , searchGoal ss == origin
                    ]

viable grid = length
  [() | (c1,n1) <- Array.assocs grid
      , (c2,n2) <- Array.assocs grid
      , c1 /= c2
      , nodeUsed n1 /= 0
      , nodeUsed n1 <= nodeSize n2 - nodeUsed n2 ]

findStart grid =
  maximum [ Coord x 0 | Coord x 0 <- Array.range (Array.bounds grid) ]

findHole grid = head [ c | (c,n) <- Array.assocs grid, nodeUsed n == 0 ]

data SearchState = SearchState
  { searchGoal, searchHole :: !Coord }
  deriving (Eq, Ord, Read, Show)

next :: Array Coord Node -> SearchState -> [(SearchState, Int, Int)]
next grid SearchState{..} =
  [ (SearchState newGoal newHole,1,h)
     | newHole <- cardinalNeighbors searchHole
     , node    <- indexArray grid newHole
     , nodeSize node < cutoff
     , let newGoal
             | searchGoal == newHole = searchHole
             | otherwise             = searchGoal

           h = manhattanDistance newGoal origin
             + manhattanDistance newHole newGoal
             - 1 ]
