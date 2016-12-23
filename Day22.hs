{-# Language RecordWildCards #-}
module Main where

import Common
import Control.Monad
import Data.Map ( Map )
import GridCoord
import Search
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Data.Map as Map

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

parseInput :: String -> Map Coord Node
parseInput
  = Map.filter (\n -> nodeUsed n < cutoff)
  . Map.fromList . parseLines parseNode
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
             [ cost | (SearchState (Coord 0 0) _, cost)
                        <- astar (next grid) (SearchState start hole)
                    ]

viable grid = length
  [() | (c1,n1) <- Map.toList grid
      , (c2,n2) <- Map.toList grid
      , c1 /= c2
      , nodeUsed n1 /= 0
      , nodeUsed n1 <= nodeSize n2 - nodeUsed n2 ]

findStart grid = maximum [ Coord x 0 | Coord x 0 <- Map.keys grid ]
findHole  grid = head [ c | (c,n) <- Map.toList grid, nodeUsed n == 0 ]

data SearchState = SearchState
  { searchGoal, searchHole :: !Coord }
  deriving (Eq, Ord, Read, Show)

next :: Map Coord Node -> SearchState -> [(SearchState, Int, Int)]
next grid SearchState{..} =
  [ (SearchState newGoal newHole,1,h)
     | newHole <- cardinalNeighbors searchHole
     , Map.member newHole grid
     , let newGoal
             | searchGoal == newHole = searchHole
             | otherwise             = searchGoal

           h = manhattanDistance newGoal
             + manhattanDistance (diff newHole newGoal)
             - 1 ]


diff :: Coord -> Coord -> Coord
diff (Coord x1 y1) (Coord x2 y2) = Coord (x1-x2) (y1-y2)
