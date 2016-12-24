{-# Language BangPatterns #-}
module Main where

import           Common
import           Data.Char
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Set ( Set )
import qualified Data.Set as Set
import           GridCoord
import           Search

parseInput :: String -> Map Coord Char
parseInput file =
  Map.fromList [ (Coord row col, x)
                   | (row,r) <- [0..] `zip` lines file
                   , (col,x) <- [0..] `zip` r
                   , x /= '#' ]

main :: IO ()
main =
  do maze <- parseInput <$> readInputFile 24

     let targets = count isDigit (Map.elems maze)
         [start] = [ c | (c,x) <- Map.toList maze, x == '0' ]

         endings =
           [ (here,steps)
              | (seen,here,steps) <-
                    bfsOn
                      (\(seen,here,_steps) -> (here,seen))
                      (next maze)
                      (Set.singleton '0',start,0)
              , Set.size seen == targets ]

     print $ head [ steps | (_  ,steps) <- endings ]
     print $ head [ steps | (end,steps) <- endings, end == start ]

next ::
  Map Coord Char         ->
  (Set Char, Coord, Int) ->
  [(Set Char, Coord, Int)]
next maze (seen,here,steps) =
  [ (seen',here',steps')
    | let !steps' = steps + 1
    , here'  <- cardinalNeighbors here
    , Just x <- [Map.lookup here' maze]
    , let !seen' | isDigit x = Set.insert x seen
                 | otherwise = seen
    ]
