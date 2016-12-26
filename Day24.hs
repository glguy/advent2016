{-# Language BangPatterns #-}
module Main where

import           Common
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Maybe
import           GridCoord
import           Search
import qualified Data.Array.Unboxed as Array
import           Data.Array.Unboxed (Array)

type DigitSet = Int

data Entry = Entry {-# UNPACK #-} !Coord !DigitSet
  deriving (Eq, Ord)

parseInput :: String -> Array Coord Char
parseInput file =
  coordArray '#'
     [ (Coord row col, x)
             | (row,r) <- [0..] `zip` lines file
             , (col,x) <- [0..] `zip` r
             ]

main :: IO ()
main =
  do maze <- parseInput <$> readInputFile 24

     let targets =
           foldl' setBit 0
                  $ mapMaybe digitToInt'
                  $ Array.elems maze

         [start] = [ c | (c,x) <- Array.assocs maze, x == '0' ]

         endings =
           [ (here,steps)
              | (seen,here,steps) <-
                    bfsOn
                      (\(seen,here,_steps) -> Entry here seen)
                      (next maze)
                      (bit 0, start,0)
              , seen == targets ]

     print $ head [ steps | (_  ,steps) <- endings ]
     print $ head [ steps | (end,steps) <- endings, end == start ]

next ::
  Array Coord Char         ->
  (DigitSet, Coord, Int) ->
  [(DigitSet, Coord, Int)]
next maze (seen,here,steps) =
  [ (seen',here',steps')
    | let !steps' = steps + 1
    , here' <- cardinalNeighbors here
    , let x = maze Array.! here'
    , x /= '#'
    , let !seen' = case digitToInt' x of
                     Just i -> setBit seen i
                     Nothing -> seen
    ]

digitToInt' :: Char -> Maybe Int
digitToInt' x
  | isDigit x = Just (digitToInt x)
  | otherwise = Nothing
