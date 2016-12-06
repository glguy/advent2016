module Main where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Ord

main =
  do input <- lines <$> readFile "inputs/input6.txt"
     putStrLn (decode id   input)
     putStrLn (decode Down input)

decode :: Ord a => (Int -> a) -> [String] -> String
decode f xs = mostCommon f <$> transpose xs

mostCommon :: (Ord a, Ord b) => (Int -> b) -> [a] -> a
mostCommon f = fst . maximumBy (comparing (f . snd)) . tally

tally :: Ord a => [a] -> [(a,Int)]
tally xs = Map.toList (Map.fromListWith (+) [(x,1) | x <- xs])
