module Main where

import Data.List
import Data.List.Split

main :: IO ()
main =
  do input <- parseInput <$> readFile "inputs/input3.txt"
     print (count goodTriangle input)
     print (count goodTriangle (rearrange input))

count :: (a -> Bool) -> [a] -> Int
count p xs = length (filter p xs)

parseInput :: String -> [[Int]]
parseInput = chunksOf 3 . map read . words

rearrange :: [[a]] -> [[a]]
rearrange = chunksOf 3 . concat . transpose

goodTriangle :: [Int] -> Bool
goodTriangle xs = x + y > z
  where
    [x,y,z] = sort xs
