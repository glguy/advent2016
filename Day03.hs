module Main where

import Common
import Data.List
import Data.List.Split

main :: IO ()
main =
  do input <- parseInput <$> readInputFile 3
     print (count goodTriangle input)
     print (count goodTriangle (rearrange input))

parseInput :: String -> [[Int]]
parseInput = chunksOf 3 . map read . words

rearrange :: [[a]] -> [[a]]
rearrange = chunksOf 3 . concat . transpose

goodTriangle :: [Int] -> Bool
goodTriangle xs = x + y > z
  where
    [x,y,z] = sort xs
