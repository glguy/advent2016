module Main where

import Data.Foldable (foldl')
import Data.Array

main :: IO ()
main =
  do cmds <- lines <$> readFile "inputs/input2.txt"
     putStrLn (computeCode keys1 cmds)
     putStrLn (computeCode keys2 cmds)

keys1 :: Array (Int,Int) Char
keys1 = listArray ((-1,-1),(1,1))
  "123\
  \456\
  \789"

keys2 :: Array (Int,Int) Char
keys2 = listArray ((-2,-2),(2,2))
  "..1..\
  \.234.\
  \56789\
  \.ABC.\
  \..D.."

computeCode :: Array (Int,Int) Char -> [String] -> String
computeCode ks cmds = map (ks!) (tail (scanl (process ks) (0,0) cmds))

process ::
  Array (Int,Int) Char {- ^ key pad           -} ->
  (Int,Int)            {- ^ starting position -} ->
  String               {- ^ command           -} ->
  (Int,Int)            {- ^ stopping position -}
process ks = foldl' aux
  where
    aux pos mov
      | isValid ks pos' = pos'
      | otherwise       = pos
      where
        pos' = step pos mov

isValid :: Array (Int,Int) Char -> (Int,Int) -> Bool
isValid ks i = inRange (bounds ks) i && ks ! i /= '.'

step :: (Int,Int) -> Char -> (Int,Int)
step (r,c) mov =
  case mov of
    'L' -> (r,c-1)
    'R' -> (r,c+1)
    'U' -> (r-1,c)
    'D' -> (r+1,c)
