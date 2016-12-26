module Main where

import Common
import Data.Foldable (foldl')
import Data.Array
import GridCoord

main :: IO ()
main =
  do cmds <- lines <$> readInputFile 2
     putStrLn (computeCode keys1 cmds)
     putStrLn (computeCode keys2 cmds)

keys1 :: Array Coord Char
keys1 = listArray (Coord (-1) (-1), Coord 1 1)
  "123\
  \456\
  \789"

keys2 :: Array Coord Char
keys2 = listArray (Coord (-2) (-2), Coord 2 2)
  "..1..\
  \.234.\
  \56789\
  \.ABC.\
  \..D.."

computeCode :: Array Coord Char -> [String] -> String
computeCode ks cmds = map (ks!) (tail (scanl (process ks) origin cmds))

process ::
  Array Coord Char {- ^ key pad           -} ->
  Coord            {- ^ starting position -} ->
  String           {- ^ command           -} ->
  Coord            {- ^ stopping position -}
process ks = foldl' aux
  where
    aux pos mov
      | isValid ks pos' = pos'
      | otherwise       = pos
      where
        pos' = step pos mov

isValid :: Array Coord Char -> Coord -> Bool
isValid ks i = maybe False (/= '.') (indexArray ks i)

step :: Coord -> Char -> Coord
step (Coord x y) mov =
  case mov of
    'L' -> Coord (x-1) y
    'R' -> Coord (x+1) y
    'U' -> Coord x (y-1)
    'D' -> Coord x (y+1)
    _   -> error ("Bad move: " ++ [mov])
