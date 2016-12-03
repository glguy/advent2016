module Main where

import           Data.Foldable (foldl')
import qualified Data.Map as Map
import           Data.Map (Map)

main :: IO ()
main =
  do cmds <- lines <$> readFile "input2.txt"
     putStrLn (go keys1 (1,1) cmds)
     putStrLn (go keys2 (3,3) cmds)

keys1 :: Map (Int,Int) Char
keys1 = Map.fromList
  [ ((0,0),'1'), ((1,0),'2'), ((2,0),'3')
  , ((0,1),'4'), ((1,1),'5'), ((2,1),'6')
  , ((0,2),'7'), ((1,2),'8'), ((2,2),'9')
  ]

keys2 :: Map (Int,Int) Char
keys2 = Map.fromList
  [                           ((2,0),'1')
  ,              ((1,1),'2'), ((2,1),'3'), ((3,1),'4')
  , ((0,2),'5'), ((1,2),'6'), ((2,2),'7'), ((3,2),'8'), ((4,2),'9')
  ,              ((1,3),'A'), ((2,3),'B'), ((3,3),'C')
  ,                           ((2,4),'D')
  ]

go :: Map (Int,Int) Char -> (Int,Int) -> [String] -> String
go ks start cmds =
  map (ks Map.!) (tail (scanl (process ks) start cmds))

process ::
  Map (Int,Int) Char {- ^ key pad           -} ->
  (Int,Int)          {- ^ starting position -} ->
  String             {- ^ command           -} ->
  (Int,Int)          {- ^ stopping position -}
process ks = foldl' aux
  where
    aux pos mov
      | Map.member pos' ks = pos'
      | otherwise          = pos
      where
        pos' = step pos mov

step :: (Int,Int) -> Char -> (Int,Int)
step (x,y) mov =
  case mov of
    'L' -> (x-1,y)
    'R' -> (x+1,y)
    'U' -> (x,y-1)
    'D' -> (x,y+1)
