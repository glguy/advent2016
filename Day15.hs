module Main where

import Data.List (find)

data Disc = Disc !Int !Int

main :: IO ()
main =
  do print (findTime input1)
     print (findTime input2)

findTime :: [Disc] -> Maybe Int
findTime input = find (\i -> all (passesDiscAt i) (zip [1..] input)) [0..]

passesDiscAt :: Int -> (Int, Disc) -> Bool
passesDiscAt i (t,Disc posN pos0) = (pos0 + i + t) `rem` posN == 0

input1, input2 :: [Disc]
input1 =
  [ Disc 13  1
  , Disc 19 10
  , Disc  3  2
  , Disc  7  1
  , Disc  5  3
  , Disc 17  5
  ]
input2 = input1 ++ [Disc 11 0]
