-- Strings are inefficient but it got the job done in time
module Main where

import Data.List

myInput :: String
myInput = "01111001100111011"

part1, part2 :: Int
part1 = 272
part2 = 35651584

main :: ()
main =
  do putStrLn (checksum (take part1 (filler myInput)))
     putStrLn (checksum (take part2 (filler myInput)))
     return ()

toggle :: Char -> Char
toggle '0' = '1'
toggle '1' = '0'

filler :: String -> String
filler seed = seed ++ expand seed

expand :: String -> String
expand sofar = sofar' ++ expand (sofar ++ sofar')
  where
    sofar' = '0' : map toggle (reverse sofar)

checksum :: String -> String
checksum xs = checksum' (length xs) xs

checksum' n xs
  | odd n = xs
  | otherwise = checksum' (n`quot`2) (reduce xs)

reduce (x:y:xs)
  | x == y    = '1' : reduce xs
  | otherwise = '0' : reduce xs
reduce _ = []
