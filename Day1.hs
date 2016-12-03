module Main where

import           Data.List.Split
import qualified Data.Set as Set
import           Data.Set (Set)

data Pos = Pos !Integer !Integer
  deriving (Eq, Ord, Show, Read)

parseInput :: String -> [(Char,Integer)]
parseInput xs = [ (x, read xs) | x:xs <- splitOn ", " xs ]

main :: IO ()
main =
  do xs <- parseInput <$> readFile "input1.txt"
     let locs = walk (Pos 0 0) (Pos 0 1) xs
     print (distance (last locs))
     print (distance <$> duplicate locs)

duplicate :: [Pos] -> Maybe Pos
duplicate = aux Set.empty
  where
    aux seen [] = Nothing
    aux seen (x:xs)
      | Set.member x seen = Just x
      | otherwise         = aux (Set.insert x seen) xs

distance :: Pos -> Integer
distance (Pos x y) = abs x + abs y

walk :: Pos -> Pos -> [(Char,Integer)] -> [Pos]
walk loc dir [] = [loc]
walk loc dir ((a,b):steps) = wander loc dir' b steps
  where
    dir' = turn a dir

wander :: Pos -> Pos -> Integer -> [(Char,Integer)] -> [Pos]
wander loc dir 0 steps = walk loc dir steps
wander loc dir n steps = loc : wander loc' dir (n-1) steps
  where
    loc' = step dir loc

turn :: Char -> Pos -> Pos
turn 'L' (Pos dx dy) = Pos (-dy) dx
turn 'R' (Pos dx dy) = Pos dy (-dx)

step :: Pos -> Pos -> Pos
step (Pos dx dy) (Pos x y) = Pos (x + dx) (y + dy)
