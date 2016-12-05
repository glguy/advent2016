module Main where

import           Data.List
import           Data.List.Split
import qualified Data.Set as Set
import           Data.Set (Set)

-- | Positions on the grid
data Pos = Pos !Int !Int
  deriving (Eq, Ord, Show, Read)

-- | Vectors used for incremental steps
data Vec = Vec !Int !Int
  deriving (Eq, Ord, Show, Read)

data Command = Command !Char !Int

parseInput :: String -> [Command]
parseInput xs = [ Command x (read xs) | x:xs <- splitOn ", " xs ]

main :: IO ()
main =
  do cmds <- parseInput <$> readFile "inputs/input1.txt"
     let path = computePath cmds
     print (part1 path)
     print (part2 path)

-- | Given a list of steps determine the ultimate Manhattan-distance from
-- the starting position.
part1 :: [Pos] -> Int
part1 = distance . last

part2 :: [Pos] -> Maybe Int
part2 = fmap distance . duplicate

computePath :: [Command] -> [Pos]
computePath = toPositions origin . toSteps north
  where
    origin = Pos 0 0
    north  = Vec 0 1

-- | Find the first duplicate element in a list
duplicate :: Ord a => [a] -> Maybe a
duplicate = aux Set.empty
  where
    aux _    [] = Nothing
    aux seen (x:xs)
      | Set.member x seen = Just x
      | otherwise         = aux (Set.insert x seen) xs

distance :: Pos -> Int
distance (Pos x y) = abs x + abs y

toSteps ::
  Vec       {- ^ initial direction  -} ->
  [Command] {- ^ commands           -} ->
  [Vec]     {- ^ list of directions -}
toSteps dir0 cmds = concat (snd (mapAccumL aux dir0 cmds))
  where
    aux dir (Command lr steps) =
      let dir' = turn lr dir
      in (dir', replicate steps dir')

toPositions ::
  Pos   {- ^ origin -} ->
  [Vec] {- ^ steps  -} ->
  [Pos] {- ^ path   -}
toPositions = scanl step

turn :: Char -> Vec -> Vec
turn 'L' (Vec dx dy) = Vec (-dy) dx
turn 'R' (Vec dx dy) = Vec dy (-dx)
turn c   _           = error ("Bad instruction: " ++ [c])

step :: Pos -> Vec -> Pos
step (Pos x y) (Vec dx dy) = Pos (x + dx) (y + dy)
