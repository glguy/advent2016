module Main where

import           Common

import           Data.List
import qualified Data.Set as Set
import           Text.Megaparsec
import           Text.Megaparsec.String

-- | Coorditions on the grid
data Coord = Coord !Int !Int
  deriving (Eq, Ord, Show, Read)

-- | Vectors used for incremental steps
data Vec = Vec !Int !Int
  deriving (Eq, Ord, Show, Read)

data Command = Command !Char !Int

parser :: Parser [Command]
parser = (Command <$> oneOf "LDRU" <*> number) `sepBy` string ", "

main :: IO ()
main =
  do cmds <- parseOrDie parser <$> readInputFile 1
     let path = computePath cmds
     print (part1 path)
     print (part2 path)

-- | Given a list of steps determine the ultimate Manhattan-distance from
-- the starting position.
part1 :: [Coord] -> Int
part1 = distance . last

part2 :: [Coord] -> Maybe Int
part2 = fmap distance . duplicate

computePath :: [Command] -> [Coord]
computePath = toCoorditions origin . toSteps north
  where
    origin = Coord 0 0
    north  = Vec 0 1

-- | Find the first duplicate element in a list
duplicate :: Ord a => [a] -> Maybe a
duplicate = aux Set.empty
  where
    aux _    [] = Nothing
    aux seen (x:xs)
      | Set.member x seen = Just x
      | otherwise         = aux (Set.insert x seen) xs

distance :: Coord -> Int
distance (Coord x y) = abs x + abs y

toSteps ::
  Vec       {- ^ initial direction  -} ->
  [Command] {- ^ commands           -} ->
  [Vec]     {- ^ list of directions -}
toSteps dir0 cmds = concat (snd (mapAccumL aux dir0 cmds))
  where
    aux dir (Command lr steps) =
      let dir' = turn lr dir
      in (dir', replicate steps dir')

toCoorditions ::
  Coord   {- ^ origin -} ->
  [Vec] {- ^ steps  -} ->
  [Coord] {- ^ path   -}
toCoorditions = scanl step

turn :: Char -> Vec -> Vec
turn 'L' (Vec dx dy) = Vec (-dy) dx
turn 'R' (Vec dx dy) = Vec dy (-dx)
turn c   _           = error ("Bad instruction: " ++ [c])

step :: Coord -> Vec -> Coord
step (Coord x y) (Vec dx dy) = Coord (x + dx) (y + dy)
