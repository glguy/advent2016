module Main where

import           Common
import           GridCoord

import           Data.List
import qualified Data.Set as Set
import           Text.Megaparsec
import           Text.Megaparsec.String

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
part1 = manhattanDistance origin . last

part2 :: [Coord] -> Maybe Int
part2 = fmap (manhattanDistance origin) . duplicate

computePath :: [Command] -> [Coord]
computePath = toCoords origin . toSteps north
  where
    north  = Vec 0 (-1)

-- | Find the first duplicate element in a list
duplicate :: Ord a => [a] -> Maybe a
duplicate = aux Set.empty
  where
    aux _    [] = Nothing
    aux seen (x:xs)
      | Set.member x seen = Just x
      | otherwise         = aux (Set.insert x seen) xs

-- | Compute steps taken by following a list of commands
toSteps ::
  Vec       {- ^ initial direction  -} ->
  [Command] {- ^ commands           -} ->
  [Vec]     {- ^ list of directions -}
toSteps dir0 cmds = concat (snd (mapAccumL aux dir0 cmds))
  where
    aux dir (Command lr steps) =
      let dir' = turn lr dir
      in (dir', replicate steps dir')

toCoords ::
  Coord   {- ^ origin -} ->
  [Vec]   {- ^ steps  -} ->
  [Coord] {- ^ path   -}
toCoords = scanl step

turn :: Char -> Vec -> Vec
turn 'L' (Vec dx dy) = Vec (-dy) dx
turn 'R' (Vec dx dy) = Vec dy (-dx)
turn c   _           = error ("Bad instruction: " ++ [c])

step :: Coord -> Vec -> Coord
step (Coord x y) (Vec dx dy) = Coord (x + dx) (y + dy)
