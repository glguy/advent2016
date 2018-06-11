module Main where

import Common                 (Parser, parseLines, readInputFile)
import Data.List              (sort)
import Text.Megaparsec.Char   (char)
import Text.Megaparsec.Char.Lexer (decimal)

type Blacklist = [(Integer,Integer)]

parseEntry :: Parser (Integer,Integer)
parseEntry = (,) <$> decimal <* char '-' <*> decimal

main :: IO ()
main =
  do blacklist <- processBlacklist . parseLines parseEntry <$> readInputFile 20
     print (lowest     blacklist)
     print (countValid blacklist)

-- | Remove overlaps and ensure that the blacklist surrounds
-- the whole address space.
processBlacklist :: Blacklist -> Blacklist
processBlacklist xs =
  removeOverlap (sort ([(-1,-1),(2^32,2^32)] ++ xs))

lowest :: Blacklist -> Integer
lowest ((_,hi):_) = hi+1
lowest _          = 0

removeOverlap :: Blacklist -> Blacklist
removeOverlap ((lo1,hi1):(lo2,hi2):rest)
  | hi1 >= lo2-1 = removeOverlap ((lo1, max hi1 hi2) : rest)
removeOverlap (x:xs) = x : removeOverlap xs
removeOverlap [] = []

countValid :: Blacklist -> Integer
countValid xs = sum (zipWith gapSize xs (tail xs))
  where
    gapSize (_,hi1) (lo2,_) = lo2 - hi1 - 1
