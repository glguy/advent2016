module Main where

import Data.Char
import Data.Monoid
import Data.Foldable
import Data.List
import Data.Ord
import Data.Map (Map)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Data.Map as Map

data Entry = Entry { roomName :: [String], sectorId :: Int, roomHash :: String }

hashLength :: Int
hashLength = 5

main =
  do input <- parseInput <$> readFile "input4.txt"
     let valid = filter isGoodEntry input
     print (sum (map sectorId valid))
     for_ valid $ \e ->
       putStrLn (decryptEntry e ++ " " ++ show (sectorId e))

decryptEntry :: Entry -> String
decryptEntry e = unwords (map (map (decrypt (sectorId e))) (roomName e))

isGoodEntry :: Entry -> Bool
isGoodEntry e = roomHash e == computeHash (concat (roomName e))

computeHash :: String -> String
computeHash x = take hashLength (map fst (sortBy ordering (Map.toList counts)))
  where
    counts = Map.fromListWith (+) [ (a,1) | a <- x ]
    ordering (xa,xn) (ya,yn)
       = compare yn xn -- descending
      <> compare xa ya -- ascending

parseInput :: String -> [Entry]
parseInput s =
  case parse (many parserEntry) "input" s of
    Left e -> error (show e)
    Right e -> e

parserEntry :: Parser Entry
parserEntry = Entry
  <$> some letterChar `endBy` char '-'
  <*> (read <$> some digitChar)
  <*  char '['
  <*> some letterChar
  <*  char ']'
  <*  eol

decrypt :: Int -> Char -> Char
decrypt n c = chr ((ord c - ord 'a' + n) `rem` 26 + ord 'a')
