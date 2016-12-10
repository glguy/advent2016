module Main where

import Common
import Data.Char
import Data.Monoid
import Data.Foldable
import Data.List
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Data.Map as Map

data Entry = Entry { roomName :: [String], sectorId :: Int, roomHash :: String }

hashLength :: Int
hashLength = 5

main :: IO ()
main =
  do input <- parseInput <$> readInputFile 4
     let valid = filter isGoodEntry input
     print (sum (map sectorId valid))
     for_ valid $ \e ->
       putStrLn $ show (sectorId e)
               ++ ": "
               ++ decryptEntry e

decryptEntry :: Entry -> String
decryptEntry e = unwords (map (map (decrypt (sectorId e))) (roomName e))

isGoodEntry :: Entry -> Bool
isGoodEntry e = roomHash e == computeHash (concat (roomName e))

computeHash :: String -> String
computeHash x = take hashLength (map fst (sortBy ordering (Map.toList counts)))
  where
    counts = Map.fromListWith (+) [ (a,1::Int) | a <- x ]
    ordering (xa,xn) (ya,yn)
       = compare yn xn -- descending
      <> compare xa ya -- ascending

parseInput :: String -> [Entry]
parseInput s =
  case parse parserEntries "input" s of
    Left  e -> error (show e)
    Right e -> e

parserEntries :: Parser [Entry]
parserEntries = many parserEntry <* eof

parserEntry :: Parser Entry
parserEntry = Entry
  <$> some letterChar `endBy` char '-'
  <*> parserNumber
  <*> bracketed (some letterChar)
  <*  eol

parserNumber :: Num a => Parser a
parserNumber = fromInteger . read <$> some digitChar

bracketed :: Parser a -> Parser a
bracketed = between (char '[') (char ']')

decrypt :: Int -> Char -> Char
decrypt n c = chr ((ord c - ord 'a' + n) `mod` 26 + ord 'a')
