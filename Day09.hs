{-# Language LambdaCase #-}
module Main where

import Common
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main =
  do xs <- lines <$> readInputFile 9
     print (sum (map decode1 xs))
     print (sum (map decode2 xs))

decode1 :: String -> Int
decode1 = mkDecode (\n xs -> n * length xs)

decode2 :: String -> Int
decode2 = mkDecode (\n xs -> n * decode2 xs)

mkDecode ::
  (Int -> String -> Int) {- ^ repeated segment logic -} ->
  String                 {- ^ input string           -} ->
  Int                    {- ^ decoded length         -}
mkDecode f = parseOrDie (sum <$> many (plain <|> repeated))
  where
    plain = length <$> some (satisfy (/='('))
    repeated =
       do len <- char '(' *> number <* char 'x'
          f <$> number <* char ')' <*> replicateM len anyChar
