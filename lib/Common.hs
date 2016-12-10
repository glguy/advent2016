module Common where

import           System.Environment
import           Text.Megaparsec
import           Text.Megaparsec.String
import           Text.Megaparsec.Lexer (integer)

readInputFile :: Int -> IO String
readInputFile n =
  do args <- getArgs
     name <- case args of
               []    -> return ("inputs/input" ++ show n ++ ".txt")
               [arg] -> return arg
               _     -> fail "Too many arguments"
     readFile name

count :: (a -> Bool) -> [a] -> Int
count p xs = length (filter p xs)

parseOrDie :: Parser a -> String -> a
parseOrDie p str =
  case parse p "input" str of
    Left e   -> error (parseErrorPretty e)
    Right xs -> xs

parseLines :: Parser a -> String -> [a]
parseLines p = parseOrDie (many (p <* eol) <* eof)

number :: Num a => Parser a
number = fromInteger <$> integer
{-# SPECIALIZE number :: Parser Int #-}
{-# SPECIALIZE number :: Parser Integer #-}

bracketed :: Parser a -> Parser a
bracketed = between (char '[') (char ']')
