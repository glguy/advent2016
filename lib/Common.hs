{-# Language BangPatterns #-}
module Common where

import           Control.Monad.State
import           System.Environment
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)
import           Data.Void

type Parser = Parsec Void String

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
  case parse p str str of
    Left e   -> error (errorBundlePretty e)
    Right xs -> xs

parseLines :: Parser a -> String -> [a]
parseLines p = parseOrDie (many (p <* eol) <* eof)

number :: Num a => Parser a
number =
   char '-' *> (negate <$> number) <|>
   fromInteger <$> decimal
{-# SPECIALIZE number :: Parser Int #-}
{-# SPECIALIZE number :: Parser Integer #-}

bracketed :: Parser a -> Parser a
bracketed = between (char '[') (char ']')

parenthesized :: Parser a -> Parser a
parenthesized = between (char '(') (char ')')

wholestring :: String -> Parser String
wholestring = try . string

strictState :: MonadState s m => m a -> m a
strictState m = do !_ <- get; m
{-# INLINE strictState #-}
