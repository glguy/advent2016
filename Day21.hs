module Main where

import Common
import Text.Megaparsec
import Text.Megaparsec.String
import Data.List

data Scramble
  = RotateRight Int
  | RotateLeft Int
  | SwapPosition Int Int
  | SwapLetter Char Char
  | RotateChar Char
  | ReversePositions Int Int
  | MovePosition Int Int
  deriving Show

parseScramble :: Parser Scramble
parseScramble =
  RotateRight 1    <$ wholestring "rotate right 1 step"                 <|>
  RotateRight      <$ wholestring "rotate right "                       <*> number     <* string " steps"          <|>
  RotateLeft 1     <$ wholestring "rotate left 1 step"                  <|>
  RotateLeft       <$ wholestring "rotate left "                        <*> number     <* string " steps"          <|>
  SwapPosition     <$ wholestring "swap position "                      <*> number     <* string " with position " <*> number     <|>
  SwapLetter       <$ wholestring "swap letter "                        <*> letterChar <* string " with letter "   <*> letterChar <|>
  RotateChar       <$ wholestring "rotate based on position of letter " <*> letterChar <|>
  ReversePositions <$ wholestring "reverse positions "                  <*> number     <* string " through "       <*> number     <|>
  MovePosition     <$ wholestring "move position "                      <*> number     <* string " to position "   <*> number

part1, part2 :: String
part1 = "abcdefgh"
part2 = "fbgdceah"

main =
  do inp <- parseLines parseScramble <$> readInputFile 21
     putStrLn $ foldl (flip forward) part1 inp
     putStrLn $ foldr backward part2 inp

rotateRight :: Int -> [a] -> [a]
rotateRight n xs = b ++ a
  where
    n' = n `mod` length xs
    (a,b) = splitAt (length xs - n') xs

rotateLeft :: Int -> [a] -> [a]
rotateLeft n xs = b ++ a
  where
    n' = n `mod` length xs
    (a,b) = splitAt (n') xs

set :: Int -> a -> [a] -> [a]
set i x xs = a ++ [x] ++ b
  where
    (a,_:b) = splitAt i xs

forward scram =
  case scram of
    RotateRight i -> rotateRight i
    RotateLeft  i -> rotateLeft i
    SwapPosition i j -> \xs -> set i (xs!!j)
                             $ set j (xs!!i) xs
    SwapLetter x y -> map $ \a -> if a == x then y else if a == y then x else a
    RotateChar e -> rotatePositionOf e
    ReversePositions i j -> reverseRange i j
    MovePosition i j -> movePosition i j

backward scram =
  case scram of
    RotateRight i -> rotateLeft i
    RotateLeft  i -> rotateRight i
    SwapPosition i j -> \xs -> set i (xs!!j)
                             $ set j (xs!!i) xs
    SwapLetter x y -> map $ \a -> if a == x then y else if a == y then x else a
    RotateChar e -> \xs ->
        let Just r = find (\a -> rotatePositionOf e a == xs)
                   $ map (`rotateRight` xs) [0..]
        in r
    ReversePositions i j -> reverseRange i j
    MovePosition i j -> movePosition j i

rotatePositionOf e xs = rotateRight j xs
  where
    Just i = elemIndex e xs
    j | i >=4     = i + 2
      | otherwise = i + 1

reverseRange i j xs = a ++ reverse c ++ d
  where
    (a,b) = splitAt i xs
    (c,d) = splitAt (j-i+1) b

movePosition i j xs = c ++ [x] ++ d
  where
    (a,x:b) = splitAt i xs
    (c,d) = splitAt j (a++b)
