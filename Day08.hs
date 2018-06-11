{-# Language TupleSections #-}
module Main (main) where

import Common
import Control.Concurrent
import Control.Monad
import Data.Array.IO
import Data.Foldable
import Text.Megaparsec
import Text.Megaparsec.Char

rows, cols :: Int
rows = 6
cols = 50

data Command
  = Rect      !Int !Int
  | RotateCol !Int !Int
  | RotateRow !Int !Int
  deriving Show

main :: IO ()
main =
  do xs <- readInputFile 8
     interp (parseLines parser xs)

interp :: [Command] -> IO ()
interp cmds =
  do a <- newArray ((0,0),(cols-1,rows-1)) False
          :: IO (IOUArray (Int,Int) Bool)

     for_ cmds $ \cmd ->
       do interpCommand a cmd
          print cmd
          drawScreen a
          threadDelay 25000

     n <- countPixels a
     putStrLn ("Pixels: " ++ show n)

drawScreen :: IOUArray (Int,Int) Bool -> IO ()
drawScreen a =
  for_ [0..5] $ \y ->
    do xs <- traverse (\x -> readArray a (x,y)) [0..cols-1]
       putStrLn (map toBlock xs)

countPixels :: IOUArray (Int,Int) Bool -> IO Int
countPixels a =
  do xs <- getElems a
     return $! length (filter id xs)

toBlock :: Bool -> Char
toBlock True  = '█'
toBlock False = ' '

interpCommand :: IOUArray (Int,Int) Bool -> Command -> IO ()
interpCommand a (Rect xn yn) =
  for_ [0 .. xn-1] $ \x ->
  for_ [0 .. yn-1] $ \y ->
  writeArray a (x,y) True

interpCommand a (RotateCol x n) = rotate a (x,) rows n
interpCommand a (RotateRow y n) = rotate a (,y) cols n

rotate :: (Ix i, MArray a e m) => a i e -> (Int -> i) -> Int -> Int -> m ()
rotate a f len n =
  do reverseRange a f 0 (len-1-n)
     reverseRange a f (len-n) (len-1)
     reverseRange a f 0 (len-1)

reverseRange :: (Ix i, MArray a e m) => a i e -> (Int -> i) -> Int -> Int -> m ()
reverseRange a f lo hi =
  when (lo < hi) $
    do swap a (f lo) (f hi)
       reverseRange a f (lo+1) (hi-1)

swap :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swap a i j =
  do t <- readArray a i
     writeArray a i =<< readArray a j
     writeArray a j t

parser :: Parser Command
parser =
  RotateRow <$ wholestring "rotate row y="    <*> number <* string " by " <*> number <|>
  RotateCol <$ wholestring "rotate column x=" <*> number <* string " by " <*> number <|>
  Rect      <$ string "rect " <*> number <* char 'x' <*> number
