module Main where

import Data.Array.IO
import Data.Foldable
import Control.Concurrent
import Control.Monad

data Command
  = Rect      !Int !Int
  | RotateCol !Int !Int
  | RotateRow !Int !Int
  deriving (Read, Show, Eq, Ord)

main :: IO ()
main =
  do xs <- readFile "inputs/input8.txt"
     interp (map parseCommand (lines xs))

interp :: [Command] -> IO ()
interp cmds =
  do a <- newArray ((0,0),(49,5)) False
          :: IO (IOUArray (Int,Int) Bool)

     for_ cmds $ \cmd ->
       do interpCommand a cmd
          print cmd
          drawScreen a
          threadDelay 75000

     n <- countPixels a
     putStrLn ("Pixels: " ++ show n)

drawScreen :: IOUArray (Int,Int) Bool -> IO ()
drawScreen a =
  for_ [0..5] $ \y ->
    do xs <- readRow a y
       putStrLn (map toBlock xs)

countPixels :: IOUArray (Int,Int) Bool -> IO Int
countPixels a =
  do xs <- getElems a
     return $! length (filter id xs)

readRow :: MArray a e m => a (Int,Int) e -> Int -> m [e]
readRow a y =
  do ((xlo,_),(xhi,_)) <- getBounds a
     traverse (\x -> readArray a (x,y)) [xlo..xhi]

readCol :: MArray a e m => a (Int,Int) e -> Int -> m [e]
readCol a x =
  do ((_,ylo),(_,yhi)) <- getBounds a
     traverse (\y -> readArray a (x,y)) [ylo..yhi]

writeRow :: MArray a e m => a (Int,Int) e -> Int -> [e] -> m ()
writeRow a y xs =
  do ((xlo,_),(xhi,_)) <- getBounds a
     zipWithM_ (\x -> writeArray a (x,y)) [xlo..xhi] xs

writeCol :: MArray a e m => a (Int,Int) e -> Int -> [e] -> m ()
writeCol a x ys =
  do ((_,ylo),(_,yhi)) <- getBounds a
     zipWithM_ (\y -> writeArray a (x,y)) [ylo..yhi] ys

toBlock :: Bool -> Char
toBlock True  = '█'
toBlock False = ' '

interpCommand :: IOUArray (Int,Int) Bool -> Command -> IO ()
interpCommand a (Rect x y) =
  for_ [0 .. x-1] $ \x ->
  for_ [0 .. y-1] $ \y ->
  writeArray a (x,y) True

interpCommand a (RotateCol x n) =
  do xs <- readCol a x
     writeCol a x (rotate n xs)

interpCommand a (RotateRow y n) =
  do xs <- readRow a y
     writeRow a y (rotate n xs)

rotate :: Int -> [a] -> [a]
rotate n xs = b ++ a
  where
    len   = length xs
    (a,b) = splitAt ((-n) `mod` len) xs

lexer :: String -> [String]
lexer "" = []
lexer xs =
  case lex xs of
    [(x,ys)] -> x : lexer ys
    _ -> error ("lexer: " ++ xs)

parseCommand :: String -> Command
parseCommand cmd =
  case lexer cmd of
    ["rotate","row","y","=",y,"by",n]    -> RotateRow (read y) (read n)
    ["rotate","column","x","=",x,"by",n] -> RotateCol (read x) (read n)
    ["rect",x,'x':y]                     -> Rect (read x) (read y)
    _ -> error cmd
