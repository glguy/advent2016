module Common where

import System.Environment

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
