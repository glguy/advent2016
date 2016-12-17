-- Parallelized, functional that runs in under a seconds using 8 threads
-- and which uses around 50k maximum residency using 1 thread.
module Main where

import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import           Control.Parallel.Strategies (parMap, rseq)
import           Control.Parallel
import           System.IO

myInput :: Vector.Vector Bool
myInput = Vector.fromList (toBool <$> "01111001100111011")

toBool :: Char -> Bool
toBool x = x == '1'

fromBool :: Bool -> Char
fromBool x = if x then '1' else '0'

part1, part2 :: Int
part1 = 272
part2 = 35651584

main :: IO ()
main =
  do hSetBuffering stdout NoBuffering
     putStrLn (solve part1)
     putStrLn (solve part2)

solve :: Int -> String
solve n = buildChecksum n (buildLookup n)

buildChecksum :: Int -> (Int -> Bool) -> String
buildChecksum n f
  | odd n     = parMap rseq (fromBool . f) [0..n-1]
  | otherwise = buildChecksum (n`quot`2) $ \i -> i `seq` f (i*2) == f (i*2+1)

buildLookup :: Int -> Int -> Bool
buildLookup target = aux (Vector.length myInput) (Vector.unsafeIndex myInput)
  where
    aux n f
      | n >= target = f
      | otherwise = aux (2*n+1) $ \i ->
                      case compare i n of
                        LT -> f i
                        GT -> not (f (2*n-i))
                        EQ -> False
