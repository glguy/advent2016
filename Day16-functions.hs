-- Parallelized, functional that runs in 0.7 seconds with -N
-- and which uses around 36k maximum residency using 1 thread.

{-# Language MagicHash #-}
module Main (main) where

import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import           Control.Parallel.Strategies (parMap, rseq)
import           System.IO
import           UnboxFunctions
import           GHC.Prim

myInput :: Vector.Vector Bool
myInput = fromList' (toBool <$> "01111001100111011")

-- Allow vector allocate the right size of vector
fromList' :: Vector.Unbox a => [a] -> Vector a
fromList' xs = Vector.fromListN (length xs) xs

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

buildChecksum :: Int -> (Int# -> Bool) -> String
buildChecksum sz f
  | odd sz    = parMap rseq (fromBool . (f $#)) [0..sz-1]
  | otherwise = buildChecksum (sz`quot`2) $ unbox $ \i ->
                  (f $# i*2) == (f $# (i*2+1::Int))

buildLookup :: Int -> Int# -> Bool
buildLookup sz = aux (Vector.length myInput)
                     (unbox (Vector.unsafeIndex myInput))
  where
    aux n f
      | n >= sz = f
      | otherwise = aux (2*n+1) $ unbox $ \i ->
                      case compare i n of
                        LT ->       f $# i
                        GT -> not $ f $# 2*n-i
                        EQ -> False
