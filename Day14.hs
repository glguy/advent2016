{-# Language OverloadedStrings #-}
module Main where

import Control.Monad
import Crypto.Hash
import Data.ByteArray (convert)
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra
import Data.List
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L

myinp :: B.ByteString
myinp = "qzyelonm"

main :: IO ()
main =
  do print (solve 1)
     print (solve 2017)

-- | Hash a bytestring to to ASCII encoded, lowercase hex
hashmd5 :: B.ByteString -> B.ByteString
hashmd5 str
  = L.toStrict
  $ toLazyByteStringWith md5strategy L.empty
  $ byteStringHex
  $ convert (hash str :: Digest MD5)
  where
    md5strategy = untrimmedStrategy 32 32

iteratedHash :: Int -> B.ByteString -> B.ByteString
iteratedHash n x
  | n <= 0 = x
  | otherwise = iteratedHash (n-1) (hashmd5 x)

seed :: Int -> B.ByteString
seed i = myinp <> B8.pack (show i)

solve :: Int -> Int
solve iterations =
  search (map (B8.unpack . iteratedHash iterations . seed) [0..]) !! 63

search :: [String] -> [Int]
search hashes =
  [ i | (i,h:hs) <- zip [0..] (tails hashes)
      , start <- take 1 [ x | x:y:z:_ <- tails h, x==y, y==z]
      , any (replicate 5 start `isInfixOf`) (take 1000 hs)
      ]
