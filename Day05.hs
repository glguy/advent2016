module Main where

import           Crypto.Hash
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import           Data.Monoid
import           System.IO

main :: IO ()
main =
  do hSetBuffering stdout NoBuffering
     putStrLn password1
     putStrLn password2

input :: B.ByteString
input = B.pack "ffykfhsq"

password1 :: String
password1 = take 8 (map fst digitStream)

password2 :: String
password2 = go Map.empty digitStream
  where
    go seen _ | Map.size seen == 8 = ""
    go _ [] = error "digitStream ran out!"
    go seen ((pos,c) : rest)
      | '0' <= pos, pos < '8', Map.notMember pos seen = rendered ++ go seen' rest
      | otherwise           =  go seen rest
      where
        seen' = Map.insert pos c seen
        rendered = "\r" ++ [ Map.findWithDefault '_' i seen' | i <- ['0'..'7'] ]

digitStream :: [(Char,Char)]
digitStream = go (0 :: Int)
  where
    go i =
      case splitAt 5 (show (hash (input <> B.pack (show i)) :: Digest MD5)) of
        ("00000",c1:c2:_) -> (c1,c2) : go (i+1)
        _ -> go (i+1)
