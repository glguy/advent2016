module Main where

import           Control.Monad
import           Crypto.Hash.MD5
import           Data.List
import           Search (bfs)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import           Text.Printf

input :: String
input = "lpvhkcbi"


main :: IO ()
main =
  do let paths = [ path | (3,3,path) <- bfs nextStates initialState ]
         shortestPath = head paths
         longestPath  = last paths
     putStrLn shortestPath
     print (length longestPath)


initialState :: (Int,Int,String)
initialState = (0,0,"")


isValidLocation :: Int -> Int -> Bool
isValidLocation x y = 0 <= x && x < 4
                   && 0 <= y && y < 4


nextStates :: (Int,Int,String) -> [(Int,Int,String)]
nextStates (3,3,path) = []
nextStates (x,y,path) =
  do step <- directions path
     let (x',y') = move step x y
     guard (isValidLocation x' y')
     return (x',y',path++[step])

hexRep :: BS.ByteString -> String
hexRep bs = printf "%02x" =<< BS.unpack bs

hashmd5 :: String -> String
hashmd5 str = hexRep (hash (B8.pack str))


directions :: String -> [Char]
directions path = ways
  where
    a:b:c:d:_ = hashmd5 (input ++ path)

    isOpen x = x `elem` "bcdef"

    ways = [ 'U' | isOpen a ] ++
           [ 'D' | isOpen b ] ++
           [ 'L' | isOpen c ] ++
           [ 'R' | isOpen d ]


move :: Char -> Int -> Int -> (Int,Int)
move 'U' x y = (x,y-1)
move 'D' x y = (x,y+1)
move 'L' x y = (x-1,y)
move 'R' x y = (x+1,y)
