module Main where

import           Control.Monad
import           Crypto.Hash
import           Data.List
import           Search (bfs)
import qualified Data.ByteString.Char8 as B8

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


hashmd5 :: String -> String
hashmd5 str = show (hash (B8.pack str) :: Digest MD5)


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
