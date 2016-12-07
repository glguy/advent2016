module Main where

import Control.Monad
import Data.List

main :: IO ()
main =
  do xs <- parseInput <$> readFile "inputs/input7.txt"
     print (length (filter supportsTLS xs))
     print (length (filter supportsSSL xs))

data Address = Address [Segment]
  deriving (Read, Show, Ord, Eq)

data Segment = Supernet String | Hypernet String
  deriving (Read, Show, Ord, Eq)

parseInput :: String -> [Address]
parseInput = map parseAddress . lines

parseAddress :: String -> Address
parseAddress = Address . unfoldr aux
  where
    aux [] = Nothing
    aux ('[':xs) =
      case break (==']') xs of
        (hypernet,_:ys) -> Just (Hypernet hypernet, ys)
        _               -> error "parseAddress: mismatched brackets"
    aux xs =
      case break (=='[') xs of
        (normal,ys) -> Just (Supernet normal, ys)

supportsTLS :: Address -> Bool
supportsTLS (Address xs) = any checkSupernet xs && all checkHypernet xs
  where
    checkSupernet (Supernet ys) = hasABBA ys
    checkSupernet _             = False

    checkHypernet (Hypernet ys) = not (hasABBA ys)
    checkHypernet _             = True

    hasABBA xs = any isABBA (tails xs)

    isABBA (w:x:y:z:_) = w == z && x == y && w /= x
    isABBA _ = False

supportsSSL :: Address -> Bool
supportsSSL (Address xs) =
  not $ null $ do Supernet s <- xs
                  x:y:z:_    <- tails s
                  guard (x == z && x /= y)
                  Hypernet h <- xs
                  guard ( [y,x,y] `isInfixOf` h )
