module Main where

import Common
import Control.Monad
import Data.List
import Text.Megaparsec
import Text.Megaparsec.String

main :: IO ()
main =
  do xs <- parseLines parser <$> readInputFile 7
     print (length (filter supportsTLS xs))
     print (length (filter supportsSSL xs))

newtype Address = Address [Segment]
  deriving (Read, Show, Ord, Eq)

data Segment = Supernet String | Hypernet String
  deriving (Read, Show, Ord, Eq)

parser :: Parser Address
parser = Address <$> many segment
  where
    segment = Hypernet <$> bracketed (many letterChar) <|>
              Supernet <$>            some letterChar

supportsTLS :: Address -> Bool
supportsTLS (Address xs) = any checkSupernet xs && all checkHypernet xs
  where
    checkSupernet (Supernet ys) = hasABBA ys
    checkSupernet _             = False

    checkHypernet (Hypernet ys) = not (hasABBA ys)
    checkHypernet _             = True

    hasABBA ys = any isABBA (tails ys)

    isABBA (w:x:y:z:_) = w == z && x == y && w /= x
    isABBA _ = False

supportsSSL :: Address -> Bool
supportsSSL (Address xs) =
  not $ null $ do Supernet s <- xs
                  x:y:z:_    <- tails s
                  guard (x == z && x /= y)
                  Hypernet h <- xs
                  guard ( [y,x,y] `isInfixOf` h )
