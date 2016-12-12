{-# Language LambdaCase #-}
module Main where

import           Common
import           Control.Lens
import           Data.Foldable
import qualified Data.Map.Strict as Map
import           Data.Map (Map)
import qualified Data.Vector as Vector
import           Data.Vector
import           Text.Megaparsec
import           Text.Megaparsec.String

main :: IO ()
main =
  do program <- Vector.fromList . parseLines parseFile <$> readInputFile 12
     print (execute program 0)
     print (execute program 1)

data Value = Num !Int | Reg !Char
 deriving Show

data Inst
  = Copy Value !Char
  | Inc !Char
  | Dec !Char
  | Jnz Value !Int
 deriving Show

type Regs = Map Char Int

value :: Parser Value
value = Num <$> number <|> Reg <$> letterChar

parseFile :: Parser Inst
parseFile =
  Copy <$ wholestring "cpy " <*> value <* char ' ' <*> anyChar <|>
  Jnz  <$ wholestring "jnz " <*> value <* char ' ' <*> number <|>
  Inc  <$ wholestring "inc " <*> letterChar <|>
  Dec  <$ wholestring "dec " <*> letterChar

reg :: Functor f => Char -> LensLike' f Regs Int
reg r = at r . non 0

execute :: Vector Inst -> Int -> Regs
execute program c = Map.singleton 'c' c &~ goto 0
  where
    rval = \case
      Num i -> return i
      Reg r -> use (reg r)

    step = \case
      Copy i o -> 1 <$ (reg o <~ rval i)
      Inc r    -> 1 <$ (reg r += 1)
      Dec r    -> 1 <$ (reg r -= 1)
      Jnz i o  -> do v <- rval i
                     return $! if v == 0 then 1 else o

    goto pc =
      for_ (program Vector.!? pc) $ \o ->
        do offset <- step o
           goto (pc + offset)