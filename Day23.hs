{-# Language LambdaCase #-}
module Main where

import           AsmProg
import           Common
import           Control.Lens
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Debug.Trace
import           Text.Megaparsec
import           Text.Megaparsec.String

main :: IO ()
main =
  do program <- Vector.fromList . parseLines parseFile <$> readInputFile 23
     print (execute program 7)
     print (execute program 12)

data Value = Num !Int | Reg !Char
 deriving Show

data Inst
  = Copy Value !Char
  | Inc !Char
  | Dec !Char
  | Jnz Value Value
  | Tgl Value
 deriving Show

pReg :: Parser Char
pReg = oneOf "abcd"

pValue :: Parser Value
pValue = Num <$> number <|> Reg <$> pReg

parseFile :: Parser Inst
parseFile =
  Copy <$ wholestring "cpy " <*> pValue <* char ' ' <*> pReg <|>
  Jnz  <$ wholestring "jnz " <*> pValue <* char ' ' <*> pValue <|>
  Tgl  <$ wholestring "tgl " <*> pValue <|>
  Inc  <$ wholestring "inc " <*> pReg <|>
  Dec  <$ wholestring "dec " <*> pReg

execute :: Vector Inst -> Int -> Registers
execute program0 a = zeroRegisters &~ mainEntry
  where
    mainEntry =
      do reg 'a' .= a
         goto program0 0

    rval = \case
      Num i -> return i
      Reg r -> use (reg r)

    step pc program = \case
      Copy i o -> (1,program) <$ (reg o <~ rval i)
      Inc r    -> (1,program) <$ (reg r += 1)
      Dec r    -> (1,program) <$ (reg r -= 1)
      Tgl r    -> do v <- rval r
                     return (1, toggle program (pc+v))
      Jnz i o  -> do v  <- rval i
                     o' <- rval o
                     return $! (if v == 0 then 1 else o', program)

    goto program pc =
      for_ (program Vector.!? pc) $ \o ->
        do (offset,program') <- step pc program o
           goto program' (pc + offset :: Int)

toggle :: Vector Inst -> Int -> Vector Inst
toggle program pc =
  traceShow pc $
  program & ix pc %~ \oper ->
    case oper of
      Inc x         -> Dec x
      Dec x         -> Inc x
      Tgl   (Reg x) -> Inc x
      Jnz x (Reg y) -> Copy x y
      Copy x y      -> Jnz x (Reg y)
      _ -> error ("Nonsense toggle: " ++ show pc ++ " " ++ show oper)
