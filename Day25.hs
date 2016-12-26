{-# Language LambdaCase #-}
module Main where

import           AsmProg
import           Common
import           Control.Lens
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Text.Megaparsec
import           Text.Megaparsec.String
import           Control.Monad.Trans.State

main :: IO ()
main =
  do program <- Vector.fromList . parseLines parseFile <$> readInputFile 25
     print $ find (execute program) [1..]

data Value = Num !Int | Reg !Char
 deriving Show

data Inst
  = Copy Value !Char
  | Inc !Char
  | Dec !Char
  | Jnz Value Value
  | Out Value
 deriving Show

pReg :: Parser Char
pReg = oneOf "abcd"

pValue :: Parser Value
pValue = Num <$> number <|> Reg <$> pReg

parseFile :: Parser Inst
parseFile =
  Copy <$ wholestring "cpy " <*> pValue <* char ' ' <*> pReg <|>
  Jnz  <$ wholestring "jnz " <*> pValue <* char ' ' <*> pValue <|>
  Inc  <$ wholestring "inc " <*> pReg <|>
  Dec  <$ wholestring "dec " <*> pReg <|>
  Out  <$ wholestring "out " <*> pValue

data Progress = NeedOne | NeedZero

execute :: Vector Inst -> Int -> Bool
execute program a = evalState theMain zeroRegisters
  where
    theMain = do reg 'a' .= a
                 goto NeedZero Set.empty 0

    rval = \case
      Num i -> return i
      Reg r -> use (reg r)

    step pc progress targets = \case
      Out o ->
         do v <- rval o
            case (progress, v) of
              (NeedOne,  1) -> goto NeedZero targets (pc+1)
              (NeedZero, 0) ->
                  do registers <- get
                     let now = (pc,registers)
                     if Set.member now targets then
                       return True
                     else
                       goto NeedOne (Set.insert now targets) (pc+1)
              _ -> return False

      Copy i o -> do reg o <~ rval i
                     goto progress targets (pc+1)

      Inc r    -> do reg r += 1
                     goto progress targets (pc+1)

      Dec r    -> do reg r -= 1
                     goto progress targets (pc+1)

      Jnz i o  -> do v  <- rval i
                     o' <- rval o
                     let pcOff = if v == 0 then 1 else o'
                     goto progress targets (pc+pcOff)

    goto progress targets pc =
      case (program Vector.!? pc) of
        Nothing -> return False
        Just o  -> step pc progress targets o
