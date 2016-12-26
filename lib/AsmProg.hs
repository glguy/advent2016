{-# Language TemplateHaskell #-}
module AsmProg where

import Control.Lens
import Control.Monad.State
import Common
import Text.Megaparsec
import Text.Megaparsec.String

data Registers = Registers { _regA, _regB, _regC, _regD :: !Int }
  deriving (Read, Show, Eq, Ord)

makeLenses ''Registers

zeroRegisters :: Registers
zeroRegisters = Registers 0 0 0 0

class HasRegisters a where
  reg :: Functor f => Register -> LensLike' f a Int

newtype Register = Register Char
  deriving (Show, Eq, Ord)

instance HasRegisters Registers where
  reg (Register 'a') = regA
  reg (Register 'b') = regB
  reg (Register 'c') = regC
  reg (Register 'd') = regD
  reg (Register r  ) = error ("Bad register: " ++ [r])
  {-# INLINE reg #-}

data Value = Num !Int | Reg !Register
 deriving Show

rval :: (MonadState r m, HasRegisters r) => Value -> m Int
rval v =
  case v of
    Num i -> return i
    Reg r -> use (reg r)
{-# INLINE rval #-}

pValue :: Parser Value
pValue = Num <$> number <|> Reg <$> pReg

pReg :: Parser Register
pReg = Register <$> oneOf "abcd"
