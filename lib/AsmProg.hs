{-# Language TemplateHaskell #-}
module AsmProg where

import Control.Lens

data Registers = Registers { _regA, _regB, _regC, _regD :: !Int }
  deriving (Read, Show, Eq, Ord)

makeLenses ''Registers

zeroRegisters :: Registers
zeroRegisters = Registers 0 0 0 0

reg :: Functor f => Char -> LensLike' f Registers Int
reg 'a' = regA
reg 'b' = regB
reg 'c' = regC
reg 'd' = regD
reg r   = error ("Bad register: " ++ [r])
{-# INLINE reg #-}
