{-# Language BangPatterns #-}
{-# Language MagicHash #-}
{-# Language TemplateHaskell #-}
{-# Language RankNTypes #-}

module Small where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Applicative
import Data.Monoid
import GHC.Exts
------------------------------------------------------------------------
-- Instructions
------------------------------------------------------------------------

data Register = PC | A | B | C | D
  deriving Read

data Registers = Registers
  { registerPC, registerA, registerB, registerC, registerD :: !Int }
  deriving (Show, Eq)

------------------------------------------------------------------------
-- Implementation of basic instructions in terms of a register machine
------------------------------------------------------------------------

example str = increment (read str)

increment :: Register -> State Registers ()
increment = cases (\r -> do x <- reg r
                            set r (x+1)
                            x <- reg PC
                            set r (x+1))

cases x = \r -> case r of
  A  -> inline x A
  B  -> inline x B
  C  -> inline x C
  D  -> inline x D
  PC -> inline x PC

------------------------------------------------------------------------
-- MachineT: An implementation of MonadMachine
------------------------------------------------------------------------

reg A  = gets registerA
reg B  = gets registerB
reg C  = gets registerC
reg D  = gets registerD
reg PC = gets registerPC
{-# INLINE reg #-}

set A  x = modify' $ \rs -> rs { registerA  = x }
set B  x = modify' $ \rs -> rs { registerB  = x }
set C  x = modify' $ \rs -> rs { registerC  = x }
set D  x = modify' $ \rs -> rs { registerD  = x }
set PC x = modify' $ \rs -> rs { registerPC = x }
{-# INLINE set #-}

newtype Machine = Machine (forall r. (Registers -> r) -> Registers -> r)

instance Monoid Machine where
  mempty = Machine id
  mappend (Machine f) (Machine g) = Machine (\k rs -> f (g k) rs)
