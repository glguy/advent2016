
{-# Language MagicHash, FunctionalDependencies, TypeInType #-}
module UnboxFunctions where

import GHC.Prim
import GHC.Types

class Unbox (b :: Type) (u :: TYPE k) | b -> u where
  unbox :: (b -> a) -> u -> a
  ($#)  :: (u -> a) -> b -> a

infixr 0 $#

instance Unbox Int Int# where
  unbox = \f i -> f (I# i)
  ($#)  = \f (I# i) -> f i
  {-# INLINE unbox #-}
  {-# INLINE ($#) #-}

instance Unbox Word Word# where
  unbox = \f i -> f (W# i)
  ($#)  = \f (W# i) -> f i
  {-# INLINE unbox #-}
  {-# INLINE ($#) #-}
