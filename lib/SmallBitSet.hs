module SmallBitSet where

import Prelude hiding (null, insert, delete)
import Data.Foldable (foldl')
import Data.Bits

newtype SmallBitSet = SmallBitSet { setRep :: Int }
  deriving (Eq, Ord)

instance Show SmallBitSet where
  showsPrec p x = showParen (11 >= p)
                $ showString "fromList "
                . shows (toList x)

{-# INLINE null #-}
null :: SmallBitSet -> Bool
null (SmallBitSet x) = x == 0

toList :: SmallBitSet -> [Int]
toList (SmallBitSet x) = [ i | i <- [0..finiteBitSize x - 1 - countLeadingZeros x], testBit x i ]

fromList :: [Int] -> SmallBitSet
fromList xs = SmallBitSet (foldl' setBit 0 xs)

{-# INLINE member #-}
member :: Int -> SmallBitSet -> Bool
member i (SmallBitSet x) = testBit x i

{-# INLINE insert #-}
insert :: Int -> SmallBitSet -> SmallBitSet
insert i (SmallBitSet x) = SmallBitSet (setBit x i)

{-# INLINE delete #-}
delete :: Int -> SmallBitSet -> SmallBitSet
delete i (SmallBitSet x) = SmallBitSet (clearBit x i)

{-# INLINE difference #-}
difference :: SmallBitSet -> SmallBitSet -> SmallBitSet
difference (SmallBitSet x) (SmallBitSet y) =
  SmallBitSet (x .&. complement y)

{-# INLINE intersection #-}
intersection :: SmallBitSet -> SmallBitSet -> SmallBitSet
intersection (SmallBitSet x) (SmallBitSet y) = SmallBitSet (x .&. y)

{-# INLINE union #-}
union :: SmallBitSet -> SmallBitSet -> SmallBitSet
union (SmallBitSet x) (SmallBitSet y) = SmallBitSet (x .|. y)

{-# INLINE singleton #-}
singleton :: Int -> SmallBitSet
singleton i = SmallBitSet (bit i)

{-# INLINE empty #-}
empty :: SmallBitSet
empty = SmallBitSet 0
