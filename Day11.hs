{-# Language BangPatterns #-}
module Main where

import Control.Monad
import Data.List
import Search (bfsOn)
import Data.Bits
import Data.Monoid
import SmallBitSet (SmallBitSet)
import qualified SmallBitSet as SBS
import qualified Data.IntMap as IntMap

main :: IO ()
main =
  do print (solutionSteps part1)
     print (solutionSteps part2)


solutionSteps :: Building -> Maybe Int
solutionSteps =
  fmap bldgSteps . find isSolved . bfsOn mkRep advanceBuilding

                -- gen      micro
data Floor = Floor !SmallBitSet !SmallBitSet
  deriving (Eq, Ord, Show)

data Building = Building
  { bldgSteps    :: !Int
  , lowerFloors  :: [Floor]
  , currentFloor :: Floor
  , higherFloors :: [Floor]
  }
  deriving Show


isEmptyFloor :: Floor -> Bool
isEmptyFloor (Floor x y) = SBS.null x && SBS.null y

isSolved :: Building -> Bool
isSolved b = null (higherFloors b) && all isEmptyFloor (lowerFloors b)

isValidFloor :: Floor -> Bool
isValidFloor (Floor gens mics) =
  SBS.null gens || SBS.null (SBS.difference mics gens)

pickFromFloor :: Floor -> [ (SmallBitSet, SmallBitSet, Floor) ]
pickFromFloor (Floor gs ms) =
  pair ++ twoGens ++ twoMics ++ oneGen ++ oneMic
  where
    gens = SBS.toList gs
    mics = SBS.toList ms
    twoGens = do xs <- SBS.fromList <$> pick2 gens
                 return (xs, SBS.empty, Floor (SBS.difference gs xs) ms)
    twoMics = do xs <- SBS.fromList <$> pick2 mics
                 return (SBS.empty, xs, Floor gs (SBS.difference ms xs))
    pair    = do x <- SBS.singleton <$>
                        take 1 (SBS.toList (SBS.intersection gs ms))
                 return (x, x, Floor (SBS.difference gs x) (SBS.difference ms x))
    oneGen  = do x <- SBS.singleton <$> gens
                 return (x, SBS.empty, Floor (SBS.difference gs x) ms)
    oneMic  = do x <- SBS.singleton <$> mics
                 return (SBS.empty, x, Floor gs (SBS.difference ms x))

pick2 :: [a] -> [[a]]
pick2 xs = [ [x,y] | x:ys <- tails xs, y <- ys ]

advanceBuilding :: Building -> [Building]
advanceBuilding b =
  do (gens, mics, floor') <- pickFromFloor (currentFloor b)
     guard (isValidFloor floor')
     moveUp gens mics b { currentFloor = floor' }
        <> moveDown gens mics b { currentFloor = floor' }

moveUp :: SmallBitSet -> SmallBitSet -> Building -> [Building]
moveUp g m b =
  case higherFloors b of
    Floor gens mics:xs
      | isValidFloor here ->
               return $! Building (bldgSteps b + 1)
                                  ( currentFloor b : lowerFloors b)
                                  here
                                  xs
        where here = Floor (SBS.union g gens) (SBS.union m mics)
    _ -> []

moveDown :: SmallBitSet -> SmallBitSet -> Building -> [Building]
moveDown g m b =
  case lowerFloors b of
    Floor gens mics:xs
      | isValidFloor here ->
               return $! Building (bldgSteps b + 1)
                                  xs
                                  here
                                  ( currentFloor b : higherFloors b)
        where here = Floor (SBS.union g gens) (SBS.union m mics)
    _ -> []


-- | Characterize a 4-floor building with up to 7 generator/chip pairs
mkRep :: Building -> Int
mkRep (Building _ x y z) =
  foldl' aux (length x)
    (x ++ y : z)
  where
    aux acc (Floor gens mics) =
      acc             `shiftL` 14 .|.
      SBS.setRep gens `shiftL`  7 .|.
      SBS.setRep mics


mkFloor :: [Int] -> [Int] -> Floor
mkFloor xs ys = Floor (SBS.fromList xs) (SBS.fromList ys)

part1 :: Building
part1 = Building 0 [] ( mkFloor [1] [1])
                      [ mkFloor [2,3,4,0] []
                      , mkFloor [] [2,3,4,0]
                      , mkFloor [] [] ]

part2 :: Building
part2 = Building 0 [] ( mkFloor [1,6,0] [1,6,0])
                      [ mkFloor [2,3,4,5] []
                      , mkFloor [] [2,3,4,5]
                      , mkFloor [] [] ]
