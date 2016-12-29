{-# Language TemplateHaskell, BangPatterns #-}
module Main where

import Control.Monad
import Data.List
import Search (bfsOn)
import Data.Bits
import Data.Monoid
import SmallBitSet (SmallBitSet)
import qualified SmallBitSet as SBS
import Control.Lens

-- Types ---------------------------------------------------------------

data Floor = Floor !SmallBitSet !SmallBitSet -- ^ gen micro
  deriving (Eq, Ord, Show)

data Building = Building
  { _bldgSteps    :: !Int
  , _lowerFloors  :: [Floor]
  , _currentFloor :: Floor
  , _higherFloors :: [Floor]
  }
  deriving Show

makeLenses ''Building

-- Main logic and parameters -------------------------------------------

main :: IO ()
main =
  do print (solutionSteps part1)
     print (solutionSteps part2)

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

solutionSteps :: Building -> Maybe Int
solutionSteps =
  fmap (view bldgSteps) . find isSolved . bfsOn mkRep advanceBuilding

--Floor operations -----------------------------------------------------

mkFloor :: [Int] -> [Int] -> Floor
mkFloor xs ys = Floor (SBS.fromList xs) (SBS.fromList ys)

isEmptyFloor :: Floor -> Bool
isEmptyFloor (Floor x y) = SBS.null x && SBS.null y

isValidFloor :: Floor -> Bool
isValidFloor (Floor gens mics) =
  SBS.null gens || SBS.null (SBS.difference mics gens)

floorUnion :: Floor -> Floor -> Floor
floorUnion (Floor x y) (Floor u v) = Floor (SBS.union x u) (SBS.union y v)

floorDifference :: Floor -> Floor -> Floor
floorDifference (Floor x y) (Floor u v) =
  Floor (SBS.difference x u) (SBS.difference y v)

pickFromFloor :: Floor -> [Floor]
pickFromFloor (Floor gs ms) =
  pair ++ twoGens ++ twoMics ++ oneGen ++ oneMic
  where
    gens = SBS.toList gs
    mics = SBS.toList ms
    twoGens = do xs <- SBS.fromList <$> pick2 gens
                 return $! Floor xs SBS.empty
    twoMics = do xs <- SBS.fromList <$> pick2 mics
                 return $! Floor SBS.empty xs
    pair    = do x <- SBS.singleton <$>
                        take 1 (SBS.toList (SBS.intersection gs ms))
                 return $! Floor x x
    oneGen  = do x <- SBS.singleton <$> gens
                 return $! Floor x SBS.empty
    oneMic  = do x <- SBS.singleton <$> mics
                 return $! Floor SBS.empty x

pick2 :: [a] -> [[a]]
pick2 xs = [ [x,y] | x:ys <- tails xs, y <- ys ]

-- Building operations -------------------------------------------------

isSolved :: Building -> Bool
isSolved b = null (b^.higherFloors) && all isEmptyFloor (b^.lowerFloors)

advanceBuilding :: Building -> [Building]
advanceBuilding b =
  [ b2 | subset <- pickFromFloor (b^.currentFloor)
       , let b1 = b & currentFloor %~ (`floorDifference` subset)
       , isValidFloor (b1^.currentFloor)
       , b2 <- move subset (Lens lowerFloors) (Lens higherFloors) b1
            <> move subset (Lens higherFloors) (Lens lowerFloors) b1
       ]

{-# INLINE move #-}
move ::
  Floor ->
  ReifiedLens' Building [Floor] ->
  ReifiedLens' Building [Floor] ->
  Building ->
  [Building]
move subset (Lens back) (Lens front) b =
  [ b & bldgSteps    +~ 1
      & back         %~ cons (b^.currentFloor)
      & currentFloor .~ here
      & front        .~ xs
  | next:xs <- [ b^.front ]
  , let here = floorUnion next subset
  , isValidFloor here
  ]

-- | Characterize a 4-floor building with up to 7 generator/chip pairs
mkRep :: Building -> Int
mkRep (Building _ x y z) = foldl' aux (length x) (x ++ y : z)
  where
    aux acc (Floor gens mics) =
      acc             `shiftL` 14 .|.
      SBS.setRep gens `shiftL`  7 .|.
      SBS.setRep mics
