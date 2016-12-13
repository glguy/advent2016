module Main where

import Control.Monad
import Data.List
import Search (bfsOnInt)
import Data.Bits
import Data.Monoid
import qualified Data.IntMap as IntMap

main :: IO ()
main =
  do print (solutionSteps part1)
     print (solutionSteps part2)


solutionSteps :: Building -> Maybe Int
solutionSteps = fmap bldgSteps . find isSolved . bfsOnInt mkRep advanceBuilding

                -- gen      micro
data Floor = Floor [Int] [Int]
  deriving (Eq, Ord, Show)

data Building = Building
  { bldgSteps    :: !Int
  , lowerFloors  :: [Floor]
  , currentFloor :: Floor
  , higherFloors :: [Floor]
  }
  deriving Show


isEmptyFloor :: Floor -> Bool
isEmptyFloor (Floor x y) = null x && null y

isSolved :: Building -> Bool
isSolved b = null (higherFloors b) && all isEmptyFloor (lowerFloors b)

isValidFloor :: Floor -> Bool
isValidFloor (Floor gens mics) = null gens || all (`elem` gens) mics

moveFrom :: Floor -> Floor -> Bool -> [(Floor, Floor)]

moveFrom here there allowPairs =
  do (here', there') <- ((if allowPairs then movePair else mempty) <>
                         moveGens <> moveMics) here there
     guard (isValidFloor here')
     guard (isValidFloor there')
     return (here', there')

moveGens, moveMics, movePair :: Floor -> Floor -> [(Floor, Floor)]

moveGens (Floor gens mics) (Floor gens' mics') =
  do sel <- pick gens
     return (Floor (gens \\ sel) mics
            ,Floor (sort (sel++gens')) mics'
            )

moveMics (Floor gens mics) (Floor gens' mics') =
  do sel <- pick mics
     return (Floor gens (mics \\ sel)
            ,Floor gens' (sort (sel++mics'))
            )

movePair (Floor gens mics) (Floor gens' mics') =
  do sel <- intersect gens mics
     return (Floor (delete sel gens) (delete sel mics)
            ,Floor (insert sel gens') (insert sel mics')
            )

pick :: [a] -> [[a]]
pick xs = [ [x,y] | x:ys <- tails xs, y <- ys ]
       <> map pure xs

advanceBuilding :: Building -> [Building]
advanceBuilding b = moveUp b ++ moveDown b

moveUp :: Building -> [Building]
moveUp b =
  case higherFloors b of
    [] -> []
    x:xs -> do (here,there) <- moveFrom (currentFloor b) x True
               return $! Building (bldgSteps b + 1)
                                  (here : lowerFloors b)
                                  there
                                  xs

moveDown :: Building -> [Building]
moveDown b =
  case lowerFloors b of
    [] -> []
    x:xs -> do (here,there) <- moveFrom (currentFloor b) x False
               return $! Building (bldgSteps b + 1)
                                  xs
                                  there
                                  (here : higherFloors b)

-- | Characterize a 4-floor building with up to 8 generator/chip pairs
mkRep :: Building -> Int
mkRep (Building _ x y z) =
  foldl' aux (length x `shiftL` 32)
    (zip [0..] (x ++ y : z))
  where
    aux acc (floorId,Floor gens mics) =
      foldl' aux2 (foldl' aux1 acc gens) mics
      where
        aux1 acc gen = acc + floorId `shiftL` (4*gen)
        aux2 acc mic = acc + floorId `shiftL` (4*mic+2)

part1 :: Building
part1 = Building 0 [] (Floor [1] [1])
                      [ Floor [2,3,4,0] []
                      , Floor [] [2,3,4,0]
                      , Floor [] [] ]

part2 :: Building
part2 = Building 0 [] (Floor [1,6,0] [1,6,0])
                      [ Floor [2,3,4,5] []
                      , Floor [] [2,3,4,5]
                      , Floor [] [] ]

isOneSolved :: Building -> Bool
isOneSolved (Building _ [Floor [] []] _ _) = True
isOneSolved _ = False

dropOne :: Building -> Building
dropOne (Building x _ a b) = Building x [] a b

alternative :: Building -> [Int]
alternative (Building n [] _ []) = return n
alternative b =
  do x <- filter isOneSolved (bfsOnInt mkRep advanceBuilding b)
     alternative (dropOne x)
