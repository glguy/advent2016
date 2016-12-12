module Main where

import Control.Monad
import Data.List
import Search (bfs)
import qualified Data.IntMap as IntMap

main :: IO ()
main =
  do print (solutionSteps part1)
     print (solutionSteps part2)


solutionSteps :: Building -> Maybe Int
solutionSteps = fmap bldgSteps . find isSolved . bfs advanceBuilding

                -- gen      micro
data Floor = Floor [Int] [Int]
  deriving (Eq, Ord, Show)

data Building = Building
  { bldgSteps    :: !Int
  , floorNumber  :: !Int -- helps speed up comparisons
  , lowerFloors  :: [Floor]
  , currentFloor :: Floor
  , higherFloors :: [Floor]
  }
  deriving Show

-- | Ignore step count
instance Eq Building where
  Building _ n a b c == Building _ m x y z = n == m && b == y && a == x && c == z

-- | Ignore step count
instance Ord Building where
  Building _ n a b c `compare` Building _ m x y z =
    compare n m `mappend`
    compare b y `mappend`
    compare a x `mappend`
    compare c z

isEmptyFloor :: Floor -> Bool
isEmptyFloor (Floor x y) = null x && null y

isSolved :: Building -> Bool
isSolved b = null (higherFloors b) && all isEmptyFloor (lowerFloors b)

isValidFloor :: Floor -> Bool
isValidFloor (Floor gens mics) = null gens || all (`elem` gens) mics

moveFrom, moveGens, moveMics, movePair :: Floor -> Floor -> [(Floor, Floor)]
moveFrom here there =
  do (here', there') <- moveGens here there ++ movePair here there ++ moveMics here there
     guard (isValidFloor here')
     guard (isValidFloor there')
     return (here', there')

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
       ++ map pure xs

advanceBuilding :: Building -> [Building]
advanceBuilding b = moveUp b ++ moveDown b

moveUp :: Building -> [Building]
moveUp b =
  case higherFloors b of
    [] -> []
    x:xs -> do (here,there) <- moveFrom (currentFloor b) x
               return $! Building (bldgSteps b + 1)
                                  (floorNumber b + 1)
                                  (here : lowerFloors b)
                                  there
                                  xs

moveDown :: Building -> [Building]
moveDown b =
  case lowerFloors b of
    [] -> []
    x:xs -> do (here,there) <- moveFrom (currentFloor b) x
               return $! Building (bldgSteps b + 1)
                                  (floorNumber b - 1)
                                  xs
                                  there
                                  (here : higherFloors b)

part1 :: Building
part1 = Building 0 1 [] (Floor [1] [1])
                      [ Floor [2,3,4,5] []
                      , Floor [] [2,3,4,5]
                      , Floor [] [] ]

part2 :: Building
part2 = Building 0 1 [] (Floor [1,6,7] [1,6,7])
                      [ Floor [2,3,4,5] []
                      , Floor [] [2,3,4,5]
                      , Floor [] [] ]
