module Search where

import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

{-# INLINE dfs #-}
dfs :: Ord a => (a -> [a]) -> a -> [a]
dfs next start = aux start (const []) Set.empty
  where
    aux x rest seen
      | Set.member x seen = rest seen
      | otherwise = x : foldr aux rest (next x) (Set.insert x seen)

{-# INLINE bfs #-}
bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs = bfsOn id

{-# INLINE [0] bfsOn #-}
bfsOn :: Ord b => (a -> b) -> (a -> [a]) -> a -> [a]
bfsOn rep next start = aux Set.empty [start] []
  where
    aux _    [] [] = []
    aux seen [] ys = aux seen (reverse ys) []
    aux seen (x:xs) ys
      | Set.member r seen = aux seen xs ys
      | otherwise = x : aux (Set.insert r seen) xs (next x ++ ys)
      where r = rep x

{-# RULES "bfsOn/Int" bfsOn = bfsOnInt #-}
{-# INLINE bfsOnInt #-}
bfsOnInt :: (a -> Int) -> (a -> [a]) -> a -> [a]
bfsOnInt rep next start = aux IntSet.empty [start] []
  where
    aux _    [] [] = []
    aux seen [] ys = aux seen (reverse ys) []
    aux seen (x:xs) ys
      | IntSet.member x' seen = aux seen xs ys
      | otherwise = x : aux (IntSet.insert x' seen) xs (next x ++ ys)
      where
        x' = rep x
