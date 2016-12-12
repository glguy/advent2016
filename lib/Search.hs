module Search where

import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

dfs :: Ord a => (a -> [a]) -> a -> [a]
dfs next start = aux start (const []) Set.empty
  where
    aux x rest seen
      | Set.member x seen = rest seen
      | otherwise = x : foldr aux rest (next x) (Set.insert x seen)
{-# INLINE dfs #-}

bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs next start = aux Set.empty [start] []
  where
    aux _    [] [] = []
    aux seen [] ys = aux seen (reverse ys) []
    aux seen (x:xs) ys
      | Set.member x seen = aux seen xs ys
      | otherwise = x : aux (Set.insert x seen) xs (next x ++ ys)
{-# INLINE bfs #-}

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
{-# INLINE bfsOnInt #-}
