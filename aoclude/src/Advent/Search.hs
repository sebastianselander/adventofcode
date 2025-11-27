module Advent.Search where

import Advent.Queue qualified as Queue
import Data.Set qualified as Set

dfs :: Ord a => (a -> [a]) -> a -> [a]
dfs f s = dfsN f [s]

dfsN :: (Ord a) => (a -> [a]) -> [a] -> [a]
dfsN = dfsOnN id

dfsOnN :: (Ord r) => (a -> r) -> (a -> [a]) -> [a] -> [a]
dfsOnN rep next = go Set.empty
  where
    go _ [] = []
    go !visited (q : qs)
        | Set.member r visited = go visited qs
        | otherwise = q : go (Set.insert r visited) qs'
      where
        r = rep q
        qs' = next q ++ qs

bfs :: (Ord a) => (a -> [a]) -> a -> [a]
bfs f s = bfsN f [s]

bfsN :: (Ord a) => (a -> [a]) -> [a] -> [a]
bfsN = bfsOnN id

bfsOnN :: (Ord r) => (a -> r) -> (a -> [a]) -> [a] -> [a]
bfsOnN rep next = go Set.empty . Queue.fromList
  where
    go _ Queue.Empty = []
    go !visited (q Queue.:<| qs)
        | Set.member r visited = go visited qs
        | otherwise = q : go (Set.insert r visited) qs'
      where
        r = rep q
        qs' = Queue.appendList qs (next q)
