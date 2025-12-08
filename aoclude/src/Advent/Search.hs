module Advent.Search where

import Advent.Queue qualified as Queue
import Data.Set qualified as Set
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map

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

clique :: Ord a => a -> Map a (Set a) -> Set a
clique x g = go [x] Set.empty
  where
    go [] seen = seen
    go (x : xs) seen
        | Set.member x seen = go xs seen
        | otherwise = case Map.lookup x g of
            Nothing -> go xs seen
            Just neighbors -> go (Set.toList neighbors <> xs) (Set.insert x seen)
