{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Advent.Flow where

import Advent.Prelude (elemOn)
import Algorithm.Search
import Prelude hiding (lookup)

data Network a = Network
    { edges :: [Edge a]
    , networkSource :: a
    , networkSink :: a
    }
    deriving (Show, Eq, Functor, Traversable, Foldable)

data EdgeType = Forward | Backward
    deriving (Show, Eq)

data Edge a = Edge
    { source :: a
    , sink :: a
    , flow :: Int
    }
    deriving (Show, Eq, Functor, Traversable, Foldable)

mkNetwork :: (Ord a) => a -> a -> [(a, a, Int)] -> Network a
mkNetwork src target = foldr f (Network [] src target)
  where
    f (a, b, c) = insert a (b, c)

lookup :: (Eq a) => a -> Network a -> [Edge a]
lookup a network = filter ((== a) . source) network.edges

insert :: (Ord vertex) => vertex -> (vertex, Int) -> Network vertex -> Network vertex
insert from (to, cost) network@(Network g src target)
    | elemOn sink from edges = network
    | otherwise = Network (Edge from to cost : Edge to from 0 : g) src target
  where
    edges = lookup to network

-- | Precondition: non-empty
bottleneck :: [Edge a] -> Int
bottleneck [] = 0
bottleneck xs = minimum $ fmap flow xs

augmentingPath :: forall a. (Ord a) => a -> a -> Network a -> Maybe [Edge a]
augmentingPath src target network =
    fmap path ((src :) <$> bfs next (== target) src)
  where
    next :: a -> [a]
    next node = sink <$> filter ((> 0) . flow) (lookup node network)

    path :: (Ord a) => [a] -> [Edge a]
    path [] = []
    path [_] = []
    path (y : x : xs) = case lookup y network of
        edges -> case [e | e <- edges, e.sink == x] of
            (first : _) -> first : path (x : xs)
            [] -> []

updateEdges :: (Show a, Eq a) => Int -> [Edge a] -> Network a -> Network a
updateEdges n edges (Network network src target) =
    Network
        [ if
            | (from, to) `elem` vertexPairs -> Edge from to (flow - n)
            | (to, from) `elem` vertexPairs -> Edge from to (flow + n)
            | otherwise -> Edge from to flow
        | Edge from to flow <- network
        ]
        src
        target
  where
    vertexPairs = fmap (\e -> (e.source, e.sink)) edges

edmondsKarp :: (Show a, Ord a) => Network a -> Network a
edmondsKarp network = case augmentingPath network.networkSource network.networkSink network of
    Nothing -> network
    Just edges ->
        let residual = updateEdges (bottleneck edges) edges network
         in edmondsKarp residual

maxFlow :: (Eq a) => Network a -> Int
maxFlow (Network g _ sink) = sum [flow | Edge from _ flow <- g, from == sink]

-- flow: 30
-- Algorithm Design - Kleinberg and Tardos
example1 :: Network Char
example1 =
    mkNetwork
        's'
        't'
        [ ('u', 't', 10)
        , ('v', 't', 20)
        , ('u', 'v', 30)
        , ('s', 'v', 10)
        , ('s', 'u', 20)
        ]

-- flow: 6
-- https://www.youtube.com/watch?v=VbeTl1gG4l4
example2 :: Network Char
example2 =
    mkNetwork
        'a'
        'f'
        [ ('a', 'b', 2)
        , ('a', 'c', 8)
        , ('b', 'c', 2)
        , ('b', 'd', 7)
        , ('b', 'e', 3)
        , ('c', 'e', 6)
        , ('d', 'e', 2)
        , ('d', 'f', 3)
        , ('e', 'f', 4)
        ]

-- flow: 5
-- Wikipedia: Edmonds-Karp
example3 :: Network Char
example3 =
    mkNetwork
        'a'
        'g'
        [ ('a', 'b', 3)
        , ('a', 'd', 3)
        , ('b', 'c', 4)
        , ('c', 'a', 3)
        , ('c', 'd', 1)
        , ('c', 'e', 2)
        , ('d', 'e', 2)
        , ('d', 'f', 6)
        , ('e', 'g', 1)
        , ('e', 'b', 1)
        , ('f', 'g', 9)
        , ('e', 'g', 1)
        ]

-- flow: 37
-- https://www.youtube.com/watch?v=rjwwU8dNXug
example4 :: Network Char
example4 =
    mkNetwork
        'a'
        'd'
        [ ('a', 'b', 20)
        , ('a', 'g', 9)
        , ('a', 'f', 11)
        , ('b', 'c', 7)
        , ('b', 'e', 20)
        , ('c', 'd', 24)
        , ('e', 'd', 13)
        , ('e', 'c', 17)
        , ('f', 'b', 9)
        , ('f', 'e', 17)
        , ('g', 'f', 8)
        ]
