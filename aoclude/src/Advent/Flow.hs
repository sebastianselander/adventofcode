{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Advent.Flow where

import Advent.Prelude (elemOn)
import Algorithm.Search
import Prelude hiding (lookup)

newtype Network a = Network
    { unNetwork :: [Edge a]
    }
    deriving (Show, Eq, Functor, Semigroup, Monoid, Traversable, Foldable)

data EdgeType = Forward | Backward
    deriving (Show, Eq)

data Edge a = Edge
    { source :: a
    , sink :: a
    , flow :: Int
    }
    deriving (Show, Eq, Functor, Traversable, Foldable)

fromList :: (Ord a) => [(a, a, Int)] -> Network a
fromList = foldr f mempty
  where
    f (a, b, c) = insert a b c

lookup :: (Eq a) => a -> Network a -> [Edge a]
lookup a (Network g) = filter ((== a) . source) g

insert :: (Eq a) => a -> a -> Int -> Network a -> Network a
insert from to cost (Network g)
    | elemOn sink from edges = Network g
    | otherwise = Network $ Edge from to cost : Edge to from 0 : g
  where
    edges = lookup to (Network g)

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
updateEdges n edges (Network network) =
    Network
        [ if
            | (from, to) `elem` vertexPairs -> Edge from to (flow - n)
            | (to, from) `elem` vertexPairs -> Edge from to (flow + n)
            | otherwise -> Edge from to flow
        | Edge from to flow <- network
        ]
  where
    vertexPairs = fmap (\e -> (e.source, e.sink)) edges

edmondsKarp :: (Show a, Ord a) => a -> a -> Network a -> Network a
edmondsKarp source sink network = case augmentingPath source sink network of
    Nothing -> network
    Just edges ->
        let availableFlow = bottleneck edges
            residual = updateEdges availableFlow edges network
         in edmondsKarp source sink residual

maxFlow :: (Eq a) => a -> Network a -> Int
maxFlow sink (Network g) = sum [flow | Edge from _ flow <- g, from == sink]

-- flow: 30
-- source: 's'
-- sink: 't'
example1 :: Network Char
example1 =
    fromList
        [ ('u', 't', 10)
        , ('v', 't', 20)
        , ('u', 'v', 30)
        , ('s', 'v', 10)
        , ('s', 'u', 20)
        ]

-- flow: 6
-- source: 'a'
-- sink: 'f'
example2 :: Network Char
example2 =
    fromList
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
-- source: 'a' 
-- sink: 'g'
example3 :: Network Char
example3 =
    fromList
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
-- source: 'a'
-- sink: 'd'
example4 :: Network Char
example4 =
    fromList
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
