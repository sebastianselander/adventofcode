{-# LANGUAGE OverloadedRecordDot #-}

module Advent.Graph where

import Advent.Prelude (elemOn)
import Algorithm.Search
import Prelude hiding (lookup)

newtype Network a = Network
    { unNetwork :: [Edge a]
    }
    deriving (Show, Eq, Functor, Semigroup, Monoid, Traversable, Foldable)

data Edge a = Edge {forward :: Edge' a, backward :: Edge' a}
    deriving (Show, Eq, Functor, Traversable, Foldable)

data Edge' a = Edge'
    { source :: a
    , sink :: a
    , flow :: Int
    }
    deriving (Show, Eq, Functor, Traversable, Foldable)

fromList :: (Ord a) => [(a, a, Int)] -> Network a
fromList = foldr insert' (Network [])
  where
    insert' (a, b, c) = insert a (b, c)

lookup :: (Eq a) => a -> Network a -> [Edge a]
lookup a (Network g) = filter ((== a) . source . forward) g

insert :: (Ord vertex) => vertex -> (vertex, Int) -> Network vertex -> Network vertex
insert from (to, cost) (Network g)
    | elemOn (sink . forward) from edges = Network g
    | otherwise = Network $ Edge{forward = Edge' from to cost, backward = Edge' to from 0} : g
  where
    edges = lookup to (Network g)

-- | Precondition: non-empty
bottleneck :: [Edge a] -> Int
bottleneck = minimum . fmap (flow . forward)

augmentingPath :: forall a. (Ord a) => a -> a -> Network a -> Maybe [Edge a]
augmentingPath s t g =
    fmap path ((s :) <$> bfs next (== t) s)
  where
    next node = sink . forward <$> filter ((> 0) . flow . forward) (lookup node g)
    path :: (Ord a) => [a] -> [Edge a]
    path [] = []
    path [_] = []
    path (y : x : xs) = case lookup y g of
        edges -> case [e | e <- edges, e.forward.sink == x] of
            [] -> []
            (first : _) -> first : path (x : xs)

updateEdge :: Int -> Edge a -> Edge a
updateEdge n (Edge forw back) = Edge (updateEdge' (\x -> x - n) forw) (updateEdge' (+ n) back)
  where
    updateEdge' f (Edge' source sink v) = Edge' source sink (f v)

edmondsKarp :: (Show a, Ord a) => a -> a -> Network a -> Network a
edmondsKarp source sink network = case augmentingPath source sink network of
    Nothing -> network
    Just edges ->
        let availableFlow = bottleneck edges
            newEdges = fmap (updateEdge availableFlow) edges
         in edmondsKarp
                source
                sink
                (Network $ newEdges <> [x | x <- network.unNetwork, x `notElem` edges])

maxFlow :: (Eq a) => a -> Network a -> Int
maxFlow sink (Network g) = sum [back.flow | Edge _ back <- g, back.source == sink]

-- flow: 30
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
-- Wikipedia: Edmonds-Karp
-- BUG: flow is 4
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
