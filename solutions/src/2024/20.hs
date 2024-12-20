module Main where

import Advent.Coord
import Advent.Format (format)
import Advent.Queue (Queue (..))
import Advent.Queue qualified as Queue
import Data.Array (range)
import Data.Map (Map, (!))
import Data.Map qualified as Map

main :: IO ()
main = do
    input <- Map.fromList . coordLines <$> [format|2024 20 (%y%n)*|]
    let s = head [x | (x, 'S') <- Map.assocs input]
    let e = head [x | (x, 'E') <- Map.assocs input]
    let grid = foldr (uncurry Map.insert) input [(s, '.'), (e, '.')]
    let box = boundingBox (Map.keys grid)
    let costNoCheat = costs s grid ! e
    let reachable n v =
            [ (s', manhattan v s')
            | s' <- range box
            , manhattan v s' <= n
            , Just '.' <- [Map.lookup s' grid]
            ]
    let costFromEnd = costs e grid
    let costFromStart = costs s grid
    let savings n v =
            [ best
            | (rs, dist) <- reachable n v
            , let best = costNoCheat - ((costFromStart ! v) + (costFromEnd ! rs) + dist)
            , best >= 100
            ]
    let solve n = length (concat [savings n k | k <- Map.keys grid, Just '.' <- [Map.lookup k grid]])
    print $ solve 2
    print $ solve 20

costs :: Coord -> Map Coord Char -> Map Coord Int
costs s grid = bfs mempty $ Queue.fromList [(s, 0)]
  where
    bfs _ Queue.Empty = mempty
    bfs visited ((curr, cost) :<| q)
        | Just n <- Map.lookup curr visited
        , n < cost =
            bfs visited q
        | otherwise =
            Map.insert curr cost $
                bfs
                    (Map.insert curr cost visited)
                    (Queue.appendList q neighs)
      where
        neighs = [(v, cost + 1) | v <- cardinal curr, Just '.' == Map.lookup v grid]
