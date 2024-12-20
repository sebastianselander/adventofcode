module Main where

import Advent.Coord (
    Coord,
    boundingBox,
    cardinal,
    coordLines,
    manhattan,
 )
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
    let costNoCheat = buildGraph s grid ! e
    let reachable n v =
            [ (s', manhattan v s')
            | s' <- range box
            , manhattan v s' <= n
            , Just '.' <- pure (Map.lookup s' grid)
            ]
    let costFromEnd = buildGraph e grid
    let costFromStart = buildGraph s grid
    let savings n v =
            [ ()
            | (rs, dist) <- reachable n v
            , (costNoCheat - ((costFromStart ! v) + (costFromEnd ! rs) + dist)) >= 100
            ]
    let solve n =
            sum
                [ length (savings n k)
                | k <- Map.keys grid
                , Just '.' <- pure (Map.lookup k grid)
                ]
    print $ solve 2
    print $ solve 20

buildGraph :: Coord -> Map Coord Char -> Map Coord Int
buildGraph start grid = bfs mempty $ Queue.fromList [(start, 0)]
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
