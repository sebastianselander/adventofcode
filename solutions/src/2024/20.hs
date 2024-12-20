module Main where

import Advent.Coord
import Advent.Format (format, format')
import Advent.Prelude
import Advent.Queue (Queue (..))
import Advent.Queue qualified as Queue
import Algorithm.Search
import Data.Array (range)
import Data.List (group, sort)
import Data.List.Extra (nubOrdOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Tuple.Extra (thd3)
import Debug.Trace (traceShowId)

main :: IO ()
main = do
    input <- Map.fromList . coordLines <$> [format|2024 20 (%y%n)*|]
    let s = head [s | (s, 'S') <- Map.assocs input]
    let e = head [s | (s, 'E') <- Map.assocs input]
    let grid = Map.insert s '.' $ Map.insert e '.' input
    let box = boundingBox (Map.keys grid)
    let Just (og, _) = shortest grid s e
    let reachable n s = [(s', manhattan s s') | s' <- range box, manhattan s s' <= n, Just '.' <- [Map.lookup s' grid]]
    let costFromEnd = costs e grid
    let costFromStart = costs s grid
    let savings n s =
            [ (s, rs, best)
            | (rs, dist) <- reachable 2 s
            , let c = costFromEnd ! rs
            , let c' = costFromStart ! s
            , let best = og - (c' + c + dist)
            , best >= 100
            ]
    print $ length $ concat $ group $ sort $ fmap thd3 $ nubOrdOn (\(s, rs, _) -> (s, rs)) $ concat [savings 20 k | k <- Map.keys grid, Just '.' <- [Map.lookup k grid]]

-- let new =
--         [ og - c
--         | (k, '#') <- Map.assocs grid
--         , removable k
--         , let grid' = Map.insert k '.' grid
--         , (Just (c, _)) <- [shortest grid' s e]
--         ]
--       where
--         removable k = length [k' | k' <- cardinal k, Just '.' <- [Map.lookup k' grid]] >= 2
-- print $ countBy (>= 100) new
-- print $ length $ reachable s

(!) :: (Eq a1) => [(a1, a2)] -> a1 -> a2
(!) xs x = fromJust $ lookup x xs

shortest :: Map Coord Char -> Coord -> Coord -> Maybe (Int, [Coord])
shortest grid s e = dijkstra neighs (\_ _ -> 1) (e ==) s
  where
    neighs curr = [v | v <- cardinal curr, Just '.' == Map.lookup v grid]

costs :: Coord -> Map Coord Char -> [(Coord, Int)]
costs s grid = bfs mempty $ Queue.fromList [(s, 0)]
  where
    bfs _ Queue.Empty = mempty
    bfs visited ((curr, cost) :<| q)
        | Just n <- Map.lookup curr visited
        , n < cost =
            bfs visited q
        | otherwise = (curr, cost) : bfs (Map.insert curr cost visited) (Queue.appendList q neighs)
      where
        neighs = [(v, cost + 1) | v <- cardinal curr, Just '.' == Map.lookup v grid]

-- bestcosts :: Coord -> Array Coord Char -> [(Coord, Int, Bool)]
-- bestcosts s grid = bfs False mempty $ Queue.fromList [(s, 0, False)]
--   where
--     bfs _ _ Queue.Empty = mempty
--     bfs cheated visited ((curr, cost, cheat) :<| q)
--         | Just n <- Map.lookup curr visited
--         , n < cost =
--             bfs cheated visited q
--         | otherwise = (curr, cost, cheat) : bfs (cheat || cheated) (Map.insert curr cost visited) (Queue.appendList q neighs)
--       where
--         neighs = [(v, cost + 1, False) | v <- cardinal curr, Just '.' == arrIx grid v] <> if not cheated then cheats else []
--         cheats =
--             [ (v, cost + 2, True)
--             | v <-
--                 [ right (right curr)
--                 , left (left curr)
--                 , above (above curr)
--                 , below (below curr)
--                 , right (above curr)
--                 , left (above curr)
--                 , right (below curr)
--                 , left (below curr)
--                 ]
--             , Just '.' == arrIx grid v
--             ]
