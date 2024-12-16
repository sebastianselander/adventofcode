{-# LANGUAGE ViewPatterns #-}

module Main where

import Advent.Coord (Coord (C), cardinal, cardinalOn, coordLines, drawPicture, east, north, turnLeft, turnRight)
import Advent.Format (format, format')
import Advent.PQueue (PQueue, insert, singleton, viewWithPriority)
import Advent.Queue (Queue ((:<|)))
import Advent.Queue qualified as Q
import Data.List (groupBy)
import Data.List.Extra (groupOn, nubOrd, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Advent.Coord (west)
import Debug.Trace (traceShow)
import Data.Array.Base (memcpy_thaw)

main :: IO ()
main = do
    input <- Map.fromList . coordLines <$> [format|2024 16 (%s%n)*|]
    let s = start input
    let e = end input
    let walked = dijk input (s, east, 0) mempty e
    let cost = snd walked
    print cost
    -- print $ length $ nubOrd $ back s cost input e
    let cost_graph = nubOrd $ costGraph s input
    let nodes = nubOrd $ backtrack s cost_graph [(e, cost)]
    print $ length nodes

    putStrLn $ drawPicture $ Map.fromList ((,'O') <$> nodes)

start, end :: Map.Map a Char -> a
start grid = head [c | (c, 'S') <- Map.toList grid]
end grid = head [c | (c, 'E') <- Map.toList grid]

backtrack :: Coord -> [(Coord, Int)] -> [(Coord, Int)] -> [Coord]
backtrack s grid [] = []
backtrack s grid ((curr, cost) : rest)
    | curr == s = curr : backtrack s grid rest
    | otherwise = curr : backtrack s grid (rest <> [(p, v) | n <- cardinal curr, (p, v) <- filter (\(x, c) -> x == n) grid, cost - v == 1 || cost - v == 1001])

dijk ::
    Map Coord Char ->
    (Coord, Coord, Int) ->
    Set Coord ->
    Coord ->
    ([(Coord, Coord, Int)], Int)
dijk grid (s, dir, cost) = walk grid (singleton cost (s, dir, [(s, east, 0)]))

walk ::
    Map Coord Char ->
    PQueue (Coord, Coord, [(Coord, Coord, Int)]) ->
    Set Coord ->
    Coord ->
    ([(Coord, Coord, Int)], Int)
walk grid q vis end = case viewWithPriority q of
    Nothing -> ([], 0)
    Just (cost, (curr, _, acc), q')
        | end == curr -> (acc, cost)
        | Set.member curr vis -> walk grid q' vis end
    Just (cost, (curr, dir, acc), q) -> walk grid q' (Set.insert curr vis) end
      where
        q' =
            foldr
                (\(a, b, c, d) ac -> insert a (b, c, d) ac)
                q
                [ (cost', n, dir', (n, dir', cost') : acc)
                | n <- cardinalOn (\c -> Map.lookup c grid /= Just '#') curr
                , let dir' = n - curr
                , let cost' = cost + if dir' /= dir then 1001 else 1
                ]

costGraph :: Coord -> Map Coord Char -> [(Coord, Int)]
costGraph s grid = bfs mempty $ Q.fromList [(s, east, 0), (s, north, 1000)]
  where
    bfs _ Q.Empty = []
    bfs visited ((curr, dir, cost) :<| queue)
        | Just n <- Map.lookup (curr, dir) visited
        , n < cost =
            bfs visited queue
        | otherwise = (curr, cost) : bfs (Map.insert (curr, dir) cost visited) (Q.appendList queue neighs)
      where
        neighs =
            [ (curr + dir', dir', cost')
            | n <- [curr + dir, curr + turnLeft dir, curr + turnRight dir]
            , Just '#' /= Map.lookup n grid
            , let dir' = n - curr
            , let cost' = cost + if dir' /= dir then 1001 else 1
            , case Map.lookup (n, dir') visited of
                Nothing -> True
                Just v -> v > cost'
            ]

back :: Coord -> Int -> Map Coord Char -> Coord -> [Coord]
back start cost grid coord = dfs mempty cost [] (coord, west)
  where
    dfs vis cost acc (curr,dir) 
        | Just n <- Map.lookup (curr,dir) vis
        , n > cost = [] 
        | start == curr && cost == 0 = curr:acc
        | cost <= 0 = [] 
        | otherwise = undefined
          where
            neighbors = [ ((v, dir'),cost') | v <- cardinal curr, Map.lookup v grid /= Just '#', let dir' = v - curr, let cost' = if dir' /= dir then 1001 else 1 ]
