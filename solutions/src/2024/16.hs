module Main where

import Advent.Coord (
    Coord,
    cardinal,
    coordLines,
    east,
 )
import Advent.Format (format)
import Advent.Queue (Queue ((:<|)))
import Advent.Queue qualified as Queue
import Control.Arrow (second)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = do
    input <- coordLines <$> [format|2024 16 (%s%n)*|]
    let s = head [c | (c, 'S') <- input]
    let e = head [c | (c, 'E') <- input]
    let graph =
            Map.fromListWith
                (++)
                ( second (: [])
                    <$> Set.toList (buildGraph s (Map.fromList input))
                )
    let cost = minimum $ fromJust $ Map.lookup e graph
    print cost
    print $ length $ backtrack s graph [(e, cost)]

backtrack :: Coord -> Map Coord [Int] -> [(Coord, Int)] -> Set Coord
backtrack _ _ [] = mempty
backtrack s grid ((curr, cost) : rest)
    | curr == s = Set.insert curr $ backtrack s grid rest
    | otherwise = Set.insert curr $ backtrack s grid (neighbors <> rest)
  where
    neighbors =
        [ (n, v')
        | n <- cardinal curr
        , Just v <- pure (Map.lookup n grid)
        , v' <- v
        , cost - v' `elem` [1,1001]
        ]

buildGraph :: Coord -> Map Coord Char -> Set (Coord, Int)
buildGraph s grid = bfs mempty $ Queue.fromList [(s, east, 0)]
  where
    bfs _ Queue.Empty = mempty
    bfs visited ((curr, dir, cost) :<| queue)
        | Just n <- Map.lookup (curr, dir) visited
        , n < cost =
            bfs visited queue
        | otherwise =
            Set.insert (curr, cost) $
                bfs (Map.insert (curr, dir) cost visited) (Queue.appendList queue neighbors)
      where
        neighbors =
            [ (n, dir', cost + if dir' /= dir then 1001 else 1)
            | n <- cardinal curr
            , Just '#' /= Map.lookup n grid
            , let dir' = n - curr
            ]
