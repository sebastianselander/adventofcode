module Main where

import Advent.Coord (
    Coord,
    cardinal,
    coordLines,
    manhattan,
 )
import Advent.Format (format)
import Advent.Queue (Queue (..), appendList, fromList)
import Data.Array (range)
import Data.Map (Map, (!))
import Data.Map qualified as Map

main :: IO ()
main = do
    input <- Map.fromList . coordLines <$> [format|2024 20 (%y%n)*|]
    let s = head [x | (x, 'S') <- Map.assocs input]
        e = head [x | (x, 'E') <- Map.assocs input]
        grid = foldr (uncurry Map.insert) input [(s, '.'), (e, '.')]
        costsFromEnd = distanceMap e grid
        costsFromStart = distanceMap s grid
        reachable n v =
            [ (s', manhattan v s')
            | s' <- range (v - 20, v + 20)
            , manhattan v s' <= n
            , Just '.' <- pure (Map.lookup s' grid)
            ]
        solve n =
            length
                [ ()
                | v <- Map.keys grid
                , Just '.' <- pure (Map.lookup v grid)
                , (rs, mh) <- reachable n v
                , costsFromStart ! e - (costsFromStart ! v) - (costsFromEnd ! rs) - mh >= 100
                ]
    print $ solve 2
    print $ solve 20

distanceMap :: Coord -> Map Coord Char -> Map Coord Int
distanceMap start grid = bfs mempty $ fromList [(start, 0)]
  where
    bfs _ Empty = mempty
    bfs visited ((curr, cost) :<| q)
        | Just n <- Map.lookup curr visited
        , n < cost =
            bfs visited q
        | otherwise =
            Map.insert curr cost $
                bfs
                    (Map.insert curr cost visited)
                    ( appendList
                        q
                        ( [ (v, cost + 1)
                          | v <- cardinal curr
                          , Just '.' == Map.lookup v grid
                          ]
                        )
                    )
