module Main where

import Advent.Coord (
    Coord (..),
    cardinal,
    coordLines,
    coordToChar,
    manhattan,
    origin,
 )
import Advent.Format (format)
import Data.List.Extra (dropEnd)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.MemoTrie (memo3)
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = do
    input <- [format|2024 21 (%y%n)*|]
    let keypad = fmap direction . allPaths
    let calculate n xs = sum $ zipWith (dp n) ('A' : xs) xs
    let solve n =
            sum
                [ v * minimum (fmap (calculate n) (keypad line))
                | line <- input
                , let v = read (dropEnd 1 line)
                ]
    print $ solve 2
    print $ solve 25

dp :: Int -> Char -> Char -> Int
dp = memo3 go
  where
    go 1 from to = manhattan (coord from) (coord to) + 1
    go depth from to = minimum [sum $ zipWith next ('A' : p) p | p <- ps]
      where
        ps = fmap direction (shortestPaths directional (coord from) (coord to))
        next = dp (depth - 1)
        directional = Map.delete origin $ Map.fromList $ coordLines ["_^A", "<v>"]

coord :: Char -> Coord
coord c =
    fromJust $
        lookup
            c
            [ ('^', C 0 1)
            , ('v', C 1 1)
            , ('>', C 1 2)
            , ('<', C 1 0)
            , ('A', C 0 2)
            ]

shortestPaths :: Map Coord Char -> Coord -> Coord -> [[Coord]]
shortestPaths grid from to =
    filter ((== manhattan from to + 1) . length) $
        Set.toList $
            bfs mempty mempty from
  where
    bfs :: [Coord] -> Set Coord -> Coord -> Set [Coord]
    bfs acc visited x
        | x == to = Set.singleton (reverse (x : acc))
        | Set.member x visited = mempty
        | otherwise = Set.unions $ fmap (bfs (x : acc) (Set.insert x visited)) ns
      where
        ns = [n | n <- cardinal x, isJust (Map.lookup n grid)]

direction :: [Coord] -> String
direction xs =
    [ if (to - fr) == origin
        then 'A'
        else fromJust $ coordToChar (to - fr)
    | (fr, to) <- zip xs (tail xs)
    ]
        <> "A"

allPaths :: String -> [[Coord]]
allPaths as = catPaths (fmap (uncurry (shortestPaths numerical)) ps)
  where
    ps = zip (coords numerical as) (tail (coords numerical as))
    catPaths [] = error "empty input"
    catPaths [a] = a
    catPaths (xs : ys : xss) = catPaths ([x <> y | x <- xs, y <- ys] : xss)
    numerical = Map.delete (C 3 0) $ Map.fromList $ coordLines ["789", "456", "123", "_0A"]

coords :: Map Coord Char -> String -> [Coord]
coords grid xs =
    [ fromJust $ lookup x keypad
    | let keypad = [(v, k) | (k, v) <- Map.assocs grid]
    , x <- 'A' : xs
    ]
