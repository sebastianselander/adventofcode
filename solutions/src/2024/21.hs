{-# LANGUAGE ViewPatterns #-}

module Main where

import Advent.Coord
    ( cardinal, coordLines, coordToChar, manhattan, origin, Coord(..) )
import Advent.Format (format, format')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Data.List.Extra (dropEnd)
import Data.MemoTrie (memo3)

main :: IO ()
main = do
    input <- [format|2024 21 (%y%n)*|]
    let h = fmap human input 
    let f xs = sum $ zipWith (dp 24) ('A' : xs) xs
    print $ sum $ zipWith (*) (fmap (minimum . fmap f) h) (fmap (read . dropEnd 1) input)

numerical :: Map Coord Char
numerical = Map.delete (C 3 0) $ Map.fromList $ coordLines ["789", "456", "123", "_0A"]

directional :: Map Coord Char
directional = Map.delete origin $ Map.fromList $ coordLines ["_^A", "<v>"]

dp :: Int -> Char -> Char -> Int
dp = memo3 go
  where
    go :: Int -> Char -> Char -> Int
    go 0 from to = manhattan (coord from) (coord to) + 1
    go depth from to = minimum [sum $ zipWith next ('A' : p) p | p <- ps]
      where
        ps = fmap ((<> "A") . direction) (shortestPaths directional (coord from) (coord to))
        next = dp (depth - 1)

coord :: Char -> Coord
coord c = fromJust $ lookup c pad

char :: Coord -> Char
char c = fromJust $ lookup c (fmap swap pad)

pad :: [(Char, Coord)]
pad = [('^', C 0 1), ('v', C 1 1), ('>', C 1 2), ('<', C 1 0), ('A', C 0 2)]

shortestPaths :: Map Coord Char -> Coord -> Coord -> [[Coord]]
shortestPaths grid from to = filter ((== manhattan from to + 1) . length) $ Set.toList $ bfs mempty mempty from
  where
    bfs :: [Coord] -> Set Coord -> Coord -> Set [Coord]
    bfs acc visited x
        | x == to = Set.singleton (reverse (x : acc))
        | Set.member x visited = mempty
        | otherwise = Set.unions $ fmap (bfs (x : acc) (Set.insert x visited)) ns
      where
        ns = [n | n <- cardinal x, isJust (Map.lookup n grid)]

human :: String -> [String]
human = fmap ((<> "A") . direction) . path numerical

robot :: String -> [String]
robot = fmap direction . path directional

direction :: [Coord] -> String
direction [] = ""
direction xs =
    [ if (t - f) == origin
        then 'A'
        else fromMaybe '_' $ coordToChar (t - f)
    | (f, t) <- zip xs (tail xs)
    ]

path :: Map Coord Char -> String -> [[Coord]]
path grid xs = go ys -- concat [head (shortestPaths from to) | (from, to) <- ps]
  where
    ps = zip (coords grid xs) (tail (coords grid xs))
    ys = fmap (uncurry (shortestPaths grid)) ps
    go [] = error "empty input"
    go [a] = a
    go (xs : ys : xss) = go ([x <> y | x <- xs, y <- ys] : xss)

coords :: Map Coord Char -> String -> [Coord]
coords grid xs =
    [ fromJust $ lookup x keypad
    | let keypad = [(v, k) | (k, v) <- Map.assocs grid]
    , x <- 'A' : xs
    ]
