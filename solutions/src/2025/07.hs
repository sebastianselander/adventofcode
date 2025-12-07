module Main where

import Advent.Coord (Coord, below, coordArray, left, right)
import Advent.Format (format)
import Advent.Prelude ((!?))
import Data.Array (Array, assocs)
import Data.MemoTrie (memo)
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = do
    xs <- coordArray @Array <$> [format|2025 7 (%y%n)*|]
    let start = head [x | (x, 'S') <- assocs xs]
    let (p1, p2) = unzip $ dfs xs [start] mempty
    print $ sum p1
    print $ head p2

step :: Coord -> Array Coord Char -> [Coord]
step c xs = case xs !? bel of
    Nothing -> []
    Just '^' -> [right bel, left bel]
    Just _ -> [bel]
  where
    bel = below c

dfs :: Array Coord Char -> [Coord] -> Set Coord -> [(Int, Int)]
dfs _ [] _ = []
dfs arr (x : xs) seen
    | x `Set.member` seen = dfs arr xs seen
    | otherwise = case step x arr of
        ys ->
            (max 0 (length ys - 1), sum (fmap (search arr) ys))
                : dfs arr (ys <> xs) (Set.insert x seen)

search :: Array Coord Char -> Coord -> Int
search arr = memo go
  where
    go x = case step x arr of
        [] -> 1
        ys -> sum $ fmap (search arr) ys
