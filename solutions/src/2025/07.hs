module Main where

import Advent.Coord
import Advent.Format
import Advent.Prelude
import Data.Array
import Data.MemoTrie
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = do
    xs <- coordArray @Array <$> [format|2025 7 (%y%n)*|]
    let start = head [x | (x, 'S') <- assocs xs]
    print $ dfs xs [start] mempty
    print $ dfs2 xs start

step :: Coord -> Array Coord Char -> (Int, [Coord])
step c xs = case xs !? bel of
    Nothing -> (0, [])
    Just '^' -> (1, [right bel, left bel])
    Just _ -> (0, [bel])
  where
    bel = below c

dfs :: Array Coord Char -> [Coord] -> Set Coord -> Int
dfs _ [] _ = 0
dfs arr (x : xs) seen
    | x `Set.member` seen = dfs arr xs seen
    | otherwise = case step x arr of
        (n, ys) -> n + dfs arr (ys <> xs) (Set.insert x seen)

dfs2 :: Array Coord Char -> Coord -> Int
dfs2 arr = memo go
  where
    go x =
        case step x arr of
            (_, []) -> 1
            (_, ys) -> sum $ fmap (dfs2 arr) ys
