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
    arr <- coordArray @Array <$> [format|2025 7 (%y%n)*|]
    let start = head [x | (x, 'S') <- assocs arr]
    let dfs :: [Coord] -> Set Coord -> Int
        dfs [] _ = 0
        dfs (x:xs) seen
          | x `Set.member` seen = dfs xs seen
          | otherwise = case arr !? x of
                Nothing -> dfs xs seen
                Just '^' -> 1 + dfs (left (below x) : right (below x) : xs) (Set.insert x seen)
                Just _ -> dfs (below x : xs) (Set.insert x seen)
    let countPaths :: Coord -> Int
        countPaths = memo go
        go c = case arr !? c of
            Nothing -> 1
            Just '^' -> countPaths  (left (below c)) + countPaths  (right (below c))
            Just _ -> countPaths  (below c)
    print $ dfs [start] mempty
    print $ countPaths start
