module Main where

import Advent.Coord (
    Coord (..),
    charToCoord,
    coordLines,
    left,
    right,
 )
import Advent.Format (format)
import Data.List (foldl')
import Data.Map (Map, (!))
import Data.Map qualified as Map

main :: IO ()
main = do
    (input, moves') <- [format|2024 15 (%y%n)*%n(%y%n)*|]
    let Just moves = mapM charToCoord (concat moves')
    print $
        sum
            [ 100 * r + c
            | (C r c, 'O') <-
                Map.toList (foldl' (step part1) (buildGrid input) moves)
            ]
    print $
        sum
            [ 100 * r + c
            | (C r c, '[') <-
                Map.toList (foldl' (step part2) (buildGrid (fmap expand input)) moves)
            ]

buildGrid :: [[Char]] -> Map Coord Char
buildGrid = Map.fromList . coordLines

expand :: String -> String
expand = concatMap newTile
  where
    newTile '#' = "##"
    newTile 'O' = "[]"
    newTile '.' = ".."
    newTile '@' = "@."
    newTile _ = error "bad input"

at :: Map a Char -> a
at grid = head [k | (k, v) <- Map.toList grid, v == '@']

part2 :: Map Coord Char -> Coord -> Coord -> [(Coord, Char)]
part2 grid p d@(C 0 _) = part1 grid p d
part2 grid p d = [(p + d, grid ! p) | p <- moved] <> [(p, '.') | p <- moved]
  where
    moved = dfs [p] [p + d]
    dfs :: [Coord] -> [Coord] -> [Coord]
    dfs acc [] = acc
    dfs acc (pos : ps)
        | pos `elem` acc = dfs acc ps
        | otherwise = case grid ! pos of
            '#' -> []
            '[' -> dfs (pos : acc) (right pos : pos + d : ps)
            ']' -> dfs (pos : acc) (left pos : pos + d : ps)
            _ -> dfs acc ps

part1 :: Map Coord Char -> Coord -> Coord -> [(Coord, Char)]
part1 grid p d = go [] p
  where
    go acc p = case grid ! p of
        '.' -> acc
        '#' -> []
        c -> go ((p + d, c) : acc) (p + d)

updates :: Map Coord Char -> Coord -> [(Coord, Char)] -> Map Coord Char
updates grid _ [] = grid
updates grid c xs = Map.insert c '.' (foldr (uncurry Map.insert) grid xs)

step :: (Map Coord Char -> Coord -> t -> [(Coord, Char)]) -> Map Coord Char -> t -> Map Coord Char
step f grid d = updates grid fish (f grid fish d)
  where
    fish = at grid
