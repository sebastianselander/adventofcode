module Main where

import Advent.Coord
import Advent.Format (getRawInput)
import Data.List.Extra (splitOn, trimEnd)
import Data.Map (Map, (!))
import Data.Map qualified as Map

main :: IO ()
main = do
    (grid, Just moves) <- parse <$> getRawInput 2024 15
    print $ sum [100 * r + c | (C r c, 'O') <- Map.toList (foldl (step part1) grid moves)]
    print $ sum [100 * r + c | (C r c, '[') <- Map.toList (foldl (step part2) (expand grid) moves)]

parse :: String -> (Map Coord Char, Maybe [Coord])
parse inp =
    ( Map.fromList $ coordLines (lines l)
    , sequence $ fmap charToCoord =<< lines (trimEnd r)
    )
  where
    [l, r] = splitOn "\n\n" inp

expand :: Map Coord Char -> Map Coord Char
expand grid =
    Map.fromList $
        concat
            [ [(k * multiply, vl), (right (k * multiply), vr)]
            | (k, v) <- Map.toList grid
            , let (vl, vr) = newTile v
            , let multiply = C 1 2
            ]
  where
    newTile '#' = ('#', '#')
    newTile 'O' = ('[', ']')
    newTile '.' = ('.', '.')
    newTile '@' = ('@', '.')
    newTile _ = error "error tile"

at :: Map a Char -> a
at grid = head [k | (k, v) <- Map.toList grid, v == '@']

part2 :: Map Coord Char -> Coord -> Coord -> [(Coord, Char)]
part2 grid p d@(C 0 _) = part1 grid p d
part2 grid p d =
    let moved = dfs [p] [p + d]
     in ((\x -> (x + d, grid ! x)) <$> moved) -- place boxes at new pos
            <> ((,'.') <$> moved) -- remove boxes from old pos
  where
    dfs :: [Coord] -> [Coord] -> [Coord]
    dfs acc [] = acc
    dfs acc (p : ps)
        | p `elem` acc = dfs acc ps
        | me == '[' = dfs (p : acc) (right p : p + d : ps)
        | me == ']' = dfs (p : acc) (left p : p + d : ps)
        | me == '.' = dfs acc ps
        | me == '@' = dfs acc ps
        | otherwise = []
      where
        me = grid ! p

part1 :: Map Coord Char -> Coord -> Coord -> [(Coord, Char)]
part1 grid p d = go [] p d
  where
    go acc p d
        | me == '.' = acc
        | me == '#' = []
        | otherwise = go ((p + d, me) : acc) (p + d) d
      where
        me = grid ! p

updates :: Map Coord Char -> Coord -> [(Coord, Char)] -> Map Coord Char
updates grid _ [] = grid
updates grid c xs = Map.insert c '.' (foldr (uncurry Map.insert) grid xs)

step :: (Map Coord Char -> Coord -> t -> [(Coord, Char)]) -> Map Coord Char -> t -> Map Coord Char
step f grid d = updates grid fish (f grid fish d)
  where
    fish = at grid
