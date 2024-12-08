module Main where

import Advent.Coord (
    Coord (..),
    boundingBox,
    contains,
 )
import Advent.Format (getArrayInput)
import Data.Array (Array, assocs, indices)
import Data.List.Extra (nubOrd)

main :: IO ()
main = do
    input <- getArrayInput 2024 8
    let box = boundingBox $ indices input
    let solve f = length . nubOrd . concatMap (filter (contains box) . f) . pairs
    print $ solve antinodes input
    print $ solve (antinodes2 box) input

pairs :: Array Coord Char -> [(Coord, Coord)]
pairs grid =
    [ (i, i')
    | (i, c) <- assocs grid
    , (i', c') <- assocs grid
    , i /= i'
    , c == c'
    , c /= '.'
    ]

antinodes :: (Coord, Coord) -> [Coord]
antinodes (l, r) = [2 * l - r, 2 * r - l]

antinodes2 :: (Coord, Coord) -> (Coord, Coord) -> [Coord]
antinodes2 box (l, r) = takeWhile (contains box) =<< [iterate (+ (l - r)) l, iterate (+ (r - l)) r]
