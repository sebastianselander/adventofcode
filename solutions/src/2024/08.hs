module Main where

import Advent.Coord
    ( Coord(..),
      coordArray,
      boundingBox,
      contains,
      coordCol,
      coordRow )
import Advent.Format
import Data.Array (Array, assocs, indices)
import Data.List.Extra (nubOrd)

main :: IO ()
main = do
    input <- getArrayInput 2024 8
    let Just box = boundingBox $ indices input
    print $ length $ filter (contains box) $ nubOrd $ concatMap ((\(a,b) -> [a,b]) . antinodes) (pairs input)
    print $ length $ nubOrd $ concatMap (filter (contains box) . antinodes2 box) (pairs input)


pairs :: Array Coord Char -> [(Coord, Coord)]
pairs grid =
    [ (i, i')
    | (i, c) <- assocs grid
    , (i', c') <- assocs grid
    , i /= i'
    , c == c'
    , c /= '.'
    , c' /= '.'
    ]

antinodes :: (Coord, Coord) -> (Coord, Coord)
antinodes (l, r) = (l + C (coordRow l - coordRow r) (coordCol l - coordCol r), r + C (coordRow r - coordRow l) (coordCol r - coordCol l))

antinodes2 :: (Coord, Coord) -> (Coord, Coord) -> [Coord]
antinodes2 box (l, r) =
    let l' = C (coordRow l - coordRow r) (coordCol l - coordCol r)
        r' = C (coordRow r - coordRow l) (coordCol r - coordCol l)
        ls = takeWhile (contains box) $ iterate (+ l') l
        rs = takeWhile (contains box) $ iterate (+ r') r
     in ls <> rs
