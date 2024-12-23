module Main where

import Advent.Coord (Coord, above, below, coordArray, left, ne, nw, right, se, sw)
import Advent.Format (format)
import Advent.Prelude (count)
import Data.Array (Array, Ix, indices, (!))
import Data.Array.Base ((!?))
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    input <- [format|2024 4 (%s%n)*|]
    let arr = coordArray input
    let part1 = sum [xmasLook c arr | c <- indices arr]
    let part2 = count 2 [masLook c arr | c <- indices arr]
    print part1
    print part2

lookupAll :: (Ix k) => Array k a -> [k] -> [a]
lookupAll m = mapMaybe (m !?)

xmasLook :: Coord -> Array Coord Char -> Int
xmasLook c m
    | m ! c /= 'X' = 0
    | otherwise =
        let four f = take 4 $ iterate f c
         in count "XMAS" $
                fmap
                    (lookupAll m)
                    [ four above
                    , four below
                    , four right
                    , four left
                    , four (+ ne)
                    , four (+ nw)
                    , four (+ se)
                    , four (+ sw)
                    ]

masLook :: Coord -> Array Coord Char -> Int
masLook c m
    | m ! c /= 'A' = 0
    | otherwise = do
        let upr = right $ above c
        let upl = left $ above c
        let downl = left $ below c
        let downr = right $ below c
        count "MAS" $
            fmap
                (lookupAll m)
                [ [downl, c, upr]
                , [downr, c, upl]
                , [upr, c, downl]
                , [upl, c, downr]
                ]
