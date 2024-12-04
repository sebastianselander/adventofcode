module Main where

import Advent.Coord (Coord, above, below, coordLines, left, right)
import Advent.Format (format)
import Advent.Prelude (count, countBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    input <- [format|2024 4 (%s%n)*|]
    let m = Map.fromList $ coordLines input
    let part1 = sum [xmasLook c m | c <- Map.keys m]
    let part2 = count 2 [masLook c m | c <- Map.keys m]
    print part1
    print part2

lookupAll :: (Ord k) => Map k a -> [k] -> [a]
lookupAll m = mapMaybe (`Map.lookup` m)

xmasLook :: Coord -> Map Coord Char -> Int
xmasLook c m = do
    let four f = take 4 $ iterate f c
    count "XMAS" $
        fmap
            (lookupAll m)
            [ four above
            , four below
            , four right
            , four left
            , four (right . above)
            , four (left . above)
            , four (right . below)
            , four (left . below)
            ]

masLook :: Coord -> Map Coord Char -> Int
masLook c m = do
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
