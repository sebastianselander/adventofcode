module Main where

import Advent.Coord (
    Coord,
    above,
    below,
    coordLines,
    left,
    right,
 )
import Advent.Format (format)
import Advent.Prelude (count, countBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    input <- [format|2024 4 (%s%n)*|]
    let m = Map.fromList $ coordLines input
    let part1 = sum [xmasLook c m | c <- Map.keys m]
    let part2 = count 2 [masLook c m | c <- Map.keys m]
    print part1
    print part2

mas :: [String] -> Int
mas = countBy (== "MAS")

xmas :: [String] -> Int
xmas = countBy (== "XMAS")

lookAll :: (Ord k) => Map k a -> [k] -> [Maybe a]
lookAll m cs = [Map.lookup ix m | ix <- cs]

xmasLook :: Coord -> Map Coord Char -> Int
xmasLook c m = do
    let takes f = take 4 $ iterate f c
    xmas $
        fmap
            (catMaybes . lookAll m)
            [ takes above
            , takes below
            , takes right
            , takes left
            , takes (right . above)
            , takes (left . above)
            , takes (right . below)
            , takes (left . below)
            ]

masLook :: Coord -> Map Coord Char -> Int
masLook c m = do
    let upr = right $ above c
    let upl = left $ above c
    let downl = left $ below c
    let downr = right $ below c
    mas $
        fmap
            (catMaybes . lookAll m)
            [ [downl, c, upr]
            , [downr, c, upl]
            , [upr, c, downl]
            , [upl, c, downr]
            ]
