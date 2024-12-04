module Main where

import Advent.Coord
import Advent.Format
import Advent.Prelude
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Debug.Trace (traceShowId)

main :: IO ()
main = do
    input <- [format|2024 4 (%s%n)*|]
    let s = Map.fromList $ coordLines input
    let res = sum [xmas $ look c s | c <- Map.keys s]
    let part2 = count 2 [p2 c s | c <- Map.keys s]
    print res
    print part2

mas :: [String] -> Int
mas = countBy (== "MAS")

xmas :: [String] -> Int
xmas = countBy (== "XMAS")

look :: Coord -> Map Coord Char -> [String]
look c m = do
    let up = take 4 $ iterate above c
    let down = take 4 $ iterate below c
    let r = take 4 $ iterate right c
    let l = take 4 $ iterate left c
    let upr = take 4 $ iterate (right . above) c
    let upl = take 4 $ iterate (left . above) c
    let downl = take 4 $ iterate (left . below) c
    let downr = take 4 $ iterate (right . below) c
    let f cs = [Map.lookup ix m | ix <- cs]
    [ catMaybes $ f up
        , catMaybes $ f down
        , catMaybes $ f r
        , catMaybes $ f l
        , catMaybes $ f upr
        , catMaybes $ f upl
        , catMaybes $ f downl
        , catMaybes $ f downr
        ]

p2 :: Coord -> Map Coord Char -> Int
p2 c m = do
    let upr = (right . above) c
    let upl = (left . above) c
    let downl = (left . below) c
    let downr = (right . below) c
    mas [ catMaybes [Map.lookup ix m | ix <- [downl, c, upr]]
        , catMaybes [Map.lookup ix m | ix <- [downr, c, upl]]
        , catMaybes [Map.lookup ix m | ix <- [upr, c, downl]]
        , catMaybes [Map.lookup ix m | ix <- [upl, c, downr]]
        ]
