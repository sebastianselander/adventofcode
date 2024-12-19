module Main where

import Advent.Format (format, format')
import Advent.Prelude
import Advent.Coord
import Control.Arrow (first)

safeInit [] = []
safeInit xs = init xs

main :: IO ()
main = do
    input <- fmap (first init) [format'|2024 19 %s&( )%n%n(%s%n)*|]
    print input

possible :: String -> [String] -> Bool
possible want available = undefined
