module Main where

import Advent.Coord (Coord, contains, neighborsOn)
import Advent.Format (getArrayInput)
import Data.Array (Array, bounds, indices, (!), (//))
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    s <- getArrayInput 2025 4
    print $ head $ sim s
    print $ sum $ sim s

access :: Coord -> Array Coord Char -> Maybe Coord
access c arr
    | arr ! c == '@'
    , length ([() | cs <- neighborsOn (contains (bounds arr)) c, arr ! cs == '@']) < 4 =
        Just c
    | otherwise = Nothing

sim :: Array Coord Char -> [Int]
sim arr = if null x then [] else length x : sim (arr // map (,'.') x)
  where
    x = mapMaybe (`access` arr) (indices arr)
