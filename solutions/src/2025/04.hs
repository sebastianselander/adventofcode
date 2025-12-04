module Main where

import Advent.Coord (Coord, neighbors)
import Advent.Format (getArrayInput)
import Advent.Prelude ((!?))
import Data.Array (Array, indices, (!), (//))
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    s <- getArrayInput 2025 4
    print $ head $ sim s
    print $ sum $ sim s

accessible :: Coord -> Array Coord Char -> Maybe Coord
accessible c arr
    | arr ! c == '@'
    , length ([() | cs <- neighbors c, arr !? cs == Just '@']) < 4 =
        Just c
    | otherwise = Nothing

sim :: Array Coord Char -> [Int]
sim arr
    | null rolls = []
    | otherwise = length rolls : sim (arr // map (,'.') rolls)
  where
    rolls = mapMaybe (`accessible` arr) (indices arr)
