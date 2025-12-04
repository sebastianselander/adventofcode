module Main where

import Advent.Coord
import Advent.Format
import Data.Array
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    s <- getArrayInput 2025 4
    print $ length $ accessible s
    print $ run s

access :: Coord -> Array Coord Char -> Maybe Coord
access c arr =
    if (arr ! c == '@')
        && ( 4
                > length
                    ( filter
                        (== '@')
                        ([arr ! cs | cs <- neighborsOn (contains (bounds arr)) c])
                    )
           )
        then Just c
        else Nothing

accessible :: Array Coord Char -> [Coord]
accessible arr = mapMaybe (`access` arr) (indices arr)

run :: Array Coord Char -> Int
run arr = if null x then 0 else length x + run (arr // map (,'.') x)
  where
    x = accessible arr
