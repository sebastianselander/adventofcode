module Main where

import Advent.Coord (
    Coord,
    north,
    turnRight,
 )
import Advent.Format (getArrayInput)
import Advent.Prelude (arrIx, countBy)
import Data.Array (Array, assocs, (//))
import Data.List.Extra (nubOrdOn)
import Data.Set (Set, insert, member, toList)

main :: IO ()
main = do
    grid <- getArrayInput 2024 6
    let start = head [k | (k, '^') <- assocs grid]
    let walked = nubOrdOn snd $ toList (walk mempty north start grid)
    print $ length walked
    print $ countBy null [walk mempty north start (grid // [(k, '#')]) | (_, k) <- walked]

walk ::
    Set (Coord, Coord) ->
    Coord ->
    Coord ->
    Array Coord Char ->
    Set (Coord, Coord)
walk visited d p grid =
    if (d, p) `member` visited
        then mempty
        else case arrIx grid (d + p) of
            Nothing -> insert (d, p) visited
            Just '#' -> walk visited (turnRight d) p grid
            Just _ -> walk (insert (d, p) visited) d (d + p) grid
