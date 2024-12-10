module Main where

import Advent.Coord (Coord, cardinalOn, coordArray)
import Advent.Format (format)
import Advent.Prelude (arrIx)
import Data.Array.Base (UArray, assocs, (!))
import Data.Bool (bool)
import Data.List.Extra (nubOrd)

main :: IO ()
main = do
    grid <- coordArray @UArray <$> [format|2024 10 (%d*%n)*|]
    let starts = [ix | (ix, n) <- assocs grid, n == 0]
    let walked = [walk grid s | s <- starts]
    print $ length $ concatMap nubOrd walked
    print $ length $ concat walked

walk :: UArray Coord Int -> Coord -> [Coord]
walk grid s = bool id (s :) (current == 9) $ concatMap (walk grid) (next s)
  where
    current = grid ! s
    next = cardinalOn ((Just (current + 1) ==) . arrIx grid)
