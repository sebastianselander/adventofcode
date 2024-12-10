module Main where

import Advent.Coord (Coord, cardinalOn, coordArray)
import Advent.Format (format)
import Advent.Prelude (arrIx)
import Data.Array (Array, assocs, (!))
import Data.List (nub)

main :: IO ()
main = do
    input <- coordArray @Array <$> [format|2024 10 (%d*%n)*|]
    print $ length $ concatMap nub [walk input s | s <- starts input]
    print $ length $ concat [walk input s | s <- starts input]

starts :: Array Coord Int -> [Coord]
starts grid = [ix | (ix, n) <- assocs grid, n == 0]

walk :: Array Coord Int -> Coord -> [Coord]
walk grid s = f $ concatMap (walk grid) (next s)
  where
    f = if grid ! s == 9 then (s :) else id
    next s =
        cardinalOn
            ( \s' -> case arrIx grid s' of
                Just x -> grid ! s + 1 == x
                Nothing -> False
            )
            s
