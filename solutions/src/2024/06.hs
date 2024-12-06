module Main where

import Advent.Coord (
    Coord,
    coordArray,
    north,
    turnRight,
 )
import Advent.Format (format)
import Advent.Prelude (arrIx, count)
import Data.Array (Array, assocs, (//))
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = do
    input <- [format|2024 6 (%t*%n)*|]
    let grid = coordArray input
    let start = head [k | (k, '^') <- assocs grid]
    let walked = fromJust $ walk start grid
    print $ Set.size walked
    print $ count Nothing [walk start (grid // [(k, '#')]) | k <- Set.toList walked]

walk :: Coord -> Array Coord Char -> Maybe (Set Coord)
walk start grid = go mempty north start
  where
    go :: Set (Coord, Coord) -> Coord -> Coord -> Maybe (Set Coord)
    go seen d p =
        if Set.member (d, p) seen
            then Nothing
            else case arrIx grid (d + p) of
                Nothing -> Just $ Set.insert p $ Set.map snd seen
                Just '#' -> go seen (turnRight d) p
                Just _ -> go (Set.insert (d, p) seen) d (d + p)
