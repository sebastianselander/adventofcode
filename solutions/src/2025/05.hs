module Main where

import Advent.Box (Box (Dim, Pt), intersectBox, size, unionBoxes)
import Advent.Format (format)
import Data.List (nub)

main :: IO ()
main = do
    (as, bs :: [Int]) <- [format|2025 5 (%u-%u%n)*%n(%u%n)*|]
    let ranges = [Dim l (u + 1) Pt | (l, u) <- as]
        boxes = [Dim x (x + 1) Pt | x <- bs]
    print $ length $ nub [box | range <- ranges, box <- boxes, Just _ <- [intersectBox range box]]
    print $ sum (size <$> unionBoxes ranges)
