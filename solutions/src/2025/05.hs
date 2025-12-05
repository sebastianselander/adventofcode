module Main where

import Advent.Format (format)
import Advent.Range (mergeRanges)
import Data.Ix (inRange, rangeSize)
import Data.List (nub)

main :: IO ()
main = do
    (rngs, xs :: [Int]) <- [format|2025 5 (%u-%u%n)*%n(%u%n)*|]
    print $ length $ nub [x | rn <- rngs, x <- xs, rn `inRange` x]
    print $ sum (rangeSize <$> mergeRanges rngs)
