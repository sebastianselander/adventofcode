module Main where

import Advent.Format (format)
import Advent.Prelude (count)
import Data.Composition ((.:))
import Data.List (sort)
import Data.Tuple.Extra (both)

main :: IO ()
main = do
    input <- [format|2024 1 ((%i   %i)%n)*|]
    let (xs, ys) = both sort $ unzip input
    print $ sum $ zipWith (abs .: (-)) xs ys
    print $ sum [x * count x ys | x <- xs]
