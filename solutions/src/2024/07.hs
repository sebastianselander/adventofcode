module Main where

import Advent.Format (format)
import Advent.Prelude (fixed)
import Data.List.Extra (find)
import Data.Maybe (mapMaybe)
import Prelude hiding ((||))

main :: IO ()
main = do
    input <- [format|2024 7 (%i: (%i& )%n)*|]
    print $ sum $ mapMaybe (calibrate [(*), (+)]) input
    print $ sum $ mapMaybe (calibrate [(*), (+), (||)]) input

calibrate :: [Int -> Int -> Int] -> (Int, [Int]) -> Maybe Int
calibrate _ (_, []) = Nothing
calibrate fs (result, y : ys) = find (== result) $ go y ys
  where
    go :: Int -> [Int] -> [Int]
    go acc [] = [acc]
    go acc (x : xs)
        | acc > result = []
        | otherwise = concat [go (f acc x) xs | f <- fs]

(||) :: Int -> Int -> Int
(||) n m = n * fixed (> m) (* 10) 10 + m
