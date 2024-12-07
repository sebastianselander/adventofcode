module Main where

import Advent.Format (format)
import Data.List.Extra (find)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    input <- [format|2024 7 (%i: (%i& )%n)*|]
    print $ sum $ mapMaybe calibrate input
    print $ sum $ mapMaybe calibrate2 input

calibrate :: (Int, [Int]) -> Maybe Int
calibrate (_, []) = Nothing
calibrate (result, y : ys) = find (== result) $ go y ys
  where
    go :: Int -> [Int] -> [Int]
    go acc [] = [acc]
    go acc (x : xs)
        | acc > result = []
        | otherwise = go (acc + x) xs <> go (acc * x) xs

calibrate2 :: (Int, [Int]) -> Maybe Int
calibrate2 (_, []) = Nothing
calibrate2 (result, y : ys) = find (== result) $ go y ys
  where
    go :: Int -> [Int] -> [Int]
    go acc [] = [acc]
    go acc (x : xs)
        | acc > result = []
        | otherwise = go (acc `cat` x) xs <> go (x + acc) xs <> go (x * acc) xs

cat :: Int -> Int -> Int
cat n m = read $ show n <> show m
