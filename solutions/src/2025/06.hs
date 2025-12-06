module Main where

import Advent.Format
import Data.Char (isDigit)
import Data.List (transpose)
import Data.List.Extra (splitOn)

main :: IO ()
main = do
    xs <- lines <$> getRawInput 2025 6
    let (ints, ops) = (fmap read . words <$> init xs, words $ last xs)
    print $ sum $ fmap (uncurry calc) (zip (transpose ints) ops)
    print $ sum (solve <$> splitOn ["     "] (transpose xs))

solve :: [String] -> Int
solve xs =
    let ys = fmap (read @Int . filter isDigit) xs
     in calc ys [last $ head xs]

calc :: [Int] -> String -> Int
calc xs "*" = product xs
calc xs "+" = sum xs
