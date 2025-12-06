module Main where

import Advent.Format
import Data.Char (isDigit)
import Data.List (transpose)
import Data.List.Extra (splitOn)

main :: IO ()
main = do
    xs <- lines <$> getRawInput 2025 6
    let ints = fmap read . words <$> init xs
    let ops = words $ last xs
    print $ sum $ fmap (uncurry calculate) (zip (transpose ints) ops)
    print $ sum (solve <$> splitOn [replicate (length xs) ' '] (transpose xs))

solve :: [String] -> Int
solve xs@(x:_) = calculate (fmap (read . filter isDigit) xs) [last $ x]

calculate :: [Int] -> String -> Int
calculate xs "*" = product xs
calculate xs "+" = sum xs
