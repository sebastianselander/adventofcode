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
    print $ sum $ fmap (uncurry calculate) (zip ops (transpose ints))
    print $ sum (solve <$> splitOn [replicate (length xs) ' '] (transpose xs))

solve :: [String] -> Int
solve xs@(x:_) = calculate [last x] (fmap (read . filter isDigit) xs)

calculate :: String -> [Int] -> Int
calculate "*" = product
calculate "+" = sum
