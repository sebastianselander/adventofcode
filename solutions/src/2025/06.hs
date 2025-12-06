module Main where

import Advent.Format
import Data.Char (isDigit)
import Data.List (transpose)
import Data.List.Extra (splitOn)

main :: IO ()
main = do
    (ints, ops) <- [format'|2025 6 (( *%u *)*%n)*( *(\*|\+)! *)*%n|]
    xs <- transpose . lines <$> getRawInput 2025 6
    print $ sum $ fmap (uncurry calc) (zip (transpose ints) ops)
    print $ sum (solve <$> splitOn ["     "] xs)

solve :: [String] -> Int
solve xs =
    let ys = fmap (read @Int . filter isDigit) xs
     in case last (head xs) of
            '+' -> sum ys
            '*' -> product ys

calc :: [Int] -> String -> Int
calc xs "*" = product xs
calc xs "+" = sum xs
