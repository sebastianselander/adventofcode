module Main where

import Advent.Format (format)
import Advent.Prelude
import Data.Char (digitToInt)

main :: IO ()
main = do
    s <- [format|2025 3 (%s%n)*|]
    let digs = fmap (fmap digitToInt) s
    print $ sum $ fmap largest digs
    print $ sum $ fmap (fromDigits 10 . largest2 11) digs

largest :: [Int] -> Int
largest xs = m * 10 + maximum (drop 1 $ dropWhile (/= m) xs)
  where
    m = maximum (init xs)

largest2 :: Int -> [Int] -> [Int]
largest2 0 xs = [maximum xs]
largest2 n xs = m : largest2 (n-1) (drop 1 $ dropWhile (/=m) xs)
  where
    m = maximum (times n init xs)
