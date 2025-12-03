module Main where

import Advent.Format (format)
import Advent.Prelude (fromDigits, times)

main :: IO ()
main = do
    s <- [format|2025 3 (%d*%n)*|]
    print $ sum $ fmap (fromDigits 10 . largest 2) s
    print $ sum $ fmap (fromDigits 10 . largest 12) s

largest :: Int -> [Int] -> [Int]
largest 1 xs = [maximum xs]
largest n xs = m : largest (n - 1) (drop 1 $ dropWhile (/= m) xs)
  where
    m = maximum (times (n - 1) init xs)
