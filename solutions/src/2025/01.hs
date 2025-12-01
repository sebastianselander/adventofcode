module Main where

import Advent.Format (format)
import Advent.Prelude (count)

main :: IO ()
main = do
    s <- [format|2025 01 (%c%u%n)*|]
    print $ count 0 $ rot s
    print $ count 0 $ rot2 50 s

f :: (Num a) => Char -> a -> a -> a
f 'L' = (-)
f 'R' = (+)

rot :: [(Char, Int)] -> [Int]
rot = scanl (\acc (dir, n) -> f dir acc n `mod` 100) 50

rot2 :: Int -> [(Char, Int)] -> [Int]
rot2 _ [] = []
rot2 code ((dir, n) : xs)
    | n > 0 = new : rot2 new ((dir, n - 1) : xs)
    | otherwise = rot2 code xs
  where
    new = f dir code 1 `mod` 100
