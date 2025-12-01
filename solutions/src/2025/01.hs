module Main where

import Advent.Format (format, format', fmt)
import Advent.Prelude (count)
import Debug.Trace (traceShow)

main :: IO ()
main = do
    s <- [format|2025 01 ((R|L)!%u%n)*|]
    print $ count 0 $ rot 50 s
    print $ count 0 $ rot2 50 s

rot :: Int -> [(String, Int)] -> [Int]
rot _ [] = []
rot code (("L", n):xs) = ((code - n) `mod` 100) : rot ((code - n) `mod` 100) xs
rot code (("R", n):xs) = ((code + n) `mod` 100) : rot ((code + n) `mod` 100) xs
rot _ _ = undefined

rot2 :: Int -> [(String, Int)] -> [Int]
rot2 _ [] = []
rot2 code ((dir, n):xs) = case dir of
    "L" | n > 0 -> ((code - 1) `mod` 100) : rot2 ((code - 1) `mod` 100) ((dir, n - 1):xs)
    "R" | n > 0 -> ((code + 1) `mod` 100) : rot2 ((code + 1) `mod` 100) ((dir, n - 1):xs)
    _ -> rot2 code xs
