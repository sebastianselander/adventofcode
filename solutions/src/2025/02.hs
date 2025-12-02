module Main where

import Advent.Format (format)
import Advent.Prelude (same, toDigits, chunks)
import Data.List (tails)

main :: IO ()
main = do
    s <- [format|2025 2 (%u-%u)&,%n|]
    let rngs = [[from .. to] | (from, to) <- s]
    print $ sum $ fmap (sum . filter (invalid . toDigits 10)) rngs
    print $ sum $ fmap (sum . filter (invalid2 . toDigits 10)) rngs

invalid2 :: [Int] -> Bool
invalid2 n = any (\x -> same (chunks (length x) n)) (init $ drop 1 $ tails n)

invalid :: [Int] -> Bool
invalid n = uncurry (==) $ splitAt (length n `div` 2) n
