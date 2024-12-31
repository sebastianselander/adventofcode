module Main where

import Advent.Format (format)
import Advent.Prelude (countBy)
import Data.List (isPrefixOf)

main :: IO ()
main = do
    input <- [format|2015 5 (%s%n)*|]
    print input
    let vowels = "aeiou"
    let bad = ["ab", "cd", "pq", "xy"]
    print $
        length
            [ ()
            | x <- input
            , countBy (`elem` vowels) x >= 3
            , row x
            , not (any (`subsequence` x) bad)
            ]

    print $ length [() | x <- input, part2 x]

part2 :: [Char] -> Bool
part2 str = twice str && repeats str

twice :: [Char] -> Bool
twice (a : b : xs) = subsequence [a, b] xs || twice (b : xs)
twice _ = False

repeats :: (Eq a) => [a] -> Bool
repeats (a : b : c : xs) = a == c || repeats (b : c : xs)
repeats _ = False

row :: String -> Bool
row [] = False
row [_] = False
row (x : y : xs)
    | x == y = True
    | otherwise = row (y : xs)

subsequence :: String -> String -> Bool
subsequence _ [] = False
subsequence s xs
    | s `isPrefixOf` xs = True
    | otherwise = subsequence s (drop 1 xs)
