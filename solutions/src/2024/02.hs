{-# LANGUAGE ParallelListComp #-}

module Main where

import Advent.Format (format)
import Advent.Prelude (countOn, deleteAt, safeTail, (...))

main :: IO ()
main = do
    input <- [format|2024 2 (%i& %n)*|]
    print $ countOn id $ fmap safe input
    print $ countOn id $ fmap (any safe . removals) input

safe :: [Int] -> Bool
safe xs =
    (byPair (<) xs || byPair (>) xs)
        && byPair (\x y -> abs (x - y) `elem` [1, 2, 3]) xs
  where
    byPair f xs = and [f x y | x <- xs | y <- safeTail xs]

removals :: [Int] -> [[Int]]
removals xs = [deleteAt n xs | n <- 0 ... length xs]
