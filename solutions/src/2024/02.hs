module Main where

import Advent.Format (format)
import Advent.Prelude (countOn, deleteAt)
import Data.List

main :: IO ()
main = do
    input <- [format|2024 2 (%i& %n)*|]
    print $ countOn id $ fmap safe input
    print $ countOn id $ fmap (any safe . removals) input

safe :: [Int] -> Bool
safe x = (byPair (<) x || byPair (>) x) && byPair (\x y -> abs (x - y) >= 1 && abs (x - y) <= 3) x
  where
    byPair f (x : y : xs) = f x y && byPair f (y : xs)
    byPair _ _ = True

removals :: [Int] -> [[Int]]
removals xs = [deleteAt n xs | n <- [0 .. length xs - 1]]
