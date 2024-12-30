{-# LANGUAGE ParallelListComp #-}

module Main where

import Advent.Format (format)
import Advent.Prelude (count, deletes)
import Data.Composition ((.:))

main :: IO ()
main = do
    input <- [format|2024 2 (%i& %n)*|]
    print $ count True $ fmap safe input
    print $ count True $ fmap (any safe . deletes 1) input

safe :: [Int] -> Bool
safe xs =
    (byPair (<) xs || byPair (>) xs)
        && byPair ((`elem` [1 .. 3]) . abs .: (-)) xs
  where
    byPair f xs = and [f x y | x <- xs | y <- drop 1 xs]
