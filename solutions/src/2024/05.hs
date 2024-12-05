module Main where

import Advent.Format (format)
import Control.Arrow (first)
import Data.Either (partitionEithers)
import Data.List (partition, sortBy)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Tuple (swap)

main :: IO ()
main = do
    input <- catMaybes <$> [format|2024 5 (%n|((%i\|%i)|%i&,)%n)*|]
    let (rules, list) = first (fmap swap) $ partitionEithers input
    let (goods, bads) = partition (inOrder mempty rules) list
    print $ sum $ fmap middle goods
    print $ sum $ fmap (middle . fixOrder rules) bads

lookupRule :: Int -> [(Int, Int)] -> [Int]
lookupRule n = mapMaybe f
  where
    f (a, b)
        | a == n = Just b
        | otherwise = Nothing

inOrder :: [Int] -> [(Int, Int)] -> [Int] -> Bool
inOrder !seen rules [] = True
inOrder !seen rules (x : xs)
    | x `elem` seen = False
    | otherwise = case lookupRule x rules of
        ys -> inOrder (ys <> seen) rules xs

middle :: [Int] -> Int
middle xs = xs !! (length xs `div` 2)

fixOrder :: [(Int, Int)] -> [Int] -> [Int]
fixOrder rules xs = fst <$> sortBy f [(x, ys) | x <- xs, let ys = lookupRule x rules]
  where
    f (a, xs) (b, ys)
        | a `elem` ys = GT
        | b `elem` xs = LT
        | otherwise = EQ
