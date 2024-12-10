{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Advent.Format (format)
import Control.Arrow ((&&&))
import Data.Maybe (catMaybes)
import Data.Sequence (Seq (..), fromList)

main :: IO ()
main = do
    input <- [format|2024 9 (%d*)%n|]
    let p = part1 input
    print $
        sum $
            zipWith (*) [0 ..] $
                take (length (catMaybes p)) $
                    uncurry merge $
                        (id &&& reverse) p
    print $ total 0 $ move $ fromList $ part2 0 input

data Block = File Int Int | Free Int
    deriving (Show)

total :: Int -> Seq Block -> Int
total _ Empty = 0
total n (Free m :<| xs) = total (n + m) xs
total n (File m c :<| xs) = sum (take m $ map (* c) [n ..]) + total (n + m) xs

part1 :: [Int] -> [Maybe Int]
part1 xs = go 0 xs
  where
    go _ [] = []
    go !n (file : free : xs) =
        replicate file (Just n)
            <> replicate free Nothing
            <> go (1 + n) xs
    go !n [file] = replicate file (Just n)

part2 :: Int -> [Int] -> [Block]
part2 _ [] = []
part2 n (file : free : xs) = File file n : Free free : part2 (n + 1) xs
part2 n [x] = [File x n]

merge :: [Maybe Int] -> [Maybe Int] -> [Int]
merge [] _ = []
merge _ [] = []
merge (Just x : xs) ys = x : merge xs ys
merge (Nothing : xs) (Just y : ys) = y : merge xs ys
merge xs (Nothing : ys) = merge xs ys

move :: Seq Block -> Seq Block
move Empty = Empty
move (xs :|> Free n) = move xs :|> Free n
move (xs :|> File file c) = case fit xs of
    Nothing -> move xs :|> File file c
    Just new -> move new :|> Free file
  where
    fit :: Seq Block -> Maybe (Seq Block)
    fit Empty = Nothing
    fit (Free n :<| xs)
        | n >= file = Just (File file c :<| Free (n - file) :<| xs)
        | otherwise = (Free n :<|) <$> fit xs
    fit (x :<| xs) = (x :<|) <$> fit xs
