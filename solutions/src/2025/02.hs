module Main where

import Advent.Format (format)
import Advent.Prelude
import Data.List

main :: IO ()
main = do
    s <- [format|2025 2 (%u-%u)&,%n|]
    print $ sum [ sum (invalids [from .. to]) | (from, to) <- s]
    print $ sum [ sum $ filter invalid2 [from .. to] | (from, to) <- s]

invalid2 :: Int -> Bool
invalid2 n = any (\x -> go True x n') (init $ tail $ inits n')
  where
    go False _ _ = False
    go True _ [] = True
    go True v xs = go (v == take l xs) v (drop l xs)
      where
        l = length v
    n' = show n

invalids :: [Int] -> [Int]
invalids = filter invalid

invalid :: Int -> Bool
invalid n = ((length n' `mod` 2) /= 1) && (let (fi, la) = splitAt (length n' `div` 2) n'
  in read @Int fi == read la)
  where
    n' = show n
