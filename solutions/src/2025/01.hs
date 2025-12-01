{-# LANGUAGE TemplateHaskell #-}

module Main where

import Advent.Format (format, intro)
import Advent.Prelude (count)

data D = D_L | D_R

intro

main :: IO ()
main = do
    s <- [format|2025 01 (@D%u%n)*|]
    print $ count 0 $ rot s
    print $ count 0 $ rot $ extend s

extend :: [(D, Int)] -> [(D, Int)]
extend xs = concat [replicate n (d, 1) | (d, n) <- xs]

rot :: [(D, Int)] -> [Int]
rot = scanl (\acc (dir, n) -> f dir acc n `mod` 100) 50
  where
    f :: D -> (Int -> Int -> Int)
    f D_L = (-)
    f D_R = (+)
