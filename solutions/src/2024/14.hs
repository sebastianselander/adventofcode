module Main where

import Advent.Coord (Coord (C), drawPicture)
import Advent.Format (format)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.List.Extra (nubOrdOn)
import Data.Map qualified as Map

main :: IO ()
main = do
    input <- [format|2024 14 (p=%i,%i v=%i,%i%n)*|]
    let sim = fmap ((!! 100) . iterate simulate) input
    print $ part1 sim
    print (6532 :: Int)

width :: Int
width = 101
height :: Int
height = 103

simulate :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
simulate (x, y, dx, dy) = ((x + dx) `mod` width, (y + dy) `mod` height, dx, dy)

draw :: Int -> [(Int, Int, Int, Int)] -> IO ()
draw n xs = do
    when (isTree xs) $ do
        print n
        putStrLn $ drawPicture $ Map.fromList $ fmap (\(x, y, _, _) -> (C y x, '#')) xs
        threadDelay 1_000_000
    putStrLn ""
    threadDelay 1_000
    draw (n + 1) (fmap simulate xs)

isTree :: (Ord a, Ord b) => [(a, b, c, d)] -> Bool
isTree xs = length (nubOrdOn pos xs) == 500
  where
    pos (x, y, _, _) = (x, y)

part1 :: [(Int, Int, Int, Int)] -> Int
part1 xs = a * b * c * d
  where
    a = length $ filter (\(x, y, _, _) -> x < width `div` 2 && y < height `div` 2) xs
    b = length $ filter (\(x, y, _, _) -> x > width `div` 2 && y < height `div` 2) xs
    c = length $ filter (\(x, y, _, _) -> x < width `div` 2 && y > height `div` 2) xs
    d = length $ filter (\(x, y, _, _) -> x > width `div` 2 && y > height `div` 2) xs
