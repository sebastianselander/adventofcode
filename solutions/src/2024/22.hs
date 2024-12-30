module Main where

import Advent.Format (format)
import Advent.Prelude (times)
import Data.Bits (xor)
import Data.Map.Strict (Map, elems, fromListWith, unionsWith)

main :: IO ()
main = do
    input <- [format|2024 22 (%u%n)*|]
    print $ foldr ((+) . times 2000 secret) 0 input
    print $ maximum (elems (unionsWith (+) (fmap pair input)))

bananas :: Int -> [Int]
bananas n = fmap (`mod` 10) $ take 2000 $ iterate secret n

pair :: Int -> Map (Int, Int, Int, Int) Int
pair n = fromListWith (\_ x -> x) $ zip s (drop 4 b)
  where
    b = bananas n
    s = changes $ zipWith (-) (tail b) b

changes :: [Int] -> [(Int, Int, Int, Int)]
changes (a : b : c : d : xs) = (a, b, c, d) : changes (b : c : d : xs)
changes _ = []

prune :: Int -> Int
prune = (`mod` 16777216)

secret :: Int -> Int
secret x =
    let a = prune (x * 64 `xor` x)
        b = prune (a `div` 32 `xor` a)
     in prune (b * 2048 `xor` b)
