module Main where

import Advent.Format (format, format')
import Advent.Prelude (apN)
import Data.Bits (xor)
import Data.List (sort)
import Data.List.Extra (nubOrd)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Debug.Trace

main :: IO ()
main = do
    input <- [format|2024 22 (%u%n)*|]
    print $ sum $ fmap p1 input
    let fixed = fmap pair input
    print $ maximum (Map.elems (Map.unionsWith (+) fixed))

p1 :: Int -> Int
p1 = apN 2000 sec

bananas :: Int -> [Int]
bananas n = fmap (`mod` 10) $ take 2000 $ iterate sec n

pair :: Int -> Map (Int, Int, Int, Int) Int
pair n = Map.fromListWith (\ _ x -> x) $ zip s (drop 4 b)
  where
    b = bananas n
    s = seqs $ diffs b

seqs :: [Int] -> [(Int, Int, Int, Int)]
seqs (a : b : c : d : xs) = (a, b, c, d) : seqs (b : c : d : xs)
seqs _ = []

diffs :: [Int] -> [Int]
diffs secs = zipWith (-) (tail secs) secs

modn :: Int
modn = 16777216

sec :: Int -> Int
sec x =
    let a = ((x * 64) `xor` x) `mod` modn
        b = ((a `div` 32) `xor` a) `mod` modn
        c = ((b * 2048) `xor` b) `mod` modn
     in c
