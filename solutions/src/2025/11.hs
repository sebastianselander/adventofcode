module Main where

import Advent.Format (format)
import Data.Map qualified as Map
import Data.MemoTrie (memo2)

main :: IO ()
main = do
    m <- Map.fromList <$> [format|2025 11 (%s: (%s& )%n)*|]
    let countPaths = memo2 go
        go x n
            | x == "out" = if n >= 2 then 1 else 0
            | Just ns <- Map.lookup x m =
                sum [countPaths v n' | let n' = if x `elem` ["dac", "fft"] then n + 1 else n, v <- ns]
            | otherwise = 0
    print (countPaths "you" 2)
    print (countPaths "svr" 0)
