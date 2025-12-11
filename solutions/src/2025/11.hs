module Main where

import Advent.Format (format)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.MemoTrie (memo2)
import Data.Time.Clock

-- 0.028 - 0.034
main :: IO ()
main = do
    m <- Map.fromList <$> [format|2025 11 (%s: (%s& )%n)*|]
    let countPaths = memo2 go
        go x n
            | x == "out" = if n >= 2 then 1 else 0
            | Just ns <- Map.lookup x m =
                sum
                    [ countPaths v n'
                    | v <- ns
                    , let n' = if x `elem` ["dac", "fft"] then n + 1 else n
                    ]
            | otherwise = 0
    print $ countPaths "you" 2
    print $ countPaths "svr" 0
