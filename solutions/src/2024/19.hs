module Main where

import Advent.Format (format)
import Advent.Prelude (countBy)
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)
import Data.MemoTrie (memo2)

main :: IO ()
main = do
    (patterns, designs) <- [format|2024 19 %s&(, )%n%n(%s%n)*|]
    let dp = memo2 go
        go _ "" = 1 :: Int
        go p d = sum (fmap (dp p) (mapMaybe (`stripPrefix` d) p))
        combinations = fmap (dp patterns) designs
    print $ countBy (> 0) combinations
    print $ sum combinations
