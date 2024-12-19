module Main where

import Advent.Format (format)
import Advent.Prelude (countBy)
import Data.List (isPrefixOf)
import Data.MemoTrie (memo2)

main :: IO ()
main = do
    (colors, onsens) <- [format|2024 19 %s&(, )%n%n(%s%n)*|]
    let combinations = fmap (dp colors) onsens
    print $ countBy (> 0) combinations
    print $ sum combinations

dp :: [String] -> String -> Int
dp = memo2 go
  where
    go available want = countBy null next + sum (fmap (dp available) next)
      where
        next = [drop (length av) want | av <- available, av `isPrefixOf` want]
