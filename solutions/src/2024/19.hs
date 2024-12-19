module Main where

import Advent.Format (format)
import Advent.Prelude (countBy)
import Data.List (isPrefixOf)
import Data.MemoTrie (memo2)

main :: IO ()
main = do
    (colors', onsens) <- [format|2024 19 %s&( )%n%n(%s%n)*|]
    let colors = fmap (filter (/= ',')) colors'
    let combinations = fmap (dp colors) onsens
    print $ countBy (>0) combinations
    print $ sum combinations

patternMatch :: String -> [String] -> [String]
patternMatch want available = [drop (length av) want | av <- available, av `isPrefixOf` want]

dp :: [String] -> String -> Int
dp = memo2 go
  where
    go available x = case patternMatch x available of
        [] -> 0
        xs -> countBy null xs + sum (fmap (dp available) xs)
