module Main where

import Advent.Format
import Data.Array (Ix(inRange))
import Data.List (nub)
import Data.Range


main :: IO ()
main = do
    (rngs, xs) <- [format|2025 5 (%u-%u%n)*%n(%u%n)*|]
    print $ length $ nub [ x | rn <- rngs, x <- xs, rn `Data.Array.inRange` (x :: Int)]
    print $ fresh rngs

fresh :: [(Int, Int)] -> Int
fresh xs = sum $ fmap f $ mergeRanges $ fmap mkRange xs
  where
    f (SpanRange (Bound x _) (Bound y _))= y - x + 1
    f (SingletonRange x) = 1
    f r = error $ show r

mkRange :: (Int, Int) -> Range Int
mkRange (a,b) = SpanRange (Bound a Inclusive) (Bound b Inclusive)
