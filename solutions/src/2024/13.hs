module Main where

import Advent.Format (format)
import Control.Monad (guard)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    input <- [format|2024 13 (Button A: X+%i, Y+%i%nButton B: X+%i, Y+%i%nPrize: X=%i, Y=%i%n%n)*|]
    print $ sum $ mapMaybe (solve 0) input
    print $ sum $ mapMaybe (solve 10_000_000_000_000) input

solve :: Int -> (Int, Int, Int, Int, Int, Int) -> Maybe Int
solve extra (xa, ya, xb, yb, xp, yp) = do
    let n = (yb * (xp + extra) - xb * (yp + extra)) `div` (yb * xa - xb * ya)
    let m = (xp + extra - n * xa) `div` xb
    guard $ (n * xa + m * xb) == (xp + extra)
    guard $ (n * ya + m * yb) == (yp + extra)
    pure (3 * n + m)
