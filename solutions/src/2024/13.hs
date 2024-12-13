module Main where

import Advent.Format (format)
import Control.Monad (guard)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    input <- [format|2024 13 (Button A: X+%i, Y+%i%nButton B: X+%i, Y+%i%nPrize: X=%i, Y=%i%n%n)*|]
    let doubles =
            fmap
                ( \(a, b, c, d, e, f) ->
                    ( fromIntegral a
                    , fromIntegral b
                    , fromIntegral c
                    , fromIntegral d
                    , fromIntegral e
                    , fromIntegral f
                    )
                )
                input
    print $ sum $ mapMaybe (solve 0) doubles
    print $ sum $ mapMaybe (solve 10000000000000) doubles

-- Boring
solve :: Double -> (Double, Double, Double, Double, Double, Double) -> Maybe Integer
solve extra (xa, ya, xb, yb, xp, yp) = do
    let n = (yb * (xp + extra) - xb * (yp + extra)) / (yb * xa - xb * ya)
    let m = ((xp + extra) - n * xa) / xb
    guard $ isInt n && isInt m
    pure (3 * floor n + floor m)

isInt :: Double -> Bool
isInt x = abs (fromIntegral @Integer (floor x) - x) < 0.01
