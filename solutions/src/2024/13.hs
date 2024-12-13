{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Advent.Format (format, format')
import Control.Monad (guard)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace, traceShow, traceShowId)

main :: IO ()
main = do
    input <- [format|2024 13 (Button A: X+%i, Y+%i%nButton B: X+%i, Y+%i%nPrize: X=%i, Y=%i%n%n)*|]
    print $ sum $ fmap (play 100) input
    print $ sum $ mapMaybe (solve' . update) input

update ::
    ( Integral a1
    , Integral a2
    , Integral a3
    , Integral a4
    , Integral a5
    , Integral a6
    , Num a7
    , Num b
    , Num c
    , Num d
    , Num f
    ) =>
    (a1, a2, a3, a4, a5, a6) ->
    (a7, b, c, d, f, f)
update (a, b, c, d, e, f) =
    ( fromIntegral a
    , fromIntegral b
    , fromIntegral c
    , fromIntegral d
    , fromIntegral e
    , fromIntegral f
    )

play :: Int -> (Int, Int, Int, Int, Int, Int) -> Int
play cap (xa, ya, xb, yb, xp, yp) =
    headSafe 0 $
        sort
            [ 3 * n + m
            | n <- [0 .. cap]
            , m <- [0 .. cap]
            , n * xa + m * xb == xp
            , n * ya + m * yb == yp
            ]

solve' :: (Double, Double, Double, Double, Double, Double) -> Maybe Integer
solve' (xa, ya, xb, yb, xp', yp') = do
    let yp = yp' + 10000000000000
    let xp = xp' + 10000000000000
    let n = (yb * xp - xb * yp) / (yb * xa - xb * ya)
    let m = (xp - n * xa) / xb
    guard $ trace ("n: " <> show n <> ", m: " <> show m) $ isInt n && isInt m
    pure (3 * floor n + floor m)

isInt :: Double -> Bool
isInt x = abs (fromIntegral @Integer (floor x) - x) < 0.01

headSafe :: a -> [a] -> a
headSafe x [] = x
headSafe _ (x : _) = x
