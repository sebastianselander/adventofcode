module Main where

import Advent.Format
import Advent.Prelude (count)
import Data.List (sortOn, transpose)
import Data.List.Extra (groupOn)

main :: IO ()
main = do
    input <- [format|2024 25 ((%y%n)*)&%n|]
    let isLock (x : _) = all (== '#') x
        isLock _ = False
    let deduce xs = (isLock xs, fmap (pred . count '#') (transpose xs))
    let [keys, locks] = groupOn fst $ sortOn fst $ fmap deduce input
    print $
        length
            [ ()
            | (_, [a, b, c, d, e]) <- keys
            , (_, [m, n, o, p, q]) <- locks
            , a + m <= 5
            , b + n <= 5
            , c + o <= 5
            , d + p <= 5
            , e + q <= 5
            ]
