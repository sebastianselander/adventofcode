module Main where

import Advent.Format (format)
import Advent.Prelude (count)
import Data.List (sortOn, transpose)
import Data.List.Extra (groupOn)

main :: IO ()
main = do
    input <- [format|2024 25 ((%y%n)*)&%n|]
    let isLock (x : _) = all (== '#') x
        isLock _ = False
    let deduce xs = (isLock xs, fmap (pred . count '#') (transpose xs))
    let [keys, locks] = fmap (fmap snd) $ groupOn fst $ sortOn fst $ fmap deduce input
    print $
        length
            [ ()
            | xs <- keys
            , ys <- locks
            , all (<= 5) $ zipWith (+) xs ys
            ]
