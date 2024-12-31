module Main where

import Advent.Format (format)
import Data.List (sortOn)
import Data.List.Extra (groupOn)

main :: IO ()
main = do
    input <- [format|2024 25 ((%y%n)*)&%n|]
    let isLock (x : _) = all (== '#') x
        isLock _ = False
    let [keys, locks] = groupOn isLock $ sortOn isLock input
    let f '#' '#' = False
        f _ _ = True
    print $ length [ () | k <- keys, l <- locks, all and (zipWith (zipWith f) k l)]
