module Main where

import Advent.Format (format)
import Data.IntMap (IntMap)
import Data.IntMap qualified as Map
import Data.IntMap.Strict ((!))
import Data.Tuple.Extra (both)

main :: IO ()
main = do
    input <- [format|2024 11 %i& %n|]
    let blinked = iterate run $ Map.fromList [(k, 1) | k <- input]
    let stones = sum . Map.elems
    print $ stones $ blinked !! 25
    print $ stones $ blinked !! 75

run :: IntMap Int -> IntMap Int
run m = Map.fromListWith (+) $ concatMap blink (Map.keys m)
  where
    blink :: Int -> [(Int, Int)]
    blink key =
        let keyCount = m ! key
         in case rule key of
                Left n -> [(n, keyCount)]
                Right (x, y) -> [(x, keyCount), (y, keyCount)]

rule :: Int -> Either Int (Int, Int)
rule 0 = Left 1
rule n
    | even len = Right $ both read $ splitAt (len `div` 2) n'
    | otherwise = Left (2024 * n)
  where
    n' = show n
    len = length n'
