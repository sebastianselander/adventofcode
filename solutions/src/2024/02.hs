module Main where

import Advent.Box
import Advent.Chinese
import Advent.Coord
import Advent.Coord3
import Advent.Flow
import Advent.Format
import Advent.Group
import Advent.Nat
import Advent.PQueue
import Advent.Permutation
import Advent.Prelude
import Advent.Queue
import Advent.ReadS
import Data.List (delete, tails)
import GHC.RTS.Flags (ProfFlags (descrSelector))

main :: IO ()
main = do
    file <- readFile "inputs/2024/test.txt"
    real <- readFile "inputs/2024/02.txt"
    let input = [fmt|(%i& %n)*|] real
    let xs = fmap (\x -> (decreasing x || increasing x) && diff x) input
    print (countElem False xs)
    print $ countElem True $ fmap (any ok) $ fmap foo input

increasing :: [Int] -> Bool
increasing (x : y : xs) = x < y && increasing (y : xs)
increasing _ = True

decreasing :: [Int] -> Bool
decreasing (x : y : xs) = x > y && decreasing (y : xs)
decreasing _ = True

diff :: [Int] -> Bool
diff (x : y : xs) = abs (x - y) >= 1 && abs (x - y) <= 3 && diff (y : xs)
diff _ = True

ok :: [Int] -> Bool
ok xs = (\x -> (increasing x || decreasing x) && diff x) $ xs

foo :: [Int] -> [[Int]]
foo xs = [h <> t | n <- [0 .. length xs - 1], let (h, _ : t) = splitAt n xs]
