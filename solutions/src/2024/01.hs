module Main where
import Advent.Format (format,fmt)
import Data.List
import Advent.Prelude (countElem)

main :: IO ()
main = do
    inp <- readFile "inputs/2024/01.txt"
    let (lefts, rights) = unzip $ [fmt|((%i   %i)%n)*|] inp
    let xs = sort lefts
    let ys = sort rights
    print $ sum $ zipWith (\x y -> abs $ x - y ) xs ys
    print $ sum [ x * countElem x ys | x <- xs ]

