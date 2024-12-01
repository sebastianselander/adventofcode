module Main where
import Advent.Format (format,fmt)
import Data.List
import Advent.Prelude (countElem, both)
import Data.Composition ((.:))

main :: IO ()
main = do
    input <- [format|2024 1 ((%i   %i)%n)*|]
    let (xs, ys) = both sort $ unzip input
    print $ sum $ zipWith (abs .: (-)) xs ys
    print $ sum [x * countElem x ys | x <- xs]
