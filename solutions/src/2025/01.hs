module Main where

import Advent.Format (format, format', fmt)

main :: IO ()
main = do
    s <- [format|2025 01 (%u%n)*|]
    print s
