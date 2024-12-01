module Main where

import Advent.Format
import Advent.Prelude

main :: IO ()
main = do
    input <- [format|2024 2 <here>|]
    print input
