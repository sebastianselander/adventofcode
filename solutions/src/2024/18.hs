module Main where

import Advent.Coord (Coord (C), boundingBox, cardinal, contains)
import Advent.Format (format)
import Algorithm.Search (bfs)
import Data.List (findIndex, inits)
import Data.Maybe (fromJust, isNothing)
import Data.Set (fromList, notMember)
import Data.Tuple.Extra (swap)

main :: IO ()
main = do
    input <- [format|2024 18 (%u,%u%n)*|]
    let coords = fmap (uncurry C . swap) input
    let box@(start, end) = boundingBox coords
    let path c blocks = bfs next (== end) c
          where
            next curr = [ns | ns <- cardinal curr, box `contains` ns, ns `notMember` blocks]
    let paths = fmap (path start . fromList) (drop 1 (inits coords))
    print $ length $ fromJust (paths !! 1024)
    putStrLn $ tail $ init $ show $ input !! fromJust (findIndex isNothing paths)
