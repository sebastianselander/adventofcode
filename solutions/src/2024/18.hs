module Main where

import Advent.Coord (Coord (..), boundingBox, cardinal, contains)
import Advent.Format (format)
import Algorithm.Search (bfs)
import Data.List (findIndex, inits)
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set, fromList, notMember)
import Data.Tuple.Extra (swap)

main :: IO ()
main = do
    input <- [format|2024 18 (%u,%u%n)*|]
    let coords = uncurry C . swap <$> input
    let box = boundingBox coords
    let path :: Coord -> Set Coord -> Maybe [Coord]
        path c blocks = bfs next (== snd box) c
          where
            next c' = [ns | ns <- cardinal c', box `contains` ns, ns `notMember` blocks]
    let paths = fmap (path (C 0 0) . fromList) (drop 1 (inits coords)) -- inits is slow
    print $ length $ fromJust (paths !! 1024)
    putStrLn $ (\(x, y) -> show x <> "," <> show y) $ input !! fromJust (findIndex isNothing paths)
