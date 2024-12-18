module Main where

import Advent.Coord (Coord (C), boundingBox, cardinal, contains)
import Advent.Format (format)
import Algorithm.Search (bfs)
import Data.List (findIndex)
import Data.Maybe (fromJust, isNothing)
import Data.Set (insert, notMember)
import Data.Tuple.Extra (swap)

main :: IO ()
main = do
    input <- [format|2024 18 (%u,%u%n)*|]
    let coords = fmap (uncurry C . swap) input
    let box@(start, end) = boundingBox coords
    let path c blocks = length <$> bfs next (== end) c
          where
            next curr = [ns | ns <- cardinal curr, box `contains` ns, ns `notMember` blocks]
    let paths' = tail $ scanl (\(_, acc) x -> (path start $ insert x acc, insert x acc)) (Nothing, mempty) coords
    print $ fromJust (fst $ paths' !! 1024)
    putStrLn $ tail $ init $ show $ input !! (1024 + fromJust (findIndex (isNothing . fst) (drop 1024 paths')))
