module Main where

import Advent.Coord (Coord (..), cardinal, contains)
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
    print $ length $ fromJust $ path (C 0 0) (fromList $ take 1024 coords)
    putStrLn $
        (\(x, y) -> show x <> "," <> show y) $
            input
                !! pred
                    ( fromJust $
                        findIndex isNothing $
                            fmap (path (C 0 0) . fromList) (inits coords)
                    )

box :: (Coord, Coord)
box = (C 0 0, C 70 70)

path :: Coord -> Set Coord -> Maybe [Coord]
path c blocks = bfs next (== snd box) c
  where
    next c' = [ns | ns <- cardinal c', box `contains` ns, ns `notMember` blocks]
