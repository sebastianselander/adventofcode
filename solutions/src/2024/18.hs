module Main where

import Advent.Coord (Coord (..), cardinal, contains, turnAround)
import Advent.Format (format)
import Algorithm.Search (bfs)
import Data.List (findIndex, inits, intercalate)
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set, notMember, fromList)
import Data.Tuple.Extra (swap)

main :: IO ()
main = do
    input <- fmap (uncurry C . swap) <$> [format|2024 18 (%u,%u%n)*|]
    print $ length $ fromJust $ walk (C 0 0) (fromList $ take 1024 input)
    putStrLn $
        intercalate "," $
            (\(C r c) -> [show c, show r]) $
                abs $
                    turnAround $
                        input
                            !! pred
                                ( fromJust $
                                    findIndex isNothing $
                                        fmap (walk (C 0 0) . fromList) (inits input)
                                )

box :: (Coord, Coord)
box = (C 0 0, C 70 70)

walk :: Coord -> Set Coord -> Maybe [Coord]
walk c blocks = bfs next (== snd box) c
  where
    next c' = [ns | ns <- cardinal c', box `contains` ns, ns `notMember` blocks]
