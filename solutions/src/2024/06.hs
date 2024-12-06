module Main where

import Advent.Coord (
    Coord,
    above,
    below,
    coordLines,
    left,
    right,
 )
import Advent.Format (format)
import Advent.Prelude (count)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = do
    input <- [format|2024 6 (%t*%n)*|]
    let grid = Map.fromList $ coordLines input
    let start = findStart grid
    let walked = fromJust $ walk start grid
    print $ Set.size walked
    print $ count Nothing [walk start (Map.insert k '#' grid) | k <- Set.toList walked]

findStart :: (Ord a) => Map a Char -> a
findStart m = head [k | k <- Map.keys m, let c = Map.lookup k m, c == Just '^']

data Dir = U | D | L | R
    deriving (Eq, Ord)

walk :: Coord -> Map Coord Char -> Maybe (Set Coord)
walk start grid = go mempty U start
  where
    go st dir' pos =
        let
            (newDir, dir) = case dir' of
                U -> (R, above)
                D -> (L, below)
                L -> (U, left)
                R -> (D, right)
         in
            if Set.member (dir', pos) st
                then Nothing
                else case Map.lookup (dir pos) grid of
                    Just '.' -> go (Set.insert (dir', pos) st) dir' (dir pos)
                    Just '^' -> go (Set.insert (dir', pos) st) dir' (dir pos)
                    Just '#' -> go st newDir pos
                    Nothing -> Just $ Set.insert pos $ Set.map snd st
