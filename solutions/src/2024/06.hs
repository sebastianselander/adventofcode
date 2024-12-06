module Main where

import Advent.Coord
import Advent.Format
import Data.Array
import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import Prelude hiding (Left, Right)
import Advent.Prelude

main :: IO ()
main = do
    input <- lines <$> [format|2024 6 (%t)*|]
    let grid = Map.fromList $ coordLines input
    let start = findStart grid
    let walked = Set.map snd $ fromJust $ walk start grid
    let coords = count Nothing [ walk start (Map.insert k 'c' grid) | k <- Map.keys grid]
    print $ Set.size walked
    print coords

findStart :: (Ord a) => Map a Char -> a
findStart m = fromJust $ lookup (Just '^') [(c, k) | k <- Map.keys m, let c = Map.lookup k m]

data Dir = Up | Down | Left | Right
    deriving (Show, Eq, Ord)

walk :: Coord -> Map Coord Char -> Maybe (Set (Dir, Coord))
walk start grid = go mempty Up start
  where
    go st dir' pos =
        let
            (newDir, dir) = case dir' of
                Up -> (Right, above)
                Down -> (Left, below)
                Left -> (Up, left)
                Right -> (Down, right)
         in
            ( if Set.member (dir', pos) st
                then Nothing
                else
                    ( case Map.lookup (dir pos) grid of
                        Just '.' -> go (Set.insert (dir', pos) st) dir' (dir pos)
                        Just '^' -> go (Set.insert (dir', pos) st) dir' (dir pos)
                        Just '#' -> go st newDir pos
                        Nothing -> Just $ Set.insert (dir', pos) st
                    )
            )
