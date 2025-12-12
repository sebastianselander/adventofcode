module Main where

import Advent.Format (format)
import Advent.Prelude
import Advent.Coord
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Prelude hiding (flip)

main :: IO ()
main = do
    (blocks, rows) <- [format|2025 12 ($(~(%u:)%n(%y+%n)*%n))*((%ux%u:)( %u)*%n)*|]
    let presents = map (map head ) blocks
    let regions = map (uncurry intoRegion) rows
    print $ count True $ map (`validRegion` presents) regions

intoCoords :: [[Char]] -> [Coord]
intoCoords xs = [ i | (i,'#') <- Map.toList (coordMap xs)]

intoRegion :: (Int, Int) -> [Int] -> (Map Coord Bool, IntMap Int)
intoRegion (wid, len) xs = (coordMap (replicate len (replicate wid False)), IntMap.fromList (zip [0..] xs))

validRegion :: (Map Coord Bool, IntMap Int) -> [[[Char]]] -> Bool
validRegion (grid, indices) presents
  = numDots <= length (Map.keys grid)
  where
    toBeUsed =  [ replicate n (intoCoords p) | (i, p) <- zip [0..] presents, let Just n = IntMap.lookup i indices]
    numDots = length (concat $ concat toBeUsed)
