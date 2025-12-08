module Main where

import Advent.Coord3 (Coord3 (..), euclidean)
import Advent.Format (format)
import Data.List ( nub, sortOn, elemIndex )
import Data.Maybe (fromJust)
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.DisjointSet qualified as DSet

main :: IO ()
main = do
    xs <- [format|2025 8 (%u,%u,%u%n)*|]
    let boxes = [C3 x y z | (x, y, z) <- xs]
        pairs = sortOn (uncurry euclidean) [(x, y)
                                     | (i, x) <- zip [0 ..] boxes
                                     , y <- drop i boxes
                                     , x /= y]
        g = scanl (flip (uncurry DSet.union)) DSet.empty pairs
        pairIndex = fromJust (elemIndex True (fmap (any ((1000==) . Set.size) . DSet.toSets) g)) - 1
        (C3 x1 _ _, C3 x2 _ _) = pairs !! pairIndex
        largestThree = product . map length . take 3 . nub . sortOn (Down . Set.size)
    print (largestThree (DSet.toSets (g !! 999)))
    print (x1 * x2)
