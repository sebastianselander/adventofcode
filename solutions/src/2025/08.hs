module Main where

import Advent.Coord3 (Coord3 (..), euclidean)
import Advent.Format (format)
import Advent.Search (clique)
import Data.List ( nub, sortOn )
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = do
    xs <- [format|2025 8 (%u,%u,%u%n)*|]
    let boxes = [C3 x y z | (x, y, z) <- xs]
    let emptyMap = Map.fromList $ map (,mempty) boxes
    let pairs = pairUp boxes
    let g = scanl (flip (uncurry insertLink)) emptyMap pairs !! 999
    print $ product $ map length $
                take 3 $
                    nub $
                        sortOn (Down . length) (fmap (`clique` g) (Map.keys g))
    print $ p2 emptyMap pairs

pairUp :: [Coord3] -> [(Coord3, Coord3)]
pairUp xs =
    map snd $
        sortOn
            fst
            [(euclidean x y, (x, y)) | (i, x) <- zip [0 ..] xs, y <- drop i xs, x /= y]

insertLink :: Coord3 -> Coord3 -> Map Coord3 (Set Coord3) -> Map Coord3 (Set Coord3)
insertLink this link xs =
    Map.insert this (Set.insert link l) $
        Map.insert link (Set.insert this r) xs
  where
    (l, r) = (xs Map.! this, xs Map.! link)

p2 :: Map Coord3 (Set Coord3) -> [(Coord3, Coord3)] -> Int
p2 linked ((l@(C3 x1 _ _), r@(C3 x2 _ _)) : xs) =
    if Set.size (clique l linked') == 1000
        then x1 * x2
        else p2 linked' xs
  where
    linked' = insertLink l r linked
