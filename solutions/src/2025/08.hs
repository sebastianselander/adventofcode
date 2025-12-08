module Main where

import Advent.Coord3
import Advent.Format
import Data.List (nub, sortOn)
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
    let g = junctions 1000 emptyMap (pairUp boxes)
    let keys = Map.keys g
    print $
        product $
            map length $
                take 3 $
                    nub $
                        sortOn (Down . length) (fmap (\k -> clique [k] Set.empty g) keys)
    print $ p2 emptyMap (pairUp boxes)

pairUp :: [Coord3] -> [(Coord3, Coord3)]
pairUp xs =
    map snd $
        sortOn
            fst
            [ (euclidean x y, (x, y))
            | (i, x) <- zip [0 ..] xs
            , y <- drop i xs
            , x /= y
            ]

junctions :: Int -> Map Coord3 (Set Coord3) -> [(Coord3, Coord3)] -> Map Coord3 (Set Coord3)
junctions 0 linked _ = linked
junctions _ linked [] = linked
junctions n linked ((l, r) : xs) =
    junctions (n - 1) (insertLink l r linked) xs

insertLink :: Coord3 -> Coord3 -> Map Coord3 (Set Coord3) -> Map Coord3 (Set Coord3)
insertLink this link xs =
    Map.insert this (Set.insert link l) $
        Map.insert link (Set.insert this r) xs
  where
    (l, r) = (xs Map.! this, xs Map.! link)

clique :: [Coord3] -> Set Coord3 -> Map Coord3 (Set Coord3) -> Set Coord3
clique [] seen _ = seen
clique (x : xs) seen g
    | Set.member x seen = clique xs seen g
    | otherwise = case g Map.! x of
        neighbors -> clique (Set.toList neighbors <> xs) (Set.insert x seen) g

p2 :: Map Coord3 (Set Coord3) -> [(Coord3, Coord3)] -> Int
p2 _ [] = error "not found"
p2 linked ((l@(C3 x1 _ _), r@(C3 x2 _ _)) : xs) =
    if Set.size (clique [l] mempty linked') == 1000
        then x1 * x2
        else p2 linked' xs
  where
    linked' = insertLink l r linked
