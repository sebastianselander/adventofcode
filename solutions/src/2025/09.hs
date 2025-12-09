{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Advent.Coord (Coord (..), boundingBox, neighbors, outline)
import Advent.Format (format)
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = do
    xs <- [format|2025 9 (%u,%u%n)*|]
    let scalar = 249 -- incorrect answer if I go higher
    let coords' = map (\(x, y) -> (C (y `div` scalar) (x `div` scalar), C y x)) xs
    let decompressed = Map.fromList coords'
    let coords = map fst coords'
    let (C x1 y1, C x2 y2) = boundingBox coords
    let polygon = flood (Set.fromList $ outline coords) [C ((x2 - x1) `div` 2 - 1) ((y2 - y1) `div` 2)]
    print $ maximum [area a b | (a, b) <- rectangles (map (uncurry C) xs)]
    print $ maximum [area (decompressed ! aa) (decompressed ! bb) | (aa, bb) <- rectangles coords, inside polygon aa bb]

flood :: Set Coord -> [Coord] -> Set Coord
flood seen [] = seen
flood seen (x : xs)
    | Set.member x seen = flood seen xs
    | otherwise = flood (Set.insert x seen) (neighbors x <> xs)

area :: Coord -> Coord -> Int
area (C x1 y1) (C x2 y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

rectangles :: [Coord] -> [(Coord, Coord)]
rectangles xs = [(C x1 y1, C x2 y2) | (i, C x1 y1) <- zip [0 ..] xs, C x2 y2 <- drop i xs]

inside :: Set Coord -> Coord -> Coord -> Bool
inside poly a b =
    and
        [ Set.member (C r c) poly
        | r <- [r1 .. r2]
        , c <- [c1 .. c2]
        , r == r1 || r == r2 || c == c1 || c == c2
        ]
  where
    (C r1 c1, C r2 c2) = boundingBox [a, b]
