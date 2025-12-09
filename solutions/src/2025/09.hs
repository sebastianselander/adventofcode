{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Advent.Coord (Coord (..), boundingBox, neighbors)
import Advent.Format (format)
import Data.List.Extra (maximumOn)
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
    let polygon = flood (Set.fromList $ perimeter coords) [C ((x2 - x1) `div` 2 - 1) ((y2 - y1) `div` 2)]
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

-- | The list of coords must be ordered clockwise or counter-clockwise
perimeter :: [Coord] -> [Coord]
perimeter [] = []
perimeter (x : xs) = go (x : xs)
  where
    go [] = []
    go [a] =
        let (C ax ay) = a
            (C bx by) = x
         in case x - a of
                C 0 dy
                    | dy > 0 -> [C ax y | y <- [ay .. by]]
                    | otherwise -> [C ax y | y <- [by .. ay]]
                C dx 0
                    | dx > 0 -> [C x ay | x <- [ax .. bx]]
                    | otherwise -> [C x ay | x <- [bx .. ax]]
                _ -> error "impossible"
    go (a@(C ax ay) : b@(C bx by) : ys) =
        case b - a of
            C 0 dy
                | dy > 0 -> [C ax y | y <- [ay .. by]] <> go (b : ys)
                | otherwise -> [C ax y | y <- [by .. ay]] <> go (b : ys)
            C dx 0
                | dx > 0 -> [C x ay | x <- [ax .. bx]] <> go (b : ys)
                | otherwise -> [C x ay | x <- [bx .. ax]] <> go (b : ys)
            c -> error $ "impossible: " <> show c
