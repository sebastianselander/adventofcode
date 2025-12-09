{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

{- HLINT ignore "Redundant multi-way if" -}

module Main where

import Advent.Coord
import Advent.Format
import Advent.Prelude
import Data.List (group, sort)
import Data.List.Extra (maximumOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)
import Data.Tuple.Extra (fst3)
import Debug.Trace (trace, traceShow, traceShowId)
import Data.List (sortBy)

-- P2 wrong: 4621384368
-- P2 wrong: 4759930955
main :: IO ()
main = do
    xs <- [format|2025 9 (%u,%u%n)*|]
    let scalar = 1000
    let coords' = map (\(x, y) -> (C (y `div` scalar) (x `div` scalar), C y x)) xs
    let ogMap = Map.fromList coords'
    let (coords, _) = unzip coords'
    let c@(C x1 y1, C x2 y2) = boundingBox coords
    print c
    let start = C (((x2 - x1) `div` 2) - 10) ((y2 - y1) `div` 2)
    print start
    let poly = flood (Set.fromList $ perim coords) [start]
    print $ Set.size poly
    let (n, (a ,b)) = maximumOn fst [(n, (a, b)) | (n, (a, b)) <- largest coords, rectPeri poly a b]
    let v c1 c2 = 
            let C x1 y1 = ogMap Map.! c1
                C x2 y2 = ogMap Map.! c2
             in 
            (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
    -- putStrLn $ drawPicture $ Map.fromList $ map (,'#') $ Set.toList poly
    print a
    print b
    print $ v a b

corners :: Coord -> Coord -> [Coord]
corners (C x1 y1) (C x2 y2) = 
    [ C (min x1 x2) (min y1 y2)
    , C (min x1 x2) (max y1 y2)
    , C (max x1 x2) (max y1 y2)
    , C (max x1 x2) (min y1 y2)
    ]

floodRect :: Set Coord -> [Coord] -> Set Coord
floodRect p xs = flood p [floodStart xs]

flood :: Set Coord -> [Coord] -> Set Coord
flood seen [] = seen
flood seen xs | Set.size seen `mod` 10_000 == 0 && traceShow (Set.size seen) False = undefined
flood seen (x : xs)
    | Set.member x seen = flood seen xs
    | otherwise = flood (Set.insert x seen) (neighbors x <> xs)

floodStart :: [Coord] -> Coord
floodStart xs = upLeft + 1
  where
    (upLeft, _) = boundingBox xs

largest :: [Coord] -> [(Int, (Coord, Coord))]
largest xs =
    [ ( v
      , (C x1 y1, C x2 y2)
      )
    | (i, C x1 y1) <- zip [0 ..] xs
    , C x2 y2 <- drop i xs
    , let v = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
    ]

rectPeri :: Set Coord -> Coord -> Coord -> Bool
rectPeri poly a b = and [ Set.member (C r c) poly | r <- [r1 .. r2], c <- [c1 .. c2]]
  where
    (C r1 c1 ,C r2 c2) = boundingBox [a, b]

perim :: [Coord] -> [Coord]
perim [] = []
perim (x : xs) = go (x : xs)
  where
    go [] = []
    go [a] =
        let (C ax ay) = a
            (C bx by) = x
         in case x - a of
                C 0 dy
                    | dy > 0 ->  [C ax y | y <- [ay .. by]]
                    | otherwise ->  [C ax y | y <- [by .. ay]]
                C dx 0
                    | dx > 0 ->  [C x ay | x <- [ax .. bx]]
                    | otherwise ->  [C x ay | x <- [bx .. ax]]
                _ -> error "impossible"
    go (a@(C ax ay) : b@(C bx by) : ys) =
        case b - a of
            C 0 dy
                | dy > 0 ->  [C ax y | y <- [ay .. by]] <> go (b : ys)
                | otherwise ->  [C ax y | y <- [by .. ay]] <> go (b : ys)
            C dx 0
                | dx > 0 ->  [C x ay | x <- [ax .. bx]] <> go (b : ys)
                | otherwise ->  [C x ay | x <- [bx .. ax]] <> go (b : ys)
            c -> error $ "impossible: " <> show c
