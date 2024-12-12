{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Advent.Coord
import Advent.Format (format, format')
import Advent.Prelude (arrIx, count)
import Control.Arrow ((&&&))
import Data.Array (Array, indices, (!))
import Data.Foldable (foldl', toList)
import Data.List (sort, (\\))
import Data.List.Extra (nubBy, nubOn, nubOrd, nubOrdOn)
import Data.Map qualified as Map
import Data.Set (Set, isSubsetOf)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShow, traceShowId)

main :: IO ()
main = do
    input <- coordArray @Array <$> [format|2024 12 (%t*%n)*|]
    let coords = Set.fromList $ indices input
    let calc f x = sum $ uncurry (*) . (length &&& f) <$> x
    let regs = regions coords input
    print $ calc (length . perimeter) regs
    let re = fmap (expand . toList) regs
    -- putStrLn . draw . border $ head re
    -- putStrLn $ draw $ border $ head re
    let p2 =  fmap (draw . border) re
    -- mapM_ putStrLn p2
    print $ sum $ fmap (uncurry (*)) $ zip (fmap length regs) (fmap (corners . border) re)

corners :: [Coord] -> Int
corners xs = count True [ subset (ne x) ls || subset (nw x) ls || subset (se x) ls || subset (sw x) ls | x <- xs, let ls = cardinalOn (`elem` xs) x]
    where
      ne x = fmap (+x) [north, east]
      nw x = fmap (+x) [north, west]
      se x = fmap (+x) [south, east]
      sw x = fmap (+x) [south, west]
      subset xs ys = and [ x `elem` ys | x <- xs]

-- walk :: [Coord] -> [[(Int,[Coord])]]
-- walk [] = []
-- walk zs = fmap (dfs origin 0 mempty) zs
--   where
--     dfs :: Coord -> Int -> Set Coord -> [Coord] -> (Int, [Coord])
--     dfs _ n visited [] = (n, toList visited)
--     dfs dir n visited (current : xs) 
--         | null adj = (n, toList visited)
--         | dir /= dir' = dfs dir' (n + 1)  (Set.insert current visited) (adj <> xs)
--         | otherwise = dfs dir' n  (Set.insert current visited) (adj <> xs)
--       where
--         dir' = current - head adj
--         adj = cardinalOn (\y -> y `elem` zs && y `Set.notMember` visited) current

regions :: Set Coord -> Array Coord Char -> [Set Coord]
regions coords grid = Set.toList $ foldl' f mempty (toList coords)
  where
    f acc x
        | any (Set.member x) acc = acc
        | otherwise = Set.insert res acc
      where
        res = region mempty grid x

region :: Set Coord -> Array Coord Char -> Coord -> Set Coord
region vis grid coord = dfs [coord] vis
  where
    char = grid ! coord
    dfs [] visited = visited
    dfs (current : xs) visited = dfs (adjacents <> xs) (Set.insert current visited)
      where
        adjacents = flip cardinalOn current $ \x -> case arrIx grid x of
            Nothing -> False
            Just c -> c == char && Set.notMember x visited

draw [] = ""
draw zs = drawPicture (Map.fromList $ map (,'#') zs)

expand :: [Coord] -> [Coord]
expand xs = [C r c | C row col <- xs, (r, c) <- combs row col]
  where
    combs row col =
        [ (row * 3, col * 3)
        , (row * 3 + 1, col * 3)
        , (row * 3 + 2, col * 3)
        , (row * 3, col * 3 + 1)
        , (row * 3 + 1, col * 3 + 1)
        , (row * 3 + 2, col * 3 + 1)
        , (row * 3, col * 3 + 2)
        , (row * 3 + 1, col * 3 + 2)
        , (row * 3 + 2, col * 3 + 2)
        ]

enclosed :: [Coord] -> [Coord]
enclosed [] = []
enclosed points = concat [ray False (C r c) | r <- [0 .. rows], c <- [0 .. cols]]
  where
    box = boundingBox points
    rows = coordRow $ snd box
    cols = coordCol $ snd box
    ray b c
        | not $ box `contains` c = []
        | c `elem` points = ray (not b) (right c)
        | otherwise = add $ ray b (right c)
      where
        add
            | b = (c :)
            | otherwise = id

border :: (Foldable f) => f Coord -> [Coord]
border zs = nubOrd $ concat [neighs | z <- toList zs, let neighs = neighborsOn (`notElem` zs) z]
  where
    inside = enclosed (toList zs)

{-
part2 zs = undefined
  where
    peri = nubOrd $ perimeter zs

walk :: [Coord] -> Int
walk xs =  undefined
  where
    go seen c
      | Set.member c seen = 0
      | otherwise = undefined

part2' zs = trace steps $ trace steps2 $ border zs
  where
    steps = drawPicture (Map.fromList $ map (,'A') (nubOrd $ perimeter zs))
    steps2 = drawPicture (Map.fromList $ map (,'S') $ border zs)

part2 zs = trace ("\n" <> draw bord){- walk bord (head peri) +-} walk bord (head bord)
  where
    peri = nubOrd $ perimeter zs
    bord = border peri
    walk :: [Coord] -> Coord -> [Coord]
    walk xs y = nubOrd $ go origin [] xs [y]
      where
        go :: Coord -> [Coord] -> [Coord] -> [Coord] -> [Coord]
        go _ _ _ [] = []
        go dir vis xs (x : rest) = case neigh of
            [] -> []
            (y : ys)
                | dir + x == y -> concatMap continue (y : ys)
                | otherwise -> x : concatMap continue (y : ys)
          where
            continue y = go (y - x) (x : vis) xs (y : rest)
            neigh = cardinalOn (\y -> y `notElem` vis && y `elem` xs) x
-}
