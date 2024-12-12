module Main where

import Advent.Coord (
    Coord (..),
    cardinalOn,
    east,
    neighborsOn,
    north,
    perimeter,
    south,
    west,
 )
import Advent.Format (getArrayInput)
import Advent.Prelude (arrIx)
import Control.Arrow ((&&&))
import Data.Array (Array, indices, (!))
import Data.Foldable (toList)
import Data.List.Extra (nubOrd)
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = do
    grid <- getArrayInput 2024 12
    let coords = Set.fromList $ indices grid
    let regs = regions coords grid
    let expanded = fmap (expand . toList) regs
    print $ sum $ uncurry (*) . (length &&& (length . perimeter)) <$> regs
    print $ sum (uncurry (*) <$> zip (fmap length regs) (fmap (length . corners . border) expanded))

corners :: [Coord] -> [Coord]
corners xs =
    [ x
    | x <- xs
    , let ls = cardinalOn (`elem` xs) x
    , subset (ne x) ls || subset (nw x) ls || subset (se x) ls || subset (sw x) ls
    ]
  where
    ne x = fmap (+ x) [north, east]
    nw x = fmap (+ x) [north, west]
    se x = fmap (+ x) [south, east]
    sw x = fmap (+ x) [south, west]

    subset zs ys = and [z `elem` ys | z <- zs]

regions :: Set Coord -> Array Coord Char -> [Set Coord]
regions coords grid = Set.toList $ foldr f mempty (toList coords)
  where
    f x acc
        | any (Set.member x) acc = acc
        | otherwise = Set.insert (region mempty x) acc
      where
        region :: Set Coord -> Coord -> Set Coord
        region vis coord = dfs [coord] vis
          where
            char = grid ! coord
            dfs [] visited = visited
            dfs (current : xs) visited = dfs (adjacents <> xs) (Set.insert current visited)
              where
                adjacents = flip cardinalOn current $ \x -> case arrIx grid x of
                    Nothing -> False
                    Just c -> c == char && Set.notMember x visited

expand :: [Coord] -> [Coord]
expand xs =
    [ C r c
    | C row col <- xs
    , r <- [row * 3, row * 3 + 1, row * 3 + 2]
    , c <- [col * 3, col * 3 + 1, col * 3 + 2]
    ]

border :: (Foldable f) => f Coord -> [Coord]
border zs = nubOrd $ concat [neighs | z <- toList zs, let neighs = neighborsOn (`notElem` zs) z]
