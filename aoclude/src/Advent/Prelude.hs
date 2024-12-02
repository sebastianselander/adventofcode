{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Advent.Prelude where

import Data.Bifunctor
import Data.Foldable qualified as Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map
import Data.Maybe (fromJust)

{- | Bottom if the list does not contain both elements
  Quite slow probaby
-}
byOrder :: (Ord a) => [a] -> a -> a -> Ordering
byOrder xs y z = compare (fromJust $ elemIndex y xs) (fromJust $ elemIndex z xs)

slidingWindows :: forall a. Int -> [a] -> [[a]]
slidingWindows n l = Prelude.take n <$> Data.List.tails l

toTuple :: forall a f. (Foldable f) => f a -> (a, a)
toTuple xs = case Foldable.toList xs of
    [l, r] -> (l, r)
    _ -> error "ERROR: more than two elements"

-- | Returns a map of frequencies of elements
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = Data.Map.fromListWith (+) . Prelude.foldr ((:) . (,1)) mempty

-- | Error if the index is out of bounds
setAt :: Int -> a -> [a] -> [a]
setAt n x = (\(l, r) -> l ++ x : (drop 1) r) . Prelude.splitAt n

-- | Does nothing if index out of bounds
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt _ _ [] = []
updateAt n f (x:xs) 
  | n == 0 = f x : xs
  | otherwise = x : updateAt (n - 1) f xs

-- | Delete element at index
deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt 0 (_:xs) = xs
deleteAt n (x:xs) = x : deleteAt (n-1) xs

findIndicesElem :: (Foldable t) => (a -> Bool) -> t a -> [(a, Int)]
findIndicesElem p = Prelude.reverse . fst . Data.List.foldl' go ([], 0)
  where
    go (l, n) x
        | p x = ((x, n) : l, n + 1)
        | otherwise = (l, n + 1)

apN :: Int -> (a -> a) -> a -> a
apN 0 _ !x = x
apN !n f !x = apN (n - 1) f (f x)

opPairs :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
opPairs f (a, aa) (b, bb) = (f a b, f aa bb)

fixed :: (Eq a) => (a -> a) -> a -> a
fixed f !x = if x == y then x else fixed f y
  where
    y = f x

elemOn :: (Eq b, Foldable f) => (a -> b) -> b -> f a -> Bool
elemOn f e = Foldable.foldr ((||) . (== e) . f) False

both :: (Bifunctor f) => (a -> b) -> f a a -> f b b
both f = bimap f f

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = drop 1 xs

countOn :: (Foldable f) => (a -> Bool) -> f a -> Int
countOn f = Prelude.foldr (\x acc -> if f x then acc + 1 else acc) 0

countElem :: (Foldable f, Eq a) => a -> f a -> Int
countElem e = Prelude.foldr (\x acc -> if x == e then acc + 1 else acc) 0

-- | Generate a range inclusive in the lower bound, exclusive in the upper bound
(...) :: (Num a, Enum a) => a -> a -> [a]
(...) a b = [a .. b - 1]

-- | Generate a range inclusive in the lower bound, inclus in the upper bound
-- equivalent to `[a .. b]`
(..=) :: (Num a, Enum a) => a -> a -> [a]
(..=) a b = [a .. b]
