{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Advent.Prelude where

import Control.Applicative (Alternative (empty))
import Data.Array.Base qualified as AB
import Data.Array.IArray qualified as A
import Data.Foldable (toList)
import Data.Foldable qualified as Foldable
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (
    elemIndex,
    foldl',
    inits,
    mapAccumL,
    sortBy,
    tails,
 )
import Data.Map (Map)
import Data.Map qualified
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Arr qualified as GA
import GHC.Stack (HasCallStack)

{- | Bottom if the list does not contain both elements
  Quite slow probaby
-}
byOrder :: (Ord a) => [a] -> a -> a -> Ordering
byOrder xs y z = compare (fromJust $ elemIndex y xs) (fromJust $ elemIndex z xs)

slidingWindows :: forall a. Int -> [a] -> [[a]]
slidingWindows n l = Prelude.take n <$> Data.List.tails l

-- | Returns a map of frequencies of elements
counts :: (Foldable f, Ord a) => f a -> Map a Int
counts = Data.Map.fromListWith (+) . Prelude.foldr ((:) . (,1)) mempty

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    case splitAt n xs of
        (a, b) -> a : chunks n b

-- | Index an array returning 'Nothing' if the index is out of bounds.
arrIx :: (A.IArray a e, A.Ix i, Alternative f) => a i e -> i -> f e
arrIx a i
    | A.inRange b i = pure $! AB.unsafeAt a (GA.unsafeIndex b i)
    | otherwise = empty
  where
    b = A.bounds a
{-# INLINE arrIx #-}

uniqueAssignment ::
    (Traversable t, Ord a) =>
    -- | element must map to one of the corresponding set members
    t (Set a) ->
    -- | possible assignments
    [t a]
uniqueAssignment m =
    [ snd (mapAccumL (\(x : xs) _ -> (xs, x)) (IntMap.elems a) m)
    | a <- go IntMap.empty (zip [0 ..] (toList m))
    ]
  where
    go :: (Ord a) => IntMap a -> [(Int, Set a)] -> [IntMap a]
    go a xs =
        case sortBy (comparing (Set.size . snd)) xs of
            [] -> [a]
            (k, vs) : rest ->
                do
                    v <- Set.toList vs
                    go (IntMap.insert k v a) (fmap (Set.delete v) <$> rest)

{- | Convert a big-endian list of digits to a single number.

>>> fromDigits 10 [1,2,3,4]
1234

>>> fromDigits 2 [12]
12

>>> fromDigits 10 []
0
-}
fromDigits :: (HasCallStack) => (Integral a) => a -> [a] -> a
fromDigits base
    | base < 2 = error "fromDigits: bad base"
    | otherwise = foldl' (\acc x -> acc * base + x) 0

{- | Convert a number to a list of digits in a given radix.

>>> toDigits 2 12
[1,1,0,0]

>>> toDigits 10 1234
[1,2,3,4]

>>> toDigits 10 0
[]
-}
toDigits :: (HasCallStack) => (Integral a) => a -> a -> [a]
toDigits base x
    | base < 2 = error "toDigits: bad base"
    | x < 0 = error "toDigits: negative number"
    | otherwise = go [] x
  where
    go xs 0 = xs
    go xs n = case quotRem n base of
        (n', digit) -> go (digit : xs) n'

-- | Error if the index is out of bounds
setAt :: Int -> a -> [a] -> [a]
setAt n x = (\(l, r) -> l ++ x : drop 1 r) . Prelude.splitAt n

-- | Does nothing if index out of bounds
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt _ _ [] = []
updateAt n f (x : xs)
    | n == 0 = f x : xs
    | otherwise = x : updateAt (n - 1) f xs

-- | Delete element at index
deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt 0 (_ : xs) = xs
deleteAt n (x : xs) = x : deleteAt (n - 1) xs

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

fixed :: (a -> Bool) -> (a -> a) -> a -> a
fixed p f !x = if p x then x else fixed p f (f x)

elemOn :: (Eq b, Foldable f) => (a -> b) -> b -> f a -> Bool
elemOn f e = Foldable.foldr ((||) . (== e) . f) False

count :: (Foldable f, Eq a) => a -> f a -> Int
count = countBy . (==)

-- | Count the number of elements in a foldable value that satisfy a predicate.
countBy :: (Foldable f) => (a -> Bool) -> f a -> Int
countBy p = foldl' (\acc x -> if p x then acc + 1 else acc) 0

same :: (Foldable t) => (Eq a) => t a -> Bool
same x =
    case toList x of
        [] -> True
        y : ys -> all (y ==) ys

{- | Returns a list of ways to select an element from a list without
replacement.
-}
pickOne :: [a] -> [(a, [a])]
pickOne xs = [(x, l ++ r) | (l, x : r) <- zip (inits xs) (tails xs)]

powerset :: (Eq a) => [a] -> [[a]]
powerset [] = [[]]
powerset (x : xs) = [x : ps | ps <- powerset xs] <> powerset xs

-- Generate all sublists of at least length n
deletes :: Int -> [a] -> [[a]]
deletes n xs = foldMap (`combinations` xs) [length xs - n .. length xs]

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x : xs)
    | k > length (x : xs) = []
    | otherwise = map (x :) (combinations (k - 1) xs) ++ combinations k xs

binarySearch :: (Integral a) => a -> (a -> Bool) -> a -> a -> a
binarySearch e p lo hi
    | lo > hi = lo
    | p e = binarySearch e p (mi + 1) hi
    | otherwise = binarySearch e p lo mi
  where
    mi = (lo + hi) `div` 2
