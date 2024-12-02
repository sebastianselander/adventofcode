{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Advent.Prelude where

import Data.Bifunctor
import Data.Foldable qualified as Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map
import Data.Maybe (fromJust)
import Data.Foldable (toList)
import Control.Applicative (Alternative)
import qualified Data.Array.IArray as A
import qualified Data.Array.Base as AB
import qualified GHC.Arr as GA
import Control.Applicative (Alternative(empty))
import Data.IntMap (IntMap)
import Data.Set (Set)
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Data.Ord (comparing)
import GHC.Stack (HasCallStack)

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
counts :: (Foldable f, Ord a) => f a -> Map a Int
counts = Data.Map.fromListWith (+) . Prelude.foldr ((:) . (,1)) mempty

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  case splitAt n xs of
    (a,b) -> a : chunks n b

-- | Index an array returning 'Nothing' if the index is out of bounds.
arrIx :: (A.IArray a e, A.Ix i, Alternative f) => a i e -> i -> f e
arrIx a i
  | A.inRange b i = pure $! AB.unsafeAt a (GA.unsafeIndex b i)
  | otherwise = empty
  where b = A.bounds a
{-# Inline arrIx #-}

uniqueAssignment ::
  (Traversable t, Ord a) =>
  t (Set a) {- ^ element must map to one of the corresponding set members -} ->
  [t a]     {- ^ possible assignments -}
uniqueAssignment m =
  [ snd (mapAccumL (\(x:xs) _ -> (xs,x)) (IntMap.elems a) m)
  | a <- go IntMap.empty (zip [0..] (toList m))]
  where
    go :: Ord a => IntMap a -> [(Int, Set a)] -> [IntMap a]
    go a xs =
      case sortBy (comparing (Set.size . snd)) xs of
        [] -> [a]
        (k,vs):rest ->
          do v <- Set.toList vs
             go (IntMap.insert k v a) (fmap (Set.delete v) <$> rest)

-- | Convert a big-endian list of digits to a single number.
--
-- >>> fromDigits 10 [1,2,3,4]
-- 1234
--
-- >>> fromDigits 2 [12]
-- 12
--
-- >>> fromDigits 10 []
-- 0
fromDigits :: HasCallStack => Integral a => a -> [a] -> a
fromDigits base
  | base < 2  = error "fromDigits: bad base"
  | otherwise = foldl' (\acc x -> acc * base + x) 0

-- | Convert a number to a list of digits in a given radix.
--
-- >>> toDigits 2 12
-- [1,1,0,0]
--
-- >>> toDigits 10 1234
-- [1,2,3,4]
--
-- >>> toDigits 10 0
-- []
toDigits :: HasCallStack => Integral a => a -> a -> [a]
toDigits base x
  | base < 2  = error "toDigits: bad base"
  | x < 0     = error "toDigits: negative number"
  | otherwise = go [] x
  where
    go xs 0 = xs
    go xs n = case quotRem n base of
                (n', digit) -> go (digit:xs) n'

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

count :: (Foldable f, Eq a) => a -> f a -> Int
count = countBy . (==)

-- | Count the number of elements in a foldable value that satisfy a predicate.
countBy :: Foldable f => (a -> Bool) -> f a -> Int
countBy p = foldl' (\acc x -> if p x then acc+1 else acc) 0

same :: Foldable t => Eq a => t a -> Bool
same x =
  case toList x of
    []   -> True
    y:ys -> all (y ==) ys

-- | Returns a list of ways to select an element from a list without
-- replacement.
pickOne :: [a] -> [(a, [a])]
pickOne xs = [ (x, l++r) | (l,x:r) <- zip (inits xs) (tails xs) ]

-- | Generate a range inclusive in the lower bound, exclusive in the upper bound
(...) :: (Num a, Enum a) => a -> a -> [a]
(...) a b = [a .. b - 1]

-- | Generate a range inclusive in the lower bound, inclus in the upper bound
-- equivalent to `[a .. b]`
(..=) :: (Num a, Enum a) => a -> a -> [a]
(..=) a b = [a .. b]

