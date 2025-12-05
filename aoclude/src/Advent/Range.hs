module Advent.Range where

import Data.Range qualified as Range

mergeRanges :: (Enum a, Bounded a, Ord a) => [(a, a)] -> [(a, a)]
mergeRanges = fmap fromRange . Range.mergeRanges . fmap toRange

-- | Inclusive in lower and upper bound
toRange :: (a, a) -> Range.Range a
toRange (a, b) = Range.SpanRange (Range.Bound a Range.Inclusive) (Range.Bound b Range.Inclusive)

fromRange :: (Bounded a, Enum a) => Range.Range a -> (a, a)
fromRange (Range.SpanRange (Range.Bound a ba) (Range.Bound b bb)) = case (ba, bb) of
    (Range.Inclusive, Range.Inclusive) -> (a, b)
    (Range.Inclusive, Range.Exclusive) -> (a, pred b)
    (Range.Exclusive, Range.Inclusive) -> (succ a, b)
    (Range.Exclusive, Range.Exclusive) -> (succ a, pred b)
fromRange (Range.SingletonRange a) = (a, a)
fromRange (Range.LowerBoundRange a) = case Range.boundType a of
    Range.Exclusive -> (succ (Range.boundValue a), maxBound)
    Range.Inclusive -> (Range.boundValue a, maxBound)
fromRange (Range.UpperBoundRange a) = case Range.boundType a of
    Range.Inclusive -> (minBound, Range.boundValue a)
    Range.Exclusive -> (minBound, pred (Range.boundValue a))
fromRange Range.InfiniteRange = (minBound, maxBound)
