{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE StandaloneDeriving #-}

module Advent.Box where

import Control.Monad (foldM)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Kind (Type)
import Data.List (foldl1')
import GHC.Stack (HasCallStack)

import Advent.Coord (Coord (C))
import Advent.Coord3 (Coord3 (C3))
import Advent.Nat (FromNatural, Nat (S, Z), UnfoldNat (unfoldNat))
import Advent.ReadS (P (unP), pread, preadParen, tok)

-- | Type synonym for 'Box' allowing the use of natural number literals.
type Box' n = Box (FromNatural n)

-- | An n-dimensional box.
data Box :: Nat -> Type where
    Pt ::
        -- | A single point
        Box 'Z
    Dim ::
        -- | inclusive lower bound
        !Int ->
        -- | exclusive upper bound
        !Int ->
        -- | lower dimensional box
        Box n ->
        -- | A box extended along an axis
        Box ('S n)

deriving instance Show (Box n)
deriving instance Eq (Box n)
deriving instance Ord (Box n)

{- | Returns the number of points contained in a box.

>>> size Pt -- 0D point
1

>>> size (Dim 1 4 Pt) -- 1D segment; length
3

>>> size (Dim 1 4 (Dim 0 3 Pt)) -- 2D rectangle; area
9

>>> size (Dim 1 4 (Dim 0 3 (Dim 0 2 Pt))) -- 3D cuboid; volume
18
-}
size :: Box n -> Int
size Pt = 1
size (Dim lo hi box) = (hi - lo) * size box

{- | The intersection of two boxes is the intersection of their segments.

>>> intersectBox (Dim 0 2 (Dim 0 3 Pt)) (Dim 1 4 (Dim 2 4 Pt))
Just (Dim 1 2 (Dim 2 3 Pt))
-}
intersectBox :: Box n -> Box n -> Maybe (Box n)
intersectBox Pt Pt = Just Pt
intersectBox (Dim a b xs) (Dim c d ys) =
    [Dim x y zs | let x = max a c, let y = min b d, x < y, zs <- intersectBox xs ys]

-- | Intersection of one or more boxes.
intersectBoxes :: (HasCallStack) => [Box n] -> Maybe (Box n)
intersectBoxes [] = error "intersectBoxes: empty intersection"
intersectBoxes (x : xs) = foldM intersectBox x xs

{- | Subtract the first box from the second box returning a list of boxes
that cover all the remaining area.

>>> subtractBox (Dim 2 3 Pt) (Dim 0 4 Pt)
[Dim 0 2 Pt,Dim 3 4 Pt]

>>> subtractBox (Dim 3 5 Pt) (Dim 0 4 Pt)
[Dim 0 3 Pt]

>>> subtractBox (Dim 0 1 Pt) (Dim 1 2 Pt)
[Dim 1 2 Pt]

>>> subtractBox (Dim 0 1 (Dim 0 1 Pt)) (Dim 0 2 (Dim 0 2 Pt))
[Dim 1 2 (Dim 0 2 Pt),Dim 0 1 (Dim 1 2 Pt)]

>>> subtractBox (Dim 0 9 Pt) (Dim 3 6 Pt)
[]
-}
subtractBox ::
    -- | remove this
    Box n ->
    -- | from this
    Box n ->
    -- | leaving these
    [Box n]
subtractBox b1 b2 =
    case intersectBox b1 b2 of
        Nothing -> [b2]
        Just b -> subtractBox' b b2

{- | Worker for 'subtractBox' where the first argument is a
subset of the second argument.
-}
subtractBox' :: Box n -> Box n -> [Box n]
subtractBox' Pt Pt = []
subtractBox' (Dim a b xs) (Dim c d ys) =
    [Dim c a ys | c < a]
        ++ [Dim a b zs | zs <- subtractBox' xs ys]
        ++ [Dim b d ys | b < d]

{- | Compute the box that encompasses both arguments. This might cover
extra elements as no such box might exist that is the perfect union
of the two boxes.

>>> coverBox (Dim 2 3 Pt) (Dim 0 4 Pt)
Dim 0 4 Pt

>>> coverBox (Dim 1 3 Pt) (Dim 2 4 Pt)
Dim 1 4 Pt

>>> coverBox (Dim 0 1 Pt) (Dim 3 4 Pt)
Dim 0 4 Pt
-}
coverBox :: Box n -> Box n -> Box n
coverBox (Dim a b x) (Dim c d y) = Dim (min a c) (max b d) (coverBox x y)
coverBox Pt Pt = Pt

-- | Compute the box that encompasses all of the boxes in the list.
coverBoxes :: (HasCallStack) => [Box n] -> Box n
coverBoxes = foldl1' coverBox

{- | Given a list of potentially overlapping boxes create a new list
of boxes that cover the same region but which do not overlap.

Note that this function does not attempt to combine boxes.

>>> unionBoxes [Dim 2 3 Pt, Dim 0 4 Pt]
[Dim 2 3 Pt,Dim 0 2 Pt,Dim 3 4 Pt]

>>> unionBoxes [Dim 1 3 Pt, Dim 2 4 Pt]
[Dim 1 3 Pt,Dim 3 4 Pt]

>>> unionBoxes [Dim 0 1 Pt, Dim 3 4 Pt]
[Dim 0 1 Pt,Dim 3 4 Pt]
-}
unionBoxes :: [Box a] -> [Box a]
unionBoxes = foldr add []
  where
    add box rest = box : concatMap (subtractBox box) rest

instance (UnfoldNat n) => Read (Box n) where
    readsPrec p = unP (getCompose (unfoldNat pt dim))
      where
        pt :: Compose P Box 'Z
        pt = Compose (preadParen False (Pt <$ tok "Pt"))

        dim :: Compose P Box m -> Compose P Box ('S m)
        dim (Compose more) =
            Compose
                ( preadParen
                    (p >= 11)
                    (Dim <$ tok "Dim" <*> pread <*> pread <*> more)
                )

rectToCoord :: Box (S (S Z)) -> (Coord, Coord)
rectToCoord (Dim l1 u1 (Dim l0 u0 Pt)) = (C l0 l1, C (u0 - 1) (u1 - 1))

coordToRect :: (Coord, Coord) -> Box (S (S Z))
coordToRect (C l0 l1, C u0 u1) = Dim l1 (u1 + 1) (Dim l0 (u0 + 1) Pt)

cubeToCoord3 :: Box (S (S (S Z))) -> (Coord3, Coord3)
cubeToCoord3 (Dim l2 u2 (Dim l1 u1 (Dim l0 u0 Pt))) = (C3 l0 l1 l2, C3 (u0 - 1) (u1 - 1) (u2 - 1))

coord3ToCube :: (Coord3, Coord3) -> Box (S (S (S Z)))
coord3ToCube (C3 l0 l1 l2, C3 u0 u1 u2) = Dim l2 (u2 + 1) (Dim l1 (u1 + 1) (Dim l0 (u0 + 1) Pt))
