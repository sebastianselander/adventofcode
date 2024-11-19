{-# Language UnboxedTuples #-}

module Advent.Chinese (Mod(..), toMod, chinese) where

import Control.Monad (foldM)
import GHC.Num.Integer (integerGcde)

-- | A package of a residue and modulus. To 'toMod' when constructing
-- to ensure the reduced invariant is maintained.
data Mod = Mod { residue, modulus :: !Integer }
  deriving (Eq, Read, Show)

-- | Construct an element of 'Mod' with a given value and modulus.
-- Modulus must be greater than zero.
toMod ::
  Integer {- ^ integer -} ->
  Integer {- ^ modulus -} ->
  Mod     {- ^ residue mod modulus -}
toMod r m
  | m > 0     = Mod (r `mod` m) m -- needs to be `mod` to handle negative values
  | otherwise = error ("toMod: invalid modulus " ++ show m)

chinese' :: Mod -> Mod -> Maybe Mod
chinese' (Mod n1 m1) (Mod n2 m2)
  | d == 1
  = Just $! toMod (m2          * n1 * v + m1          * n2 * u) (m1          * m2)
  | (n1 - n2) `rem` d == 0
  = Just $! toMod (m2 `quot` d * n1 * v + m1 `quot` d * n2 * u) (m1 `quot` d * m2)
  | otherwise = Nothing
  where
    (d, u, v) = integerGcde m1 m2

-- | Find an integer that is equal to all the given numbers individually
-- considering the modulus of those numbers.
--
-- Example: If @x = 2 (mod 3) = 3 (mod 5) = 2 (mod 7)@ then @x = 23@
-- 
-- >>> chinese [toMod 2 3, toMod 3 5, toMod 2 7]
-- Just 23
chinese :: [Mod] -> Maybe Integer
chinese []     = Just 0
chinese (x:xs) = residue <$> foldM chinese' x xs
