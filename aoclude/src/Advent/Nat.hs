{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Advent.Nat where

import GHC.TypeNats qualified as T

-- | Natural numbers (used for type index)
data Nat
    = -- | zero
      Z
    | -- | successor
      S Nat

-- | Covert from GHC type literal syntax to an inductively defined natural
type family FromNatural (n :: T.Natural) :: Nat where
    FromNatural 0 = 'Z
    FromNatural n = 'S (FromNatural (n T.- 1))

class UnfoldNat n where
    unfoldNat :: f 'Z -> (forall m. f m -> f ('S m)) -> f n

instance UnfoldNat 'Z where
    unfoldNat z _ = z

instance (UnfoldNat n) => UnfoldNat ('S n) where
    unfoldNat z s = s (unfoldNat z s)
