-- | ξ α β is the smallest ordinal γ such that α, β < γ and γ is not the value
-- | of ξ for any smaller α or same α with smaller β.

module Data.Ordinal.Xi where

import Prelude

import Control.Ordinal (class Ordinal)

data Ξ = Ø | Ξ Ξ Ξ

instance eqXi :: Eq Ξ where
  eq Ø Ø = true
  eq (Ξ ξ1 ξ2) (Ξ ξ3 ξ4) = ξ1 == ξ3 && ξ2 == ξ4
  eq _ _ = false

instance ordXi :: Ord Ξ where
  compare Ø Ø = EQ
  compare Ø _ = LT
  compare _ Ø = GT
  compare κ@(Ξ α β) λ@(Ξ γ δ)
    | α == γ && β < δ = LT
    | α < γ && β < λ = LT
    | α > γ && κ <= δ = LT
  compare κ λ = case compare λ κ of
                       GT -> LT
                       LT -> GT
                       x -> x

instance xiOrdinal :: Ordinal Ξ where
  z = Ø
  succ = succ
  lim = lim

ξ :: Ξ -> Ξ -> Ξ
ξ = Ξ

succ :: Ξ -> Ξ
succ = ξ Ø

lim :: Ξ -> Ξ
lim = flip ξ Ø
