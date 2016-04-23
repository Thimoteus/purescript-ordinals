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
  succ _ = Ø
  lim _ = Ø

infix 0 Ξ as ##

succ :: Ξ -> Ξ
succ Ø = Ø ## Ø
succ (λ ## κ) = Ø
