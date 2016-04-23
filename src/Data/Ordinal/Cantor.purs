module Data.Ordinal.Cantor
  ( Cantor
  , ω
  , fromInt
  , isFinite
  , isTransfinite
  , exp
  , degree
  , exponents
  ) where

import Prelude

import Data.List (List(..), singleton, reverse, drop, zipWith, concat, concatMap)
import Data.Foldable (intercalate)

import Control.Ordinal (class Ordinal)

infixr 6 Cons as :

-- | Cantor normal form. Every ordinal can be uniquely represented in the form
-- | c₁∙ω^β₁ + c₂∙ω^β₂ + .. + c₀∙ω^β₀, for ordinals β₁ >= β₂ >= .. >= β₀ and positive
-- | integers c₁, c₂ .. c₀.
-- | A `Cantor` constructed from a sum represents a transfinite ordinal where
-- | each element of the list is a term in the sum, in little endian form, ordered
-- | by the coefficient.
data Cantor = N Int | Σ (List { coeff :: Int, exp :: Cantor })
type CantorRec = { coeff :: Int, exp :: Cantor }

instance showCantor :: Show Cantor where
  show (N n) = "Finite " <> show n
  show (Σ xs) = intercalate " + " $ map show' xs where
    show' r = show r.coeff <> "*exp (" <> show r.exp <> ")"

instance eqCantor :: Eq Cantor where
  eq (N m) (N n) = m == n
  eq (N _) _ = false
  eq _ (N _) = false
  eq (Σ xs) (Σ ys) = eq' xs ys
    where
      eq' (x : xs) (y : ys) = if x.exp == y.exp && x.coeff == y.coeff
                                 then eq (dropsimp xs) (dropsimp ys)
                                 else false
      eq' Nil Nil = true
      eq' _ _ = false

instance ordCantor :: Ord Cantor where
  compare (N n) (N m) = compare n m
  compare (N _) (Σ _) = LT
  compare (Σ _) (N _) = GT
  compare (Σ xs) (Σ ys) = compare' (reverse xs) (reverse ys)
    where
      compare' Nil Nil = EQ
      compare' Nil ys = LT
      compare' xs Nil = GT
      compare' (x : xs) (y : ys) =
        case compare x.exp y.exp of
             EQ -> case compare x.coeff y.coeff of
                        EQ -> compare (droprevsimp xs) (droprevsimp ys)
                        x -> x
             x -> x

instance ordinalCantor :: Ordinal Cantor where
  z = N 0
  succ = succ
  lim = exp

droprevsimp :: List CantorRec -> Cantor
droprevsimp = simplify <<< Σ <<< reverse <<< drop 1

dropsimp :: List CantorRec -> Cantor
dropsimp = simplify <<< Σ <<< drop 1

ω :: Cantor
ω = Σ (singleton { coeff: 1, exp: N 1 })

fromInt :: Int -> Cantor
fromInt n | n < 0 = N 0
fromInt n = N n

isFinite :: Cantor -> Boolean
isFinite (N _) = true
isFinite _ = false

isTransfinite :: Cantor -> Boolean
isTransfinite (Σ _) = true
isTransfinite _ = false

simplify :: Cantor -> Cantor
simplify (Σ ({ exp = N 0, coeff } : Nil)) = N coeff
simplify (Σ Nil) = N 0
simplify x = x

succ :: Cantor -> Cantor
succ (N n) = N (n + 1)
succ (Σ ({ exp = N 0, coeff = n } : rs)) = Σ ({ exp: N 0, coeff: n + 1 } : rs)
succ (Σ xs) = Σ ({ exp: N 0, coeff: 1 } : xs)

exp :: Cantor -> Cantor
exp λ = Σ (singleton { coeff: 1, exp: λ })

addCantor :: Cantor -> Cantor -> Cantor
addCantor (N n) (N m) = N (n + m)
addCantor (N _) λ = λ
addCantor (Σ ({ exp = N 0, coeff = n } : rs)) (N m) = Σ ({ exp: N 0, coeff: n + m } : rs)
addCantor (Σ xs) (N n) = Σ ({ exp: N 0, coeff: n } : xs)
addCantor (Σ xs) (Σ ys) = Σ $ concat $ zipWith addRecord xs ys

addRecord :: CantorRec -> CantorRec -> List CantorRec
addRecord r@{ exp = β, coeff = c } r'@{ exp = β', coeff = c' }
  | β' > β = singleton r'
  | β' == β = singleton $ r' { coeff = c + c' }
  | otherwise = r : r' : Nil

degree :: Cantor -> Cantor
degree (N _) = N 0
degree (Σ xs) = case reverse xs of
                      (y : _) -> y.exp
                      _ -> N 0

exponents :: Cantor -> List Cantor
exponents (N _) = singleton $ N 0
exponents (Σ xs) = map _.exp xs

multCantor :: Cantor -> Cantor -> Cantor
multCantor (N m) (N n) = N (m * n)
multCantor (N 0) _ = N 0
multCantor _ (N 0) = N 0
multCantor κ (N 1) = κ
multCantor (N 1) λ = λ
multCantor (N n) (Σ xs) = Σ $ lmultFinite n <$> xs
multCantor (Σ xs) (N n) = Σ $ rmult xs { exp: N 0, coeff: n }
multCantor (Σ xs) (Σ ys) = Σ $ concatMap (rmult xs) ys

rmult :: List CantorRec -> CantorRec -> List CantorRec
rmult xs { exp = N 0, coeff = n } =
  case reverse xs of
       (y : ys) -> reverse $ y { coeff = y.coeff * n } : ys
       nil -> nil
rmult xs r = singleton $ r { exp = addCantor (degree (Σ xs)) r.exp }

lmultFinite :: Int -> CantorRec -> CantorRec
lmultFinite n { exp = N 0, coeff = m } = { exp: N 0, coeff: n*m }
lmultFinite _ r = r

-- | **WARNING**: This instance **BREAKS** the Semiring laws. Ordinal numbers
-- | do not commute under standard addition, nor do they obey right distributivity
-- | over standard multiplication.
-- | This instance is here merely to take advantage of operator overloading.
instance semiringCantor :: Semiring Cantor where
  one = N 1
  zero = N 0
  add = addCantor
  mul = multCantor
