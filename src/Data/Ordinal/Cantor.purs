-- | This module defines ordinals written in Cantor normal form with a base of
-- | ω. That is, every ordinal can be uniquely represented in the form
-- | ω^β∙c₁₁ + ω^β₂∙c₂ + .. + ω^β₀∙c₀, for ordinals β₁ >= β₂ >= .. >= β₀ and
-- | positive integers c₁, c₂ .. c₀.
-- | **WARNING**: The `Semiring` instance provided here does not obey the laws,
-- | since it is the standard addition and multiplication of ordinals. There is
-- | a law-obeying version (the Hessenberg sum/product) but that instance is not
-- | provided here.

module Data.Ordinal.Cantor
  ( Cantor
  , exp
  , hasCoeffAt, (??)
  , finite
  , finiteFromString
  , isFinite
  , isTransfinite
  , degree
  , exponents
  , ω
  ) where

import Prelude

import Data.Map (Map, toList, keys, singleton, alter, isEmpty, size, fromListWith, delete, update, lookup)
import Data.List (List(..), head, zipWith, concat, concatMap)
import Data.Tuple (Tuple(..))
import Data.Foldable (intercalate, maximum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Function (on)
import Data.HugeInt (HugeInt, fromInt, isZero, fromString, isNegative)
import Data.String (drop, takeWhile)

import Control.Ordinal (class Ordinal)

infixl 6 Tuple as &
infixl 6 type Tuple as &
infixr 5 Cons as :

-- | A `Cantor` constructed from a sum represents a transfinite ordinal where
-- | keys are exponents and values are coefficients. This allows us to easily
-- | group similar terms by merely adding coefficients, and ensures that terms
-- | are unique when they exist.
data Cantor = N HugeInt | Σ (Map Cantor HugeInt)

instance showCantor :: Show Cantor where
  show (N n) = "finite " <> showHugeInt n
  show (Σ m) = intercalate " + " $ map show' $ toList m where
    show' (_ & z) | isZero z = ""
    show' (N z & n) | isZero z = "finite " <> showHugeInt n
    show' (k & o) | o == fromInt 1 = "exp (" <> show k <> ")"
    show' (k & v) = "exp (" <> show k <> ")*" <> showHugeInt v

showHugeInt :: HugeInt -> String
showHugeInt = takeWhile (_ /= '.') <<< drop 7 <<< show

derive instance eqCantor :: Eq Cantor

instance ordCantor :: Ord Cantor where
  compare (N n) (N m) = compare n m
  compare (N _) (Σ _) = LT
  compare (Σ _) (N _) = GT
  compare κ@(Σ m) λ@(Σ m')
    | isEmpty m && isEmpty m' = EQ
    | isEmpty m = LT
    | isEmpty m' = GT
    | degree κ < degree λ = LT
    | degree κ > degree λ = GT
    | otherwise =
      case compare (κ ?? degree κ) (λ ?? degree λ) of
           EQ -> (compare `on` (τ <<< delete (degree κ))) m m'
           x -> x

instance ordinalCantor :: Ordinal Cantor where
  z = finite 0
  succ = succ
  lim = exp

alterFiniteComponent :: (Maybe HugeInt -> HugeInt) -> Map Cantor HugeInt -> Map Cantor HugeInt
alterFiniteComponent f = alter (Just <<< f) (finite 0)

succ :: Cantor -> Cantor
succ (N n) = N (n + fromInt 1)
succ (Σ m) = Σ $ alterFiniteComponent f m where
  f (Just n) = n + fromInt 1
  f _ = fromInt 1

exp :: Cantor -> Cantor
exp λ = τ $ singleton λ $ fromInt 1

-- | κ `hasCoeffAt` λ gets the integer coefficient of the degree-λ term of κ.
-- | For example, \ x -> ω `hasCoeffAt` finite x == 1, when x == 1 and 0 otherwise.
-- | You can think of it as (!!) on lists where elements are terms of a polynomial,
-- | except the case of Nothing corresponds to a coefficient of 0.
hasCoeffAt :: Cantor -> Cantor -> HugeInt
hasCoeffAt (Σ m) λ = fromMaybe zero $ lookup λ m
hasCoeffAt (N n) (N z) | isZero z = n
hasCoeffAt (N _) _ = zero

infixr 1 hasCoeffAt as ??

finite :: Int -> Cantor
finite n | n < 0 = N zero
finite n = N $ fromInt n

finiteFromString :: String -> Maybe Cantor
finiteFromString s = do
  hi <- fromString s
  if isNegative hi
     then Just $ finite 0
     else Just (N hi)

isFinite :: Cantor -> Boolean
isFinite (N _) = true
isFinite _ = false

isTransfinite :: Cantor -> Boolean
isTransfinite (Σ _) = true
isTransfinite _ = false

simplify :: Cantor -> Cantor
simplify (Σ m) | size m == 1 =
                 let f (Just (N z & v)) | isZero z = N v -- finite numbers masquerading as transfinite
                     f (Just _) = Σ m -- actual transfinite numbers
                     f _ = finite 0 -- impossible?
                  in f $ head $ toList m
               | isEmpty m = finite 0
               | hasEmptyFiniteComponent m = Σ $ delete (finite 0) m
simplify (N n) | n < zero = finite 0
simplify x = x

hasEmptyFiniteComponent :: Map Cantor HugeInt -> Boolean
hasEmptyFiniteComponent m =
  case lookup (finite 0) m of
       Just x -> isZero x
       _ -> false

τ :: Map Cantor HugeInt -> Cantor
τ = simplify <<< Σ

degree :: Cantor -> Cantor
degree (Σ m) = largestKey m
degree _ = finite 0

largestKey :: forall a. Map Cantor a -> Cantor
largestKey = fromMaybe (finite 0) <<< maximum <<< keys

-- | ω^β * c + ω^β' * c' = ω^β' * c' when β' > β (if β == β' then factor out the constants)
-- | and if β' < β then it's already in CNF.
addCantor :: Cantor -> Cantor -> Cantor
addCantor (N n) (N m) = N (n + m)
addCantor (N _) λ = λ
addCantor (Σ m) (N n) =
  let f (Just m) = m + n
      f _ = n
   in Σ $ alterFiniteComponent f m
addCantor (Σ m) (Σ m') = Σ $ addTransfinite m m'

addTransfinite :: Map Cantor HugeInt -> Map Cantor HugeInt -> Map Cantor HugeInt
addTransfinite m m' =
  let f :: Cantor & HugeInt -> Cantor & HugeInt -> List (Cantor & HugeInt)
      f k@(β & c) l@(β' & c')
        | β' > β = l : Nil
        | β' == β = β & (c + c') : Nil
        | otherwise = k : l : Nil
      ms = toList m
      ms' = toList m'
      zipped = zipWith f ms ms'
      mlist = concat zipped
   in fromListWith add mlist

exponents :: Cantor -> List Cantor
exponents (N _) = finite 0 : Nil
exponents (Σ m) = keys m

-- | When 0 < α is in CNF, degree α = β₁, leading coeff of α = c₁ 0 < β'
-- | then α * ω^β' = ω^(β₁ + β') and α * n = ω^β * nc₁₁ + ω^β₂ * c₂ + ...
multCantor :: Cantor -> Cantor -> Cantor
multCantor (N m) (N n) = N (m * n)
multCantor (N z) _ | isZero z = finite 0
multCantor _ (N z) | isZero z = finite 0
multCantor κ (N o) | o == fromInt 1 = κ
multCantor (N o) λ | o == fromInt 1 = λ
multCantor κ@(Σ m) (N n) = τ $ update (Just <<< mul n) (degree κ) m
multCantor (N n) (Σ m) =
  let f (Just n') = n * n'
      f _ = fromInt 0
   in τ $ alterFiniteComponent f m
multCantor (Σ m) (Σ m') =
  let f mp (N z & n) | isZero z = rmultFinite mp n -- the finite component of an ordinal in CNF
      f mp (κ & n) = addCantor (largestKey mp) κ & n : Nil -- key-value pairs representing transfinite summands
      ms' = toList m'
      mapped = concatMap (f m) ms' -- use left distribution
   in τ $ fromListWith add mapped

rmultFinite :: Map Cantor HugeInt -> HugeInt -> List (Cantor & HugeInt)
rmultFinite mp n =
  let d = largestKey mp
      mp' = update (Just <<< mul n) d mp
   in toList mp'

instance semiringCantor :: Semiring Cantor where
  one = finite 1
  zero = finite 0
  add = addCantor
  mul = multCantor

ω :: Cantor
ω = exp $ finite 1
