## Module Data.Ordinal.Cantor

This module defines ordinals written in Cantor normal form with a base of
ω. That is, every ordinal can be uniquely represented in the form
ω^β∙c₁₁ + ω^β₂∙c₂ + .. + ω^β₀∙c₀, for ordinals β₁ >= β₂ >= .. >= β₀ and
positive integers c₁, c₂ .. c₀.

**CAUTION**: The `Semiring` instance provided here is NOT the standard
ordinal sum/product. To conform to typeclass laws, `Semiring` follows the
Hessenberg sum and product.

The `Semigroup` instance here, however, does correspond to standard
ordinal addition. An extra function and operator (**) are provided for
standard multiplication.

#### `Cantor`

``` purescript
data Cantor
```

##### Instances
``` purescript
Show Cantor
Eq Cantor
Ord Cantor
Ordinal Cantor
Semiring Cantor
Semigroup Cantor
Monoid Cantor
```

#### `exp`

``` purescript
exp :: Cantor -> Cantor
```

#### `hasCoeffAt`

``` purescript
hasCoeffAt :: Cantor -> Cantor -> HugeInt
```

κ `hasCoeffAt` λ gets the integer coefficient of the degree-λ term of κ.
For example, \ x -> ω `hasCoeffAt` finite x == finite 1, when x == finite 1
and finite 0 otherwise.
You can think of it as (!!) on lists where elements are terms of a polynomial,
except the case of Nothing corresponds to a coefficient of zero.

#### `(??)`

``` purescript
infixr 1 hasCoeffAt as ??
```

_right-associative / precedence 1_

#### `finite`

``` purescript
finite :: Int -> Cantor
```

#### `finiteFromString`

``` purescript
finiteFromString :: String -> Maybe Cantor
```

#### `isFinite`

``` purescript
isFinite :: Cantor -> Boolean
```

#### `isTransfinite`

``` purescript
isTransfinite :: Cantor -> Boolean
```

#### `degree`

``` purescript
degree :: Cantor -> Cantor
```

#### `exponents`

``` purescript
exponents :: Cantor -> List Cantor
```

#### `multStandard`

``` purescript
multStandard :: Cantor -> Cantor -> Cantor
```

#### `(**)`

``` purescript
infixl 7 multStandard as **
```

_left-associative / precedence 7_

#### `ω`

``` purescript
ω :: Cantor
```


