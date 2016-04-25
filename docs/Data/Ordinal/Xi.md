## Module Data.Ordinal.Xi

ξ α β is the smallest ordinal γ such that α, β < γ and γ is not the value
of ξ for any smaller α or same α with smaller β.

#### `Ξ`

``` purescript
data Ξ
  = Ø
  | Ξ Ξ Ξ
```

##### Instances
``` purescript
Eq Ξ
Ord Ξ
Ordinal Ξ
```

#### `ξ`

``` purescript
ξ :: Ξ -> Ξ -> Ξ
```

#### `succ`

``` purescript
succ :: Ξ -> Ξ
```

#### `lim`

``` purescript
lim :: Ξ -> Ξ
```


