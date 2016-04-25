## Module Control.Ordinal

#### `Ordinal`

``` purescript
class (Ord λ) <= Ordinal λ where
  z :: λ
  succ :: λ -> λ
  lim :: λ -> λ
```

Typeclass laws: for all Ordinal λ,
1. `z <= λ`, with equality iff `z == λ`
2. `succ λ > λ`
3. There is no Ordinal κ with λ < κ < succ λ
4. There is no Ordinal κ with succ κ == λ when λ = lim θ, for some Ordinal θ


