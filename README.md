# purescript-ordinals [![Build Status](https://travis-ci.org/Thimoteus/purescript-ordinals.svg)](https://travis-ci.org/Thimoteus/purescript-ordinals)

*For when finite numbers just aren't enough.*

This package defines an ordinal data type based on [Cantor normal form](https://proofwiki.org/wiki/Definition:Cantor_Normal_Form).

## Notes

1. Any ordinal can be expressed in CNF, however, due to the [fixed-point lemma for
continuous functions](https://en.wikipedia.org/wiki/Fixed-point_lemma_for_normal_functions),
[almost all](https://en.wikipedia.org/wiki/Almost_all) ordinals can't be represented
using this library.
2. Standard ordinal addition and multiplication are provided by the `Semigroup`
instance `(++)` and a custom `(**)` operator, respectively. This is because
the class of ordinals **Or** with standard sums and products do not form a semiring.
However, **Or** paired with the Hessenberg sum and Hausdorff product (aka natural sum
and product, respectively) do, so those are used to provide the `Semiring` instance.

## Example usage

A finite ordinal larger than that allowed by Purescript or even than the largest
allowable Javascript `Number`:

```purescript
k :: Cantor
k = fromJust $ finiteFromString $ fromCharArray $ ['1'] <> replicate 309 '0'
```

A transfinite ordinal larger than any finite ordinal:

```purescript
omega :: Cantor
omega = exp $ finite 1
```
(or just use the exported constant `ω`).

Some arbitrary ordinal:

```purescript
k :: Cantor
k = exp (exp (exp (finite 20 + exp (finite 29) * finite 5096))) * finite 209 + finite 1
```

## Installing

`bower i --save purescript-ordinals`
