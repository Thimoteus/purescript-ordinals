module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print, log)

import Data.Ordinal.Cantor (finite, ω, (**))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Standard sum/product breaks Semiring laws:"
  log "ω ++ finite 3 == finite 3 ++ ω"
  print $ ω ++ finite 3 == finite 3 ++ ω
  log "(finite 1 ++ finite 1) ** ω == ω ** (finite 1 ++ finite 1)"
  print $ (finite 1 ++ finite 1) ** ω == ω ** (finite 1 ++ finite 1)
  log "While natural sum/product does not:"
  log "ω + finite 3 == finite 3 + ω"
  print $ ω + finite 3 == finite 3 + ω
  log "(finite 1 + finite 1) * ω == ω * (finite 1 + finite 1)"
  print $ (finite 1 + finite 1) * ω == ω * (finite 1 + finite 1)
