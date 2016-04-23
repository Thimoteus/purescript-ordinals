module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)

import Data.Ordinal.Cantor (Cantor, exp, finite)

omega :: Cantor
omega = exp (finite 1)

twotimesomega :: Cantor
twotimesomega = finite 2 * omega

omegatimestwo :: Cantor
omegatimestwo = omega * finite 2

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  print twotimesomega
  print omegatimestwo
  print $ twotimesomega == omega
  print $ exp omega
  print $ (omega + finite 1) * omega

