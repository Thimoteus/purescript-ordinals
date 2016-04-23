module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, print)

import Data.Ordinal.Cantor (exp, fromInt)

omega = exp (fromInt 1)
twotimesomega = fromInt 2 * omega
omegatimestwo = omega * fromInt 2

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  print twotimesomega
  print omegatimestwo
  print $ twotimesomega == omega
  print $ exp omega

