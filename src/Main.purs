module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import SlotMachine as SlotMachine

main :: Eff (HA.HalogenEffects (random :: RANDOM)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI SlotMachine.ui unit body
