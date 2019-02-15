module Main where

import Prelude

import App.Data
import Component (component)
import Data.Array.NonEmpty (singleton)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

jason :: Employee
jason = { name: "Jason", roles: singleton Programmer }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (component $ singleton jason) unit body
