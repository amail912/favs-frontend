module Main where

import Prelude

import Pages.App (component) as App
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI App.component unit body

logError :: forall a. Show a => a -> Aff Unit
logError = liftEffect <<< logShow
