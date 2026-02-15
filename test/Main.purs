module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Test.LegacyTests (runLegacyTests)
import Test.SpecSuite (runSpecSuite)

main :: Effect Unit
main = do
  runLegacyTests
  log "✅ All legacy tests passed"
  launchAff_ runSpecSuite
