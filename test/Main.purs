module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.LegacyTests (runLegacyTests)

main :: Effect Unit
main = do
  runLegacyTests
  log "✅ All legacy tests passed"
