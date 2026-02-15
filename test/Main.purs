module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.SpecSuite (runSpecSuite)

main :: Effect Unit
main = launchAff_ runSpecSuite
