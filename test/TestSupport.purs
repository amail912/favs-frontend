module Test.TestSupport (assert) where

import Prelude

import Effect (Effect)
import Effect.Exception (throw)

assert :: String -> Boolean -> Effect Unit
assert message condition =
  if condition then pure unit else throw message
