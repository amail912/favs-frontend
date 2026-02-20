module Test.Ui.ErrorsSpec (spec) where

import Prelude

import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Ui.Errors (FatalError(..), toFatalError)

spec :: Spec Unit
spec =
  describe "Ui.Errors" do
    it "wraps JsonDecodeError in FatalError" do
      let
        err = TypeMismatch "String"
        result = toFatalError err
      show result `shouldEqual` show (DecodeError err)
