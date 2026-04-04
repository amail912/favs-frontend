module Test.Ui.DateTimePickerSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Ui.DateTimePicker (combineIsoLocalDateTimeParts, splitIsoLocalDateTime)

spec :: Spec Unit
spec =
  describe "Ui.DateTimePicker" do
    it "splits a valid iso-local datetime into date and time parts" do
      splitIsoLocalDateTime "2026-04-04T09:30"
        `shouldEqual`
          { dateValue: "2026-04-04"
          , timeValue: "09:30"
          }

    it "returns empty parts for an invalid iso-local datetime" do
      splitIsoLocalDateTime "invalid-date-time"
        `shouldEqual`
          { dateValue: ""
          , timeValue: ""
          }

    it "recombines valid date and time parts into an iso-local datetime" do
      combineIsoLocalDateTimeParts "2026-04-04" "09:30"
        `shouldEqual` Just "2026-04-04T09:30"

    it "rejects incomplete or invalid parts" do
      combineIsoLocalDateTimeParts "2026-04-04" ""
        `shouldEqual` Nothing
