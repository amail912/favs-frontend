module Test.Helpers.DateTimeSpec (spec) where

import Prelude

import Helpers.DateTime as DateTime
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Support.Builders (unsafeDate, unsafeDateTime)

spec :: Spec Unit
spec =
  describe "Helpers.DateTime display formatting" do
    it "formats full date-times using the shared French display policy" do
      DateTime.formatDisplayDateTime (unsafeDateTime "2026-04-02T08:05")
        `shouldEqual` "02/04/2026 08:05"

    it "formats dates using the shared French display policy" do
      DateTime.formatDisplayDate (unsafeDate "2026-04-02")
        `shouldEqual` "02/04/2026"

    it "formats raw dates using the shared French display policy" do
      DateTime.formatDisplayDateRaw "2026-04-02"
        `shouldEqual` "02/04/2026"

    it "formats times using the shared French display policy" do
      DateTime.formatDisplayTimeRaw "08:05"
        `shouldEqual` "08:05"

    it "keeps invalid raw values visible instead of crashing" do
      DateTime.formatDisplayDateTimeRaw "invalid-date-time"
        `shouldEqual` "invalid-date-time"
