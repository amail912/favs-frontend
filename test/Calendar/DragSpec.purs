module Test.Calendar.DragSpec (spec) where

import Prelude

import Agenda.Drag (computeDropMinuteIndex, indexToTimeLabel)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Calendar drag & drop" do
    it "aligns drop index with offset" do
      computeDropMinuteIndex 120 20 `shouldEqual` 116

    it "clamps drop index at the start of day" do
      computeDropMinuteIndex 0 30 `shouldEqual` 0

    it "formats the drop label for an index" do
      indexToTimeLabel 116 `shouldEqual` "09:40"

    it "keeps indicator label aligned with drop start" do
      let
        cursorIndex = 120
        offset = 20
        adjusted = computeDropMinuteIndex cursorIndex offset
      indexToTimeLabel adjusted `shouldEqual` "09:40"
