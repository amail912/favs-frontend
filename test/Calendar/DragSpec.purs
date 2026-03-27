module Test.Calendar.DragSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Pages.Calendar (computeDropMinuteIndex, computeMinuteIndexFromClientY, indexToTimeLabel, resolveDroppedWindow, resolveDroppedWindowFromIndex)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Support.Builders (unsafeDateTime)

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

    it "computes a drop index from stable grid geometry" do
      computeMinuteIndexFromClientY 300 100.0 1440.0 `shouldEqual` Just 40

    it "returns Nothing when grid geometry is invalid" do
      computeMinuteIndexFromClientY 300 100.0 0.0 `shouldEqual` Nothing

    it "resolves a dropped window on the focused date and dropped time" do
      resolveDroppedWindow "2026-04-10" 20 60 120
        `shouldEqual`
          Just
            { start: unsafeDateTime "2026-04-10T09:40"
            , end: unsafeDateTime "2026-04-10T10:40"
            }

    it "does not fall back to midnight when the focus date is invalid" do
      resolveDroppedWindow "invalid-date" 20 60 120 `shouldEqual` Nothing

    it "does not reapply offset when using an already adjusted drop index" do
      resolveDroppedWindowFromIndex "2026-04-10" 60 48
        `shouldEqual`
          Just
            { start: unsafeDateTime "2026-04-10T04:00"
            , end: unsafeDateTime "2026-04-10T05:00"
            }
