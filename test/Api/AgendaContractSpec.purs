module Test.Api.AgendaContractSpec (spec) where

import Prelude

import Api.AgendaContract (Method(..), updateMethod, updatePath)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Agenda API contract" do
    it "uses POST for updating calendar items" do
      updateMethod `shouldEqual` POST

    it "builds update paths with the item id" do
      updatePath "item-123" `shouldEqual` "/api/v1/calendar-items/item-123"
