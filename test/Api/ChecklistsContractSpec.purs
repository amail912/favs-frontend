module Test.Api.ChecklistsContractSpec (spec) where

import Prelude

import Api.ChecklistsContract (basePath, itemPath)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Checklists API contract" do
    it "uses the base checklist path" do
      basePath `shouldEqual` "/api/checklist"

    it "builds checklist item paths with the id" do
      itemPath "chk-9" `shouldEqual` "/api/checklist/chk-9"
