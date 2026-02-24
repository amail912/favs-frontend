module Test.Api.NotesContractSpec (spec) where

import Prelude

import Api.NotesContract (basePath, itemPath)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Notes API contract" do
    it "uses the base notes path" do
      basePath `shouldEqual` "/api/note"

    it "builds note item paths with the id" do
      itemPath "note-42" `shouldEqual` "/api/note/note-42"
