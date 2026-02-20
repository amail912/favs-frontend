module Test.Pages.NotesSpec (spec) where

import Prelude

import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Pages.Notes (Note(..), newNote)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec =
  describe "Notes JSON" do
    it "round-trips a new note" do
      case decodeJson (encodeJson newNote) of
        Right decoded -> decoded `shouldEqual` newNote
        Left err -> fail $ "Decoding encoded new note failed: " <> show err

    it "round-trips a server note" do
      let
        serverNote = ServerNote
          { content: { title: "Server title", noteContent: "Server content" }
          , storageId: { version: "v1", id: "42" }
          }
      case decodeJson (encodeJson serverNote) of
        Right decoded -> decoded `shouldEqual` serverNote
        Left err -> fail $ "Decoding encoded server note failed: " <> show err
