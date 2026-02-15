module Test.SpecSuite (runSpecSuite) where

import Prelude

import Checklists (Checklist(..), ChecklistItem(..), removeChecklistItem)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (head, length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Notes (Note(..), newNote)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

runSpecSuite :: Aff Unit
runSpecSuite = runSpec [ consoleReporter ] do
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

  describe "Checklists" do
    it "round-trips a server checklist" do
      let
        checklist = ServerChecklist
          { content:
              { name: "Daily"
              , items:
                  [ ChecklistItem { label: "Coffee", checked: true }
                  , ChecklistItem { label: "Code", checked: false }
                  ]
              }
          , storageId: { version: "v2", id: "99" }
          }
      case decodeJson (encodeJson checklist) of
        Right decoded -> decoded `shouldEqual` checklist
        Left err -> fail $ "Decoding encoded checklist failed: " <> show err

    it "removes an item at a valid index" do
      let
        checklist = NewChecklist
          { content:
              { name: "Todos"
              , items:
                  [ ChecklistItem { label: "A", checked: false }
                  , ChecklistItem { label: "B", checked: false }
                  ]
              }
          }
      case removeChecklistItem 0 checklist of
        Just (NewChecklist { content: { items } }) -> do
          length items `shouldEqual` 1
          case head items of
            Just (ChecklistItem { label }) -> label `shouldEqual` "B"
            Nothing -> fail "Expected one checklist item after deletion"
        _ -> fail "Checklist item removal returned unexpected result"
