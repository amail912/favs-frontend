module Test.Main where

import Prelude

import Checklists (Checklist(..), ChecklistItem(..), removeChecklistItem)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (head, length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Notes (Note(..), newNote)

assert :: String -> Boolean -> Effect Unit
assert message condition =
  if condition then pure unit else throw message

main :: Effect Unit
main = do
  testNotesJsonRoundTrip
  testChecklistsJsonRoundTrip
  testChecklistItemRemoval
  log "✅ All tests passed"

testNotesJsonRoundTrip :: Effect Unit
testNotesJsonRoundTrip = do
  let
    serverNote = ServerNote
      { content: { title: "Server title", noteContent: "Server content" }
      , storageId: { version: "v1", id: "42" }
      }
  case decodeJson (encodeJson newNote) of
    Right decodedNewNote -> assert "New note JSON round-trip should preserve data" (decodedNewNote == newNote)
    Left _ -> throw "Decoding encoded new note failed"

  case decodeJson (encodeJson serverNote) of
    Right decodedServerNote -> assert "Server note JSON round-trip should preserve data" (decodedServerNote == serverNote)
    Left _ -> throw "Decoding encoded server note failed"

testChecklistsJsonRoundTrip :: Effect Unit
testChecklistsJsonRoundTrip = do
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
    Right decodedChecklist -> assert "Checklist JSON round-trip should preserve data" (decodedChecklist == checklist)
    Left _ -> throw "Decoding encoded checklist failed"

testChecklistItemRemoval :: Effect Unit
testChecklistItemRemoval = do
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
      assert "Checklist should contain one item after deletion" (length items == 1)
      case head items of
        Just (ChecklistItem { label }) -> assert "Remaining checklist item should be B" (label == "B")
        Nothing -> throw "Expected one checklist item after deletion"
    _ -> throw "Checklist item removal returned unexpected result"
