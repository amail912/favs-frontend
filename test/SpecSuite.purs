module Test.SpecSuite (runSpecSuite) where

import Prelude

import Agenda (CalendarItem(..), IntentionDraft, ItemStatus(..), ItemType(..), ValidationError(..), detectConflictIds, toNewIntention, validateIntention)
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

  describe "Agenda intentions" do
    it "round-trips a new intention" do
      let
        draft :: IntentionDraft
        draft =
          { title: "Deep work"
          , windowStart: "2026-02-19T09:00"
          , windowEnd: "2026-02-19T10:00"
          }
        newItem = toNewIntention draft
      case decodeJson (encodeJson newItem) of
        Right decoded -> decoded `shouldEqual` newItem
        Left err -> fail $ "Decoding encoded intention failed: " <> show err

    it "round-trips a scheduled block with source item id" do
      let
        scheduled =
          NewCalendarItem
            { content:
                { itemType: ScheduledBlock
                , title: "Planifie"
                , windowStart: "2026-02-19T11:00"
                , windowEnd: "2026-02-19T12:00"
                , status: Todo
                , sourceItemId: Just "source-1"
                }
            }
      case decodeJson (encodeJson scheduled) of
        Right decoded -> decoded `shouldEqual` scheduled
        Left err -> fail $ "Decoding encoded scheduled block failed: " <> show err

    it "fails validation when the title is empty" do
      let
        draft :: IntentionDraft
        draft =
          { title: "   "
          , windowStart: "2026-02-19T09:00"
          , windowEnd: "2026-02-19T10:00"
          }
      validateIntention draft `shouldEqual` Left TitleEmpty

    it "fails validation when the end is before start" do
      let
        draft :: IntentionDraft
        draft =
          { title: "Focus"
          , windowStart: "2026-02-19T10:00"
          , windowEnd: "2026-02-19T09:00"
          }
      validateIntention draft `shouldEqual` Left WindowOrderInvalid

    it "accepts a valid intention" do
      let
        draft :: IntentionDraft
        draft =
          { title: "Sprint"
          , windowStart: "2026-02-19T09:00"
          , windowEnd: "2026-02-19T10:00"
          }
      validateIntention draft `shouldEqual` Right draft

    it "flags conflicts between scheduled blocks" do
      let
        itemA =
          ServerCalendarItem
            { id: "a"
            , content:
                { itemType: ScheduledBlock
                , title: "A"
                , windowStart: "2026-02-19T09:00"
                , windowEnd: "2026-02-19T10:00"
                , status: Todo
                , sourceItemId: Just "src-a"
                }
            }
        itemB =
          ServerCalendarItem
            { id: "b"
            , content:
                { itemType: ScheduledBlock
                , title: "B"
                , windowStart: "2026-02-19T09:30"
                , windowEnd: "2026-02-19T10:30"
                , status: Todo
                , sourceItemId: Just "src-b"
                }
            }
      detectConflictIds [ itemA, itemB ] `shouldEqual` [ "a", "b" ]
