module Test.Pages.ChecklistsSpec (spec) where

import Prelude

import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (Response)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Pages.Checklists (Checklist(..), ChecklistItem(..), decodeChecklistsResponse, removeChecklistItem)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (head, length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec =
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

    it "returns empty checklists when status is 401" do
      let
        response = mkResponse 401 jsonEmptyObject
      case decodeChecklistsResponse response of
        Right checklists -> checklists `shouldEqual` []
        Left err -> fail $ "Expected empty checklists for 401 response, got: " <> show err

mkResponse :: Int -> Json -> Response Json
mkResponse code body =
  { status: StatusCode code
  , statusText: ""
  , headers: []
  , body: body
  }
