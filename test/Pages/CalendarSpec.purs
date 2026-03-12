module Test.Pages.CalendarSpec (spec) where

import Prelude

import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (Response)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Either (Either(..))
import Helpers.DateTime (formatCalendarDayDateLabelWithReference)
import Pages.Calendar (decodeCalendarItemsResponse)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec =
  describe "Calendar" do
    it "returns empty items when status is 401" do
      let
        response = mkResponse 401 jsonEmptyObject
      case decodeCalendarItemsResponse response of
        Right items -> items `shouldEqual` []
        Left err -> fail $ "Expected empty calendar items for 401 response, got: " <> show err

    it "formats a same-year day label without the year" do
      formatCalendarDayDateLabelWithReference "2026-03-12" "2026-01-01"
        `shouldEqual` "jeu. 12 mars"

    it "formats a cross-year day label with the year" do
      formatCalendarDayDateLabelWithReference "2027-03-12" "2026-01-01"
        `shouldEqual` "ven. 12 mars 2027"

    it "falls back to the raw date when the input is invalid" do
      formatCalendarDayDateLabelWithReference "invalid-date" "2026-01-01"
        `shouldEqual` "invalid-date"

mkResponse :: Int -> Json -> Response Json
mkResponse code body =
  { status: StatusCode code
  , statusText: ""
  , headers: []
  , body: body
  }
