module Test.Pages.CalendarSpec (spec) where

import Prelude

import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (Response)
import Api.Calendar (TripSharingUser(..))
import Data.Argonaut.Core (Json, fromArray, jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Encode (encodeJson, (:=), (~>))
import Data.Either (Either(..))
import Helpers.DateTime (formatCalendarDayDateLabelWithReference)
import Pages.Calendar (decodeCalendarItemsResponse, decodeSharedUsersResponse, decodeTripPlacesResponse, shareWriteErrorMessage, subscriptionWriteErrorMessage, tripWriteErrorMessage, validateShareUsername)
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

    it "decodes trip places payloads" do
      let
        response = mkResponse 200 tripPlacesBody
      case decodeTripPlacesResponse response of
        Right places -> places `shouldEqual` [ "Paris", "Lyon" ]
        Left err -> fail $ "Expected decoded trip places, got: " <> show err

    it "decodes shared users payloads" do
      let
        response = mkResponse 200 sharedUsersBody
      case decodeSharedUsersResponse response of
        Right usernames -> usernames `shouldEqual` [ "alice", "bob" ]
        Left err -> fail $ "Expected decoded shared users, got: " <> show err

    it "encodes share add payloads" do
      decodeJson (encodeJson (TripSharingUser { username: "alice" }))
        `shouldEqual`
          (Right { username: "alice" } :: Either JsonDecodeError { username :: String })

    it "rejects blank share usernames locally" do
      validateShareUsername "   " `shouldEqual` Left "Le nom d'utilisateur est obligatoire."

    it "trims share usernames before submit" do
      validateShareUsername "  alice  " `shouldEqual` Right "alice"

    it "maps known trip write errors to French feedback" do
      let
        response =
          mkResponse 409
            ("message" := "trip time window overlaps another trip" ~> jsonEmptyObject)
      tripWriteErrorMessage response `shouldEqual` "Ce trajet chevauche un autre trajet."

    it "maps known share write errors to French feedback" do
      let
        response =
          mkTextResponse 400 "{\"message\":\"username must reference an existing user\"}"
      shareWriteErrorMessage response `shouldEqual` "Ce nom d'utilisateur est introuvable."

    it "maps known subscription write errors to French feedback" do
      let
        response =
          mkTextResponse 400 "{\"message\":\"username must not be the authenticated user\"}"
      subscriptionWriteErrorMessage response `shouldEqual` "Vous ne pouvez pas vous abonner à vos propres trajets."

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

tripPlacesBody :: Json
tripPlacesBody =
  fromArray
    [ "name" := "Paris" ~> jsonEmptyObject
    , "name" := "Lyon" ~> jsonEmptyObject
    ]

sharedUsersBody :: Json
sharedUsersBody =
  fromArray
    [ "username" := "alice" ~> jsonEmptyObject
    , "username" := "bob" ~> jsonEmptyObject
    ]

mkTextResponse :: Int -> String -> Response String
mkTextResponse code body =
  { status: StatusCode code
  , statusText: ""
  , headers: []
  , body: body
  }
