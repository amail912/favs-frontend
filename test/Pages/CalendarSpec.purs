module Test.Pages.CalendarSpec (spec) where

import Prelude

import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (Response)
import Api.Calendar (PeriodTrip(..), PeriodTripGroup(..), TripSharingUser(..))
import Data.Argonaut.Core (Json, fromArray, jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Encode (encodeJson, (:=), (~>))
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Helpers.DateTime as DateTime
import Helpers.DateTime (formatCalendarDayDateLabelWithReference)
import Pages.Calendar (decodeCalendarItemsResponse, decodePeriodTripsResponse, decodeSharedUsersResponse, decodeTripPlacesResponse, normalizePeriodTripGroups, shareWriteErrorMessage, subscriptionWriteErrorMessage, tripWriteErrorMessage, validateShareUsername)
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

    it "decodes grouped period trips payloads" do
      let
        response = mkResponse 200 periodTripsBody
      case decodePeriodTripsResponse response of
        Right groups -> summarizePeriodTripGroups groups `shouldEqual` summarizePeriodTripGroups decodedPeriodTrips
        Left err -> fail $ "Expected decoded period trips, got: " <> show err

    it "normalizes grouped period trips while preserving order" do
      let
        normalized = normalizePeriodTripGroups decodedPeriodTrips
      summarizeNormalizedGroups normalized
        `shouldEqual`
          [ { username: "alice"
            , trips:
                [ { windowStart: "2026-04-02T08:00"
                  , windowEnd: "2026-04-02T09:00"
                  , departurePlaceId: "Paris"
                  , arrivalPlaceId: "Lyon"
                  }
                , { windowStart: "2026-04-02T12:00"
                  , windowEnd: "2026-04-02T13:00"
                  , departurePlaceId: "Lyon"
                  , arrivalPlaceId: "Paris"
                  }
                ]
            }
          , { username: "bob"
            , trips:
                [ { windowStart: "2026-04-01T22:00"
                  , windowEnd: "2026-04-02T01:00"
                  , departurePlaceId: "Le Mesnil"
                  , arrivalPlaceId: "Paris"
                  }
                ]
            }
          ]

    it "drops malformed trips during normalization" do
      let
        normalized = normalizePeriodTripGroups malformedPeriodTrips
      normalized `shouldEqual` []

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

periodTripsBody :: Json
periodTripsBody =
  fromArray
    [ "username" := "alice"
        ~> "trips"
          := fromArray
            [ "windowStart" := "2026-04-02T08:00"
                ~> "windowEnd" := "2026-04-02T09:00"
                ~> "departurePlaceId" := "Paris"
                ~> "arrivalPlaceId" := "Lyon"
                ~> jsonEmptyObject
            , "windowStart" := "2026-04-02T12:00"
                ~> "windowEnd" := "2026-04-02T13:00"
                ~> "departurePlaceId" := "Lyon"
                ~> "arrivalPlaceId" := "Paris"
                ~> jsonEmptyObject
            ]
        ~> jsonEmptyObject
    , "username" := "bob"
        ~> "trips"
          := fromArray
            [ "windowStart" := "2026-04-01T22:00"
                ~> "windowEnd" := "2026-04-02T01:00"
                ~> "departurePlaceId" := "Le Mesnil"
                ~> "arrivalPlaceId" := "Paris"
                ~> jsonEmptyObject
            ]
        ~> jsonEmptyObject
    ]

decodedPeriodTrips :: Array PeriodTripGroup
decodedPeriodTrips =
  [ PeriodTripGroup
      { username: "alice"
      , trips:
          [ PeriodTrip { windowStart: "2026-04-02T08:00", windowEnd: "2026-04-02T09:00", departurePlaceId: "Paris", arrivalPlaceId: "Lyon" }
          , PeriodTrip { windowStart: "2026-04-02T12:00", windowEnd: "2026-04-02T13:00", departurePlaceId: "Lyon", arrivalPlaceId: "Paris" }
          ]
      }
  , PeriodTripGroup
      { username: "bob"
      , trips:
          [ PeriodTrip { windowStart: "2026-04-01T22:00", windowEnd: "2026-04-02T01:00", departurePlaceId: "Le Mesnil", arrivalPlaceId: "Paris" }
          ]
      }
  ]

malformedPeriodTrips :: Array PeriodTripGroup
malformedPeriodTrips =
  [ PeriodTripGroup
      { username: "alice"
      , trips:
          [ PeriodTrip { windowStart: "invalid", windowEnd: "2026-04-02T09:00", departurePlaceId: "Paris", arrivalPlaceId: "Lyon" }
          ]
      }
  ]

mkTextResponse :: Int -> String -> Response String
mkTextResponse code body =
  { status: StatusCode code
  , statusText: ""
  , headers: []
  , body: body
  }

summarizePeriodTripGroups :: Array PeriodTripGroup -> Array { username :: String, trips :: Array { windowStart :: String, windowEnd :: String, departurePlaceId :: String, arrivalPlaceId :: String } }
summarizePeriodTripGroups =
  map \(PeriodTripGroup group) ->
    { username: group.username
    , trips:
        map
          ( \(PeriodTrip trip) ->
              { windowStart: trip.windowStart
              , windowEnd: trip.windowEnd
              , departurePlaceId: trip.departurePlaceId
              , arrivalPlaceId: trip.arrivalPlaceId
              }
          )
          group.trips
    }

summarizeNormalizedGroups :: Array { username :: String, trips :: Array { windowStart :: DateTime, windowEnd :: DateTime, departurePlaceId :: String, arrivalPlaceId :: String } } -> Array { username :: String, trips :: Array { windowStart :: String, windowEnd :: String, departurePlaceId :: String, arrivalPlaceId :: String } }
summarizeNormalizedGroups =
  map \group ->
    { username: group.username
    , trips:
        map
          ( \trip ->
              { windowStart: DateTime.formatLocalDateTime trip.windowStart
              , windowEnd: DateTime.formatLocalDateTime trip.windowEnd
              , departurePlaceId: trip.departurePlaceId
              , arrivalPlaceId: trip.arrivalPlaceId
              }
          )
          group.trips
    }
