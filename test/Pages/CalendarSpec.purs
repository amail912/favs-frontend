module Test.Pages.CalendarSpec (spec) where

import Prelude

import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (Response)
import Api.Calendar (PeriodTrip(..), PeriodTripGroup(..), TripSharingUser(..))
import Data.Argonaut.Core (Json, fromArray, jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Encode (encodeJson, (:=), (~>))
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Helpers.DateTime as DateTime
import Helpers.DateTime (formatCalendarDayDateLabelWithReference)
import Pages.Calendar (PresenceCueColorToken(..), SharedPresenceCuePreference(..), SharedPresenceLoadState(..), SharedPresenceState(..), buildSharedPresenceRailView, buildSharedPresenceSegmentLayouts, decodeCalendarItemsResponse, decodePeriodTripsResponse, decodePresenceCuePreferencesJson, decodeSharedUsersResponse, decodeTripPlacesResponse, deriveSharedPresence, encodePresenceCuePreferencesJson, maxVisibleSharedPresenceUsers, normalizePeriodTripGroups, presenceInspectionAriaLabel, presenceInspectionStateText, presenceInspectionTimeText, resolveSharedPresenceToneClass, shareWriteErrorMessage, sharedPresenceLaneToneClass, sharedPresenceSegmentRailClass, shouldRenderDayCalendarShell, shouldRenderSharedPresenceRail, subscriptionWriteErrorMessage, tripWriteErrorMessage, validateShareUsername)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Support.Builders (unsafeDateTime)

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

    it "derives unknown until the first trip when there is no seed trip" do
      let
        derived = deriveSharedPresence (unsafeDateTime "2026-04-02T00:00") (unsafeDateTime "2026-04-03T00:00") (normalizePeriodTripGroups noSeedPeriodTrips)
      summarizePresenceGroups derived
        `shouldEqual`
          [ { username: "alice"
            , segments:
                [ { start: "2026-04-02T00:00", end: "2026-04-02T08:00", state: "unknown" }
                , { start: "2026-04-02T08:00", end: "2026-04-02T09:00", state: "in-transit:Paris->Lyon" }
                , { start: "2026-04-02T09:00", end: "2026-04-03T00:00", state: "at-place:Lyon" }
                ]
            }
          ]

    it "derives initial place from a completed seed trip before the period" do
      let
        derived = deriveSharedPresence (unsafeDateTime "2026-04-02T00:00") (unsafeDateTime "2026-04-03T00:00") (normalizePeriodTripGroups completedSeedPeriodTrips)
      summarizePresenceGroups derived
        `shouldEqual`
          [ { username: "bob"
            , segments:
                [ { start: "2026-04-02T00:00", end: "2026-04-02T12:00", state: "at-place:Paris" }
                , { start: "2026-04-02T12:00", end: "2026-04-02T13:00", state: "in-transit:Paris->Lyon" }
                , { start: "2026-04-02T13:00", end: "2026-04-03T00:00", state: "at-place:Lyon" }
                ]
            }
          ]

    it "derives in transit from a seed trip active at the period start" do
      let
        derived = deriveSharedPresence (unsafeDateTime "2026-04-02T00:00") (unsafeDateTime "2026-04-03T00:00") (normalizePeriodTripGroups decodedPeriodTrips)
      summarizePresenceGroups derived
        `shouldEqual`
          [ { username: "alice"
            , segments:
                [ { start: "2026-04-02T00:00", end: "2026-04-02T08:00", state: "unknown" }
                , { start: "2026-04-02T08:00", end: "2026-04-02T09:00", state: "in-transit:Paris->Lyon" }
                , { start: "2026-04-02T09:00", end: "2026-04-02T12:00", state: "at-place:Lyon" }
                , { start: "2026-04-02T12:00", end: "2026-04-02T13:00", state: "in-transit:Lyon->Paris" }
                , { start: "2026-04-02T13:00", end: "2026-04-03T00:00", state: "at-place:Paris" }
                ]
            }
          , { username: "bob"
            , segments:
                [ { start: "2026-04-02T00:00", end: "2026-04-02T01:00", state: "in-transit:Le Mesnil->Paris" }
                , { start: "2026-04-02T01:00", end: "2026-04-03T00:00", state: "at-place:Paris" }
                ]
            }
          ]

    it "coalesces adjacent stable states and drops zero-length segments" do
      let
        derived = deriveSharedPresence (unsafeDateTime "2026-04-02T00:00") (unsafeDateTime "2026-04-03T00:00") (normalizePeriodTripGroups adjacentPresenceTrips)
      summarizePresenceGroups derived
        `shouldEqual`
          [ { username: "carol"
            , segments:
                [ { start: "2026-04-02T00:00", end: "2026-04-02T10:00", state: "at-place:Paris" }
                , { start: "2026-04-02T10:00", end: "2026-04-02T11:00", state: "in-transit:Paris->Lyon" }
                , { start: "2026-04-02T11:00", end: "2026-04-03T00:00", state: "at-place:Lyon" }
                ]
            }
          ]

    it "shows the rail only when shared presence data is loaded with users" do
      let
        loadedPresence = SharedPresenceLoaded (deriveSharedPresence (unsafeDateTime "2026-04-02T00:00") (unsafeDateTime "2026-04-03T00:00") (normalizePeriodTripGroups decodedPeriodTrips))
      shouldRenderSharedPresenceRail SharedPresenceLoading `shouldEqual` false
      shouldRenderSharedPresenceRail (SharedPresenceError "boom") `shouldEqual` false
      shouldRenderSharedPresenceRail (SharedPresenceLoaded []) `shouldEqual` false
      shouldRenderSharedPresenceRail loadedPresence `shouldEqual` true

    it "keeps the day shell visible when there is shared presence but no owner items" do
      let
        loadedPresence = SharedPresenceLoaded (deriveSharedPresence (unsafeDateTime "2026-04-02T00:00") (unsafeDateTime "2026-04-03T00:00") (normalizePeriodTripGroups decodedPeriodTrips))
      shouldRenderDayCalendarShell [] SharedPresenceLoading `shouldEqual` false
      shouldRenderDayCalendarShell [] loadedPresence `shouldEqual` true

    it "builds presence rail layouts in backend user order with lane metadata" do
      let
        layouts = buildSharedPresenceSegmentLayouts (deriveSharedPresence (unsafeDateTime "2026-04-02T00:00") (unsafeDateTime "2026-04-03T00:00") (normalizePeriodTripGroups decodedPeriodTrips))
      summarizePresenceRailLayouts layouts
        `shouldEqual`
          [ { username: "alice", segmentIndex: 0, laneIndex: 0, laneCount: 2, startMin: 0, duration: 480, railClass: "calendar-presence-rail__segment--unknown", toneClass: "calendar-presence-rail__segment--tone-0" }
          , { username: "alice", segmentIndex: 1, laneIndex: 0, laneCount: 2, startMin: 480, duration: 60, railClass: "calendar-presence-rail__segment--transit", toneClass: "calendar-presence-rail__segment--tone-0" }
          , { username: "alice", segmentIndex: 2, laneIndex: 0, laneCount: 2, startMin: 540, duration: 180, railClass: "calendar-presence-rail__segment--place", toneClass: "calendar-presence-rail__segment--tone-0" }
          , { username: "alice", segmentIndex: 3, laneIndex: 0, laneCount: 2, startMin: 720, duration: 60, railClass: "calendar-presence-rail__segment--transit", toneClass: "calendar-presence-rail__segment--tone-0" }
          , { username: "alice", segmentIndex: 4, laneIndex: 0, laneCount: 2, startMin: 780, duration: 660, railClass: "calendar-presence-rail__segment--place", toneClass: "calendar-presence-rail__segment--tone-0" }
          , { username: "bob", segmentIndex: 0, laneIndex: 1, laneCount: 2, startMin: 0, duration: 60, railClass: "calendar-presence-rail__segment--transit", toneClass: "calendar-presence-rail__segment--tone-1" }
          , { username: "bob", segmentIndex: 1, laneIndex: 1, laneCount: 2, startMin: 60, duration: 1380, railClass: "calendar-presence-rail__segment--place", toneClass: "calendar-presence-rail__segment--tone-1" }
          ]

    it "keeps all shared users visible when their count does not exceed the readability limit" do
      let
        oneUser = buildSharedPresenceRailView (takePresenceGroups 1 multiUserPresence)
        twoUsers = buildSharedPresenceRailView (takePresenceGroups 2 multiUserPresence)
        threeUsers = buildSharedPresenceRailView (takePresenceGroups maxVisibleSharedPresenceUsers multiUserPresence)
      summarizeSharedPresenceRailView oneUser
        `shouldEqual`
          { visibleUsernames: [ "alice" ]
          , hiddenUsernames: []
          , visibleLaneCounts: [ 1 ]
          , overflowHiddenCount: Nothing
          }
      summarizeSharedPresenceRailView twoUsers
        `shouldEqual`
          { visibleUsernames: [ "alice", "bob" ]
          , hiddenUsernames: []
          , visibleLaneCounts: [ 2, 2 ]
          , overflowHiddenCount: Nothing
          }
      summarizeSharedPresenceRailView threeUsers
        `shouldEqual`
          { visibleUsernames: [ "alice", "bob", "carol" ]
          , hiddenUsernames: []
          , visibleLaneCounts: [ 3, 3, 3 ]
          , overflowHiddenCount: Nothing
          }

    it "moves users beyond the readability limit into ordered overflow" do
      let
        railView = buildSharedPresenceRailView multiUserPresence
      summarizeSharedPresenceRailView railView
        `shouldEqual`
          { visibleUsernames: [ "alice", "bob", "carol" ]
          , hiddenUsernames: [ "dave" ]
          , visibleLaneCounts: [ 3, 3, 3 ]
          , overflowHiddenCount: Just 1
          }

    it "encodes and decodes cue preferences json" do
      let
        preferences =
          [ SharedPresenceCuePreference
              { sharedUsername: "alice"
              , placeId: "Paris"
              , colorToken: CueGreen
              }
          ]
      decodePresenceCuePreferencesJson (encodePresenceCuePreferencesJson preferences)
        `shouldEqual` Right preferences

    it "resolves a personalized cue color for a matching user and place" do
      let
        cuePreferences =
          { ownerUsername: Just "owner"
          , preferences:
              [ SharedPresenceCuePreference
                  { sharedUsername: "alice"
                  , placeId: "Paris"
                  , colorToken: CueGreen
                  }
              ]
          }
        layout =
          { username: "alice"
          , isSelf: false
          , segmentIndex: 2
          , laneIndex: 0
          , laneCount: 2
          , startMin: 540
          , duration: 180
          , state: PresenceAtPlace "Paris"
          }
      resolveSharedPresenceToneClass cuePreferences layout
        `shouldEqual` "calendar-presence-rail__segment--color-green"

    it "falls back to the lane tone when no cue preference exists" do
      let
        cuePreferences =
          { ownerUsername: Just "owner"
          , preferences: []
          }
        layout =
          { username: "alice"
          , isSelf: false
          , segmentIndex: 2
          , laneIndex: 3
          , laneCount: 4
          , startMin: 540
          , duration: 180
          , state: PresenceAtPlace "Paris"
          }
      resolveSharedPresenceToneClass cuePreferences layout
        `shouldEqual` "calendar-presence-rail__segment--tone-3"

    it "keeps transit cues on the default tone even when a place preference exists" do
      let
        cuePreferences =
          { ownerUsername: Just "owner"
          , preferences:
              [ SharedPresenceCuePreference
                  { sharedUsername: "alice"
                  , placeId: "Paris"
                  , colorToken: CueRose
                  }
              ]
          }
        layout =
          { username: "alice"
          , isSelf: false
          , segmentIndex: 1
          , laneIndex: 2
          , laneCount: 3
          , startMin: 480
          , duration: 60
          , state: PresenceInTransit { departurePlaceId: "Paris", arrivalPlaceId: "St Clair" }
          }
      resolveSharedPresenceToneClass cuePreferences layout
        `shouldEqual` "calendar-presence-rail__segment--tone-2"

    it "formats presence inspection text for unknown, place and transit states" do
      presenceInspectionStateText PresenceUnknown `shouldEqual` "Lieu inconnu"
      presenceInspectionStateText (PresenceAtPlace "Paris") `shouldEqual` "Lieu: Paris"
      presenceInspectionStateText (PresenceInTransit { departurePlaceId: "Paris", arrivalPlaceId: "St Clair" }) `shouldEqual` "Trajet: Paris → St Clair"

    it "formats presence inspection time text from layout geometry" do
      presenceInspectionTimeText
        { username: "alice"
        , segmentIndex: 1
        , laneIndex: 0
        , laneCount: 2
        , startMin: 480
        , duration: 60
        , state: PresenceInTransit { departurePlaceId: "Paris", arrivalPlaceId: "St Clair" }
        }
        `shouldEqual` "De 08:00 à 09:00"

    it "builds an aria label for an inspected presence segment" do
      let
        layout =
          { username: "alice"
          , isSelf: false
          , segmentIndex: 1
          , laneIndex: 0
          , laneCount: 2
          , startMin: 480
          , duration: 60
          , state: PresenceInTransit { departurePlaceId: "Paris", arrivalPlaceId: "St Clair" }
          }
      presenceInspectionAriaLabel "alice" layout
        `shouldEqual` "alice · Trajet: Paris → St Clair · De 08:00 à 09:00"

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

noSeedPeriodTrips :: Array PeriodTripGroup
noSeedPeriodTrips =
  [ PeriodTripGroup
      { username: "alice"
      , trips:
          [ PeriodTrip
              { windowStart: "2026-04-02T08:00"
              , windowEnd: "2026-04-02T09:00"
              , departurePlaceId: "Paris"
              , arrivalPlaceId: "Lyon"
              }
          ]
      }
  ]

completedSeedPeriodTrips :: Array PeriodTripGroup
completedSeedPeriodTrips =
  [ PeriodTripGroup
      { username: "bob"
      , trips:
          [ PeriodTrip
              { windowStart: "2026-04-01T22:00"
              , windowEnd: "2026-04-01T23:00"
              , departurePlaceId: "Le Mesnil"
              , arrivalPlaceId: "Paris"
              }
          , PeriodTrip
              { windowStart: "2026-04-02T12:00"
              , windowEnd: "2026-04-02T13:00"
              , departurePlaceId: "Paris"
              , arrivalPlaceId: "Lyon"
              }
          ]
      }
  ]

adjacentPresenceTrips :: Array PeriodTripGroup
adjacentPresenceTrips =
  [ PeriodTripGroup
      { username: "carol"
      , trips:
          [ PeriodTrip
              { windowStart: "2026-04-01T22:00"
              , windowEnd: "2026-04-01T23:00"
              , departurePlaceId: "Le Mesnil"
              , arrivalPlaceId: "Paris"
              }
          , PeriodTrip
              { windowStart: "2026-04-02T10:00"
              , windowEnd: "2026-04-02T11:00"
              , departurePlaceId: "Paris"
              , arrivalPlaceId: "Lyon"
              }
          ]
      }
  ]

multiUserPresence :: Array { username :: String, segments :: Array { start :: DateTime, end :: DateTime, state :: SharedPresenceState } }
multiUserPresence =
  [ { username: "alice"
    , segments:
        [ { start: unsafeDateTime "2026-04-02T00:00"
          , end: unsafeDateTime "2026-04-03T00:00"
          , state: PresenceAtPlace "Paris"
          }
        ]
    }
  , { username: "bob"
    , segments:
        [ { start: unsafeDateTime "2026-04-02T00:00"
          , end: unsafeDateTime "2026-04-03T00:00"
          , state: PresenceAtPlace "Lyon"
          }
        ]
    }
  , { username: "carol"
    , segments:
        [ { start: unsafeDateTime "2026-04-02T00:00"
          , end: unsafeDateTime "2026-04-03T00:00"
          , state: PresenceAtPlace "Nantes"
          }
        ]
    }
  , { username: "dave"
    , segments:
        [ { start: unsafeDateTime "2026-04-02T00:00"
          , end: unsafeDateTime "2026-04-03T00:00"
          , state: PresenceAtPlace "Lille"
          }
        ]
    }
  ]

takePresenceGroups :: Int -> Array { username :: String, segments :: Array { start :: DateTime, end :: DateTime, state :: SharedPresenceState } } -> Array { username :: String, segments :: Array { start :: DateTime, end :: DateTime, state :: SharedPresenceState } }
takePresenceGroups count =
  mapMaybeWithIndexLocal (\index item -> if index < count then Just item else Nothing)

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

summarizePresenceGroups :: Array { username :: String, segments :: Array { start :: DateTime, end :: DateTime, state :: SharedPresenceState } } -> Array { username :: String, segments :: Array { start :: String, end :: String, state :: String } }
summarizePresenceGroups =
  map \group ->
    { username: group.username
    , segments:
        map
          ( \segment ->
              { start: DateTime.formatLocalDateTime segment.start
              , end: DateTime.formatLocalDateTime segment.end
              , state: summarizePresenceState segment.state
              }
          )
          group.segments
    }

summarizePresenceState :: SharedPresenceState -> String
summarizePresenceState = case _ of
  PresenceUnknown -> "unknown"
  PresenceAtPlace placeId -> "at-place:" <> placeId
  PresenceInTransit { departurePlaceId, arrivalPlaceId } -> "in-transit:" <> departurePlaceId <> "->" <> arrivalPlaceId

summarizePresenceRailLayouts
  :: forall r
   . Array { username :: String, segmentIndex :: Int, laneIndex :: Int, laneCount :: Int, startMin :: Int, duration :: Int, state :: SharedPresenceState | r }
  -> Array { username :: String, segmentIndex :: Int, laneIndex :: Int, laneCount :: Int, startMin :: Int, duration :: Int, railClass :: String, toneClass :: String }
summarizePresenceRailLayouts =
  map \layout ->
    { username: layout.username
    , segmentIndex: layout.segmentIndex
    , laneIndex: layout.laneIndex
    , laneCount: layout.laneCount
    , startMin: layout.startMin
    , duration: layout.duration
    , railClass: sharedPresenceSegmentRailClass layout.state
    , toneClass: sharedPresenceLaneToneClass layout.laneIndex
    }

summarizeSharedPresenceRailView :: forall r s t u. { visibleGroups :: Array { username :: String | r }, hiddenGroups :: Array { username :: String | s }, visibleLayouts :: Array { laneCount :: Int | t }, overflow :: Maybe { hiddenCount :: Int | u } } -> { visibleUsernames :: Array String, hiddenUsernames :: Array String, visibleLaneCounts :: Array Int, overflowHiddenCount :: Maybe Int }
summarizeSharedPresenceRailView railView =
  { visibleUsernames: map _.username railView.visibleGroups
  , hiddenUsernames: map _.username railView.hiddenGroups
  , visibleLaneCounts: map _.laneCount railView.visibleLayouts
  , overflowHiddenCount: _.hiddenCount <$> railView.overflow
  }

mapMaybeWithIndexLocal :: forall a b. (Int -> a -> Maybe b) -> Array a -> Array b
mapMaybeWithIndexLocal fn items =
  Array.mapMaybe identity (Array.mapWithIndex fn items)
