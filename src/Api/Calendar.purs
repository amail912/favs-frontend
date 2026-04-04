module Api.Calendar
  ( Method(..)
  , updateMethod
  , updatePath
  , deletePath
  , getItemsResponse
  , getTripPlacesResponse
  , getSharedUsersResponse
  , getSubscribedUsersResponse
  , getPeriodTripsResponse
  , addSharedUserResponse
  , addSubscribedUserResponse
  , deleteSharedUserResponse
  , deleteSubscribedUserResponse
  , createItemResponse
  , updateItemResponse
  , deleteItemResponse
  , validateItemResponse
  , ValidateItemPayload(..)
  , TripPlace(..)
  , TripSharingUser(..)
  , PeriodTrip(..)
  , PeriodTripGroup(..)
  ) where

import Prelude

import Affjax.ResponseFormat (json, string)
import Affjax.Web as Affjax
import Api.Common (JsonResponse, TextResponse, jsonBody)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Effect.Aff (Aff)

data Method = GET | POST | PATCH | DELETE

derive instance methodEq :: Eq Method

instance methodShow :: Show Method where
  show GET = "GET"
  show POST = "POST"
  show PATCH = "PATCH"
  show DELETE = "DELETE"

listPath :: String
listPath = "/api/v1/calendar-items"

tripPlacesPath :: String
tripPlacesPath = "/api/v1/trip-places"

shareUsersPath :: String
shareUsersPath = "/api/v1/trip-sharing/shares"

subscribedUsersPath :: String
subscribedUsersPath = "/api/v1/trip-sharing/subscriptions"

periodTripsPath :: String
periodTripsPath = "/api/v1/trip-sharing/period-trips"

shareUserDeletePath :: String -> String
shareUserDeletePath username = shareUsersPath <> "/" <> username

subscribedUserDeletePath :: String -> String
subscribedUserDeletePath username = subscribedUsersPath <> "/" <> username

createPath :: String
createPath = "/api/v1/calendar-items"

updatePath :: String -> String
updatePath _ = "/api/v1/calendar-items"

deletePath :: String -> String
deletePath itemId = "/api/v1/calendar-items/" <> itemId

updateMethod :: Method
updateMethod = POST

validatePath :: String -> String
validatePath itemId = "/api/v1/calendar-items/" <> itemId <> "/validate"

getItemsResponse :: Aff JsonResponse
getItemsResponse = Affjax.get json listPath

getTripPlacesResponse :: Aff JsonResponse
getTripPlacesResponse = Affjax.get json tripPlacesPath

getSharedUsersResponse :: Aff JsonResponse
getSharedUsersResponse = Affjax.get json shareUsersPath

getSubscribedUsersResponse :: Aff JsonResponse
getSubscribedUsersResponse = Affjax.get json subscribedUsersPath

getPeriodTripsResponse :: String -> String -> Aff JsonResponse
getPeriodTripsResponse start end =
  Affjax.get json (periodTripsPath <> "?start=" <> start <> "&end=" <> end)

addSharedUserResponse :: TripSharingUser -> Aff TextResponse
addSharedUserResponse user = Affjax.post string shareUsersPath (jsonBody user)

addSubscribedUserResponse :: TripSharingUser -> Aff TextResponse
addSubscribedUserResponse user = Affjax.post string subscribedUsersPath (jsonBody user)

deleteSharedUserResponse :: String -> Aff TextResponse
deleteSharedUserResponse username = Affjax.delete string (shareUserDeletePath username)

deleteSubscribedUserResponse :: String -> Aff TextResponse
deleteSubscribedUserResponse username = Affjax.delete string (subscribedUserDeletePath username)

createItemResponse :: forall payload. EncodeJson payload => payload -> Aff JsonResponse
createItemResponse item = Affjax.post json createPath (jsonBody item)

updateItemResponse :: forall payload. EncodeJson payload => String -> payload -> Aff JsonResponse
updateItemResponse itemId item =
  Affjax.post json (updatePath itemId)
    (jsonBody item)

deleteItemResponse :: String -> Aff TextResponse
deleteItemResponse itemId = Affjax.delete string (deletePath itemId)

validateItemResponse :: String -> ValidateItemPayload -> Aff JsonResponse
validateItemResponse itemId payload =
  Affjax.post json (validatePath itemId)
    (jsonBody payload)

newtype ValidateItemPayload = ValidateItemPayload
  { duree_reelle_minutes :: Int }

newtype TripPlace = TripPlace
  { name :: String }

newtype TripSharingUser = TripSharingUser
  { username :: String }

newtype PeriodTrip = PeriodTrip
  { windowStart :: String
  , windowEnd :: String
  , departurePlaceId :: String
  , arrivalPlaceId :: String
  }

newtype PeriodTripGroup = PeriodTripGroup
  { username :: String
  , trips :: Array PeriodTrip
  }

instance encodeValidateItemPayload :: EncodeJson ValidateItemPayload where
  encodeJson (ValidateItemPayload payload) =
    "duree_reelle_minutes" := payload.duree_reelle_minutes ~> jsonEmptyObject

instance decodeTripPlace :: DecodeJson TripPlace where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .: "name"
    pure (TripPlace { name })

instance decodeTripSharingUser :: DecodeJson TripSharingUser where
  decodeJson json = do
    obj <- decodeJson json
    username <- obj .: "username"
    pure (TripSharingUser { username })

instance decodePeriodTrip :: DecodeJson PeriodTrip where
  decodeJson json = do
    obj <- decodeJson json
    windowStart <- obj .: "windowStart"
    windowEnd <- obj .: "windowEnd"
    departurePlaceId <- obj .: "departurePlaceId"
    arrivalPlaceId <- obj .: "arrivalPlaceId"
    pure
      ( PeriodTrip
          { windowStart
          , windowEnd
          , departurePlaceId
          , arrivalPlaceId
          }
      )

instance decodePeriodTripGroup :: DecodeJson PeriodTripGroup where
  decodeJson json = do
    obj <- decodeJson json
    username <- obj .: "username"
    trips <- obj .: "trips"
    pure (PeriodTripGroup { username, trips })

instance encodeTripSharingUser :: EncodeJson TripSharingUser where
  encodeJson (TripSharingUser payload) =
    "username" := payload.username ~> jsonEmptyObject
