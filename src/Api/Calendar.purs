module Api.Calendar
  ( Method(..)
  , updateMethod
  , updatePath
  , getItemsResponse
  , createItemResponse
  , updateItemResponse
  , validateItemResponse
  , ValidateItemPayload(..)
  ) where

import Prelude

import Affjax.ResponseFormat (json)
import Affjax.Web (get) as Affjax
import Affjax.Web (post)
import Api.Common (JsonResponse, jsonBody)
import Calendar.Model (CalendarItem)
import Data.Argonaut.Core (jsonEmptyObject)
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

createPath :: String
createPath = "/api/v1/calendar-items"

updatePath :: String -> String
updatePath _ = "/api/v1/calendar-items"

updateMethod :: Method
updateMethod = POST

validatePath :: String -> String
validatePath itemId = "/api/v1/calendar-items/" <> itemId <> "/validate"

getItemsResponse :: Aff JsonResponse
getItemsResponse = Affjax.get json listPath

createItemResponse :: CalendarItem -> Aff JsonResponse
createItemResponse item = post json createPath (jsonBody item)

updateItemResponse :: String -> CalendarItem -> Aff JsonResponse
updateItemResponse itemId item =
  post json (updatePath itemId)
    (jsonBody item)

validateItemResponse :: String -> ValidateItemPayload -> Aff JsonResponse
validateItemResponse itemId payload =
  post json (validatePath itemId)
    (jsonBody payload)

newtype ValidateItemPayload = ValidateItemPayload
  { duree_reelle_minutes :: Int }

instance encodeValidateItemPayload :: EncodeJson ValidateItemPayload where
  encodeJson (ValidateItemPayload payload) =
    "duree_reelle_minutes" := payload.duree_reelle_minutes ~> jsonEmptyObject
