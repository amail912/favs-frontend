module Api.Calendar
  ( Method(..)
  , updateMethod
  , updatePath
  , getItemsResponse
  , createItemResponse
  , updateItemResponse
  , validateItemResponse
  ) where

import Prelude

import Affjax.ResponseFormat (json)
import Affjax.Web (get) as Affjax
import Affjax.Web (post)
import Api.Common (JsonResponse, jsonBody, jsonBodyFromJson)
import Calendar.Model (CalendarItem)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode ((:=), (~>))
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

validateItemResponse :: String -> Int -> Aff JsonResponse
validateItemResponse itemId minutes =
  post json (validatePath itemId)
    (jsonBodyFromJson $ "duree_reelle_minutes" := minutes ~> jsonEmptyObject)
