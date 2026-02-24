module Api.Agenda
  ( getItemsResponse
  , createItemResponse
  , updateItemResponse
  , validateItemResponse
  ) where

import Prelude

import Affjax.ResponseFormat (json)
import Affjax.Web (patch, post)
import Affjax.Web (get) as Affjax
import Affjax.RequestBody (RequestBody(..))
import Api.Common (JsonResponse, jsonBody, jsonBodyFromJson)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode ((:=), (~>))
import Data.Maybe (fromMaybe)
import Effect.Aff (Aff)
import Agenda.Model (CalendarItem)

getItemsResponse :: Aff JsonResponse
getItemsResponse = Affjax.get json "/api/v1/calendar-items"

createItemResponse :: CalendarItem -> Aff JsonResponse
createItemResponse item = post json "/api/v1/calendar-items" (jsonBody item)

updateItemResponse :: String -> CalendarItem -> Aff JsonResponse
updateItemResponse itemId item =
  patch json ("/api/v1/calendar-items/" <> itemId)
    (fromMaybe (Json jsonEmptyObject) (jsonBody item))

validateItemResponse :: String -> Int -> Aff JsonResponse
validateItemResponse itemId minutes =
  post json ("/api/v1/calendar-items/" <> itemId <> "/validate")
    (jsonBodyFromJson $ "duree_reelle_minutes" := minutes ~> jsonEmptyObject)
