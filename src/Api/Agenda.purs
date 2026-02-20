module Api.Agenda
  ( getItemsResponse
  , createItemResponse
  , validateItemResponse
  ) where

import Prelude

import Affjax.ResponseFormat (json)
import Affjax.Web (post)
import Affjax.Web (get) as Affjax
import Api.Common (JsonResponse, jsonBody, jsonBodyFromJson)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode ((:=), (~>))
import Effect.Aff (Aff)
import Agenda.Model (CalendarItem)

getItemsResponse :: Aff JsonResponse
getItemsResponse = Affjax.get json "/api/v1/calendar-items"

createItemResponse :: CalendarItem -> Aff JsonResponse
createItemResponse item = post json "/api/v1/calendar-items" (jsonBody item)

validateItemResponse :: String -> Int -> Aff JsonResponse
validateItemResponse itemId minutes =
  post json ("/api/v1/calendar-items/" <> itemId <> "/validate")
    (jsonBodyFromJson $ "duree_reelle_minutes" := minutes ~> jsonEmptyObject)
