module Api.Agenda
  ( getItemsResponse
  , createItemResponse
  , updateItemResponse
  , validateItemResponse
  ) where

import Prelude

import Affjax.ResponseFormat (json)
import Affjax.Web (post)
import Affjax.Web (get) as Affjax
import Api.Common (JsonResponse, jsonBody, jsonBodyFromJson)
import Api.AgendaContract (createPath, listPath, updatePath, validatePath)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode ((:=), (~>))
import Effect.Aff (Aff)
import Agenda.Model (CalendarItem)

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
