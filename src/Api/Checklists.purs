module Api.Checklists
  ( getChecklistsResponse
  , writeChecklistResponse
  , deleteChecklistResponse
  ) where

import Prelude

import Affjax.ResponseFormat (json)
import Affjax.Web (delete, post, put)
import Affjax.Web (get) as Affjax
import Api.Common (JsonResponse, jsonBody)
import Domain.Checklists (Checklist(..), StorageId)
import Effect.Aff (Aff)

getChecklistsResponse :: Aff JsonResponse
getChecklistsResponse = Affjax.get json "/api/checklist"

writeChecklistResponse :: Checklist -> Aff JsonResponse
writeChecklistResponse checklist = writeFunc json "/api/checklist" (jsonBody checklist)
  where
  writeFunc = if isCreate checklist then post else put

  isCreate (NewChecklist _) = true
  isCreate (ServerChecklist _) = false

deleteChecklistResponse :: StorageId -> Aff JsonResponse
deleteChecklistResponse { id } = delete json ("/api/checklist/" <> id)
