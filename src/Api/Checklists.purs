module Api.Checklists
  ( getChecklistsResponse
  , writeChecklistResponse
  , deleteChecklistResponse
  ) where

import Affjax.ResponseFormat (json)
import Affjax.Web (delete, post, put)
import Affjax.Web (get) as Affjax
import Api.Common (JsonResponse, jsonBody)
import Api.ChecklistsContract (basePath, itemPath)
import Domain.Checklists (Checklist(..), StorageId)
import Effect.Aff (Aff)

getChecklistsResponse :: Aff JsonResponse
getChecklistsResponse = Affjax.get json basePath

writeChecklistResponse :: Checklist -> Aff JsonResponse
writeChecklistResponse checklist = writeFunc json basePath (jsonBody checklist)
  where
  writeFunc = if isCreate checklist then post else put

  isCreate (NewChecklist _) = true
  isCreate (ServerChecklist _) = false

deleteChecklistResponse :: StorageId -> Aff JsonResponse
deleteChecklistResponse { id } = delete json (itemPath id)
