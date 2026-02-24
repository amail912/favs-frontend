module Api.Notes
  ( getNotesResponse
  , writeNoteResponse
  , deleteNoteResponse
  ) where

import Affjax.ResponseFormat (json)
import Affjax.Web (delete, post, put)
import Affjax.Web (get) as Affjax
import Api.Common (JsonResponse, jsonBody)
import Api.NotesContract (basePath, itemPath)
import Effect.Aff (Aff)
import Domain.Notes (Note(..), StorageId)

getNotesResponse :: Aff JsonResponse
getNotesResponse = Affjax.get json basePath

writeNoteResponse :: Note -> Aff JsonResponse
writeNoteResponse note = writeFunc json basePath (jsonBody note)
  where
  writeFunc = if isCreate note then post else put

  isCreate (NewNote _) = true
  isCreate (ServerNote _) = false

deleteNoteResponse :: StorageId -> Aff JsonResponse
deleteNoteResponse { id } = delete json (itemPath id)
