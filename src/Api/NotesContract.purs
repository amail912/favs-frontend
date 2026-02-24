module Api.NotesContract
  ( basePath
  , itemPath
  ) where

import Prelude

basePath :: String
basePath = "/api/note"

itemPath :: String -> String
itemPath noteId = "/api/note/" <> noteId
