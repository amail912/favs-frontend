module Api.ChecklistsContract
  ( basePath
  , itemPath
  ) where

import Prelude

basePath :: String
basePath = "/api/checklist"

itemPath :: String -> String
itemPath checklistId = "/api/checklist/" <> checklistId
