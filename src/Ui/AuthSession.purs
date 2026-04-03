module Ui.AuthSession
  ( authenticatedUsernameStorageKey
  , loadAuthenticatedUsername
  , storeAuthenticatedUsername
  , clearAuthenticatedUsername
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.Common as StringCommon
import Effect (Effect)
import Ui.LocalStorage as LocalStorage

authenticatedUsernameStorageKey :: String
authenticatedUsernameStorageKey = "favs.auth.username"

loadAuthenticatedUsername :: Effect (Maybe String)
loadAuthenticatedUsername = do
  raw <- LocalStorage.getItem authenticatedUsernameStorageKey
  let trimmed = StringCommon.trim raw
  pure $
    if trimmed == "" then
      Nothing
    else
      Just trimmed

storeAuthenticatedUsername :: String -> Effect Unit
storeAuthenticatedUsername username =
  LocalStorage.setItem authenticatedUsernameStorageKey (StringCommon.trim username)

clearAuthenticatedUsername :: Effect Unit
clearAuthenticatedUsername =
  LocalStorage.removeItem authenticatedUsernameStorageKey
