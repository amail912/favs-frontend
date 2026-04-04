module Api.Auth
  ( AuthenticatedProfile(..)
  , AuthResponse
  , getAuthProfileResponse
  , isAdminProfile
  ) where

import Prelude

import Affjax (Response)
import Affjax.ResponseFormat (json)
import Affjax.Web as Affjax
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Foldable (elem)
import Data.Either (Either)
import Effect.Aff (Aff)

newtype AuthenticatedProfile = AuthenticatedProfile
  { username :: String
  , roles :: Array String
  , approved :: Boolean
  }

type AuthResponse = Either Affjax.Error (Response Json)

getAuthProfileResponse :: Aff AuthResponse
getAuthProfileResponse = Affjax.get json "/api/auth/profile"

isAdminProfile :: AuthenticatedProfile -> Boolean
isAdminProfile (AuthenticatedProfile { roles }) = elem "admin" roles

instance decodeAuthenticatedProfile :: DecodeJson AuthenticatedProfile where
  decodeJson json = do
    obj <- decodeJson json
    username <- obj .: "username"
    roles <- obj .: "roles"
    approved <- obj .: "approved"
    pure (AuthenticatedProfile { username, roles, approved })
