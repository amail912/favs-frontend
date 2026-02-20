module Api.Common
  ( JsonResponse
  , jsonBody
  , jsonBodyFromJson
  ) where

import Prelude

import Affjax (Error, Response)
import Affjax.RequestBody (RequestBody(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe(..))

type JsonResponse = Either Error (Response Json)

jsonBody :: forall a. EncodeJson a => a -> Maybe RequestBody
jsonBody = Just <<< Json <<< encodeJson

jsonBodyFromJson :: Json -> Maybe RequestBody
jsonBodyFromJson = Just <<< Json
