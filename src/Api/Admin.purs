module Api.Admin
  ( PendingSignup(..)
  , PendingSignupApprovalPayload(..)
  , getPendingSignupsResponse
  , approvePendingSignupResponse
  , deletePendingSignupResponse
  ) where

import Prelude

import Affjax.ResponseFormat (json, string)
import Affjax.Web as Affjax
import Api.Common (JsonResponse, TextResponse, jsonBody)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)

newtype PendingSignup = PendingSignup
  { username :: String }

newtype PendingSignupApprovalPayload = PendingSignupApprovalPayload
  { username :: String }

derive instance pendingSignupGeneric :: Generic PendingSignup _
derive instance pendingSignupEq :: Eq PendingSignup

instance showPendingSignup :: Show PendingSignup where
  show = genericShow

getPendingSignupsResponse :: Aff JsonResponse
getPendingSignupsResponse = Affjax.get json "/api/v1/admin/pending-signups"

approvePendingSignupResponse :: PendingSignupApprovalPayload -> Aff TextResponse
approvePendingSignupResponse payload =
  Affjax.post string "/api/v1/admin/pending-signups/approve" (jsonBody payload)

deletePendingSignupResponse :: String -> Aff TextResponse
deletePendingSignupResponse username =
  Affjax.delete string ("/api/v1/admin/pending-signups/" <> username)

instance decodePendingSignup :: DecodeJson PendingSignup where
  decodeJson json = do
    obj <- decodeJson json
    username <- obj .: "username"
    pure (PendingSignup { username })

instance encodePendingSignup :: EncodeJson PendingSignup where
  encodeJson (PendingSignup { username }) =
    "username" := username ~> jsonEmptyObject

instance encodePendingSignupApprovalPayload :: EncodeJson PendingSignupApprovalPayload where
  encodeJson (PendingSignupApprovalPayload { username }) =
    "username" := username ~> jsonEmptyObject
