module Api.Admin
  ( PendingSignup(..)
  , PendingSignupApprovalPayload(..)
  , ApprovedUser(..)
  , getPendingSignupsResponse
  , approvePendingSignupResponse
  , deletePendingSignupResponse
  , getApprovedUsersResponse
  , deleteApprovedUserResponse
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

newtype ApprovedUser = ApprovedUser
  { username :: String
  , roles :: Array String
  , approved :: Boolean
  }

derive instance pendingSignupGeneric :: Generic PendingSignup _
derive instance approvedUserGeneric :: Generic ApprovedUser _
derive instance pendingSignupEq :: Eq PendingSignup
derive instance approvedUserEq :: Eq ApprovedUser

instance showPendingSignup :: Show PendingSignup where
  show = genericShow

instance showApprovedUser :: Show ApprovedUser where
  show = genericShow

getPendingSignupsResponse :: Aff JsonResponse
getPendingSignupsResponse = Affjax.get json "/api/v1/admin/pending-signups"

approvePendingSignupResponse :: PendingSignupApprovalPayload -> Aff TextResponse
approvePendingSignupResponse payload =
  Affjax.post string "/api/v1/admin/pending-signups/approve" (jsonBody payload)

deletePendingSignupResponse :: String -> Aff TextResponse
deletePendingSignupResponse username =
  Affjax.delete string ("/api/v1/admin/pending-signups/" <> username)

getApprovedUsersResponse :: Aff JsonResponse
getApprovedUsersResponse = Affjax.get json "/api/v1/admin/users"

deleteApprovedUserResponse :: String -> Aff TextResponse
deleteApprovedUserResponse username =
  Affjax.delete string ("/api/v1/admin/users/" <> username)

instance decodePendingSignup :: DecodeJson PendingSignup where
  decodeJson json = do
    obj <- decodeJson json
    username <- obj .: "username"
    pure (PendingSignup { username })

instance decodeApprovedUser :: DecodeJson ApprovedUser where
  decodeJson json = do
    obj <- decodeJson json
    username <- obj .: "username"
    roles <- obj .: "roles"
    approved <- obj .: "approved"
    pure (ApprovedUser { username, roles, approved })

instance encodePendingSignup :: EncodeJson PendingSignup where
  encodeJson (PendingSignup { username }) =
    "username" := username ~> jsonEmptyObject

instance encodePendingSignupApprovalPayload :: EncodeJson PendingSignupApprovalPayload where
  encodeJson (PendingSignupApprovalPayload { username }) =
    "username" := username ~> jsonEmptyObject

instance encodeApprovedUser :: EncodeJson ApprovedUser where
  encodeJson (ApprovedUser { username, roles, approved }) =
    "username" := username
      ~> "roles" := roles
      ~> "approved" := approved
      ~> jsonEmptyObject
