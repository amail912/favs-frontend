module Test.Pages.AdminSpec (spec) where

import Prelude

import Api.Admin (PendingSignup(..))
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Pages.Admin
  ( PendingActionKind(..)
  , PendingSectionState(..)
  , actionButtonsDisabled
  , applyPendingLoadFailure
  , applyPendingLoadSuccess
  , beginPendingAction
  , finishPendingAction
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec =
  describe "Admin pending approvals" do
    it "decodes pending signups from backend payload" do
      let pendingSignup = PendingSignup { username: "alice" }
      case decodeJson (encodeJson pendingSignup) of
        Right decoded -> decoded `shouldEqual` pendingSignup
        Left err -> fail $ "Decoding encoded pending signup failed: " <> show err

    it "stores the loaded pending list and clears transient action state" do
      let
        initialState =
          { pendingSection: PendingLoading
          , pendingAction: Just { username: "alice", action: PendingApprove }
          , pendingFeedback: Just "old error"
          }
        loaded = applyPendingLoadSuccess [ PendingSignup { username: "alice" } ] initialState
      loaded.pendingSection `shouldEqual` PendingLoaded [ PendingSignup { username: "alice" } ]
      loaded.pendingAction `shouldEqual` Nothing
      loaded.pendingFeedback `shouldEqual` Nothing

    it "stores load failures as section errors and clears transient action state" do
      let
        initialState =
          { pendingSection: PendingLoading
          , pendingAction: Just { username: "alice", action: PendingDelete }
          , pendingFeedback: Just "old error"
          }
        failed = applyPendingLoadFailure "boom" initialState
      failed.pendingSection `shouldEqual` PendingLoadError "boom"
      failed.pendingAction `shouldEqual` Nothing
      failed.pendingFeedback `shouldEqual` Nothing

    it "tracks a single pending action and disables action buttons while it runs" do
      let
        initialState =
          { pendingSection: PendingLoaded [ PendingSignup { username: "alice" } ]
          , pendingAction: Nothing
          , pendingFeedback: Nothing
          }
        started = beginPendingAction "alice" PendingApprove initialState
      started.pendingAction `shouldEqual` Just { username: "alice", action: PendingApprove }
      actionButtonsDisabled started `shouldEqual` true

    it "restores action availability after finishing a row action" do
      let
        initialState =
          { pendingSection: PendingLoaded [ PendingSignup { username: "alice" } ]
          , pendingAction: Just { username: "alice", action: PendingDelete }
          , pendingFeedback: Nothing
          }
        finished = finishPendingAction (Just "failure") initialState
      finished.pendingAction `shouldEqual` Nothing
      finished.pendingFeedback `shouldEqual` Just "failure"
      actionButtonsDisabled finished `shouldEqual` false
