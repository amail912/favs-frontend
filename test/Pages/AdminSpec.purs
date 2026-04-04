module Test.Pages.AdminSpec (spec) where

import Prelude

import Api.Admin (ApprovedUser(..), PendingSignup(..))
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Pages.Admin
  ( ApprovedSectionState(..)
  , PendingActionKind(..)
  , PendingSectionState(..)
  , actionButtonsDisabled
  , applyApprovedLoadFailure
  , applyApprovedLoadSuccess
  , applyPendingLoadFailure
  , applyPendingLoadSuccess
  , beginApprovedDelete
  , beginPendingAction
  , closeApprovedDeleteConfirmation
  , finishApprovedDelete
  , finishPendingAction
  , isApprovedUserDeletable
  , openApprovedDeleteConfirmation
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = do
  describe "Admin pending approvals" do
    it "decodes pending signups from backend payload" do
      let pendingSignup = PendingSignup { username: "alice" }
      case decodeJson (encodeJson pendingSignup) of
        Right decoded -> decoded `shouldEqual` pendingSignup
        Left err -> fail $ "Decoding encoded pending signup failed: " <> show err

    it "stores the loaded pending list and clears transient action state" do
      let
        initialState =
          baseState
            { pendingSection = PendingLoading
            , pendingAction = Just { username: "alice", action: PendingApprove }
            , pendingFeedback = Just "old error"
            }
        loaded = applyPendingLoadSuccess [ PendingSignup { username: "alice" } ] initialState
      loaded.pendingSection `shouldEqual` PendingLoaded [ PendingSignup { username: "alice" } ]
      loaded.pendingAction `shouldEqual` Nothing
      loaded.pendingFeedback `shouldEqual` Nothing

    it "stores load failures as section errors and clears transient action state" do
      let
        initialState =
          baseState
            { pendingSection = PendingLoading
            , pendingAction = Just { username: "alice", action: PendingDelete }
            , pendingFeedback = Just "old error"
            }
        failed = applyPendingLoadFailure "boom" initialState
      failed.pendingSection `shouldEqual` PendingLoadError "boom"
      failed.pendingAction `shouldEqual` Nothing
      failed.pendingFeedback `shouldEqual` Nothing

    it "tracks a single pending action and disables action buttons while it runs" do
      let
        initialState =
          baseState
            { pendingSection = PendingLoaded [ PendingSignup { username: "alice" } ]
            , pendingAction = Nothing
            , pendingFeedback = Nothing
            }
        started = beginPendingAction "alice" PendingApprove initialState
      started.pendingAction `shouldEqual` Just { username: "alice", action: PendingApprove }
      actionButtonsDisabled started `shouldEqual` true

    it "restores action availability after finishing a row action" do
      let
        initialState =
          baseState
            { pendingSection = PendingLoaded [ PendingSignup { username: "alice" } ]
            , pendingAction = Just { username: "alice", action: PendingDelete }
            , pendingFeedback = Nothing
            }
        finished = finishPendingAction (Just "failure") initialState
      finished.pendingAction `shouldEqual` Nothing
      finished.pendingFeedback `shouldEqual` Just "failure"
      actionButtonsDisabled finished `shouldEqual` false

  describe "Admin approved users" do
    it "decodes approved users from backend payload" do
      let approvedUser = ApprovedUser { username: "alice", roles: [ "member" ], approved: true }
      case decodeJson (encodeJson approvedUser) of
        Right decoded -> decoded `shouldEqual` approvedUser
        Left err -> fail $ "Decoding encoded approved user failed: " <> show err

    it "stores the loaded approved users and clears confirmation and feedback state" do
      let
        initialState =
          baseState
            { approvedSection = ApprovedLoading
            , approvedDeleteTarget = Just "alice"
            , approvedDeleteInFlight = Just "alice"
            , approvedFeedback = Just "old error"
            }
        loaded = applyApprovedLoadSuccess [ ApprovedUser { username: "alice", roles: [ "member" ], approved: true } ] initialState
      loaded.approvedSection `shouldEqual` ApprovedLoaded [ ApprovedUser { username: "alice", roles: [ "member" ], approved: true } ]
      loaded.approvedDeleteTarget `shouldEqual` Nothing
      loaded.approvedDeleteInFlight `shouldEqual` Nothing
      loaded.approvedFeedback `shouldEqual` Nothing

    it "stores approved load failures and clears transient delete state" do
      let
        initialState =
          baseState
            { approvedSection = ApprovedLoading
            , approvedDeleteTarget = Just "alice"
            , approvedDeleteInFlight = Just "alice"
            , approvedFeedback = Just "old error"
            }
        failed = applyApprovedLoadFailure "boom" initialState
      failed.approvedSection `shouldEqual` ApprovedLoadError "boom"
      failed.approvedDeleteTarget `shouldEqual` Nothing
      failed.approvedDeleteInFlight `shouldEqual` Nothing
      failed.approvedFeedback `shouldEqual` Nothing

    it "opens and closes delete confirmation for the targeted username" do
      let
        opened = openApprovedDeleteConfirmation "alice" baseState
        closed = closeApprovedDeleteConfirmation opened
      opened.approvedDeleteTarget `shouldEqual` Just "alice"
      closed.approvedDeleteTarget `shouldEqual` Nothing

    it "tracks a delete in flight and restores feedback after completion" do
      let
        started = beginApprovedDelete "alice" (baseState { approvedDeleteTarget = Just "alice" })
        finished = finishApprovedDelete (Just "conflict") started
      started.approvedDeleteInFlight `shouldEqual` Just "alice"
      started.approvedDeleteTarget `shouldEqual` Nothing
      finished.approvedDeleteInFlight `shouldEqual` Nothing
      finished.approvedFeedback `shouldEqual` Just "conflict"

    it "marks the current authenticated admin account as non deletable" do
      isApprovedUserDeletable "alice" (ApprovedUser { username: "alice", roles: [ "admin" ], approved: true }) `shouldEqual` false
      isApprovedUserDeletable "alice" (ApprovedUser { username: "bob", roles: [ "member" ], approved: true }) `shouldEqual` true

baseState
  :: { currentUsername :: String
     , pendingSection :: PendingSectionState
     , pendingAction :: Maybe { username :: String, action :: PendingActionKind }
     , pendingFeedback :: Maybe String
     , approvedSection :: ApprovedSectionState
     , approvedDeleteTarget :: Maybe String
     , approvedDeleteInFlight :: Maybe String
     , approvedFeedback :: Maybe String
     }
baseState =
  { currentUsername: "admin"
  , pendingSection: PendingLoading
  , pendingAction: Nothing
  , pendingFeedback: Nothing
  , approvedSection: ApprovedLoading
  , approvedDeleteTarget: Nothing
  , approvedDeleteInFlight: Nothing
  , approvedFeedback: Nothing
  }
