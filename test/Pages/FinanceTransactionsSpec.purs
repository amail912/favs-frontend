module Test.Pages.FinanceTransactionsSpec (spec) where

import Prelude

import Api.FinanceContract
  ( FinanceAccount(..)
  , FinanceTransaction(..)
  , FinanceTransactionAdjustment(..)
  , FinanceTransactionCategory(..)
  , FinanceTransactionDirection(..)
  , FinanceTransactionNote(..)
  , FinanceTransactionSplitRow(..)
  , FinanceTransferLink(..)
  )
import Data.Maybe (Maybe(..))
import Data.Array (head)
import Pages.FinanceTransactions
  ( LedgerBodyState(..)
  , LedgerRemoteState(..)
  , applyLedgerLoadFailure
  , applyLedgerLoadSuccess
  , beginLedgerLoad
  , buildLedgerRows
  , deriveLedgerBodyState
  , resolveAccountLabel
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec =
  describe "Finance transactions workspace" do
    it "transitions from loading to loaded rows" do
      let
        loadingState =
          { hasActiveContext: false
          , remoteState: LedgerLoading
          }
        loadedState = applyLedgerLoadSuccess [ sampleTransaction ] [ sampleAccount ] loadingState
      case deriveLedgerBodyState loadedState of
        LedgerBodyRows rows ->
          rows `shouldEqual` buildLedgerRows [ sampleAccount ] [ sampleTransaction ]
        _ ->
          fail "Expected ledger rows state"

    it "transitions from loading to empty when no transactions and no context" do
      let
        loadingState =
          { hasActiveContext: false
          , remoteState: LedgerLoading
          }
        loadedState = applyLedgerLoadSuccess [] [] loadingState
      case deriveLedgerBodyState loadedState of
        LedgerBodyEmpty -> pure unit
        _ -> fail "Expected empty state"

    it "transitions from loading to no-results when context is active and no rows match" do
      let
        loadingState =
          { hasActiveContext: true
          , remoteState: LedgerLoading
          }
        loadedState = applyLedgerLoadSuccess [] [] loadingState
      case deriveLedgerBodyState loadedState of
        LedgerBodyNoResults -> pure unit
        _ -> fail "Expected no-results state"

    it "transitions from loading to error and can retry" do
      let
        loadingState =
          { hasActiveContext: false
          , remoteState: LedgerLoading
          }
        erroredState = applyLedgerLoadFailure "boom" loadingState
        retriedState = beginLedgerLoad erroredState
      case deriveLedgerBodyState erroredState of
        LedgerBodyError "boom" -> pure unit
        _ -> fail "Expected error state"
      case deriveLedgerBodyState retriedState of
        LedgerBodyLoading -> pure unit
        _ -> fail "Expected loading state after retry"

    it "resolves account labels and falls back to account id" do
      resolveAccountLabel [ sampleAccount ] "acc-1" `shouldEqual` "Primary account"
      resolveAccountLabel [ sampleAccount ] "missing-account" `shouldEqual` "missing-account"

    it "builds row facts from backend-supported fields only" do
      case head (buildLedgerRows [ sampleAccount ] [ sampleTransaction ]) of
        Just row -> do
          row.accountLabel `shouldEqual` "Primary account"
          row.directionLabel `shouldEqual` "Sent"
          row.categoryLabel `shouldEqual` "cat-1"
          row.hasSplit `shouldEqual` true
          row.hasTransfer `shouldEqual` true
          row.hasNote `shouldEqual` true
          row.hasAdjustment `shouldEqual` true
        _ -> fail "Expected at least one ledger row"

sampleAccount :: FinanceAccount
sampleAccount =
  FinanceAccount
    { id: "acc-1"
    , name: "Primary account"
    , status: "active"
    }

sampleTransaction :: FinanceTransaction
sampleTransaction =
  FinanceTransaction
    { id: "tx-1"
    , direction: TransactionSent
    , accountId: "acc-1"
    , amount: 120.0
    , occurredAt: "2026-05-10T09:00:00Z"
    , recordedAt: "2026-05-10T09:00:01Z"
    , transfer: Just (FinanceTransferLink { linkedTransactionId: "tx-2", linkType: "transfer" })
    , category: Just (FinanceTransactionCategory { id: "cat-1" })
    , splits: [ FinanceTransactionSplitRow { amount: 70.0, category: "cat-1" } ]
    , notes: [ FinanceTransactionNote { id: "note-1", text: "memo" } ]
    , adjustment: Just (FinanceTransactionAdjustment { kind: "balance-snapshot" })
    }
