module Test.Pages.FinanceReportsSpec (spec) where

import Prelude

import Api.Finance (encodeReportQuery)
import Api.FinanceContract (FinanceAggregateReport(..))
import Data.Maybe (Maybe(..))
import Pages.FinanceReports
  ( ReportBodyState(..)
  , ReportRemoteState(..)
  , applyReportLoadFailure
  , applyReportLoadSuccess
  , beginReportLoad
  , buildAggregateQuery
  , deriveReportBodyState
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

type TestState =
  { draftRange :: { from :: String, to :: String }
  , appliedRange :: Maybe { from :: String, to :: String }
  , remoteState :: ReportRemoteState
  , validationError :: Maybe String
  }

spec :: Spec Unit
spec =
  describe "Finance reports workspace" do
    it "transitions through loading, summary, empty, and error states" do
      let
        baseState =
          { draftRange: { from: "2026-05-01T00:00:00Z", to: "2026-05-31T23:59:59Z" }
          , appliedRange: Nothing
          , remoteState: ReportIdle
          , validationError: Nothing
          } :: TestState
        loadingState = beginReportLoad baseState
        summaryState = applyReportLoadSuccess (FinanceAggregateReport { total: 99.5, count: 2, transactionIds: [ "tx-1", "tx-2" ] }) loadingState
        emptyState = applyReportLoadSuccess (FinanceAggregateReport { total: 0.0, count: 0, transactionIds: [] }) loadingState
        errorState = applyReportLoadFailure "boom" loadingState
      case deriveReportBodyState baseState of
        ReportBodyIdle -> pure unit
        _ -> fail "Expected idle report state"
      case deriveReportBodyState loadingState of
        ReportBodyLoading -> pure unit
        _ -> fail "Expected loading report state"
      case deriveReportBodyState summaryState of
        ReportBodySummary summary ->
          summary `shouldEqual` { total: 99.5, count: 2 }
        _ -> fail "Expected summary report state"
      case deriveReportBodyState emptyState of
        ReportBodyEmpty -> pure unit
        _ -> fail "Expected empty report state"
      case deriveReportBodyState errorState of
        ReportBodyError message ->
          message `shouldEqual` "boom"
        _ -> fail "Expected error report state"

    it "builds report queries with date-only active controls" do
      let
        queryString =
          encodeReportQuery
            ( buildAggregateQuery
                { from: "2026-05-01T00:00:00Z"
                , to: "2026-05-31T23:59:59Z"
                }
            )
      queryString
        `shouldEqual`
          "?from=2026-05-01T00:00:00Z&to=2026-05-31T23:59:59Z"
