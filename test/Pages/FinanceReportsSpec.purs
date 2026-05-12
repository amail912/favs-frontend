module Test.Pages.FinanceReportsSpec (spec) where

import Prelude

import Api.Finance (encodeAnalyticsQuery)
import Api.FinanceContract
  ( FinanceAccount
  , FinanceCategory
  , FinanceAnalyticsAccountBalanceRow(..)
  , FinanceAnalyticsCashflowSeriesRow(..)
  , FinanceAnalyticsCategoryBreakdownRow(..)
  , FinanceAnalyticsReport(..)
  , FinanceAnalyticsSummary(..)
  , FinanceReportDirection
  )
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
  { filters ::
      { from :: String
      , to :: String
      , direction :: Maybe FinanceReportDirection
      , accountId :: Maybe String
      , categoryIn :: Array String
      , categoryNotIn :: Array String
      , amountMinInput :: String
      , amountMaxInput :: String
      , search :: String
      }
  , selectedCategoryIn :: String
  , selectedCategoryNotIn :: String
  , appliedRange :: Maybe { from :: String, to :: String }
  , remoteState :: ReportRemoteState
  , validationError :: Maybe String
  , accounts :: Array FinanceAccount
  , categories :: Array FinanceCategory
  }

sampleReport :: FinanceAnalyticsReport
sampleReport =
  FinanceAnalyticsReport
    { summary: FinanceAnalyticsSummary { total: 99.5, count: 2 }
    , categoryBreakdown:
        [ FinanceAnalyticsCategoryBreakdownRow
            { categoryId: "cat-1"
            , total: 99.5
            , count: 2
            }
        ]
    , cashflowSeries:
        [ FinanceAnalyticsCashflowSeriesRow
            { bucketStart: "2026-05-01T00:00:00Z"
            , bucketEnd: "2026-05-02T00:00:00Z"
            , total: 99.5
            , count: 2
            }
        ]
    , accountBalances:
        [ FinanceAnalyticsAccountBalanceRow
            { accountId: "acc-1"
            , total: 99.5
            }
        ]
    }

emptyReport :: FinanceAnalyticsReport
emptyReport =
  FinanceAnalyticsReport
    { summary: FinanceAnalyticsSummary { total: 0.0, count: 0 }
    , categoryBreakdown: []
    , cashflowSeries: []
    , accountBalances: []
    }

spec :: Spec Unit
spec =
  describe "Finance reports workspace" do
    it "transitions through loading, summary, empty, and error states" do
      let
        baseState =
          { filters:
              { from: "2026-05-01T00:00:00Z"
              , to: "2026-05-31T23:59:59Z"
              , direction: Nothing
              , accountId: Nothing
              , categoryIn: []
              , categoryNotIn: []
              , amountMinInput: ""
              , amountMaxInput: ""
              , search: ""
              }
          , selectedCategoryIn: ""
          , selectedCategoryNotIn: ""
          , appliedRange: Nothing
          , remoteState: ReportIdle
          , validationError: Nothing
          , accounts: []
          , categories: []
          } :: TestState
        loadingState = beginReportLoad baseState
        summaryState = applyReportLoadSuccess sampleReport loadingState
        emptyState = applyReportLoadSuccess emptyReport loadingState
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

    it "builds analytics query with date-only active controls" do
      let
        queryString =
          encodeAnalyticsQuery
            ( buildAggregateQuery
                { from: "2026-05-01T00:00:00Z"
                , to: "2026-05-31T23:59:59Z"
                }
            )
      queryString
        `shouldEqual`
          "?from=2026-05-01T00:00:00Z&to=2026-05-31T23:59:59Z"
