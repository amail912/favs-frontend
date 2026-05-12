module Test.Pages.AppSpec (spec) where

import Prelude

import Api.Auth (AuthenticatedProfile(..))
import Api.FinanceContract
  ( FinanceTransaction(..)
  , FinanceReportDirection(..)
  , FinanceTransactionDirection(..)
  , FinanceTransactionSplitRow
  , FinanceTransferLink(..)
  )
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Notifications.LateItems as LateItems
import Pages.App (AuthStatus(..), DefinedRoute(..), FinanceOverlay(..), Route(..), applyLateItemsLoadFailed, applyLateItemsLoaded, beginLateItemsRequest, connectedIdentityLabel, filterTransferCandidates, financeLocalPrimaryRoute, initialLateItemsState, isFinanceOverlayOpen, isFinanceRoute, parseRouteString, printRoute, resolveGuardedRoute, shouldRefreshLateItemsForRoute, shouldRenderFinanceOverlay, shouldShowFinanceCreateButton, visibleTabs)
import Pages.Calendar (ItemType(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Support.Builders (calendarContent, serverCalendarItem, unsafeDateTime)

spec :: Spec Unit
spec =
  describe "App routing and auth gating" do
    it "parses finance and admin routes" do
      parseRouteString "/finance" `shouldEqual` Right (Route defaultTransactionsRoute)
      parseRouteString "/finance/transactions" `shouldEqual` Right (Route defaultTransactionsRoute)
      parseRouteString "/finance/transactions?accountId=acc-1&from=2026-05-01T00:00:00Z&to=2026-05-31T23:59:59Z"
        `shouldEqual`
          Right
            ( Route
                ( FinanceTransactions
                    { accountId: Just "acc-1"
                    , from: Just "2026-05-01T00:00:00Z"
                    , to: Just "2026-05-31T23:59:59Z"
                    , direction: Nothing
                    , categoryIn: []
                    , categoryNotIn: []
                    , amountMin: Nothing
                    , amountMax: Nothing
                    , search: Nothing
                    }
                )
            )
      parseRouteString "/finance/reports" `shouldEqual` Right (Route FinanceReports)
      parseRouteString "/admin" `shouldEqual` Right (Route Admin)

    it "prints canonical finance routes" do
      printRoute (Route defaultTransactionsRoute) `shouldEqual` "/finance/transactions"
      printRoute
        ( Route
            ( FinanceTransactions
                { accountId: Just "acc-1"
                , from: Just "2026-05-01T00:00:00Z"
                , to: Just "2026-05-31T23:59:59Z"
                , direction: Just ReportSent
                , categoryIn: [ "cat-1", "cat-2" ]
                , categoryNotIn: [ "cat-9" ]
                , amountMin: Just 10
                , amountMax: Just 20
                , search: Just "coffee"
                }
            )
        )
        `shouldEqual`
          "/finance/transactions?accountId=acc-1&from=2026-05-01T00:00:00Z&to=2026-05-31T23:59:59Z&direction=sent&amountMin=10&amountMax=20&search=coffee&categoryIn=cat-1&categoryIn=cat-2&categoryNotIn=cat-9"
      printRoute (Route FinanceReports) `shouldEqual` "/finance/reports"

    it "parses calendar routes with and without a valid day query" do
      parseRouteString "/calendar" `shouldEqual` Right (Route (Calendar { day: Nothing, item: Nothing }))
      parseRouteString "/calendar?day=2026-04-02" `shouldEqual` Right (Route (Calendar { day: Just "2026-04-02", item: Nothing }))
      parseRouteString "/calendar?day=2026-04-02&item=task-1" `shouldEqual` Right (Route (Calendar { day: Just "2026-04-02", item: Just "task-1" }))
      parseRouteString "/calendar?day=invalid&item=task-1" `shouldEqual` Right (Route (Calendar { day: Nothing, item: Just "task-1" }))
      parseRouteString "/calendar?day=2026-04-02&item=" `shouldEqual` Right (Route (Calendar { day: Just "2026-04-02", item: Nothing }))

    it "shows finance for authenticated users and admin only for admin profiles" do
      visibleTabs AuthUnknown `shouldEqual` [ Note, Checklist, Calendar { day: Nothing, item: Nothing } ]
      visibleTabs Unauthenticated `shouldEqual` [ Note, Checklist, Calendar { day: Nothing, item: Nothing } ]
      visibleTabs (Authenticated memberProfile) `shouldEqual` [ Note, Checklist, Calendar { day: Nothing, item: Nothing }, defaultTransactionsRoute ]
      visibleTabs (Authenticated adminProfile) `shouldEqual` [ Note, Checklist, Calendar { day: Nothing, item: Nothing }, defaultTransactionsRoute, Admin ]

    it "keeps finance routes unresolved while auth status is unknown" do
      resolveGuardedRoute AuthUnknown (Route defaultTransactionsRoute) `shouldEqual` Nothing
      resolveGuardedRoute AuthUnknown (Route FinanceReports) `shouldEqual` Nothing

    it "keeps admin route unresolved while auth status is unknown" do
      resolveGuardedRoute AuthUnknown (Route Admin) `shouldEqual` Nothing

    it "gates finance routes to not-found for unauthenticated users" do
      resolveGuardedRoute Unauthenticated (Route defaultTransactionsRoute) `shouldEqual` Just NotFound
      resolveGuardedRoute Unauthenticated (Route FinanceReports) `shouldEqual` Just NotFound

    it "allows authenticated users to access finance routes" do
      resolveGuardedRoute (Authenticated memberProfile) (Route defaultTransactionsRoute) `shouldEqual` Just (Route defaultTransactionsRoute)
      resolveGuardedRoute (Authenticated memberProfile) (Route FinanceReports) `shouldEqual` Just (Route FinanceReports)

    it "identifies finance shell routes and create-button visibility" do
      isFinanceRoute defaultTransactionsRoute `shouldEqual` true
      isFinanceRoute FinanceReports `shouldEqual` true
      isFinanceRoute Note `shouldEqual` false
      financeLocalPrimaryRoute defaultTransactionsRoute `shouldEqual` Just defaultTransactionsRoute
      financeLocalPrimaryRoute FinanceReports `shouldEqual` Just FinanceReports
      financeLocalPrimaryRoute Note `shouldEqual` Nothing
      shouldShowFinanceCreateButton defaultTransactionsRoute `shouldEqual` true
      shouldShowFinanceCreateButton FinanceReports `shouldEqual` false
      shouldShowFinanceCreateButton Note `shouldEqual` false

    it "tracks finance overlay visibility only on finance routes" do
      isFinanceOverlayOpen Nothing `shouldEqual` false
      isFinanceOverlayOpen (Just FinanceCreateChooserOverlay) `shouldEqual` true
      isFinanceOverlayOpen (Just (FinanceCreateOverlay { direction: "sent", accountId: Just "acc-1", occurredAtDaySeed: Just "2026-05-20" })) `shouldEqual` true
      isFinanceOverlayOpen (Just (FinanceDetailOverlay "tx-1")) `shouldEqual` true
      shouldRenderFinanceOverlay defaultTransactionsRoute Nothing `shouldEqual` false
      shouldRenderFinanceOverlay defaultTransactionsRoute (Just FinanceCreateChooserOverlay) `shouldEqual` true
      shouldRenderFinanceOverlay defaultTransactionsRoute (Just (FinanceCreateOverlay { direction: "sent", accountId: Nothing, occurredAtDaySeed: Nothing })) `shouldEqual` true
      shouldRenderFinanceOverlay defaultTransactionsRoute (Just (FinanceDetailOverlay "tx-1")) `shouldEqual` true
      shouldRenderFinanceOverlay FinanceReports (Just FinanceCreateChooserOverlay) `shouldEqual` true
      shouldRenderFinanceOverlay Note (Just FinanceCreateChooserOverlay) `shouldEqual` false

    it "gates admin route to not-found for non-admin users" do
      resolveGuardedRoute Unauthenticated (Route Admin) `shouldEqual` Just NotFound
      resolveGuardedRoute (Authenticated memberProfile) (Route Admin) `shouldEqual` Just NotFound

    it "allows admin users to access the admin route" do
      resolveGuardedRoute (Authenticated adminProfile) (Route Admin) `shouldEqual` Just (Route Admin)

    it "keeps non-admin routes unchanged" do
      resolveGuardedRoute Unauthenticated (Route Note) `shouldEqual` Just (Route Note)
      resolveGuardedRoute (Authenticated adminProfile) (Route (Calendar { day: Nothing, item: Nothing })) `shouldEqual` Just (Route (Calendar { day: Nothing, item: Nothing }))

    it "renders the connected identity label only for authenticated users" do
      connectedIdentityLabel AuthUnknown `shouldEqual` Nothing
      connectedIdentityLabel Unauthenticated `shouldEqual` Nothing
      connectedIdentityLabel (Authenticated memberProfile) `shouldEqual` Just "Connecté: member"
      connectedIdentityLabel (Authenticated adminProfile) `shouldEqual` Just "Connecté: admin"

    it "refreshes late-items only on authenticated reminder-visible routes" do
      shouldRefreshLateItemsForRoute Unauthenticated (Route Note) `shouldEqual` false
      shouldRefreshLateItemsForRoute (Authenticated memberProfile) (Route Note) `shouldEqual` true
      shouldRefreshLateItemsForRoute (Authenticated memberProfile) (Route Checklist) `shouldEqual` true
      shouldRefreshLateItemsForRoute (Authenticated memberProfile) (Route (Calendar { day: Nothing, item: Nothing })) `shouldEqual` true
      shouldRefreshLateItemsForRoute (Authenticated memberProfile) (Route Admin) `shouldEqual` false
      shouldRefreshLateItemsForRoute (Authenticated adminProfile) (Route Admin) `shouldEqual` true
      shouldRefreshLateItemsForRoute (Authenticated memberProfile) (Route Signup) `shouldEqual` false
      shouldRefreshLateItemsForRoute (Authenticated memberProfile) NotFound `shouldEqual` false
      shouldRefreshLateItemsForRoute (Authenticated memberProfile) Root `shouldEqual` false

    it "ignores stale late-items responses and keeps items on active failures" do
      let
        staleItem = mockLateItem "stale" "Stale item"
        freshItem = mockLateItem "fresh" "Fresh item"
        firstRequest = beginLateItemsRequest initialLateItemsState
        secondRequest = beginLateItemsRequest firstRequest.lateItems
        staleLoaded = applyLateItemsLoaded firstRequest.requestId [ staleItem ] secondRequest.lateItems
        freshLoaded = applyLateItemsLoaded secondRequest.requestId [ freshItem ] secondRequest.lateItems
        thirdRequest = beginLateItemsRequest freshLoaded
        staleFailure = applyLateItemsLoadFailed secondRequest.requestId "stale error" thirdRequest.lateItems
        activeFailure = applyLateItemsLoadFailed thirdRequest.requestId "active error" thirdRequest.lateItems
      map _.title staleLoaded.items `shouldEqual` []
      staleLoaded.activeRequestId `shouldEqual` Just secondRequest.requestId
      map _.title freshLoaded.items `shouldEqual` [ "Fresh item" ]
      freshLoaded.activeRequestId `shouldEqual` Nothing
      staleFailure.isLoading `shouldEqual` true
      staleFailure.loadError `shouldEqual` Nothing
      map _.title activeFailure.items `shouldEqual` [ "Fresh item" ]
      activeFailure.loadError `shouldEqual` Just "active error"
      activeFailure.isLoading `shouldEqual` false
      activeFailure.activeRequestId `shouldEqual` Nothing

    it "filters transfer candidates by excluding source and already linked transactions" do
      let
        source = sampleFinanceTransaction "tx-1" Nothing
        eligible = sampleFinanceTransaction "tx-2" Nothing
        linked = sampleFinanceTransaction "tx-3" (Just "tx-99")
        filtered = filterTransferCandidates "tx-1" [ source, eligible, linked ]
      map (\(FinanceTransaction tx) -> tx.id) filtered `shouldEqual` [ "tx-2" ]

defaultTransactionsRoute :: DefinedRoute
defaultTransactionsRoute =
  FinanceTransactions
    { accountId: Nothing
    , from: Nothing
    , to: Nothing
    , direction: Nothing
    , categoryIn: []
    , categoryNotIn: []
    , amountMin: Nothing
    , amountMax: Nothing
    , search: Nothing
    }

memberProfile :: AuthenticatedProfile
memberProfile =
  AuthenticatedProfile
    { username: "member"
    , roles: [ "member" ]
    , approved: true
    }

adminProfile :: AuthenticatedProfile
adminProfile =
  AuthenticatedProfile
    { username: "admin"
    , roles: [ "admin" ]
    , approved: true
    }

mockLateItem :: String -> String -> LateItems.LateItem
mockLateItem id title =
  { id: Just id
  , sourceItem: serverCalendarItem id (calendarContent Task title "2026-04-10T08:00" "2026-04-10T09:00")
  , title
  , day: "2026-04-10"
  , end: unsafeDateTime "2026-04-10T09:00"
  , endDisplay: "10/04/2026 09:00"
  , plannedDurationMinutes: 60
  }

sampleFinanceTransaction :: String -> Maybe String -> FinanceTransaction
sampleFinanceTransaction id linkedTransactionId =
  FinanceTransaction
    { id
    , direction: TransactionSent
    , accountId: "acc-1"
    , amount: 12.0
    , occurredAt: "2026-05-01T10:00:00Z"
    , recordedAt: "2026-05-01T10:05:00Z"
    , counterparty: Nothing
    , description: Nothing
    , transfer: map (\linkedId -> FinanceTransferLink { linkedTransactionId: linkedId, linkType: "transfer" }) linkedTransactionId
    , category: Nothing
    , splits: [] :: Array FinanceTransactionSplitRow
    , notes: []
    , adjustment: Nothing
    }
