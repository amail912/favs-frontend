module Test.Pages.AppSpec (spec) where

import Prelude

import Api.Auth (AuthenticatedProfile(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Notifications.LateItems as LateItems
import Pages.App (AuthStatus(..), DefinedRoute(..), Route(..), applyLateItemsLoadFailed, applyLateItemsLoaded, beginLateItemsRequest, connectedIdentityLabel, initialLateItemsState, parseRouteString, resolveGuardedRoute, shouldRefreshLateItemsForRoute, visibleTabs)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Support.Builders (unsafeDateTime)

spec :: Spec Unit
spec =
  describe "App routing and auth gating" do
    it "parses the admin route" do
      parseRouteString "/admin" `shouldEqual` Right (Route Admin)

    it "parses calendar routes with and without a valid day query" do
      parseRouteString "/calendar" `shouldEqual` Right (Route (Calendar { day: Nothing, item: Nothing }))
      parseRouteString "/calendar?day=2026-04-02" `shouldEqual` Right (Route (Calendar { day: Just "2026-04-02", item: Nothing }))
      parseRouteString "/calendar?day=2026-04-02&item=task-1" `shouldEqual` Right (Route (Calendar { day: Just "2026-04-02", item: Just "task-1" }))
      parseRouteString "/calendar?day=invalid&item=task-1" `shouldEqual` Right (Route (Calendar { day: Nothing, item: Just "task-1" }))
      parseRouteString "/calendar?day=2026-04-02&item=" `shouldEqual` Right (Route (Calendar { day: Just "2026-04-02", item: Nothing }))

    it "shows admin tab only for admin profiles" do
      visibleTabs AuthUnknown `shouldEqual` [ Note, Checklist, Calendar { day: Nothing, item: Nothing } ]
      visibleTabs Unauthenticated `shouldEqual` [ Note, Checklist, Calendar { day: Nothing, item: Nothing } ]
      visibleTabs (Authenticated memberProfile) `shouldEqual` [ Note, Checklist, Calendar { day: Nothing, item: Nothing } ]
      visibleTabs (Authenticated adminProfile) `shouldEqual` [ Note, Checklist, Calendar { day: Nothing, item: Nothing }, Admin ]

    it "keeps admin route unresolved while auth status is unknown" do
      resolveGuardedRoute AuthUnknown (Route Admin) `shouldEqual` Nothing

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
  , title
  , day: "2026-04-10"
  , end: unsafeDateTime "2026-04-10T09:00"
  , endDisplay: "10/04/2026 09:00"
  , plannedDurationMinutes: 60
  }
