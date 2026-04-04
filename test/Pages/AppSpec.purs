module Test.Pages.AppSpec (spec) where

import Prelude

import Api.Auth (AuthenticatedProfile(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Pages.App (AuthStatus(..), DefinedRoute(..), Route(..), connectedIdentityLabel, parseRouteString, resolveGuardedRoute, visibleTabs)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "App routing and auth gating" do
    it "parses the admin route" do
      parseRouteString "/admin" `shouldEqual` Right (Route Admin)

    it "parses calendar routes with and without a valid day query" do
      parseRouteString "/calendar" `shouldEqual` Right (Route (Calendar { day: Nothing }))
      parseRouteString "/calendar?day=2026-04-02" `shouldEqual` Right (Route (Calendar { day: Just "2026-04-02" }))
      parseRouteString "/calendar?day=invalid" `shouldEqual` Right (Route (Calendar { day: Nothing }))

    it "shows admin tab only for admin profiles" do
      visibleTabs AuthUnknown `shouldEqual` [ Note, Checklist, Calendar { day: Nothing } ]
      visibleTabs Unauthenticated `shouldEqual` [ Note, Checklist, Calendar { day: Nothing } ]
      visibleTabs (Authenticated memberProfile) `shouldEqual` [ Note, Checklist, Calendar { day: Nothing } ]
      visibleTabs (Authenticated adminProfile) `shouldEqual` [ Note, Checklist, Calendar { day: Nothing }, Admin ]

    it "keeps admin route unresolved while auth status is unknown" do
      resolveGuardedRoute AuthUnknown (Route Admin) `shouldEqual` Nothing

    it "gates admin route to not-found for non-admin users" do
      resolveGuardedRoute Unauthenticated (Route Admin) `shouldEqual` Just NotFound
      resolveGuardedRoute (Authenticated memberProfile) (Route Admin) `shouldEqual` Just NotFound

    it "allows admin users to access the admin route" do
      resolveGuardedRoute (Authenticated adminProfile) (Route Admin) `shouldEqual` Just (Route Admin)

    it "keeps non-admin routes unchanged" do
      resolveGuardedRoute Unauthenticated (Route Note) `shouldEqual` Just (Route Note)
      resolveGuardedRoute (Authenticated adminProfile) (Route (Calendar { day: Nothing })) `shouldEqual` Just (Route (Calendar { day: Nothing }))

    it "renders the connected identity label only for authenticated users" do
      connectedIdentityLabel AuthUnknown `shouldEqual` Nothing
      connectedIdentityLabel Unauthenticated `shouldEqual` Nothing
      connectedIdentityLabel (Authenticated memberProfile) `shouldEqual` Just "Connecté: member"
      connectedIdentityLabel (Authenticated adminProfile) `shouldEqual` Just "Connecté: admin"

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
