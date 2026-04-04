module Test.Pages.AppSpec (spec) where

import Prelude

import Api.Auth (AuthenticatedProfile(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Pages.App (AuthStatus(..), DefinedRoute(..), Route(..), parseRouteString, resolveGuardedRoute, visibleTabs)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "App routing and auth gating" do
    it "parses the admin route" do
      parseRouteString "/admin" `shouldEqual` Right (Route Admin)

    it "shows admin tab only for admin profiles" do
      visibleTabs AuthUnknown `shouldEqual` [ Note, Checklist, Calendar ]
      visibleTabs Unauthenticated `shouldEqual` [ Note, Checklist, Calendar ]
      visibleTabs (Authenticated memberProfile) `shouldEqual` [ Note, Checklist, Calendar ]
      visibleTabs (Authenticated adminProfile) `shouldEqual` [ Note, Checklist, Calendar, Admin ]

    it "keeps admin route unresolved while auth status is unknown" do
      resolveGuardedRoute AuthUnknown (Route Admin) `shouldEqual` Nothing

    it "gates admin route to not-found for non-admin users" do
      resolveGuardedRoute Unauthenticated (Route Admin) `shouldEqual` Just NotFound
      resolveGuardedRoute (Authenticated memberProfile) (Route Admin) `shouldEqual` Just NotFound

    it "allows admin users to access the admin route" do
      resolveGuardedRoute (Authenticated adminProfile) (Route Admin) `shouldEqual` Just (Route Admin)

    it "keeps non-admin routes unchanged" do
      resolveGuardedRoute Unauthenticated (Route Note) `shouldEqual` Just (Route Note)
      resolveGuardedRoute (Authenticated adminProfile) (Route Calendar) `shouldEqual` Just (Route Calendar)

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
