module Test.Ui.PageFlowSpec (spec) where

import Prelude

import Affjax (Response)
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Ui.ErrorMessages (wrongStatusPost)
import Ui.Errors (FatalError(..))
import Ui.PageFlow (saveAndRefresh, updateAtWithDefault)

spec :: Spec Unit
spec =
  describe "Ui.PageFlow" do
    describe "updateAtWithDefault" do
      it "updates an existing element" do
        let
          items = [ 1, 2 ]
          result = updateAtWithDefault 0 9 (_ + 10) items
        result `shouldEqual` Just 11

      it "updates the default when index is at end" do
        let
          items = [ 1, 2 ]
          result = updateAtWithDefault 2 9 (_ + 10) items
        result `shouldEqual` Just 19

      it "returns Nothing when index is out of range" do
        let
          items = [ 1, 2 ]
          result = updateAtWithDefault 3 9 (_ + 10) items
        result `shouldEqual` Nothing

    describe "saveAndRefresh" do
      it "runs refresh on successful save" do
        ref <- liftEffect $ Ref.new 0
        let
          response = mkResponse 201 jsonEmptyObject
          save :: Unit -> ExceptT FatalError Aff (Response Json)
          save _ = pure response
          refresh = liftEffect $ Ref.modify_ (_ + 1) ref
        res <- runExceptT $ saveAndRefresh save refresh "note" unit
        case res of
          Right _ -> pure unit
          Left err -> fail $ "Expected success, got: " <> show err
        count <- liftEffect $ Ref.read ref
        count `shouldEqual` 1

      it "returns CustomFatalError when save fails" do
        let
          response = mkResponse 500 jsonEmptyObject
          save :: Unit -> ExceptT FatalError Aff (Response Json)
          save _ = pure response
          refresh = pure unit
          expected = CustomFatalError $ wrongStatusPost "note" response.status
        res <- runExceptT $ saveAndRefresh save refresh "note" unit
        case res of
          Left err -> show err `shouldEqual` show expected
          Right _ -> fail "Expected failure, got success"

mkResponse :: Int -> Json -> Response Json
mkResponse code body =
  { status: StatusCode code
  , statusText: ""
  , headers: []
  , body: body
  }
