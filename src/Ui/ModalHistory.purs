module Ui.ModalHistory
  ( subscribeToGlobalPopState
  , armModalBackNavigation
  , consumeModalBackNavigation
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Query.Event as HQE
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.History as History
import Web.HTML.Location as Location
import Web.HTML.Window as Window

subscribeToGlobalPopState
  :: forall state action slots output m
   . MonadEffect m
  => action
  -> H.HalogenM state action slots output m Unit
subscribeToGlobalPopState popStateAction = do
  win <- liftEffect window
  let target = Window.toEventTarget win
  let
    emitter = HQE.eventListener (EventType "popstate") target \_ ->
      Just popStateAction
  _ <- H.subscribe emitter
  pure unit

armModalBackNavigation
  :: forall state action slots output m
   . MonadEffect m
  => (state -> Boolean)
  -> H.HalogenM state action slots output m Unit
armModalBackNavigation isModalOpen = do
  state <- H.get
  unless (isModalOpen state) $
    liftEffect pushCurrentLocationInHistory

consumeModalBackNavigation
  :: forall state action slots output m
   . MonadEffect m
  => (state -> Boolean)
  -> H.HalogenM state action slots output m Unit
consumeModalBackNavigation isModalOpen = do
  state <- H.get
  when (isModalOpen state) $
    liftEffect goBackInHistory

pushCurrentLocationInHistory :: Effect Unit
pushCurrentLocationInHistory = do
  win <- window
  browserHistory <- Window.history win
  location <- Window.location win
  href <- Location.href location
  currentState <- History.state browserHistory
  History.pushState
    currentState
    (History.DocumentTitle "")
    (History.URL href)
    browserHistory

goBackInHistory :: Effect Unit
goBackInHistory = do
  win <- window
  browserHistory <- Window.history win
  History.back browserHistory
