module App (component) where

import Prelude hiding (div, (/), otherwise)

import Checklists (component) as Checklists
import Control.Monad.RWS (modify_)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, Slot, ComponentHTML, defaultEval, mkComponent, mkEval) as H
import Halogen (HalogenM, liftEffect, subscribe)
import Halogen.HTML (HTML, a, div, h1, nav, slot_, text)
import Halogen.HTML.Events (onClick)
import Halogen.Subscription (create, notify)
import Notes (component) as Notes
import Routing.Duplex (RouteDuplex', root, parse)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Hash (matchesWith)
import Type.Prelude (Proxy(..))
import Utils (class_)

type OpaqueSlot slot = forall query. H.Slot query Void slot
type ChildSlots = ( notes :: OpaqueSlot Unit
                  , checklists :: OpaqueSlot Unit
                  , signup :: OpaqueSlot Unit
                  )

data DefinedRoute = Note | Checklist | Signup
derive instance definedRouteGeneric :: Generic DefinedRoute _
derive instance definedRouteEq :: Eq DefinedRoute
derive instance definedRouteOrd :: Ord DefinedRoute
instance showDefinedRoute :: Show DefinedRoute where
  show = genericShow

data Route = Route DefinedRoute | NotFound
derive instance routeGeneric :: Generic Route _
derive instance routeEq :: Eq Route
derive instance ordRoute :: Ord Route
instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' DefinedRoute
routeCodec = root $ sum
  { "Note": "notes" / noArgs
  , "Checklist": "checklists" / noArgs
  , "Signup": "signup" / noArgs
  }

subscribeToRouting :: forall state slots output m. MonadEffect m => H.HalogenM state Action slots output m Unit
subscribeToRouting = do
  {emitter, listener} <- liftEffect create
  void $ liftEffect $ matchesWith (\s -> Right $ either (const NotFound) Route $ parse routeCodec s) \old new -> do
    when (old /= Just new) $ do
      notify listener $ RouteChanged new
  _ <- subscribe emitter
  pure unit

data Action = RouteChanged Route
            | InitializeRouting
data State = CurrentRoute Route
derive instance stateEqInstance :: Eq State

component :: forall q i. H.Component q i Void Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = pure InitializeRouting}
    }

initialState :: forall i. i -> State
initialState = const $ CurrentRoute $ Route Note

handleAction :: Action -> H.HalogenM State Action ChildSlots Void Aff Unit
handleAction (RouteChanged route) = do
  modify_ $ const $ CurrentRoute route
handleAction InitializeRouting = subscribeToRouting

render :: State -> H.ComponentHTML Action ChildSlots Aff
render (CurrentRoute (Route route)) =
  div [ class_ "container" ]
    [ h1 [ class_ "text-center" ] [ text "FAVS" ]
    , nav [ class_ "row nav nav-tabs" ] [ tab Note route, tab Checklist route ]
    , currentComponent route
    , div [ class_ "bottom-space" ] []
    ]
render (CurrentRoute NotFound) = text "Not Found"

currentComponent :: DefinedRoute -> H.ComponentHTML Action ChildSlots Aff
currentComponent Note = slot_ (Proxy :: _ "notes") unit Notes.component unit
currentComponent Checklist = slot_ (Proxy :: _ "checklists") unit Checklists.component unit
currentComponent Signup = slot_ (Proxy :: _ "signup") unit signupComponent unit

tab :: forall w. DefinedRoute -> DefinedRoute -> HTML w Action
tab tabRoute activeRoute =
  div [ class_ "col text-center nav-item px-0" ]
    [ a [ class_ $ "nav-link" <> (if tabRoute == activeRoute then " active" else "")
        , onClick (const $ RouteChanged (Route tabRoute))
        ]
        [ text (tabLabel tabRoute) ]
    ]

tabLabel :: DefinedRoute -> String
tabLabel Note = "Notes"
tabLabel Checklist = "Checklists"
tabLabel Signup = "Signup"

data SignupAction = SignupInitialize
type NoOutput = Void
type SignupState = Unit

signupInitialState :: SignupState
signupInitialState = unit
signupComponent :: forall q i. H.Component q i NoOutput Aff
signupComponent = H.mkComponent { initialState: const signupInitialState
                                , render: signupRender
                                , eval: H.mkEval $ H.defaultEval { handleAction = signupHandleAction
                                                                 , initialize = pure SignupInitialize
                                                                 }
                                }

signupHandleAction :: SignupAction -> HalogenM SignupState SignupAction () NoOutput Aff Unit
signupHandleAction _ = pure unit

signupRender :: forall m. SignupState -> H.ComponentHTML SignupAction () m
signupRender _ = div [] [text "This is the Signup form"]
