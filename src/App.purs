module App (component) where

import Prelude hiding (div, (/))

import Affjax.Web (Response, post, Error)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (string)
import Checklists (component) as Checklists
import Control.Monad.RWS (get, modify_)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Char (toCharCode)
import Data.Either (Either(..), either)
import Data.Foldable (all)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect)
import Data.Newtype (unwrap)
import Halogen (Component, HalogenM, Slot, ComponentHTML, defaultEval, mkComponent, mkEval) as H
import Halogen (HalogenM, liftEffect, subscribe)
import Halogen.HTML (HTML, a, div, h1, nav, slot_, text, form, label, input, button)
import Halogen.HTML.Events (onClick, onValueChange, onSubmit)
import Halogen.HTML.Properties (for, type_, name, placeholder, id, value, disabled)
import Halogen.Subscription (create, notify)
import Notes (component) as Notes
import Routing.Duplex (RouteDuplex', root, parse)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Hash (matchesWith)
import Type.Prelude (Proxy(..))
import Utils (class_)
import Web.Event.Event (Event, preventDefault)

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
  ([ h1 [ class_ "text-center" ] [ text "FAVS" ]] <>
  (if route /= Signup then [ nav [ class_ "row nav nav-tabs" ] [ tab Note route, tab Checklist route ] ] else []) <>
  [ currentComponent route
  , div [ class_ "bottom-space" ] []
  ])
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

data SignupAction = SignupInitialize | Submit Event | UsernameChanged String | PasswordChanged String
type NoOutput = Void
newtype SignupFormData = SignupFormData SignupState
type SignupState =
  { username :: String
  , password :: String
  , usernameError :: Maybe String
  , passwordError :: Maybe String
  , feedbackMessage :: Maybe String
  , submitting :: Boolean
  }

instance signupFormDataEncodeJson :: EncodeJson SignupFormData where
  encodeJson :: SignupFormData -> Json
  encodeJson (SignupFormData {username, password}) = uname ~> pass ~> jsonEmptyObject
    where uname = "username" := username
          pass = "password" := password

signupInitialState :: SignupState
signupInitialState =
  { username: ""
  , password: ""
  , usernameError: Nothing
  , passwordError: Nothing
  , feedbackMessage: Nothing
  , submitting: false
  }

signupComponent :: forall q i. H.Component q i NoOutput Aff
signupComponent = H.mkComponent { initialState: const signupInitialState
                                , render: signupRender
                                , eval: H.mkEval $ H.defaultEval { handleAction = signupHandleAction
                                                                 , initialize = pure SignupInitialize
                                                                 }
                                }
--(\err -> liftEffect (logShow "Error while trying to signup") >>= const $ pure unit)
--(\r -> liftEffect (logShow r) >>= const $ pure unit)
handleError :: Error -> HalogenM SignupState SignupAction () NoOutput Aff Unit
handleError _ = modify_ $ _ { feedbackMessage = Just "Signup failed. Please try again."
                            , submitting = false
                            }

handleResponse :: Response String -> HalogenM SignupState SignupAction () NoOutput Aff Unit
handleResponse r
  | unwrap r.status >= 200 && unwrap r.status < 300 =
      modify_ $ _ { feedbackMessage = Just "Account created successfully."
                  , submitting = false
                  , password = ""
                  }
  | otherwise =
      modify_ $ _ { feedbackMessage = Just (if String.length r.body > 0 then r.body else "Signup failed.")
                  , submitting = false
                  }

validateUsername :: String -> Maybe String
validateUsername username
  | String.length username == 0 = Just "Username cannot be empty."
  | String.length username < 3 = Just "Username must be at least 3 characters."
  | String.length username > 32 = Just "Username must be at most 32 characters."
  | not (all isAllowedUsernameChar $ String.toCharArray username) = Just "Username can only use letters, digits, '.', '_' or '-'."
  | otherwise = Nothing

isAllowedUsernameChar :: Char -> Boolean
isAllowedUsernameChar c =
  isAsciiAlpha c || isAsciiDigit c || c == '.' || c == '_' || c == '-'

isAsciiAlpha :: Char -> Boolean
isAsciiAlpha c =
  let code = toCharCode c
  in (code >= 65 && code <= 90) || (code >= 97 && code <= 122)

isAsciiDigit :: Char -> Boolean
isAsciiDigit c =
  let code = toCharCode c
  in code >= 48 && code <= 57

validatePassword :: String -> Maybe String
validatePassword password
  | String.length password == 0 = Just "Password cannot be empty."
  | String.length password < 12 = Just "Password must be at least 12 characters."
  | otherwise = Nothing

signupHandleAction :: SignupAction -> HalogenM SignupState SignupAction () NoOutput Aff Unit
signupHandleAction (Submit e) = do
  liftEffect $ preventDefault e
  formData <- get
  let
    usernameErr = validateUsername formData.username
    passwordErr = validatePassword formData.password
    hasErrors = case usernameErr, passwordErr of
      Nothing, Nothing -> false
      _, _ -> true
  modify_ $ _ { usernameError = usernameErr
              , passwordError = passwordErr
              , feedbackMessage = Nothing
              }
  if hasErrors
    then pure unit
    else do
      modify_ $ _ { submitting = true }
      resp <- liftAff $ post string "/api/signup" $ Just $ Json $ encodeJson $ SignupFormData formData
      either handleError handleResponse resp
signupHandleAction (UsernameChanged newUsername) = do
  modify_ $ _ { username = newUsername
              , usernameError = validateUsername newUsername
              , feedbackMessage = Nothing
              }
signupHandleAction (PasswordChanged newPassword) = do
  modify_ $ _ { password = newPassword
              , passwordError = validatePassword newPassword
              , feedbackMessage = Nothing
              }
signupHandleAction _ = pure unit

signupRender :: forall m. SignupState -> H.ComponentHTML SignupAction () m
signupRender state =
  div [ class_ "row justify-content-center mt-5" ]
    [ div [ class_ "col-12 col-md-8 col-lg-5" ]
        [ div [ class_ "card shadow-sm border-0" ]
            [ div [ class_ "card-body p-4" ]
                ([ div [ class_ "text-center mb-4" ]
                     [ h1 [ class_ "h3 mb-1" ] [ text "Create your account" ]
                     , div [ class_ "text-muted" ] [ text "Signup to start using FAVS" ]
                     ]
                 , form [ onSubmit Submit ]
                     ([ label [ class_ "form-label fw-semibold", for "signup-username" ] [ text "Username" ]
                      , input [ id "signup-username"
                              , type_ InputText
                              , name "username"
                              , class_ "form-control"
                              , placeholder "Choose a username"
                              , value state.username
                              , onValueChange UsernameChanged
                              ]
                      ]
                      <> maybe [] (\err -> [ div [ class_ "invalid-feedback d-block mb-2" ] [ text err ] ]) state.usernameError
                      <> [ label [ class_ "form-label fw-semibold mt-2", for "signup-password" ] [ text "Password" ]
                         , input [ id "signup-password"
                                 , type_ InputPassword
                                 , name "password"
                                 , class_ "form-control"
                                 , placeholder "At least 12 characters"
                                 , value state.password
                                 , onValueChange PasswordChanged
                                 ]
                         ]
                      <> maybe [] (\err -> [ div [ class_ "invalid-feedback d-block mb-2" ] [ text err ] ]) state.passwordError
                      <> maybe [] (\msg -> [ div [ class_ "alert alert-secondary mt-3 mb-0" ] [ text msg ] ]) state.feedbackMessage
                      <> [ button [ type_ ButtonSubmit
                                  , class_ "btn btn-primary w-100 mt-3"
                                  , disabled state.submitting
                                  ]
                                  [ text (if state.submitting then "Submitting..." else "Create account") ]
                         ])
                 ])
            ]
        ]
    ]
