module App (component) where

import Prelude hiding (div, (/))

import Affjax.Web (Response, post, Error)
import Affjax.Web as AffjaxWeb
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (string)
import Checklists (component) as Checklists
import Control.Monad.RWS (get, modify_)
import Data.Array (head)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Char (toCharCode)
import Data.Either (Either(..), either)
import Data.Foldable (all)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String
import Data.String.Common as StringCommon
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect)
import Data.Newtype (unwrap)
import Foreign (Foreign, unsafeToForeign)
import Halogen (Component, HalogenM, Slot, ComponentHTML, defaultEval, mkComponent, mkEval) as H
import Halogen (HalogenM, liftEffect, subscribe)
import Halogen.HTML (HTML, a, div, h1, nav, slot_, text, form, label, input, button)
import Halogen.HTML.Events (onClick, onValueChange, onSubmit)
import Halogen.HTML.Properties (for, type_, name, placeholder, id, value, disabled)
import Halogen.Subscription (create, notify)
import Notes (component) as Notes
import Routing.Duplex (RouteDuplex', root, parse, print)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.PushState (PushStateInterface, makeInterface, matchesWith)
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

data Route = Root | Route DefinedRoute | NotFound
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

parseRouteString :: String -> Either Unit Route
parseRouteString rawPath =
  Right $
    if path == "/" || path == "" then Root
    else either (const NotFound) Route $ parse routeCodec path
  where
  withoutQuery = fromMaybe rawPath $ head $ StringCommon.split (Pattern "?") rawPath
  path = fromMaybe withoutQuery $ head $ StringCommon.split (Pattern "#") withoutQuery

subscribeToRouting :: forall state slots output m. MonadEffect m => PushStateInterface -> H.HalogenM state Action slots output m Unit
subscribeToRouting nav = do
  {emitter, listener} <- liftEffect create
  void $ subscribe emitter
  void $ liftEffect $ matchesWith parseRouteString (\old new -> do
    when (old /= Just new) $ do
      notify listener $ RouteChanged new
    ) nav
  pure unit

data Action = RouteChanged Route
            | NavigateTo DefinedRoute
            | SignOut
            | RefreshAuthStatus
            | InitializeRouting
type State =
  { currentRoute :: Route
  , nav :: Maybe PushStateInterface
  , isAuthenticated :: Boolean
  }

component :: forall q i. H.Component q i Void Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = pure InitializeRouting}
    }

initialState :: forall i. i -> State
initialState = const
  { currentRoute: Route Note
  , nav: Nothing
  , isAuthenticated: false
  }

historyState :: Foreign
historyState = unsafeToForeign unit

navigateWith
  :: (PushStateInterface -> Foreign -> String -> Effect Unit)
  -> Maybe PushStateInterface
  -> DefinedRoute
  -> H.HalogenM State Action ChildSlots Void Aff Unit
navigateWith navFn maybeNav route =
  case maybeNav of
    Just nav -> liftEffect $ navFn nav historyState (print routeCodec route)
    Nothing -> pure unit

statusOk :: forall a. Response a -> Boolean
statusOk r = unwrap r.status >= 200 && unwrap r.status < 300

probeAuth :: forall state action slots. H.HalogenM state action slots Void Aff Boolean
probeAuth = do
  resp <- liftAff $ AffjaxWeb.get string "/api/note"
  pure $ either (const false) statusOk resp

handleAction :: Action -> H.HalogenM State Action ChildSlots Void Aff Unit
handleAction (RouteChanged Root) = do
  st <- get
  modify_ _ { currentRoute = Route Note }
  navigateWith _.replaceState st.nav Note
handleAction (RouteChanged route) =
  modify_ _ { currentRoute = route }
handleAction (NavigateTo route) = do
  st <- get
  modify_ _ { currentRoute = Route route }
  navigateWith _.pushState st.nav route
handleAction SignOut = do
  st <- get
  _ <- liftAff $ post string "/api/signout" Nothing
  modify_ _ { isAuthenticated = false, currentRoute = Route Signup }
  navigateWith _.pushState st.nav Signup
handleAction InitializeRouting = do
  nav <- liftEffect makeInterface
  modify_ _ { nav = Just nav }
  subscribeToRouting nav
  handleAction RefreshAuthStatus
handleAction RefreshAuthStatus = do
  authed <- probeAuth
  modify_ _ { isAuthenticated = authed }

render :: State -> H.ComponentHTML Action ChildSlots Aff
render { currentRoute: Route route, isAuthenticated } =
  div [ class_ "container" ]
  ([ h1 [ class_ "text-center" ] [ text "FAVS" ]
   , authMenu isAuthenticated
   ] <>
  (if route /= Signup then [ nav [ class_ "row nav nav-tabs" ] [ tab Note route, tab Checklist route ] ] else []) <>
  [ currentComponent route
  , div [ class_ "bottom-space" ] []
  ])
render { currentRoute: Root } = text ""
render { currentRoute: NotFound, isAuthenticated } =
  div [ class_ "container py-5" ]
    [ authMenu isAuthenticated
    , div [ class_ "row justify-content-center" ]
        [ div [ class_ "col-12 col-md-10 col-lg-7" ]
            [ div [ class_ "card shadow-sm border-0" ]
                [ div [ class_ "card-body p-5 text-center" ]
                    [ div [ class_ "display-4 fw-bold mb-2" ] [ text "404" ]
                    , h1 [ class_ "h4 mb-3" ] [ text "Page introuvable" ]
                    , div [ class_ "text-muted mb-4" ] [ text "La route demandee n'existe pas ou n'est plus disponible." ]
                    , div [ class_ "d-flex justify-content-center gap-2 flex-wrap" ]
                        [ button [ class_ "btn btn-primary", onClick (const $ NavigateTo Note) ] [ text "Aller aux notes" ]
                        , button [ class_ "btn btn-outline-secondary", onClick (const $ NavigateTo Signup) ] [ text "Se connecter / S'inscrire" ]
                        ]
                    ]
                ]
            ]
        ]
    ]

authMenu :: forall w. Boolean -> HTML w Action
authMenu isAuthenticated =
  div [ class_ "auth-menu d-flex justify-content-end mb-2" ]
    [ if isAuthenticated
        then button [ class_ "btn btn-outline-secondary btn-sm", onClick (const SignOut) ] [ text "Se deconnecter" ]
        else button [ class_ "btn btn-outline-secondary btn-sm", onClick (const $ NavigateTo Signup) ] [ text "Signup" ]
    ]

currentComponent :: DefinedRoute -> H.ComponentHTML Action ChildSlots Aff
currentComponent Note = slot_ (Proxy :: _ "notes") unit Notes.component unit
currentComponent Checklist = slot_ (Proxy :: _ "checklists") unit Checklists.component unit
currentComponent Signup = slot_ (Proxy :: _ "signup") unit signupComponent unit

tab :: forall w. DefinedRoute -> DefinedRoute -> HTML w Action
tab tabRoute activeRoute =
  div [ class_ "col text-center nav-item px-0" ]
    [ a [ class_ $ "nav-link" <> (if tabRoute == activeRoute then " active" else "")
        , onClick (const $ NavigateTo tabRoute)
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
