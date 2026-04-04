module Pages.App
  ( component
  , AuthStatus(..)
  , DefinedRoute(..)
  , Route(..)
  , connectedIdentityLabel
  , parseRouteString
  , resolveGuardedRoute
  , visibleTabs
  ) where

import Prelude hiding (div, (/))

import Affjax.Web (Response, post)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (string)
import Api.Auth (AuthenticatedProfile(..), getAuthProfileResponse, isAdminProfile)
import Pages.Admin (component) as Admin
import Pages.Calendar (component) as Calendar
import Pages.Checklists (component) as Checklists
import Control.Monad.RWS (get, modify_)
import Data.Array (head)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Bifunctor (lmap)
import Data.Char (toCharCode)
import Data.Either (Either(..), either)
import Data.Foldable (all, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
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
import Halogen (HalogenM, liftEffect, raise, subscribe)
import Halogen.HTML (HTML, a, div, h1, nav, slot, slot_, text, form, label, input, button)
import Halogen.HTML.Events (onClick, onValueChange, onSubmit)
import Halogen.HTML.Properties (for, type_, name, placeholder, id, value, disabled)
import Halogen.Subscription as Sub
import Pages.Notes (component) as Notes
import Routing.Duplex (RouteDuplex', root, parse, print)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.PushState (PushStateInterface, makeInterface, matchesWith)
import Type.Prelude (Proxy(..))
import Ui.AuthSession as AuthSession
import Ui.Utils (class_)
import Web.Event.Event (Event, preventDefault)

type OpaqueSlot slot = forall query. H.Slot query Void slot
type ChildSlots =
  ( notes :: OpaqueSlot Unit
  , checklists :: OpaqueSlot Unit
  , calendar :: OpaqueSlot Unit
  , admin :: OpaqueSlot Unit
  , signup :: AuthSlot Unit
  , signin :: AuthSlot Unit
  )

data DefinedRoute = Note | Checklist | Calendar | Admin | Signup | Signin

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
  , "Calendar": "calendar" / noArgs
  , "Admin": "admin" / noArgs
  , "Signup": "signup" / noArgs
  , "Signin": "signin" / noArgs
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
  { emitter, listener } <- liftEffect Sub.create
  void $ subscribe emitter
  void $ liftEffect $ matchesWith parseRouteString
    ( \old new ->
        when (old /= Just new)
          $ Sub.notify listener
          $ RouteChanged new
    )
    nav
  pure unit

data Action
  = RouteChanged Route
  | NavigateTo DefinedRoute
  | SignOut
  | HandleAuthOutput AuthOutput
  | RefreshAuthStatus
  | InitializeRouting

type State =
  { currentRoute :: Route
  , nav :: Maybe PushStateInterface
  , authStatus :: AuthStatus
  }

data AuthStatus
  = AuthUnknown
  | Unauthenticated
  | Authenticated AuthenticatedProfile

component :: forall q i. H.Component q i Void Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = pure InitializeRouting
        }
    }

initialState :: forall i. i -> State
initialState = const
  { currentRoute: Route Note
  , nav: Nothing
  , authStatus: AuthUnknown
  }

historyState :: Foreign
historyState = unsafeToForeign unit

navigateWith
  :: (PushStateInterface -> Foreign -> String -> Effect Unit)
  -> Maybe PushStateInterface
  -> DefinedRoute
  -> H.HalogenM State Action ChildSlots Void Aff Unit
navigateWith navFn maybeNav route = maybe (pure unit) (\nav -> liftEffect $ navFn nav historyState (print routeCodec route)) maybeNav

statusOk :: forall a. Response a -> Boolean
statusOk r = unwrap r.status >= 200 && unwrap r.status < 300

resolveGuardedRoute :: AuthStatus -> Route -> Maybe Route
resolveGuardedRoute authStatus route = case route of
  Route Admin ->
    case authStatus of
      AuthUnknown -> Nothing
      Authenticated profile | isAdminProfile profile -> Just route
      _ -> Just NotFound
  _ -> Just route

visibleTabs :: AuthStatus -> Array DefinedRoute
visibleTabs authStatus =
  [ Note, Checklist, Calendar ] <> guard (canViewAdminNav authStatus) [ Admin ]
  where
  canViewAdminNav (Authenticated profile) = isAdminProfile profile
  canViewAdminNav _ = false

loadProfileAuthStatus :: forall state action slots. H.HalogenM state action slots Void Aff AuthStatus
loadProfileAuthStatus = do
  resp <- liftAff getAuthProfileResponse
  pure $
    case resp of
      Left _ -> Unauthenticated
      Right response ->
        if unwrap response.status == 401 then
          Unauthenticated
        else if statusOk response then
          case lmap show (decodeJson response.body) of
            Right profile -> Authenticated profile
            Left _ -> Unauthenticated
        else
          Unauthenticated

data AuthOutput = SignupSucceeded String | SigninSucceeded AuthenticatedProfile
type AuthSlot slot = forall query. H.Slot query AuthOutput slot

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
handleAction (HandleAuthOutput (SignupSucceeded username)) = do
  st <- get
  liftEffect $ AuthSession.storeAuthenticatedUsername username
  modify_ _ { currentRoute = Route Signin, authStatus = Unauthenticated }
  navigateWith _.pushState st.nav Signin
handleAction (HandleAuthOutput (SigninSucceeded profile@(AuthenticatedProfile { username }))) = do
  st <- get
  liftEffect $ AuthSession.storeAuthenticatedUsername username
  modify_ _ { currentRoute = Route Note, authStatus = Authenticated profile }
  navigateWith _.pushState st.nav Note
handleAction SignOut = do
  st <- get
  _ <- liftAff $ post string "/api/signout" Nothing
  liftEffect AuthSession.clearAuthenticatedUsername
  modify_ _ { authStatus = Unauthenticated, currentRoute = Route Signin }
  navigateWith _.pushState st.nav Signin
handleAction InitializeRouting = do
  nav <- liftEffect makeInterface
  modify_ _ { nav = Just nav }
  subscribeToRouting nav
  handleAction RefreshAuthStatus
handleAction RefreshAuthStatus = do
  authStatus <- loadProfileAuthStatus
  case authStatus of
    Authenticated (AuthenticatedProfile { username }) ->
      liftEffect $ AuthSession.storeAuthenticatedUsername username
    _ ->
      liftEffect AuthSession.clearAuthenticatedUsername
  modify_ _ { authStatus = authStatus }

render :: State -> H.ComponentHTML Action ChildSlots Aff
render { currentRoute, authStatus } =
  case resolveGuardedRoute authStatus currentRoute of
    Nothing -> renderLoading authStatus
    Just (Route route) ->
      div [ class_ "container" ]
        ( [ h1 [ class_ "text-center" ] [ text "FAVS" ]
          , authMenu authStatus
          ]
            <> (if route /= Signup && route /= Signin then [ nav [ class_ "row nav nav-tabs" ] (map (\tabRoute -> tab tabRoute route) (visibleTabs authStatus)) ] else [])
            <> [ currentComponent authStatus route ]
        )
    Just Root -> text ""
    Just NotFound -> renderNotFound authStatus

renderLoading :: AuthStatus -> H.ComponentHTML Action ChildSlots Aff
renderLoading authStatus =
  div [ class_ "container py-5" ]
    [ authMenu authStatus
    , div [ class_ "row justify-content-center" ]
        [ div [ class_ "col-12 col-md-10 col-lg-7" ]
            [ div [ class_ "card shadow-sm border-0" ]
                [ div [ class_ "card-body p-5 text-center text-muted" ]
                    [ text "Chargement..." ]
                ]
            ]
        ]
    ]

renderNotFound :: AuthStatus -> H.ComponentHTML Action ChildSlots Aff
renderNotFound authStatus =
  div [ class_ "container py-5" ]
    [ authMenu authStatus
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

connectedIdentityLabel :: AuthStatus -> Maybe String
connectedIdentityLabel = case _ of
  Authenticated (AuthenticatedProfile { username }) -> Just ("Connecté: " <> username)
  _ -> Nothing

authMenu :: forall w. AuthStatus -> HTML w Action
authMenu authStatus =
  div [ class_ "auth-menu d-flex justify-content-end align-items-center gap-2 flex-wrap mb-2" ]
    case authStatus of
      Authenticated _ ->
        [ div [ class_ "text-muted small auth-menu__identity" ] [ text (fromMaybe "" (connectedIdentityLabel authStatus)) ]
        , button [ class_ "btn btn-outline-secondary btn-sm", onClick (const SignOut) ] [ text "Se deconnecter" ]
        ]
      Unauthenticated ->
        [ button [ class_ "btn btn-outline-secondary btn-sm", onClick (const $ NavigateTo Signin) ] [ text "Signin" ]
        , button [ class_ "btn btn-outline-secondary btn-sm", onClick (const $ NavigateTo Signup) ] [ text "Signup" ]
        ]
      AuthUnknown -> []

currentComponent :: AuthStatus -> DefinedRoute -> H.ComponentHTML Action ChildSlots Aff
currentComponent _ Note = slot_ (Proxy :: _ "notes") unit Notes.component unit
currentComponent _ Checklist = slot_ (Proxy :: _ "checklists") unit Checklists.component unit
currentComponent _ Calendar = slot_ (Proxy :: _ "calendar") unit Calendar.component unit
currentComponent (Authenticated (AuthenticatedProfile { username })) Admin =
  slot_ (Proxy :: _ "admin") unit Admin.component { currentUsername: username }
currentComponent _ Admin = text ""
currentComponent _ Signup = slot (Proxy :: _ "signup") unit signupComponent unit HandleAuthOutput
currentComponent _ Signin = slot (Proxy :: _ "signin") unit signinComponent unit HandleAuthOutput

tab :: forall w. DefinedRoute -> DefinedRoute -> HTML w Action
tab tabRoute activeRoute =
  div [ class_ "col text-center nav-item px-0" ]
    [ a
        [ class_ $ "nav-link" <> guard (tabRoute == activeRoute) " active"
        , onClick (const $ NavigateTo tabRoute)
        ]
        [ text (tabLabel tabRoute) ]
    ]

tabLabel :: DefinedRoute -> String
tabLabel Note = "Notes"
tabLabel Checklist = "Checklists"
tabLabel Calendar = "Calendar"
tabLabel Admin = "Admin"
tabLabel Signup = "Signup"
tabLabel Signin = "Signin"

type AuthState =
  { username :: String
  , password :: String
  , usernameError :: Maybe String
  , passwordError :: Maybe String
  , feedbackMessage :: Maybe String
  , submitting :: Boolean
  }

newtype AuthRequestData = AuthRequestData { username :: String, password :: String }

instance authRequestDataEncodeJson :: EncodeJson AuthRequestData where
  encodeJson :: AuthRequestData -> Json
  encodeJson (AuthRequestData { username, password }) = uname ~> pass ~> jsonEmptyObject
    where
    uname = "username" := username
    pass = "password" := password

jsonRequestBody :: forall a. EncodeJson a => a -> Maybe RequestBody
jsonRequestBody = Just <<< Json <<< encodeJson

authInitialState :: AuthState
authInitialState =
  { username: ""
  , password: ""
  , usernameError: Nothing
  , passwordError: Nothing
  , feedbackMessage: Nothing
  , submitting: false
  }

data AuthAction = AuthInitialize | AuthSubmit Event | AuthUsernameChanged String | AuthPasswordChanged String

type AuthSubmitConfig action output =
  { endpoint :: String
  , networkError :: String
  , responseError :: String
  , onSuccess :: AuthSuccess -> HalogenM AuthState action () output Aff Unit
  , resetPasswordOnSuccess :: Boolean
  }

data AuthSuccess
  = SignupSuccess String
  | SigninSuccess AuthenticatedProfile

signupSubmitConfig :: AuthSubmitConfig AuthAction AuthOutput
signupSubmitConfig =
  { endpoint: "/api/signup"
  , networkError: "Signup failed. Please try again."
  , responseError: "Signup failed."
  , onSuccess: case _ of
      SignupSuccess username -> raise (SignupSucceeded username)
      _ -> pure unit
  , resetPasswordOnSuccess: true
  }

signinSubmitConfig :: AuthSubmitConfig AuthAction AuthOutput
signinSubmitConfig =
  { endpoint: "/api/signin"
  , networkError: "Signin failed. Please try again."
  , responseError: "Signin failed."
  , onSuccess: case _ of
      SigninSuccess profile -> raise (SigninSucceeded profile)
      _ -> pure unit
  , resetPasswordOnSuccess: false
  }

authComponent
  :: AuthRenderConfig
  -> AuthSubmitConfig AuthAction AuthOutput
  -> forall q i
   . H.Component q i AuthOutput Aff
authComponent renderConfig submitConfig =
  H.mkComponent
    { initialState: const authInitialState
    , render: renderAuth renderConfig
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAuthAction { submitConfig }
        , initialize = pure AuthInitialize
        }
    }

signupComponent :: forall q i. H.Component q i AuthOutput Aff
signupComponent = authComponent signupRenderConfig signupSubmitConfig

signinComponent :: forall q i. H.Component q i AuthOutput Aff
signinComponent = authComponent signinRenderConfig signinSubmitConfig

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
  let
    code = toCharCode c
  in
    (code >= 65 && code <= 90) || (code >= 97 && code <= 122)

isAsciiDigit :: Char -> Boolean
isAsciiDigit c =
  let
    code = toCharCode c
  in
    code >= 48 && code <= 57

validatePassword :: String -> Maybe String
validatePassword password
  | String.length password == 0 = Just "Password cannot be empty."
  | String.length password < 12 = Just "Password must be at least 12 characters."
  | otherwise = Nothing

handleAuthSubmit
  :: forall action output
   . AuthSubmitConfig action output
  -> Event
  -> HalogenM AuthState action () output Aff Unit
handleAuthSubmit cfg e = do
  liftEffect $ preventDefault e
  formData <- get
  let
    usernameErr = validateUsername formData.username
    passwordErr = validatePassword formData.password
    hasErrors = case usernameErr, passwordErr of
      Nothing, Nothing -> false
      _, _ -> true
  modify_ $ _
    { usernameError = usernameErr
    , passwordError = passwordErr
    , feedbackMessage = Nothing
    }
  if hasErrors then pure unit
  else do
    modify_ $ _ { submitting = true }
    if cfg.endpoint == "/api/signin" then do
      resp <- liftAff $ post string cfg.endpoint (jsonRequestBody $ AuthRequestData { username: formData.username, password: formData.password })
      either
        (\_ -> modify_ $ _ { feedbackMessage = Just cfg.networkError, submitting = false })
        ( \r ->
            if statusOk r then
              case (parseJson r.body >>= decodeJson) of
                Right profile -> do
                  modify_ $ _
                    { submitting = false
                    , password = if cfg.resetPasswordOnSuccess then "" else formData.password
                    }
                  cfg.onSuccess (SigninSuccess profile)
                Left _ ->
                  modify_ $ _ { feedbackMessage = Just cfg.responseError, submitting = false }
            else
              modify_ $ _ { feedbackMessage = Just (if String.length r.body > 0 then r.body else cfg.responseError), submitting = false }
        )
        resp
    else do
      resp <- liftAff $ post string cfg.endpoint (jsonRequestBody $ AuthRequestData { username: formData.username, password: formData.password })
      either
        (\_ -> modify_ $ _ { feedbackMessage = Just cfg.networkError, submitting = false })
        ( \r ->
            if statusOk r then do
              modify_ $ _
                { submitting = false
                , password = if cfg.resetPasswordOnSuccess then "" else formData.password
                }
              cfg.onSuccess (SignupSuccess formData.username)
            else
              modify_ $ _ { feedbackMessage = Just (if String.length r.body > 0 then r.body else cfg.responseError), submitting = false }
        )
        resp

handleUsernameChanged :: forall action output. String -> HalogenM AuthState action () output Aff Unit
handleUsernameChanged newUsername =
  modify_ $ _
    { username = newUsername
    , usernameError = validateUsername newUsername
    , feedbackMessage = Nothing
    }

handlePasswordChanged :: forall action output. String -> HalogenM AuthState action () output Aff Unit
handlePasswordChanged newPassword =
  modify_ $ _
    { password = newPassword
    , passwordError = validatePassword newPassword
    , feedbackMessage = Nothing
    }

handleAuthAction
  :: { submitConfig :: AuthSubmitConfig AuthAction AuthOutput }
  -> AuthAction
  -> HalogenM AuthState AuthAction () AuthOutput Aff Unit
handleAuthAction cfg = case _ of
  AuthSubmit e -> handleAuthSubmit cfg.submitConfig e
  AuthUsernameChanged newUsername -> handleUsernameChanged newUsername
  AuthPasswordChanged newPassword -> handlePasswordChanged newPassword
  _ -> pure unit

type AuthRenderConfig =
  { title :: String
  , subtitle :: String
  , idPrefix :: String
  , usernamePlaceholder :: String
  , passwordPlaceholder :: String
  , submitLabel :: String
  , submittingLabel :: String
  }

signupRenderConfig :: AuthRenderConfig
signupRenderConfig =
  { title: "Create your account"
  , subtitle: "Signup to start using FAVS"
  , idPrefix: "signup"
  , usernamePlaceholder: "Choose a username"
  , passwordPlaceholder: "At least 12 characters"
  , submitLabel: "Create account"
  , submittingLabel: "Submitting..."
  }

signinRenderConfig :: AuthRenderConfig
signinRenderConfig =
  { title: "Sign in"
  , subtitle: "Welcome back to FAVS"
  , idPrefix: "signin"
  , usernamePlaceholder: "Your username"
  , passwordPlaceholder: "Your password"
  , submitLabel: "Sign in"
  , submittingLabel: "Signing in..."
  }

renderAuth :: forall m. AuthRenderConfig -> AuthState -> H.ComponentHTML AuthAction () m
renderAuth cfg state =
  div [ class_ "row justify-content-center mt-5" ]
    [ div [ class_ "col-12 col-md-8 col-lg-5" ]
        [ div [ class_ "card shadow-sm border-0" ]
            [ div [ class_ "card-body p-4" ]
                ( [ div [ class_ "text-center mb-4" ]
                      [ h1 [ class_ "h3 mb-1" ] [ text cfg.title ]
                      , div [ class_ "text-muted" ] [ text cfg.subtitle ]
                      ]
                  , form [ onSubmit AuthSubmit ]
                      ( [ label [ class_ "form-label fw-semibold", for (cfg.idPrefix <> "-username") ] [ text "Username" ]
                        , input
                            [ id (cfg.idPrefix <> "-username")
                            , type_ InputText
                            , name "username"
                            , class_ "form-control"
                            , placeholder cfg.usernamePlaceholder
                            , value state.username
                            , onValueChange AuthUsernameChanged
                            ]
                        ]
                          <> foldMap (\err -> [ div [ class_ "invalid-feedback d-block mb-2" ] [ text err ] ]) state.usernameError
                          <>
                            [ label [ class_ "form-label fw-semibold mt-2", for (cfg.idPrefix <> "-password") ] [ text "Password" ]
                            , input
                                [ id (cfg.idPrefix <> "-password")
                                , type_ InputPassword
                                , name "password"
                                , class_ "form-control"
                                , placeholder cfg.passwordPlaceholder
                                , value state.password
                                , onValueChange AuthPasswordChanged
                                ]
                            ]
                          <> foldMap (\err -> [ div [ class_ "invalid-feedback d-block mb-2" ] [ text err ] ]) state.passwordError
                          <> foldMap (\msg -> [ div [ class_ "alert alert-secondary mt-3 mb-0" ] [ text msg ] ]) state.feedbackMessage
                          <>
                            [ button
                                [ type_ ButtonSubmit
                                , class_ "btn btn-primary w-100 mt-3"
                                , disabled state.submitting
                                ]
                                [ text (if state.submitting then cfg.submittingLabel else cfg.submitLabel) ]
                            ]
                      )
                  ]
                )
            ]
        ]
    ]
