module Pages.App
  ( component
  , AuthStatus(..)
  , CalendarRouteState
  , TransactionsRouteState
  , FinanceCreateLaunch
  , DefinedRoute(..)
  , FinanceOverlay(..)
  , Route(..)
  , LateItemsState
  , LateItemsQuickCompleteState
  , initialLateItemsState
  , beginLateItemsRequest
  , applyLateItemsLoaded
  , applyLateItemsLoadFailed
  , connectedIdentityLabel
  , parseRouteString
  , printRoute
  , isFinanceOverlayOpen
  , isFinanceRoute
  , financeLocalPrimaryRoute
  , shouldRenderFinanceOverlay
  , shouldShowFinanceCreateButton
  , resolveGuardedRoute
  , shouldRefreshLateItemsForRoute
  , visibleTabs
  ) where

import Prelude hiding (div, (/))

import Affjax (printError)
import Affjax.Web (Response, post)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (string)
import Api.Auth (AuthenticatedProfile(..), getAuthProfileResponse, isAdminProfile)
import Api.Calendar (getItemsResponse, updateItemResponse)
import Api.Finance (createReceivedTransaction, createSentTransaction, getAccounts)
import Api.FinanceContract
  ( CreateFinanceTransaction(..)
  , FinanceAccount(..)
  , FinanceAccountsQuery(..)
  , FinanceAccountsStatus(..)
  , FinanceTransaction(..)
  , FinanceTransactionAdjustment(..)
  , FinanceTransactionCategory(..)
  , FinanceTransactionDirection(..)
  , FinanceTransactionNote(..)
  , FinanceTransactionSplitRow(..)
  , FinanceTransferLink(..)
  )
import Pages.Admin (component) as Admin
import Pages.Calendar (CalendarRouteOutput(..), CalendarView(..), CalendarItem(..), component, decodeCalendarItemsResponse) as Calendar
import Pages.Checklists (component) as Checklists
import Pages.FinanceTransactions (FinanceDetailSnapshot, Output(..), component) as FinanceTransactions
import Control.Monad.RWS (get, modify_)
import Data.Array (find, head, length, mapMaybe, null)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Bifunctor (lmap)
import Data.Char (toCharCode)
import Data.Either (Either(..), either)
import Data.Foldable (all, fold, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String
import Data.String.Common as StringCommon
import Data.String.Pattern (Pattern(..))
import Helpers.DateTime as DateTime
import Notifications.LateItems as LateItems
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Effect.Now (nowDateTime)
import Data.Newtype (unwrap)
import Foreign (Foreign, unsafeToForeign)
import Halogen (Component, HalogenM, Slot, ComponentHTML, defaultEval, mkComponent, mkEval) as H
import Halogen (HalogenM, liftEffect, raise, subscribe)
import Halogen.HTML (HTML, a, button, div, form, h1, input, label, li, nav, option, select, slot, slot_, span, text, ul)
import Halogen.HTML.Events (onClick, onValueChange, onSubmit)
import Halogen.HTML.Properties (for, type_, name, placeholder, id, value, disabled)
import Halogen.Subscription as Sub
import Pages.Notes (component) as Notes
import Routing.PushState (PushStateInterface, makeInterface, matchesWith)
import Type.Prelude (Proxy(..))
import Ui.AuthSession as AuthSession
import Ui.CreateButton as CreateButton
import Ui.Modal as Modal
import Ui.ModalHistory as ModalHistory
import Ui.Toast as Toast
import Ui.Utils (class_)
import Ui.WindowScroll as WindowScroll
import Web.Event.Event (Event, preventDefault)
import Web.HTML (window)
import Web.HTML.Location as Location
import Web.HTML.Window as Window

type OpaqueSlot slot = forall query. H.Slot query Void slot
type CalendarSlot slot = forall query. H.Slot query Calendar.CalendarRouteOutput slot
type TransactionsSlot slot = forall query. H.Slot query FinanceTransactions.Output slot
type ChildSlots =
  ( notes :: OpaqueSlot Unit
  , checklists :: OpaqueSlot Unit
  , calendar :: CalendarSlot Unit
  , financeTransactions :: TransactionsSlot Unit
  , admin :: OpaqueSlot Unit
  , signup :: AuthSlot Unit
  , signin :: AuthSlot Unit
  )

type CalendarRouteState =
  { day :: Maybe String
  , item :: Maybe String
  }

type TransactionsRouteState =
  { accountId :: Maybe String
  , from :: Maybe String
  , to :: Maybe String
  }

data DefinedRoute = Note | Checklist | Calendar CalendarRouteState | FinanceTransactions TransactionsRouteState | FinanceReports | Admin | Signup | Signin

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

data FinanceOverlay
  = FinanceCreateChooserOverlay
  | FinanceCreateOverlay FinanceCreateLaunch
  | FinanceDetailOverlay String

derive instance financeOverlayEq :: Eq FinanceOverlay
derive instance financeOverlayGeneric :: Generic FinanceOverlay _

instance showFinanceOverlay :: Show FinanceOverlay where
  show = genericShow

data FinanceCreateIntent
  = FinanceCreateExpense
  | FinanceCreateIncome

derive instance financeCreateIntentEq :: Eq FinanceCreateIntent
derive instance financeCreateIntentGeneric :: Generic FinanceCreateIntent _

instance showFinanceCreateIntent :: Show FinanceCreateIntent where
  show = genericShow

type FinanceCreateLaunch =
  { direction :: String
  , accountId :: Maybe String
  , occurredAtDaySeed :: Maybe String
  }

parseRouteString :: String -> Either Unit Route
parseRouteString rawPath =
  Right $
    if path == "/" || path == "" then Root
    else case parseDefinedRoutePath path of
      Just (Calendar _) ->
        Route (Calendar { day: parseCalendarDay query, item: parseCalendarItem query })
      Just (FinanceTransactions _) ->
        Route
          ( FinanceTransactions
              ( normalizeTransactionsRouteState
                  { accountId: parseTransactionsAccountId query
                  , from: parseTransactionsFrom query
                  , to: parseTransactionsTo query
                  }
              )
          )
      Just route ->
        Route route
      Nothing ->
        NotFound
  where
  withoutHash = fromMaybe rawPath $ head $ StringCommon.split (Pattern "#") rawPath
  path = fromMaybe withoutHash $ head $ StringCommon.split (Pattern "?") withoutHash
  query =
    case StringCommon.split (Pattern "?") withoutHash of
      [ _, rawQuery ] -> rawQuery
      _ -> ""

defaultCalendarRoute :: DefinedRoute
defaultCalendarRoute = Calendar { day: Nothing, item: Nothing }

defaultTransactionsRoute :: DefinedRoute
defaultTransactionsRoute = FinanceTransactions { accountId: Nothing, from: Nothing, to: Nothing }

normalizeCalendarRouteState :: CalendarRouteState -> CalendarRouteState
normalizeCalendarRouteState { day, item } =
  { day: day >>= \raw -> if DateTime.isLocalDate raw then Just raw else Nothing
  , item: item >>= nonEmptyStringMaybe
  }

normalizeTransactionsRouteState :: TransactionsRouteState -> TransactionsRouteState
normalizeTransactionsRouteState { accountId, from, to } =
  { accountId: accountId >>= nonEmptyStringMaybe
  , from: from >>= nonEmptyStringMaybe
  , to: to >>= nonEmptyStringMaybe
  }

routeFromDefined :: DefinedRoute -> Route
routeFromDefined = Route

printDefinedRoutePath :: DefinedRoute -> String
printDefinedRoutePath = case _ of
  Note -> "/notes"
  Checklist -> "/checklists"
  Calendar _ -> "/calendar"
  FinanceTransactions _ -> "/finance/transactions"
  FinanceReports -> "/finance/reports"
  Admin -> "/admin"
  Signup -> "/signup"
  Signin -> "/signin"

parseDefinedRoutePath :: String -> Maybe DefinedRoute
parseDefinedRoutePath = case _ of
  "/notes" -> Just Note
  "notes" -> Just Note
  "/checklists" -> Just Checklist
  "checklists" -> Just Checklist
  "/calendar" -> Just defaultCalendarRoute
  "calendar" -> Just defaultCalendarRoute
  "/finance" -> Just defaultTransactionsRoute
  "finance" -> Just defaultTransactionsRoute
  "/finance/transactions" -> Just defaultTransactionsRoute
  "finance/transactions" -> Just defaultTransactionsRoute
  "/finance/reports" -> Just FinanceReports
  "finance/reports" -> Just FinanceReports
  "/admin" -> Just Admin
  "admin" -> Just Admin
  "/signup" -> Just Signup
  "signup" -> Just Signup
  "/signin" -> Just Signin
  "signin" -> Just Signin
  _ -> Nothing

printRoute :: Route -> String
printRoute = case _ of
  Root -> "/"
  Route (Calendar calendarRoute) ->
    let
      dayQuery = map (\day -> "day=" <> day) calendarRoute.day
      itemQuery = map (\item -> "item=" <> item) calendarRoute.item
      queryParts = mapMaybe identity [ dayQuery, itemQuery ]
      querySuffix =
        if length queryParts == 0 then
          ""
        else
          "?" <> StringCommon.joinWith "&" queryParts
    in
      printDefinedRoutePath defaultCalendarRoute <> querySuffix
  Route (FinanceTransactions transactionsRoute) ->
    let
      accountIdQuery = map (\accountId -> "accountId=" <> accountId) transactionsRoute.accountId
      fromQuery = map (\fromValue -> "from=" <> fromValue) transactionsRoute.from
      toQuery = map (\toValue -> "to=" <> toValue) transactionsRoute.to
      queryParts = mapMaybe identity [ accountIdQuery, fromQuery, toQuery ]
      querySuffix =
        if length queryParts == 0 then
          ""
        else
          "?" <> StringCommon.joinWith "&" queryParts
    in
      printDefinedRoutePath defaultTransactionsRoute <> querySuffix
  Route route ->
    printDefinedRoutePath route
  NotFound -> "/not-found"

parseCalendarDay :: String -> Maybe String
parseCalendarDay rawQuery =
  findMapQueryValue "day" rawQuery >>= \value ->
    if DateTime.isLocalDate value then Just value else Nothing

parseCalendarItem :: String -> Maybe String
parseCalendarItem rawQuery =
  findMapQueryValue "item" rawQuery >>= nonEmptyStringMaybe

parseTransactionsAccountId :: String -> Maybe String
parseTransactionsAccountId rawQuery =
  findMapQueryValue "accountId" rawQuery >>= nonEmptyStringMaybe

parseTransactionsFrom :: String -> Maybe String
parseTransactionsFrom rawQuery =
  findMapQueryValue "from" rawQuery >>= nonEmptyStringMaybe

parseTransactionsTo :: String -> Maybe String
parseTransactionsTo rawQuery =
  findMapQueryValue "to" rawQuery >>= nonEmptyStringMaybe

nonEmptyStringMaybe :: String -> Maybe String
nonEmptyStringMaybe raw =
  if raw == "" then Nothing else Just raw

findMapQueryValue :: String -> String -> Maybe String
findMapQueryValue key rawQuery =
  find isMatchingPair queryPairs >>= valueFromPair
  where
  queryPairs = StringCommon.split (Pattern "&") rawQuery
  isMatchingPair pair =
    case head (StringCommon.split (Pattern "=") pair) of
      Just pairKey -> pairKey == key
      Nothing -> false
  valueFromPair pair =
    case StringCommon.split (Pattern "=") pair of
      [ _, pairValue ] -> Just pairValue
      _ -> Nothing

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

shouldCanonicalizeFinanceRoute :: Route -> Effect Boolean
shouldCanonicalizeFinanceRoute = case _ of
  Route (FinanceTransactions _) -> do
    win <- window
    location <- Window.location win
    pathname <- Location.pathname location
    pure (pathname == "/finance")
  _ ->
    pure false

data Action
  = RouteChanged Route
  | NavigateTo DefinedRoute
  | GlobalPopState
  | FinanceCreateClicked
  | FinanceCreateExpenseSelected
  | FinanceCreateIncomeSelected
  | FinanceCreateAccountChanged String
  | FinanceCreateAmountChanged String
  | FinanceCreateOccurredAtChanged String
  | SubmitFinanceCreate
  | FinanceCreateAccountsLoaded (Either String (Array FinanceAccount))
  | FinanceCreateSubmitted (Either String Unit)
  | DismissFinanceToast
  | CloseFinanceOverlay
  | NavigateToLateItem String String
  | OpenLateItemQuickComplete LateItems.LateItem
  | UpdateLateItemQuickCompleteMinutes String
  | ConfirmLateItemQuickComplete
  | CancelLateItemQuickComplete
  | HandleCalendarOutput Calendar.CalendarRouteOutput
  | HandleTransactionsOutput FinanceTransactions.Output
  | LoadLateItems
  | LateItemsLoaded Int (Array LateItems.LateItem)
  | LateItemsLoadFailed Int String
  | OpenLateItemsSheet
  | CloseLateItemsSheet
  | LoadMoreLateItems
  | SignOut
  | HandleAuthOutput AuthOutput
  | RefreshAuthStatus
  | InitializeRouting

type LateItemsQuickCompleteState =
  { itemId :: String
  , minutesInput :: String
  , validationError :: Maybe String
  , submitError :: Maybe String
  , isSubmitting :: Boolean
  }

type LateItemsState =
  { isLoading :: Boolean
  , loadError :: Maybe String
  , items :: Array LateItems.LateItem
  , visibleLimit :: Int
  , isSheetOpen :: Boolean
  , quickCompletePrompt :: Maybe LateItemsQuickCompleteState
  , nextRequestId :: Int
  , activeRequestId :: Maybe Int
  }

type State =
  { currentRoute :: Route
  , nav :: Maybe PushStateInterface
  , authStatus :: AuthStatus
  , financeOverlay :: Maybe FinanceOverlay
  , financeOverlayScrollY :: Maybe Int
  , financeCreateState :: Maybe FinanceCreateState
  , financeDetailSnapshot :: Maybe FinanceTransactions.FinanceDetailSnapshot
  , financeToastMessage :: Maybe String
  , financeToastToken :: Int
  , financeIsMobile :: Boolean
  , lateItems :: LateItemsState
  }

type FinanceCreateState =
  { launch :: FinanceCreateLaunch
  , accounts :: Array FinanceAccount
  , isLoadingAccounts :: Boolean
  , accountId :: String
  , amountInput :: String
  , occurredAtInput :: String
  , accountError :: Maybe String
  , amountError :: Maybe String
  , occurredAtError :: Maybe String
  , submitError :: Maybe String
  , isSubmitting :: Boolean
  , submitKeyNonce :: Int
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
  , financeOverlay: Nothing
  , financeOverlayScrollY: Nothing
  , financeCreateState: Nothing
  , financeDetailSnapshot: Nothing
  , financeToastMessage: Nothing
  , financeToastToken: 0
  , financeIsMobile: false
  , lateItems: initialLateItemsState
  }

initialLateItemsState :: LateItemsState
initialLateItemsState =
  { isLoading: false
  , loadError: Nothing
  , items: []
  , visibleLimit: 50
  , isSheetOpen: false
  , quickCompletePrompt: Nothing
  , nextRequestId: 1
  , activeRequestId: Nothing
  }

beginLateItemsRequest :: LateItemsState -> { requestId :: Int, lateItems :: LateItemsState }
beginLateItemsRequest lateItems =
  let
    requestId = lateItems.nextRequestId
  in
    { requestId
    , lateItems:
        lateItems
          { isLoading = true
          , loadError = Nothing
          , nextRequestId = requestId + 1
          , activeRequestId = Just requestId
          }
    }

applyLateItemsLoaded :: Int -> Array LateItems.LateItem -> LateItemsState -> LateItemsState
applyLateItemsLoaded requestId items lateItems =
  if lateItems.activeRequestId == Just requestId then
    lateItems
      { isLoading = false
      , loadError = Nothing
      , items = items
      , visibleLimit = 50
      , isSheetOpen = lateItems.isSheetOpen
      , activeRequestId = Nothing
      }
  else
    lateItems

applyLateItemsLoadFailed :: Int -> String -> LateItemsState -> LateItemsState
applyLateItemsLoadFailed requestId message lateItems =
  if lateItems.activeRequestId == Just requestId then
    lateItems
      { isLoading = false
      , loadError = Just message
      , visibleLimit = 50
      , isSheetOpen = lateItems.isSheetOpen
      , activeRequestId = Nothing
      }
  else
    lateItems

shouldRefreshLateItemsForRoute :: AuthStatus -> Route -> Boolean
shouldRefreshLateItemsForRoute authStatus route =
  case resolveGuardedRoute authStatus route of
    Just (Route definedRoute) -> canRenderLateItemsReminder authStatus definedRoute
    _ -> false

isFinanceRoute :: DefinedRoute -> Boolean
isFinanceRoute = case _ of
  FinanceTransactions _ -> true
  FinanceReports -> true
  _ -> false

financeLocalPrimaryRoute :: DefinedRoute -> Maybe DefinedRoute
financeLocalPrimaryRoute route
  | isFinanceRoute route = Just route
  | otherwise = Nothing

shouldShowFinanceCreateButton :: DefinedRoute -> Boolean
shouldShowFinanceCreateButton = case _ of
  FinanceTransactions _ -> true
  _ -> false

isFinanceOverlayOpen :: Maybe FinanceOverlay -> Boolean
isFinanceOverlayOpen = case _ of
  Nothing -> false
  Just _ -> true

shouldRenderFinanceOverlay :: DefinedRoute -> Maybe FinanceOverlay -> Boolean
shouldRenderFinanceOverlay route financeOverlay =
  isFinanceRoute route && isFinanceOverlayOpen financeOverlay

historyState :: Foreign
historyState = unsafeToForeign unit

navigateWith
  :: (PushStateInterface -> Foreign -> String -> Effect Unit)
  -> Maybe PushStateInterface
  -> Route
  -> H.HalogenM State Action ChildSlots Void Aff Unit
navigateWith navFn maybeNav route = maybe (pure unit) (\nav -> liftEffect $ navFn nav historyState (printRoute route)) maybeNav

statusOk :: forall a. Response a -> Boolean
statusOk r = unwrap r.status >= 200 && unwrap r.status < 300

resolveGuardedRoute :: AuthStatus -> Route -> Maybe Route
resolveGuardedRoute authStatus route = case route of
  Route (FinanceTransactions _) ->
    case authStatus of
      AuthUnknown -> Nothing
      Authenticated _ -> Just route
      _ -> Just NotFound
  Route FinanceReports ->
    case authStatus of
      AuthUnknown -> Nothing
      Authenticated _ -> Just route
      _ -> Just NotFound
  Route Admin ->
    case authStatus of
      AuthUnknown -> Nothing
      Authenticated profile | isAdminProfile profile -> Just route
      _ -> Just NotFound
  _ -> Just route

visibleTabs :: AuthStatus -> Array DefinedRoute
visibleTabs authStatus =
  [ Note, Checklist, defaultCalendarRoute ]
    <> guard (canViewFinanceNav authStatus) [ defaultTransactionsRoute ]
    <> guard (canViewAdminNav authStatus) [ Admin ]
  where
  canViewFinanceNav (Authenticated _) = true
  canViewFinanceNav _ = false
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
  let route = routeFromDefined Note
  modify_ _ { currentRoute = route, financeOverlay = Nothing, financeOverlayScrollY = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing }
  navigateWith _.replaceState st.nav route
  when (shouldRefreshLateItemsForRoute st.authStatus route) (handleAction LoadLateItems)
handleAction (RouteChanged route) = do
  st <- get
  modify_ _
    { currentRoute = route
    , financeOverlay = financeOverlayAfterRouteChange route st.financeOverlay
    , financeOverlayScrollY = financeOverlayScrollYAfterRouteChange route st.financeOverlay
    , financeCreateState = financeCreateStateAfterRouteChange route st.financeOverlay st.financeCreateState
    , financeDetailSnapshot = financeDetailSnapshotAfterRouteChange route st.financeOverlay st.financeDetailSnapshot
    }
  shouldNormalize <- liftEffect (shouldCanonicalizeFinanceRoute route)
  when shouldNormalize $
    navigateWith _.replaceState st.nav route
  when (shouldRefreshLateItemsForRoute st.authStatus route) (handleAction LoadLateItems)
handleAction (NavigateTo route) = do
  st <- get
  let nextRoute = routeFromDefined route
  modify_ _ { currentRoute = nextRoute, financeOverlay = Nothing, financeOverlayScrollY = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing }
  navigateWith _.pushState st.nav nextRoute
handleAction GlobalPopState =
  closeFinanceOverlayFromBrowserBack
handleAction FinanceCreateClicked =
  openFinanceCreateOverlay
handleAction FinanceCreateExpenseSelected =
  openFinanceCreateFromIntent FinanceCreateExpense
handleAction FinanceCreateIncomeSelected =
  openFinanceCreateFromIntent FinanceCreateIncome
handleAction (FinanceCreateAccountChanged raw) =
  modify_ \st ->
    st
      { financeCreateState =
          map
            (_ { accountId = raw, accountError = Nothing, submitError = Nothing })
            st.financeCreateState
      }
handleAction (FinanceCreateAmountChanged raw) =
  modify_ \st ->
    st
      { financeCreateState =
          map
            (_ { amountInput = raw, amountError = Nothing, submitError = Nothing })
            st.financeCreateState
      }
handleAction (FinanceCreateOccurredAtChanged raw) =
  modify_ \st ->
    st
      { financeCreateState =
          map
            (_ { occurredAtInput = raw, occurredAtError = Nothing, submitError = Nothing })
            st.financeCreateState
      }
handleAction SubmitFinanceCreate = do
  st <- get
  case st.financeCreateState of
    Nothing -> pure unit
    Just createState ->
      if createState.isSubmitting then
        pure unit
      else do
        let accountError = if createState.accountId == "" then Just "Account is required." else Nothing
        let amountError = validateFinanceAmount createState.amountInput
        let occurredAtError = if DateTime.isLocalDateTime createState.occurredAtInput then Nothing else Just "Occurred-at date/time is required."
        modify_ \next ->
          next
            { financeCreateState =
                map
                  ( _
                      { accountError = accountError
                      , amountError = amountError
                      , occurredAtError = occurredAtError
                      , submitError = Nothing
                      }
                  )
                  next.financeCreateState
            }
        if accountError /= Nothing || amountError /= Nothing || occurredAtError /= Nothing then
          pure unit
        else do
          let idempotencyKey = buildFinanceIdempotencyKey createState
          modify_ \next ->
            next
              { financeCreateState =
                  map
                    ( _
                        { isSubmitting = true
                        , submitError = Nothing
                        , submitKeyNonce = createState.submitKeyNonce + 1
                        }
                    )
                    next.financeCreateState
              }
          result <- liftAff (submitFinanceCreate idempotencyKey createState)
          handleAction (FinanceCreateSubmitted result)
handleAction (FinanceCreateAccountsLoaded result) =
  modify_ \st ->
    st
      { financeCreateState =
          map
            ( \createState ->
                case result of
                  Left message ->
                    createState { isLoadingAccounts = false, submitError = Just message }
                  Right accounts ->
                    createState { isLoadingAccounts = false, accounts = accounts }
            )
            st.financeCreateState
      }
handleAction (FinanceCreateSubmitted result) = do
  st <- get
  case st.financeCreateState of
    Nothing -> pure unit
    Just _ ->
      case result of
        Left message ->
          modify_ \next ->
            next
              { financeCreateState =
                  map
                    (_ { isSubmitting = false, submitError = Just message })
                    next.financeCreateState
              }
        Right _ ->
          if st.financeIsMobile then do
            modify_ \next ->
              next
                { financeOverlay = Nothing
                , financeCreateState = Nothing
                , financeDetailSnapshot = Nothing
                , financeToastMessage = Just "Transaction saved."
                , financeToastToken = next.financeToastToken + 1
                }
            latestAfterToast <- get
            let token = latestAfterToast.financeToastToken
            liftAff $ delay (Milliseconds 2500.0)
            latest <- get
            when (latest.financeToastToken == token) $
              handleAction DismissFinanceToast
          else
            modify_ \next ->
              next
                { financeCreateState =
                    map
                      ( _
                          { isSubmitting = false
                          , amountInput = ""
                          , accountError = Nothing
                          , amountError = Nothing
                          , occurredAtError = Nothing
                          , submitError = Nothing
                          }
                      )
                      next.financeCreateState
                }
handleAction DismissFinanceToast =
  modify_ _ { financeToastMessage = Nothing }
handleAction CloseFinanceOverlay = do
  consumeFinanceOverlayBackNavigation
  closeFinanceOverlayAndRestoreScroll
handleAction (NavigateToLateItem day itemId) = do
  st <- get
  let
    nextRoute =
      routeFromDefined
        ( Calendar
            ( normalizeCalendarRouteState
                { day: Just day
                , item: Just itemId
                }
            )
        )
  modify_ \nextState ->
    nextState
      { currentRoute = nextRoute
      , lateItems = nextState.lateItems { isSheetOpen = false, quickCompletePrompt = Nothing }
      }
  navigateWith _.pushState st.nav nextRoute
handleAction (OpenLateItemQuickComplete lateItem) =
  case lateItem.id of
    Nothing -> pure unit
    Just itemId -> do
      let
        normalized = LateItems.normalizeQuickCompleteDurationMinutes lateItem.plannedDurationMinutes
      when normalized.wasRounded
        ( liftEffect
            ( Console.warn
                ( "Planned duration was not a multiple of 5 for late item "
                    <> itemId
                    <> "; rounded to nearest multiple."
                )
            )
        )
      modify_ \st ->
        st
          { lateItems =
              st.lateItems
                { quickCompletePrompt =
                    Just
                      { itemId
                      , minutesInput: show normalized.value
                      , validationError: Nothing
                      , submitError: Nothing
                      , isSubmitting: false
                      }
                }
          }
handleAction (UpdateLateItemQuickCompleteMinutes raw) =
  modify_ \st ->
    st
      { lateItems =
          st.lateItems
            { quickCompletePrompt =
                map
                  ( _
                      { minutesInput = raw
                      , validationError = Nothing
                      , submitError = Nothing
                      }
                  )
                  st.lateItems.quickCompletePrompt
            }
      }
handleAction ConfirmLateItemQuickComplete = do
  st <- get
  case st.lateItems.quickCompletePrompt of
    Nothing -> pure unit
    Just prompt ->
      case LateItems.validateQuickCompleteDurationInput prompt.minutesInput of
        Left validationError ->
          modify_ \nextState ->
            nextState
              { lateItems =
                  nextState.lateItems
                    { quickCompletePrompt =
                        map
                          (_ { validationError = Just validationError, submitError = Nothing })
                          nextState.lateItems.quickCompletePrompt
                    }
              }
        Right minutes ->
          case find (\item -> item.id == Just prompt.itemId) st.lateItems.items of
            Nothing ->
              modify_ \nextState ->
                nextState
                  { lateItems =
                      nextState.lateItems
                        { quickCompletePrompt =
                            map
                              (_ { isSubmitting = false, submitError = Just "Impossible de retrouver l'item en retard." })
                              nextState.lateItems.quickCompletePrompt
                        }
                  }
            Just lateItem ->
              case LateItems.buildQuickCompleteUpdatedItem minutes lateItem of
                Nothing ->
                  modify_ \nextState ->
                    nextState
                      { lateItems =
                          nextState.lateItems
                            { quickCompletePrompt =
                                map
                                  (_ { isSubmitting = false, submitError = Just "Impossible de preparer la mise a jour de l'item en retard." })
                                  nextState.lateItems.quickCompletePrompt
                            }
                      }
                Just updatedItemPayload -> do
                  modify_ \nextState ->
                    nextState
                      { lateItems =
                          nextState.lateItems
                            { quickCompletePrompt =
                                map
                                  (_ { isSubmitting = true, validationError = Nothing, submitError = Nothing })
                                  nextState.lateItems.quickCompletePrompt
                            }
                      }
                  result <- liftAff $ updateItemResponse prompt.itemId updatedItemPayload
                  case result of
                    Left err ->
                      modify_ \nextState ->
                        nextState
                          { lateItems =
                              nextState.lateItems
                                { quickCompletePrompt =
                                    map
                                      ( _
                                          { isSubmitting = false
                                          , submitError = Just ("Impossible de mettre a jour l'item en retard: " <> printError err)
                                          }
                                      )
                                      nextState.lateItems.quickCompletePrompt
                                }
                          }
                    Right response ->
                      if statusOk response then
                        case decodeJson response.body of
                          Left _ ->
                            modify_ \nextState ->
                              nextState
                                { lateItems =
                                    nextState.lateItems
                                      { quickCompletePrompt =
                                          map
                                            ( _
                                                { isSubmitting = false
                                                , submitError = Just "Mise a jour reussie mais reponse serveur inexploitable."
                                                }
                                            )
                                            nextState.lateItems.quickCompletePrompt
                                      }
                                }
                          Right updatedItem ->
                            case updatedItem of
                              Calendar.ServerCalendarItem _ -> do
                                now <- liftEffect nowDateTime
                                modify_ \nextState ->
                                  nextState
                                    { lateItems =
                                        nextState.lateItems
                                          { items = LateItems.applyQuickCompleteUpdatedItem now updatedItem nextState.lateItems.items
                                          , quickCompletePrompt = Nothing
                                          }
                                    }
                              _ ->
                                modify_ \nextState ->
                                  nextState
                                    { lateItems =
                                        nextState.lateItems
                                          { quickCompletePrompt =
                                              map
                                                ( _
                                                    { isSubmitting = false
                                                    , submitError = Just "Mise a jour reussie mais item serveur non retourne."
                                                    }
                                                )
                                                nextState.lateItems.quickCompletePrompt
                                          }
                                    }
                      else
                        modify_ \nextState ->
                          nextState
                            { lateItems =
                                nextState.lateItems
                                  { quickCompletePrompt =
                                      map
                                        ( _
                                            { isSubmitting = false
                                            , submitError = Just ("Impossible de mettre a jour l'item en retard (HTTP " <> show (unwrap response.status) <> ").")
                                            }
                                        )
                                        nextState.lateItems.quickCompletePrompt
                                  }
                            }
handleAction CancelLateItemQuickComplete =
  modify_ \st ->
    st { lateItems = st.lateItems { quickCompletePrompt = Nothing } }
handleAction (HandleCalendarOutput (Calendar.RouteSyncRequested { viewMode, focusDate })) = do
  st <- get
  let
    nextRoute =
      case viewMode of
        Calendar.ViewDay ->
          Route (Calendar (normalizeCalendarRouteState { day: Just focusDate, item: Nothing }))
        _ ->
          Route defaultCalendarRoute
  when (st.currentRoute /= nextRoute) do
    modify_ _ { currentRoute = nextRoute }
    navigateWith _.pushState st.nav nextRoute
handleAction (HandleTransactionsOutput (FinanceTransactions.RouteSyncRequested transactionsRoute)) = do
  st <- get
  case st.currentRoute of
    Route (FinanceTransactions currentTransactionsRoute) -> do
      let nextTransactionsRoute = normalizeTransactionsRouteState transactionsRoute
      when (currentTransactionsRoute /= nextTransactionsRoute) do
        let nextRoute = Route (FinanceTransactions nextTransactionsRoute)
        modify_ _ { currentRoute = nextRoute }
        navigateWith _.pushState st.nav nextRoute
    _ ->
      pure unit
handleAction (HandleTransactionsOutput (FinanceTransactions.OpenTransactionDetail detailSnapshot)) = do
  state <- get
  when (routeCanOpenFinanceOverlay state.currentRoute) do
    scrollY <- liftEffect WindowScroll.getWindowScrollY
    armFinanceOverlayBackNavigation
    modify_ _
      { financeOverlay = Just (FinanceDetailOverlay detailSnapshot.transactionId)
      , financeOverlayScrollY = Just scrollY
      , financeDetailSnapshot = Just detailSnapshot
      }
handleAction (HandleAuthOutput (SignupSucceeded username)) = do
  st <- get
  liftEffect $ AuthSession.storeAuthenticatedUsername username
  let route = routeFromDefined Signin
  modify_ _ { currentRoute = route, authStatus = Unauthenticated, financeOverlay = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing }
  navigateWith _.pushState st.nav route
handleAction (HandleAuthOutput (SigninSucceeded profile@(AuthenticatedProfile { username }))) = do
  st <- get
  liftEffect $ AuthSession.storeAuthenticatedUsername username
  let route = routeFromDefined Note
  modify_ _ { currentRoute = route, authStatus = Authenticated profile, financeOverlay = Nothing, financeOverlayScrollY = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing }
  navigateWith _.pushState st.nav route
handleAction SignOut = do
  st <- get
  _ <- liftAff $ post string "/api/signout" Nothing
  liftEffect AuthSession.clearAuthenticatedUsername
  let route = routeFromDefined Signin
  modify_ _ { authStatus = Unauthenticated, currentRoute = route, financeOverlay = Nothing, financeOverlayScrollY = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing }
  navigateWith _.pushState st.nav route
handleAction InitializeRouting = do
  nav <- liftEffect makeInterface
  isMobile <- liftEffect isMobileViewport
  modify_ _ { nav = Just nav }
  modify_ _ { financeIsMobile = isMobile }
  subscribeToRouting nav
  subscribeToGlobalPopState
  handleAction RefreshAuthStatus
  handleAction LoadLateItems
handleAction RefreshAuthStatus = do
  authStatus <- loadProfileAuthStatus
  case authStatus of
    Authenticated (AuthenticatedProfile { username }) ->
      liftEffect $ AuthSession.storeAuthenticatedUsername username
    _ ->
      liftEffect AuthSession.clearAuthenticatedUsername
  modify_ _ { authStatus = authStatus }
handleAction LoadLateItems = do
  st <- get
  when (shouldRefreshLateItemsForRoute st.authStatus st.currentRoute) do
    let
      transition = beginLateItemsRequest st.lateItems
      requestId = transition.requestId
    modify_ _ { lateItems = transition.lateItems }
    result <- liftAff getItemsResponse
    case result of
      Left err ->
        handleAction (LateItemsLoadFailed requestId (printError err))
      Right response ->
        case Calendar.decodeCalendarItemsResponse response of
          Left _ ->
            handleAction (LateItemsLoadFailed requestId "Impossible de charger les items en retard.")
          Right items -> do
            now <- liftEffect nowDateTime
            handleAction (LateItemsLoaded requestId (LateItems.deriveLateItems now items))
handleAction (LateItemsLoaded requestId items) =
  modify_ \st -> st { lateItems = applyLateItemsLoaded requestId items st.lateItems }
handleAction (LateItemsLoadFailed requestId message) =
  modify_ \st -> st { lateItems = applyLateItemsLoadFailed requestId message st.lateItems }
handleAction OpenLateItemsSheet =
  modify_ \st -> st { lateItems = st.lateItems { isSheetOpen = true } }
handleAction CloseLateItemsSheet =
  modify_ \st -> st { lateItems = st.lateItems { isSheetOpen = false, quickCompletePrompt = Nothing } }
handleAction LoadMoreLateItems =
  modify_ \st ->
    st { lateItems = st.lateItems { visibleLimit = st.lateItems.visibleLimit + 50 } }

render :: State -> H.ComponentHTML Action ChildSlots Aff
render { currentRoute, authStatus, financeOverlay, lateItems, financeIsMobile, financeCreateState, financeDetailSnapshot, financeToastMessage } =
  case resolveGuardedRoute authStatus currentRoute of
    Nothing -> renderLoading authStatus
    Just (Route route) ->
      div [ class_ "container" ]
        ( fold
            [ [ h1 [ class_ "text-center" ] [ text "FAVS" ]
              , authMenu authStatus
              , renderFinanceToast financeToastMessage
              , renderLateItemsChip authStatus route lateItems
              ]
            , if route /= Signup && route /= Signin then [ nav [ class_ "row nav nav-tabs" ] (map (\tabRoute -> tab tabRoute route) (visibleTabs authStatus)) ] else []
            , [ currentComponent authStatus financeOverlay route ]
            , [ renderLateItemsSheet authStatus route lateItems ]
            , [ renderFinanceOverlay route financeOverlay financeIsMobile financeCreateState financeDetailSnapshot ]
            ]
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

canRenderLateItemsReminder :: AuthStatus -> DefinedRoute -> Boolean
canRenderLateItemsReminder authStatus route =
  case authStatus of
    Authenticated _ ->
      route /= Signup && route /= Signin
    _ ->
      false

renderLateItemsChip :: forall w. AuthStatus -> DefinedRoute -> LateItemsState -> HTML w Action
renderLateItemsChip authStatus route lateItems =
  if canRenderLateItemsReminder authStatus route && length lateItems.items > 0 then
    div [ class_ "app-late-items-chip-wrapper mb-2" ]
      [ button
          [ class_ "btn btn-sm app-late-items-chip"
          , onClick (const OpenLateItemsSheet)
          ]
          [ span [ class_ "app-late-items-chip__label" ] [ text "Items en retard" ]
          , span [ class_ "app-late-items-chip__count" ] [ text (show (length lateItems.items)) ]
          ]
      ]
  else
    text ""

renderLateItemsSheet :: forall w. AuthStatus -> DefinedRoute -> LateItemsState -> HTML w Action
renderLateItemsSheet authStatus route lateItems =
  if canRenderLateItemsReminder authStatus route && lateItems.isSheetOpen then
    Modal.renderBottomSheet "Items en retard"
      [ renderLateItemsSheetBody lateItems ]
      CloseLateItemsSheet
  else
    text ""

renderLateItemsSheetBody :: forall w. LateItemsState -> HTML w Action
renderLateItemsSheetBody lateItems
  | lateItems.isLoading =
      div [ class_ "app-late-items-sheet app-late-items-sheet--loading" ]
        [ text "Chargement..." ]
  | otherwise =
      div [ class_ "app-late-items-sheet" ]
        [ maybe (text "") (\message -> div [ class_ "app-late-items-sheet__error alert alert-danger" ] [ text message ]) lateItems.loadError
        , if length lateItems.items == 0 then
            div [ class_ "app-late-items-sheet__empty text-muted" ] [ text "Aucun item en retard." ]
          else
            ul [ class_ "app-late-items-list list-group" ]
              ( map (\item -> renderLateItemsRow lateItems.quickCompletePrompt item)
                  (LateItems.visibleLateItems lateItems.visibleLimit lateItems.items)
              )
        , if LateItems.hasMoreLateItems lateItems.visibleLimit lateItems.items then
            div [ class_ "app-late-items-sheet__more" ]
              [ button
                  [ class_ "btn btn-sm btn-outline-secondary app-late-items-load-more"
                  , onClick (const LoadMoreLateItems)
                  ]
                  [ text "Afficher plus" ]
              ]
          else
            text ""
        ]

renderLateItemsRow :: forall w. Maybe LateItemsQuickCompleteState -> LateItems.LateItem -> HTML w Action
renderLateItemsRow quickCompletePrompt lateItem =
  case lateItem.id of
    Just itemId ->
      let
        isPromptOpen =
          maybe false (\prompt -> prompt.itemId == itemId) quickCompletePrompt
      in
        li [ class_ "list-group-item app-late-items-row" ]
          [ div [ class_ "d-flex align-items-start justify-content-between gap-2" ]
              [ button
                  [ class_ "btn btn-link text-start text-decoration-none p-0 flex-grow-1 app-late-items-row__navigate"
                  , onClick (const (NavigateToLateItem lateItem.day itemId))
                  ]
                  [ div [ class_ "app-late-items-row__title" ] [ text lateItem.title ]
                  , div [ class_ "app-late-items-row__meta text-muted" ] [ text ("Termine le " <> lateItem.endDisplay) ]
                  ]
              , button
                  [ class_ "btn btn-sm btn-outline-success app-late-items-row__quick-complete"
                  , onClick (const (OpenLateItemQuickComplete lateItem))
                  ]
                  [ text "Valider" ]
              ]
          , if isPromptOpen then
              renderLateItemsQuickCompletePrompt quickCompletePrompt
            else
              text ""
          ]
    Nothing ->
      li [ class_ "list-group-item app-late-items-row" ]
        [ div [ class_ "app-late-items-row__title" ] [ text lateItem.title ]
        , div [ class_ "app-late-items-row__meta text-muted" ] [ text ("Termine le " <> lateItem.endDisplay) ]
        ]

renderLateItemsQuickCompletePrompt :: forall w. Maybe LateItemsQuickCompleteState -> HTML w Action
renderLateItemsQuickCompletePrompt quickCompletePrompt =
  case quickCompletePrompt of
    Nothing -> text ""
    Just prompt ->
      div [ class_ "app-late-items-row__quick-complete-form mt-2" ]
        [ div [ class_ "d-flex flex-column gap-2" ]
            [ label [ class_ "form-label mb-0" ] [ text "Durée réelle (minutes)" ]
            , input
                [ class_ "form-control form-control-sm app-late-items-row__quick-complete-input"
                , type_ InputNumber
                , value prompt.minutesInput
                , onValueChange UpdateLateItemQuickCompleteMinutes
                ]
            , maybe (text "") (\message -> div [ class_ "text-danger small app-late-items-row__quick-complete-validation" ] [ text message ]) prompt.validationError
            , maybe (text "") (\message -> div [ class_ "text-danger small app-late-items-row__quick-complete-error" ] [ text message ]) prompt.submitError
            , div [ class_ "d-flex gap-2 justify-content-end" ]
                [ button
                    [ class_ "btn btn-sm btn-outline-secondary app-late-items-row__quick-complete-cancel"
                    , onClick (const CancelLateItemQuickComplete)
                    , disabled prompt.isSubmitting
                    ]
                    [ text "Annuler" ]
                , button
                    [ class_ "btn btn-sm btn-primary app-late-items-row__quick-complete-confirm"
                    , onClick (const ConfirmLateItemQuickComplete)
                    , disabled prompt.isSubmitting
                    ]
                    [ text (if prompt.isSubmitting then "Validation..." else "Valider") ]
                ]
            ]
        ]

currentComponent :: AuthStatus -> Maybe FinanceOverlay -> DefinedRoute -> H.ComponentHTML Action ChildSlots Aff
currentComponent _ _ Note = slot_ (Proxy :: _ "notes") unit Notes.component unit
currentComponent _ _ Checklist = slot_ (Proxy :: _ "checklists") unit Checklists.component unit
currentComponent _ _ (Calendar calendarRoute) =
  slot
    (Proxy :: _ "calendar")
    unit
    Calendar.component
    { initialDay: calendarRoute.day
    , initialItemId: calendarRoute.item
    }
    HandleCalendarOutput
currentComponent _ financeOverlay (FinanceTransactions transactionsRoute) = renderFinanceShell (FinanceTransactions transactionsRoute) financeOverlay
currentComponent _ financeOverlay FinanceReports = renderFinanceShell FinanceReports financeOverlay
currentComponent (Authenticated (AuthenticatedProfile { username })) _ Admin =
  slot_ (Proxy :: _ "admin") unit Admin.component { currentUsername: username }
currentComponent _ _ Admin = text ""
currentComponent _ _ Signup = slot (Proxy :: _ "signup") unit signupComponent unit HandleAuthOutput
currentComponent _ _ Signin = slot (Proxy :: _ "signin") unit signinComponent unit HandleAuthOutput

renderFinanceShell :: DefinedRoute -> Maybe FinanceOverlay -> H.ComponentHTML Action ChildSlots Aff
renderFinanceShell route financeOverlay =
  div [ class_ "finance-shell py-3" ]
    [ div [ class_ "finance-shell__header d-flex justify-content-between align-items-center gap-3 mb-3 flex-wrap" ]
        [ nav [ class_ "finance-shell__nav nav nav-pills gap-2" ]
            (map (\tabRoute -> financeLocalTab tabRoute route) [ FinanceTransactions transactionsRoute, FinanceReports ])
        , if shouldShowFinanceCreateButton route then
            div [ class_ "finance-shell__actions" ]
              [ CreateButton.renderIconCreateButton "btn btn-primary finance-shell__create-btn" "Nouvelle transaction" FinanceCreateClicked
              , renderInlineCreateChooser route financeOverlay
              ]
          else
            text ""
        ]
    , if routeKey route == routeKey (FinanceTransactions transactionsRoute) then
        slot
          (Proxy :: _ "financeTransactions")
          unit
          FinanceTransactions.component
          transactionsRoute
          HandleTransactionsOutput
      else
        renderFinancePlaceholderBody route
    ]
  where
  transactionsRoute =
    case route of
      FinanceTransactions currentRoute -> currentRoute
      _ -> { accountId: Nothing, from: Nothing, to: Nothing }

renderInlineCreateChooser :: forall w. DefinedRoute -> Maybe FinanceOverlay -> HTML w Action
renderInlineCreateChooser route financeOverlay =
  if routeKey route /= "finance-transactions" then
    text ""
  else
    case financeOverlay of
      Just FinanceCreateChooserOverlay ->
        div [ class_ "finance-create-chooser finance-create-chooser--desktop d-grid gap-2 mt-2" ]
          [ button [ class_ "btn btn-outline-danger finance-create-chooser__expense", onClick (const FinanceCreateExpenseSelected) ] [ text "New Expense" ]
          , button [ class_ "btn btn-outline-success finance-create-chooser__income", onClick (const FinanceCreateIncomeSelected) ] [ text "New Income" ]
          ]
      _ ->
        text ""

financeLocalTab :: forall w. DefinedRoute -> DefinedRoute -> HTML w Action
financeLocalTab tabRoute activeRoute =
  a
    [ class_ $ "nav-link finance-shell__nav-link" <> guard (routeKey tabRoute == routeKey activeRoute) " active"
    , onClick (const $ NavigateTo tabRoute)
    ]
    [ text (financeLocalTabLabel tabRoute) ]

financeLocalTabLabel :: DefinedRoute -> String
financeLocalTabLabel (FinanceTransactions _) = "Transactions"
financeLocalTabLabel FinanceReports = "Reports"
financeLocalTabLabel _ = ""

routeKey :: DefinedRoute -> String
routeKey = case _ of
  FinanceTransactions _ -> "finance-transactions"
  FinanceReports -> "finance-reports"
  _ -> ""

renderFinancePlaceholderBody :: DefinedRoute -> H.ComponentHTML Action ChildSlots Aff
renderFinancePlaceholderBody route =
  div [ class_ "row justify-content-center" ]
    [ div [ class_ "col-12 col-md-10 col-lg-7" ]
        [ div [ class_ "card shadow-sm border-0 finance-route-placeholder" ]
            [ div [ class_ "card-body p-4 text-center" ]
                [ h1 [ class_ "h4 mb-2" ] [ text ("Finance " <> financeLocalTabLabel route) ]
                , div [ class_ "text-muted" ] [ text ("Surface " <> financeLocalTabLabel route <> " prête pour les prochaines stories.") ]
                ]
            ]
        ]
    ]

renderFinanceOverlay :: forall w. DefinedRoute -> Maybe FinanceOverlay -> Boolean -> Maybe FinanceCreateState -> Maybe FinanceTransactions.FinanceDetailSnapshot -> HTML w Action
renderFinanceOverlay route financeOverlay financeIsMobile financeCreateState financeDetailSnapshot =
  if shouldRenderFinanceOverlay route financeOverlay then
    case financeOverlay of
      Just FinanceCreateChooserOverlay ->
        if financeIsMobile then
          Modal.renderBottomSheet "Nouvelle transaction"
            [ div [ class_ "finance-create-chooser d-grid gap-2" ]
                [ button [ class_ "btn btn-outline-danger finance-create-chooser__expense", onClick (const FinanceCreateExpenseSelected) ] [ text "New Expense" ]
                , button [ class_ "btn btn-outline-success finance-create-chooser__income", onClick (const FinanceCreateIncomeSelected) ] [ text "New Income" ]
                ]
            ]
            CloseFinanceOverlay
        else
          text ""
      Just (FinanceCreateOverlay launch) ->
        Modal.renderModalWithActionState "Nouvelle transaction"
          [ renderFinanceCreateForm launch financeCreateState ]
          CloseFinanceOverlay
          { action: SubmitFinanceCreate
          , disabled: fromMaybe true (map _.isSubmitting financeCreateState)
          , label: if fromMaybe false (map _.isSubmitting financeCreateState) then "Saving..." else "Save transaction"
          }
      Just (FinanceDetailOverlay transactionId) ->
        Modal.renderModal "Détail transaction"
          [ renderFinanceDetailOverlayBody transactionId financeDetailSnapshot ]
          CloseFinanceOverlay
          CloseFinanceOverlay
      Nothing ->
        text ""
  else
    text ""

tab :: forall w. DefinedRoute -> DefinedRoute -> HTML w Action
tab tabRoute activeRoute =
  div [ class_ "col text-center nav-item px-0" ]
    [ a
        [ class_ $ "nav-link" <> guard (sameTab tabRoute activeRoute) " active"
        , onClick (const $ NavigateTo tabRoute)
        ]
        [ text (tabLabel tabRoute) ]
    ]

sameTab :: DefinedRoute -> DefinedRoute -> Boolean
sameTab left right =
  case left, right of
    Calendar _, Calendar _ -> true
    FinanceTransactions _, FinanceTransactions _ -> true
    FinanceTransactions _, FinanceReports -> true
    FinanceReports, FinanceTransactions _ -> true
    FinanceReports, FinanceReports -> true
    _, _ -> left == right

subscribeToGlobalPopState :: forall slots output m. MonadEffect m => H.HalogenM State Action slots output m Unit
subscribeToGlobalPopState =
  ModalHistory.subscribeToGlobalPopState GlobalPopState

armFinanceOverlayBackNavigation :: H.HalogenM State Action ChildSlots Void Aff Unit
armFinanceOverlayBackNavigation =
  ModalHistory.armModalBackNavigation (isFinanceOverlayOpen <<< _.financeOverlay)

consumeFinanceOverlayBackNavigation :: H.HalogenM State Action ChildSlots Void Aff Unit
consumeFinanceOverlayBackNavigation =
  ModalHistory.consumeModalBackNavigation (isFinanceOverlayOpen <<< _.financeOverlay)

openFinanceCreateOverlay :: H.HalogenM State Action ChildSlots Void Aff Unit
openFinanceCreateOverlay = do
  state <- get
  when (routeCanOpenFinanceOverlay state.currentRoute) do
    armFinanceOverlayBackNavigation
    isMobile <- liftEffect isMobileViewport
    modify_ _ { financeOverlay = Just FinanceCreateChooserOverlay, financeOverlayScrollY = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing, financeIsMobile = isMobile }

closeFinanceOverlayFromBrowserBack :: H.HalogenM State Action ChildSlots Void Aff Unit
closeFinanceOverlayFromBrowserBack = do
  state <- get
  when (isFinanceOverlayOpen state.financeOverlay) closeFinanceOverlayAndRestoreScroll

routeCanOpenFinanceOverlay :: Route -> Boolean
routeCanOpenFinanceOverlay = case _ of
  Route route -> isFinanceRoute route
  _ -> false

financeOverlayAfterRouteChange :: Route -> Maybe FinanceOverlay -> Maybe FinanceOverlay
financeOverlayAfterRouteChange route financeOverlay =
  if routeCanOpenFinanceOverlay route then
    financeOverlay
  else
    Nothing

financeOverlayScrollYAfterRouteChange :: Route -> Maybe FinanceOverlay -> Maybe Int
financeOverlayScrollYAfterRouteChange _ _ = Nothing

closeFinanceOverlayAndRestoreScroll :: H.HalogenM State Action ChildSlots Void Aff Unit
closeFinanceOverlayAndRestoreScroll = do
  state <- get
  case state.financeOverlay of
    Just (FinanceDetailOverlay _) ->
      case state.financeOverlayScrollY of
        Just scrollY -> do
          liftEffect (WindowScroll.setWindowScrollY scrollY)
          modify_ _ { financeOverlay = Nothing, financeOverlayScrollY = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing }
        Nothing ->
          modify_ _ { financeOverlay = Nothing, financeOverlayScrollY = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing }
    _ ->
      modify_ _ { financeOverlay = Nothing, financeOverlayScrollY = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing }

openFinanceCreateFromIntent :: FinanceCreateIntent -> H.HalogenM State Action ChildSlots Void Aff Unit
openFinanceCreateFromIntent intent = do
  state <- get
  case state.currentRoute of
    Route (FinanceTransactions transactionsRoute) -> do
      let launch = buildFinanceCreateLaunch intent transactionsRoute
      now <- liftEffect nowDateTime
      let nowLocalDateTime = DateTime.formatLocalDateTime now
      let occurredAtInput = defaultOccurredAtInput launch nowLocalDateTime
      modify_ _ { financeOverlay = Just (FinanceCreateOverlay launch), financeOverlayScrollY = Nothing, financeCreateState = Just (initialFinanceCreateState launch occurredAtInput), financeDetailSnapshot = Nothing }
      result <- liftAff fetchFinanceCreateAccounts
      handleAction (FinanceCreateAccountsLoaded result)
    _ ->
      pure unit

buildFinanceCreateLaunch :: FinanceCreateIntent -> TransactionsRouteState -> FinanceCreateLaunch
buildFinanceCreateLaunch intent transactionsRoute =
  { direction:
      case intent of
        FinanceCreateExpense -> "sent"
        FinanceCreateIncome -> "received"
  , accountId: transactionsRoute.accountId
  , occurredAtDaySeed: transactionsRoute.from >>= fromToDaySeed
  }

fromToDaySeed :: String -> Maybe String
fromToDaySeed fromValue =
  case head (StringCommon.split (Pattern "T") fromValue) of
    Just datePart ->
      if DateTime.isLocalDate datePart then Just datePart else Nothing
    Nothing ->
      Nothing

isMobileViewport :: Effect Boolean
isMobileViewport =
  map (_ <= 768) (window >>= Window.innerWidth)

initialFinanceCreateState :: FinanceCreateLaunch -> String -> FinanceCreateState
initialFinanceCreateState launch occurredAtInput =
  { launch
  , accounts: []
  , isLoadingAccounts: true
  , accountId: fromMaybe "" launch.accountId
  , amountInput: ""
  , occurredAtInput
  , accountError: Nothing
  , amountError: Nothing
  , occurredAtError: Nothing
  , submitError: Nothing
  , isSubmitting: false
  , submitKeyNonce: 0
  }

defaultOccurredAtInput :: FinanceCreateLaunch -> String -> String
defaultOccurredAtInput launch nowLocalDateTime =
  fromMaybe nowLocalDateTime (map (_ <> "T12:00") launch.occurredAtDaySeed)

fetchFinanceCreateAccounts :: Aff (Either String (Array FinanceAccount))
fetchFinanceCreateAccounts = do
  response <- getAccounts (FinanceAccountsQuery { status: Just AccountsActive })
  case response of
    Left err ->
      pure (Left ("Unable to load accounts: " <> printError err))
    Right successResponse ->
      if statusOk successResponse then
        pure (lmap show (decodeJson successResponse.body :: Either _ (Array FinanceAccount)))
      else
        pure (Left ("Unable to load accounts: status " <> show (unwrap successResponse.status)))

validateFinanceAmount :: String -> Maybe String
validateFinanceAmount raw =
  if raw == "" then
    Just "Amount is required."
  else
    case Number.fromString raw of
      Nothing -> Just "Amount must be a valid number."
      Just amount ->
        if amount > 0.0 then Nothing else Just "Amount must be strictly positive."

buildFinanceIdempotencyKey :: FinanceCreateState -> String
buildFinanceIdempotencyKey createState =
  fold
    [ "finance-create-"
    , createState.launch.direction
    , "-"
    , createState.accountId
    , "-"
    , createState.occurredAtInput
    , "-"
    , show createState.submitKeyNonce
    ]

submitFinanceCreate :: String -> FinanceCreateState -> Aff (Either String Unit)
submitFinanceCreate idempotencyKey createState =
  case Number.fromString createState.amountInput of
    Nothing ->
      pure (Left "Amount parsing failed.")
    Just amount -> do
      let payload = CreateFinanceTransaction { accountId: createState.accountId, amount, occurredAt: Just createState.occurredAtInput }
      response <- case createState.launch.direction of
        "sent" -> createSentTransaction idempotencyKey payload
        _ -> createReceivedTransaction idempotencyKey payload
      case response of
        Left err ->
          pure (Left ("Unable to save transaction: " <> printError err))
        Right successResponse ->
          if statusOk successResponse then
            pure (Right unit)
          else
            pure (Left ("Unable to save transaction: status " <> show (unwrap successResponse.status)))

renderFinanceCreateForm :: forall w. FinanceCreateLaunch -> Maybe FinanceCreateState -> HTML w Action
renderFinanceCreateForm launch maybeCreateState =
  case maybeCreateState of
    Nothing ->
      div [ class_ "finance-create-overlay text-center text-muted" ] [ text "Preparing create form..." ]
    Just createState ->
      div [ class_ "finance-create-overlay d-flex flex-column gap-2" ]
        [ div [ class_ "finance-create-overlay__direction fw-semibold" ] [ text ("Direction: " <> launch.direction) ]
        , div [ class_ "d-flex flex-column gap-1" ]
            [ label [ class_ "form-label mb-0" ] [ text "Account" ]
            , select
                [ class_ "form-select finance-create-overlay__account-input"
                , value createState.accountId
                , disabled (createState.isSubmitting || createState.isLoadingAccounts)
                , onValueChange FinanceCreateAccountChanged
                ]
                ( [ option [ value "" ] [ text "Select account" ] ]
                    <> map renderAccountOption createState.accounts
                )
            , maybe (text "") (\message -> div [ class_ "text-danger small" ] [ text message ]) createState.accountError
            ]
        , div [ class_ "d-flex flex-column gap-1" ]
            [ label [ class_ "form-label mb-0" ] [ text "Amount" ]
            , input
                [ class_ "form-control finance-create-overlay__amount-input"
                , type_ InputNumber
                , value createState.amountInput
                , disabled createState.isSubmitting
                , onValueChange FinanceCreateAmountChanged
                ]
            , maybe (text "") (\message -> div [ class_ "text-danger small" ] [ text message ]) createState.amountError
            ]
        , div [ class_ "d-flex flex-column gap-1" ]
            [ label [ class_ "form-label mb-0" ] [ text "Occurred at" ]
            , input
                [ class_ "form-control finance-create-overlay__occurred-at-input"
                , type_ InputDatetimeLocal
                , value createState.occurredAtInput
                , disabled createState.isSubmitting
                , onValueChange FinanceCreateOccurredAtChanged
                ]
            , maybe (text "") (\message -> div [ class_ "text-danger small" ] [ text message ]) createState.occurredAtError
            ]
        , maybe (text "") (\message -> div [ class_ "alert alert-danger mb-0 finance-create-overlay__submit-error" ] [ text message ]) createState.submitError
        ]
  where
  renderAccountOption (FinanceAccount account) =
    option [ value account.id ] [ text account.name ]

renderFinanceDetailOverlayBody :: forall w. String -> Maybe FinanceTransactions.FinanceDetailSnapshot -> HTML w Action
renderFinanceDetailOverlayBody transactionId maybeSnapshot =
  case maybeSnapshot of
    Nothing ->
      div [ class_ "finance-detail-overlay d-flex flex-column gap-2" ]
        [ div [ class_ "alert alert-warning mb-0" ] [ text "Transaction unavailable in current snapshot." ]
        , div [ class_ "small text-muted" ] [ text ("Requested transaction: " <> transactionId) ]
        ]
    Just { transaction: FinanceTransaction tx, accountLabel } ->
      div [ class_ "finance-detail-overlay d-flex flex-column gap-3" ]
        [ div [ class_ "d-flex flex-column gap-1" ]
            [ div [ class_ "fw-semibold" ] [ text "Core facts" ]
            , div [] [ text ("Id: " <> tx.id) ]
            , div [] [ text ("Amount: " <> show tx.amount) ]
            , div [] [ text ("Direction: " <> financeDirectionLabel tx.direction) ]
            , div [] [ text ("Account: " <> accountLabel) ]
            , div [] [ text ("Occurred at: " <> tx.occurredAt) ]
            , div [] [ text ("Recorded at: " <> tx.recordedAt) ]
            ]
        , div [ class_ "d-flex flex-column gap-1" ]
            [ div [ class_ "fw-semibold" ] [ text "Categorization" ]
            , renderCategorization tx.splits tx.category
            ]
        , div [ class_ "d-flex flex-column gap-1" ]
            [ div [ class_ "fw-semibold" ] [ text "Transfer" ]
            , renderTransfer tx.transfer
            ]
        , div [ class_ "d-flex flex-column gap-1" ]
            [ div [ class_ "fw-semibold" ] [ text "Notes" ]
            , renderNotes tx.notes
            ]
        , div [ class_ "d-flex flex-column gap-1" ]
            [ div [ class_ "fw-semibold" ] [ text "Adjustment" ]
            , renderAdjustment tx.adjustment
            ]
        ]
  where
  renderCategorization splits category =
    if not (null splits) then
      ul [ class_ "mb-0 ps-3" ] (map renderSplit splits)
    else
      div [] [ text (maybe "Uncategorized" (\(FinanceTransactionCategory entry) -> entry.id) category) ]

  renderSplit (FinanceTransactionSplitRow split) =
    li [] [ text (split.category <> ": " <> show split.amount) ]

  renderTransfer maybeTransfer =
    case maybeTransfer of
      Nothing ->
        div [] [ text "No transfer link." ]
      Just (FinanceTransferLink transfer) ->
        div [] [ text ("Linked transaction: " <> transfer.linkedTransactionId <> " (" <> transfer.linkType <> ")") ]

  renderNotes notes =
    if null notes then
      div [] [ text "No notes." ]
    else
      ul [ class_ "mb-0 ps-3" ] (map renderNote notes)

  renderNote (FinanceTransactionNote note) =
    li [] [ text ("#" <> note.id <> ": " <> note.text) ]

  renderAdjustment maybeAdjustment =
    case maybeAdjustment of
      Nothing ->
        div [] [ text "No adjustment context." ]
      Just (FinanceTransactionAdjustment adjustment) ->
        div [] [ text ("Adjustment kind: " <> adjustment.kind) ]

financeDirectionLabel :: FinanceTransactionDirection -> String
financeDirectionLabel = case _ of
  TransactionSent -> "sent"
  TransactionReceived -> "received"

renderFinanceToast :: forall w. Maybe String -> HTML w Action
renderFinanceToast maybeMessage =
  case maybeMessage of
    Nothing -> text ""
    Just message ->
      Toast.render "toast-success" message DismissFinanceToast

financeCreateStateAfterRouteChange :: Route -> Maybe FinanceOverlay -> Maybe FinanceCreateState -> Maybe FinanceCreateState
financeCreateStateAfterRouteChange route financeOverlay financeCreateState =
  if routeCanOpenFinanceOverlay route then
    case financeOverlay of
      Just (FinanceCreateOverlay _) -> financeCreateState
      _ -> Nothing
  else
    Nothing

financeDetailSnapshotAfterRouteChange :: Route -> Maybe FinanceOverlay -> Maybe FinanceTransactions.FinanceDetailSnapshot -> Maybe FinanceTransactions.FinanceDetailSnapshot
financeDetailSnapshotAfterRouteChange route financeOverlay financeDetailSnapshot =
  if routeCanOpenFinanceOverlay route then
    case financeOverlay of
      Just (FinanceDetailOverlay _) -> financeDetailSnapshot
      _ -> Nothing
  else
    Nothing

tabLabel :: DefinedRoute -> String
tabLabel Note = "Notes"
tabLabel Checklist = "Checklists"
tabLabel (Calendar _) = "Calendar"
tabLabel (FinanceTransactions _) = "Finance"
tabLabel FinanceReports = "Finance"
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
