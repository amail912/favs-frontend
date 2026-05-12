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
  , filterTransferCandidates
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
import Api.Finance
  ( categorizeTransaction
  , createReceivedTransaction
  , createSentTransaction
  , createTransactionNote
  , deleteTransactionNote
  , getCounterpartySuggestions
  , getAccounts
  , getCategories
  , getTransactions
  , linkTransfer
  , splitTransaction
  , updateTransactionMetadata
  , updateTransactionNote
  )
import Api.FinanceContract
  ( CategorizeFinanceTransaction(..)
  , CreateFinanceTransaction(..)
  , CreateFinanceTransactionNote(..)
  , FinanceAccount(..)
  , FinanceAccountsQuery(..)
  , FinanceAccountsStatus(..)
  , FinanceCategory(..)
  , FinanceTransaction(..)
  , FinanceReportDirection(..)
  , FinanceTransactionAdjustment(..)
  , FinanceTransactionCategory(..)
  , FinanceTransactionDirection(..)
  , FinanceTransactionNote(..)
  , FinanceCounterpartySuggestion(..)
  , FinanceCounterpartySuggestionsQuery(..)
  , FinanceCounterpartySuggestionsResult(..)
  , FinanceTransactionSplitRow(..)
  , FinanceTransactionsQuery(..)
  , FinanceTransferLink(..)
  , LinkFinanceTransfer(..)
  , SplitFinanceTransaction(..)
  , UpdateFinanceTransactionMetadata(..)
  , UpdateFinanceTransactionNote(..)
  )
import Pages.Admin (component) as Admin
import Pages.Calendar (CalendarRouteOutput(..), CalendarView(..), CalendarItem(..), component, decodeCalendarItemsResponse) as Calendar
import Pages.Checklists (component) as Checklists
import Pages.FinanceReports (Output(..), component) as FinanceReports
import Pages.FinanceTransactions (FinanceDetailSnapshot, Output(..), component) as FinanceTransactions
import Control.Monad.RWS (get, modify_)
import Control.Alt ((<|>))
import Data.Array (any, filter, find, head, length, mapMaybe, mapWithIndex, null, snoc)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Bifunctor (lmap)
import Data.Char (toCharCode)
import Data.Either (Either(..), either)
import Data.Foldable (all, fold, foldMap, foldl)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Number as Number
import Data.Ord (abs)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String
import Data.String.Common as StringCommon
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
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
type ReportsSlot slot = forall query. H.Slot query FinanceReports.Output slot
type ChildSlots =
  ( notes :: OpaqueSlot Unit
  , checklists :: OpaqueSlot Unit
  , calendar :: CalendarSlot Unit
  , financeTransactions :: TransactionsSlot Int
  , financeReports :: ReportsSlot Unit
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
  , direction :: Maybe FinanceReportDirection
  , categoryIn :: Array String
  , categoryNotIn :: Array String
  , amountMin :: Maybe Int
  , amountMax :: Maybe Int
  , search :: Maybe String
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
                  , direction: parseTransactionsDirection query
                  , categoryIn: parseTransactionsCategoryIn query
                  , categoryNotIn: parseTransactionsCategoryNotIn query
                  , amountMin: parseTransactionsAmountMin query
                  , amountMax: parseTransactionsAmountMax query
                  , search: parseTransactionsSearch query
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
defaultTransactionsRoute =
  FinanceTransactions
    { accountId: Nothing
    , from: Nothing
    , to: Nothing
    , direction: Nothing
    , categoryIn: []
    , categoryNotIn: []
    , amountMin: Nothing
    , amountMax: Nothing
    , search: Nothing
    }

normalizeCalendarRouteState :: CalendarRouteState -> CalendarRouteState
normalizeCalendarRouteState { day, item } =
  { day: day >>= \raw -> if DateTime.isLocalDate raw then Just raw else Nothing
  , item: item >>= nonEmptyStringMaybe
  }

normalizeTransactionsRouteState :: TransactionsRouteState -> TransactionsRouteState
normalizeTransactionsRouteState { accountId, from, to, direction, categoryIn, categoryNotIn, amountMin, amountMax, search } =
  let
    normalizedCategoryIn = normalizeCategoryValues categoryIn
    normalizedCategoryNotIn = normalizeCategoryValues (filter (\value -> not (any (_ == value) normalizedCategoryIn)) categoryNotIn)
  in
    { accountId: accountId >>= nonEmptyStringMaybe
    , from: from >>= nonEmptyStringMaybe
    , to: to >>= nonEmptyStringMaybe
    , direction
    , categoryIn: normalizedCategoryIn
    , categoryNotIn: normalizedCategoryNotIn
    , amountMin
    , amountMax
    , search: search >>= nonEmptyStringMaybe
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
      directionQuery = map (\value -> "direction=" <> encodeFinanceReportDirection value) transactionsRoute.direction
      amountMinQuery = map (\value -> "amountMin=" <> show value) transactionsRoute.amountMin
      amountMaxQuery = map (\value -> "amountMax=" <> show value) transactionsRoute.amountMax
      searchQuery = map (\value -> "search=" <> value) transactionsRoute.search
      queryParts =
        mapMaybe identity [ accountIdQuery, fromQuery, toQuery, directionQuery, amountMinQuery, amountMaxQuery, searchQuery ]
          <> map (\value -> "categoryIn=" <> value) transactionsRoute.categoryIn
          <> map (\value -> "categoryNotIn=" <> value) transactionsRoute.categoryNotIn
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

parseTransactionsDirection :: String -> Maybe FinanceReportDirection
parseTransactionsDirection rawQuery =
  findMapQueryValue "direction" rawQuery >>= decodeFinanceReportDirection

parseTransactionsCategoryIn :: String -> Array String
parseTransactionsCategoryIn rawQuery =
  findMapQueryValues "categoryIn" rawQuery

parseTransactionsCategoryNotIn :: String -> Array String
parseTransactionsCategoryNotIn rawQuery =
  findMapQueryValues "categoryNotIn" rawQuery

parseTransactionsAmountMin :: String -> Maybe Int
parseTransactionsAmountMin rawQuery =
  findMapQueryValue "amountMin" rawQuery >>= parseInt

parseTransactionsAmountMax :: String -> Maybe Int
parseTransactionsAmountMax rawQuery =
  findMapQueryValue "amountMax" rawQuery >>= parseInt

parseTransactionsSearch :: String -> Maybe String
parseTransactionsSearch rawQuery =
  findMapQueryValue "search" rawQuery >>= nonEmptyStringMaybe

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

findMapQueryValues :: String -> String -> Array String
findMapQueryValues key rawQuery =
  mapMaybe valueFromPair (filter isMatchingPair queryPairs)
  where
  queryPairs = StringCommon.split (Pattern "&") rawQuery
  isMatchingPair pair =
    case head (StringCommon.split (Pattern "=") pair) of
      Just pairKey -> pairKey == key
      Nothing -> false
  valueFromPair pair =
    case StringCommon.split (Pattern "=") pair of
      [ _, pairValue ] -> nonEmptyStringMaybe pairValue
      _ -> Nothing

parseInt :: String -> Maybe Int
parseInt raw =
  Int.fromString raw

decodeFinanceReportDirection :: String -> Maybe FinanceReportDirection
decodeFinanceReportDirection = case _ of
  "sent" -> Just ReportSent
  "received" -> Just ReportReceived
  "all" -> Just ReportAll
  _ -> Nothing

encodeFinanceReportDirection :: FinanceReportDirection -> String
encodeFinanceReportDirection = case _ of
  ReportSent -> "sent"
  ReportReceived -> "received"
  ReportAll -> "all"

normalizeCategoryValues :: Array String -> Array String
normalizeCategoryValues =
  foldl
    ( \acc raw ->
        case nonEmptyStringMaybe raw of
          Nothing -> acc
          Just value ->
            if any (_ == value) acc then acc else snoc acc value
    )
    []

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
  | FinanceCreateCounterpartyChanged String
  | FinanceCreateDescriptionChanged String
  | FinanceCreateSuggestionsLoaded (Either String (Array FinanceCounterpartySuggestion))
  | FinanceCreateSuggestionSelected String
  | SubmitFinanceCreate
  | FinanceCreateAccountsLoaded (Either String (Array FinanceAccount))
  | FinanceCreateSubmitted (Either String Unit)
  | FinanceDetailCategoriesLoaded (Either String (Array FinanceCategory))
  | FinanceDetailCounterpartyChanged String
  | FinanceDetailDescriptionChanged String
  | SubmitFinanceDetailMetadata
  | FinanceDetailMetadataSubmitted String String (Either String FinanceTransaction)
  | FinanceDetailCategoryChanged String
  | SubmitFinanceDetailCategory
  | FinanceDetailCategorySubmitted String (Either String Unit)
  | FinanceDetailNewNoteChanged String
  | SubmitFinanceDetailNewNote
  | FinanceDetailNewNoteSubmitted String (Either String FinanceTransactionNote)
  | StartFinanceDetailNoteEdit String
  | FinanceDetailNoteEditChanged String
  | CancelFinanceDetailNoteEdit
  | SubmitFinanceDetailNoteEdit
  | FinanceDetailNoteEditSubmitted String (Either String FinanceTransactionNote)
  | SubmitFinanceDetailNoteDelete String
  | FinanceDetailNoteDeleteSubmitted String (Either String Unit)
  | OpenFinanceSplitEditor
  | FinanceSplitRowAmountChanged Int String
  | FinanceSplitRowCategoryChanged Int String
  | AddFinanceSplitRow
  | RemoveFinanceSplitRow Int
  | SaveFinanceSplit
  | FinanceSplitSaved (Either String Unit)
  | CancelFinanceSplitEditor
  | OpenFinanceTransferSelector
  | FinanceTransferCandidatesLoaded (Either String (Array FinanceTransaction))
  | FinanceTransferTargetChanged String
  | SubmitFinanceTransferLink
  | FinanceTransferLinked String (Either String Unit)
  | CancelFinanceTransferSelector
  | DismissFinanceToast
  | CloseFinanceOverlay
  | NavigateToLateItem String String
  | OpenLateItemQuickComplete LateItems.LateItem
  | UpdateLateItemQuickCompleteMinutes String
  | ConfirmLateItemQuickComplete
  | CancelLateItemQuickComplete
  | HandleCalendarOutput Calendar.CalendarRouteOutput
  | HandleTransactionsOutput FinanceTransactions.Output
  | HandleReportsOutput FinanceReports.Output
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
  , financeDetailMutationState :: Maybe FinanceDetailMutationState
  , financeTransactionsSlotNonce :: Int
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
  , counterpartyInput :: String
  , descriptionInput :: String
  , suggestions :: Array FinanceCounterpartySuggestion
  , isLoadingSuggestions :: Boolean
  , suggestionsError :: Maybe String
  , accountError :: Maybe String
  , amountError :: Maybe String
  , occurredAtError :: Maybe String
  , submitError :: Maybe String
  , isSubmitting :: Boolean
  , submitKeyNonce :: Int
  }

type FinanceDetailMutationState =
  { categories :: Array FinanceCategory
  , isLoadingCategories :: Boolean
  , counterpartyDraft :: String
  , descriptionDraft :: String
  , isSubmittingMetadata :: Boolean
  , metadataError :: Maybe String
  , categoryDraft :: String
  , categoryError :: Maybe String
  , isSubmittingCategory :: Boolean
  , newNoteInput :: String
  , noteError :: Maybe String
  , isAddingNote :: Boolean
  , editingNoteId :: Maybe String
  , editingNoteInput :: String
  , isSubmittingNoteEdit :: Boolean
  , deletingNoteIds :: Array String
  , splitEditor :: Maybe FinanceSplitEditorState
  , transferSelector :: Maybe FinanceTransferSelectorState
  , hasSuccessfulMutations :: Boolean
  }

type FinanceSplitEditorState =
  { transactionTotal :: Number
  , rows :: Array FinanceSplitEditorRow
  , submitError :: Maybe String
  , isSaving :: Boolean
  }

type FinanceSplitEditorRow =
  { amountInput :: String
  , category :: String
  }

type FinanceTransferSelectorState =
  { isLoading :: Boolean
  , candidates :: Array FinanceTransaction
  , selectedTargetId :: String
  , submitError :: Maybe String
  , isSubmitting :: Boolean
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
  , financeDetailMutationState: Nothing
  , financeTransactionsSlotNonce: 0
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
  modify_ _ { currentRoute = route, financeOverlay = Nothing, financeOverlayScrollY = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing, financeDetailMutationState = Nothing }
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
    , financeDetailMutationState = financeDetailMutationStateAfterRouteChange route st.financeOverlay st.financeDetailMutationState
    }
  shouldNormalize <- liftEffect (shouldCanonicalizeFinanceRoute route)
  when shouldNormalize $
    navigateWith _.replaceState st.nav route
  when (shouldRefreshLateItemsForRoute st.authStatus route) (handleAction LoadLateItems)
handleAction (NavigateTo route) = do
  st <- get
  let nextRoute = routeFromDefined route
  modify_ _ { currentRoute = nextRoute, financeOverlay = Nothing, financeOverlayScrollY = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing, financeDetailMutationState = Nothing }
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
handleAction (FinanceCreateCounterpartyChanged raw) = do
  modify_ \st ->
    st
      { financeCreateState =
          map
            ( _
                { counterpartyInput = raw
                , submitError = Nothing
                , suggestionsError = Nothing
                , isLoadingSuggestions = raw /= ""
                }
            )
            st.financeCreateState
      }
  st <- get
  case st.financeCreateState of
    Nothing -> pure unit
    Just createState ->
      if raw == "" then
        modify_ \next ->
          next
            { financeCreateState =
                map
                  (_ { suggestions = [], isLoadingSuggestions = false, suggestionsError = Nothing })
                  next.financeCreateState
            }
      else do
        result <- liftAff (fetchCounterpartySuggestions createState.launch.direction createState.accountId raw)
        handleAction (FinanceCreateSuggestionsLoaded result)
handleAction (FinanceCreateDescriptionChanged raw) =
  modify_ \st ->
    st
      { financeCreateState =
          map
            (_ { descriptionInput = raw, submitError = Nothing })
            st.financeCreateState
      }
handleAction (FinanceCreateSuggestionsLoaded result) =
  modify_ \st ->
    st
      { financeCreateState =
          map
            ( \createState ->
                case result of
                  Left message ->
                    createState { suggestions = [], isLoadingSuggestions = false, suggestionsError = Just message }
                  Right suggestions ->
                    createState { suggestions = suggestions, isLoadingSuggestions = false, suggestionsError = Nothing }
            )
            st.financeCreateState
      }
handleAction (FinanceCreateSuggestionSelected selectedCounterparty) =
  modify_ \st ->
    st
      { financeCreateState =
          map
            (_ { counterpartyInput = selectedCounterparty, suggestions = [], suggestionsError = Nothing })
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
                , financeDetailMutationState = Nothing
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
                          , counterpartyInput = ""
                          , descriptionInput = ""
                          , suggestions = []
                          , isLoadingSuggestions = false
                          , suggestionsError = Nothing
                          , accountError = Nothing
                          , amountError = Nothing
                          , occurredAtError = Nothing
                          , submitError = Nothing
                          }
                      )
                      next.financeCreateState
                }
handleAction (FinanceDetailCategoriesLoaded result) =
  modify_ \st ->
    st
      { financeDetailMutationState =
          map
            ( \detailState ->
                case result of
                  Left message ->
                    detailState { isLoadingCategories = false, categoryError = Just message }
                  Right categories ->
                    detailState { isLoadingCategories = false, categories = categories }
            )
            st.financeDetailMutationState
      }
handleAction (FinanceDetailCounterpartyChanged raw) =
  modify_ \st ->
    st
      { financeDetailMutationState =
          map
            (_ { counterpartyDraft = raw, metadataError = Nothing })
            st.financeDetailMutationState
      }
handleAction (FinanceDetailDescriptionChanged raw) =
  modify_ \st ->
    st
      { financeDetailMutationState =
          map
            (_ { descriptionDraft = raw, metadataError = Nothing })
            st.financeDetailMutationState
      }
handleAction SubmitFinanceDetailMetadata = do
  st <- get
  case st.financeDetailSnapshot, st.financeDetailMutationState of
    Just { transaction: FinanceTransaction tx }, Just mutationState ->
      if mutationState.isSubmittingMetadata then pure unit
      else do
        let previousCounterparty = mutationState.counterpartyDraft
        let previousDescription = mutationState.descriptionDraft
        modify_ \next ->
          next
            { financeDetailMutationState =
                map
                  (_ { isSubmittingMetadata = true, metadataError = Nothing })
                  next.financeDetailMutationState
            }
        result <- liftAff (submitFinanceDetailMetadata tx.id mutationState.counterpartyDraft mutationState.descriptionDraft)
        handleAction (FinanceDetailMetadataSubmitted previousCounterparty previousDescription result)
    _, _ -> pure unit
handleAction (FinanceDetailMetadataSubmitted previousCounterparty previousDescription result) =
  case result of
    Left message ->
      modify_ \st ->
        st
          { financeDetailMutationState =
              map
                (_ { isSubmittingMetadata = false, metadataError = Just message, counterpartyDraft = previousCounterparty, descriptionDraft = previousDescription })
                st.financeDetailMutationState
          }
    Right updatedTx ->
      modify_ \st ->
        st
          { financeDetailSnapshot =
              map
                (mapDetailSnapshotTransaction (const updatedTx))
                st.financeDetailSnapshot
          , financeDetailMutationState =
              map
                (_ { isSubmittingMetadata = false, metadataError = Nothing, hasSuccessfulMutations = true })
                st.financeDetailMutationState
          }
handleAction (FinanceDetailCategoryChanged categoryId) =
  modify_ \st ->
    st
      { financeDetailMutationState =
          map
            (_ { categoryDraft = categoryId, categoryError = Nothing })
            st.financeDetailMutationState
      }
handleAction SubmitFinanceDetailCategory = do
  st <- get
  case st.financeDetailSnapshot, st.financeDetailMutationState of
    Just { transaction: FinanceTransaction tx }, Just detailState ->
      if detailState.isSubmittingCategory then
        pure unit
      else if not (null tx.splits) then
        modify_ \next ->
          next
            { financeDetailMutationState =
                map
                  (_ { categoryError = Just "Split transactions must be edited through split management." })
                  next.financeDetailMutationState
            }
      else if detailState.categoryDraft == "" then
        modify_ \next ->
          next
            { financeDetailMutationState =
                map
                  (_ { categoryError = Just "Category is required." })
                  next.financeDetailMutationState
            }
      else do
        let previousCategory = foldMap (\(FinanceTransactionCategory entry) -> entry.id) tx.category
        let draft = detailState.categoryDraft
        modify_ \next ->
          next
            { financeDetailSnapshot = map (mapDetailSnapshotTransaction (setTransactionCategory draft)) next.financeDetailSnapshot
            , financeDetailMutationState = map (_ { isSubmittingCategory = true, categoryError = Nothing }) next.financeDetailMutationState
            }
        result <- liftAff (submitFinanceDetailCategory tx.id draft)
        handleAction (FinanceDetailCategorySubmitted previousCategory result)
    _, _ ->
      pure unit
handleAction (FinanceDetailCategorySubmitted previousCategory result) =
  case result of
    Left message ->
      modify_ \st ->
        st
          { financeDetailSnapshot = map (mapDetailSnapshotTransaction (setTransactionCategory previousCategory)) st.financeDetailSnapshot
          , financeDetailMutationState =
              map
                (_ { isSubmittingCategory = false, categoryError = Just message, categoryDraft = previousCategory })
                st.financeDetailMutationState
          }
    Right _ ->
      modify_ \st ->
        st
          { financeDetailMutationState =
              map
                (_ { isSubmittingCategory = false, hasSuccessfulMutations = true })
                st.financeDetailMutationState
          }
handleAction (FinanceDetailNewNoteChanged raw) =
  modify_ \st ->
    st
      { financeDetailMutationState =
          map
            (_ { newNoteInput = raw, noteError = Nothing })
            st.financeDetailMutationState
      }
handleAction SubmitFinanceDetailNewNote = do
  st <- get
  case st.financeDetailSnapshot, st.financeDetailMutationState of
    Just { transaction: FinanceTransaction tx }, Just detailState ->
      if detailState.isAddingNote then
        pure unit
      else if StringCommon.trim detailState.newNoteInput == "" then
        modify_ \next ->
          next
            { financeDetailMutationState =
                map
                  (_ { noteError = Just "Note text is required." })
                  next.financeDetailMutationState
            }
      else do
        let noteText = StringCommon.trim detailState.newNoteInput
        modify_ \next ->
          next
            { financeDetailMutationState =
                map
                  (_ { isAddingNote = true, noteError = Nothing })
                  next.financeDetailMutationState
            }
        result <- liftAff (submitFinanceDetailNewNote tx.id noteText)
        handleAction (FinanceDetailNewNoteSubmitted noteText result)
    _, _ ->
      pure unit
handleAction (FinanceDetailNewNoteSubmitted noteText result) =
  case result of
    Left message ->
      modify_ \st ->
        st
          { financeDetailMutationState =
              map
                (_ { isAddingNote = false, noteError = Just message, newNoteInput = noteText })
                st.financeDetailMutationState
          }
    Right note ->
      modify_ \st ->
        st
          { financeDetailSnapshot = map (mapDetailSnapshotTransaction (appendTransactionNote note)) st.financeDetailSnapshot
          , financeDetailMutationState =
              map
                (_ { isAddingNote = false, noteError = Nothing, newNoteInput = "", hasSuccessfulMutations = true })
                st.financeDetailMutationState
          }
handleAction (StartFinanceDetailNoteEdit noteId) = do
  st <- get
  case findDetailNoteText noteId st.financeDetailSnapshot of
    Nothing -> pure unit
    Just noteText ->
      modify_ \next ->
        next
          { financeDetailMutationState =
              map
                (_ { editingNoteId = Just noteId, editingNoteInput = noteText, noteError = Nothing })
                next.financeDetailMutationState
          }
handleAction (FinanceDetailNoteEditChanged raw) =
  modify_ \st ->
    st
      { financeDetailMutationState =
          map
            (_ { editingNoteInput = raw, noteError = Nothing })
            st.financeDetailMutationState
      }
handleAction CancelFinanceDetailNoteEdit =
  modify_ \st ->
    st
      { financeDetailMutationState =
          map
            (_ { editingNoteId = Nothing, editingNoteInput = "", isSubmittingNoteEdit = false })
            st.financeDetailMutationState
      }
handleAction SubmitFinanceDetailNoteEdit = do
  st <- get
  case st.financeDetailSnapshot, st.financeDetailMutationState of
    Just { transaction: FinanceTransaction tx }, Just detailState ->
      case detailState.editingNoteId of
        Nothing -> pure unit
        Just noteId ->
          if detailState.isSubmittingNoteEdit then
            pure unit
          else if StringCommon.trim detailState.editingNoteInput == "" then
            modify_ \next ->
              next
                { financeDetailMutationState =
                    map
                      (_ { noteError = Just "Note text is required." })
                      next.financeDetailMutationState
                }
          else
            case findDetailNoteText noteId st.financeDetailSnapshot of
              Nothing -> pure unit
              Just previousText -> do
                let nextText = StringCommon.trim detailState.editingNoteInput
                modify_ \next ->
                  next
                    { financeDetailSnapshot = map (mapDetailSnapshotTransaction (replaceTransactionNoteText noteId nextText)) next.financeDetailSnapshot
                    , financeDetailMutationState =
                        map
                          (_ { isSubmittingNoteEdit = true, noteError = Nothing })
                          next.financeDetailMutationState
                    }
                result <- liftAff (submitFinanceDetailNoteEdit tx.id noteId nextText)
                handleAction (FinanceDetailNoteEditSubmitted previousText result)
    _, _ ->
      pure unit
handleAction (FinanceDetailNoteEditSubmitted previousText result) = do
  st <- get
  case st.financeDetailMutationState of
    Nothing -> pure unit
    Just detailState ->
      case detailState.editingNoteId of
        Nothing -> pure unit
        Just noteId ->
          case result of
            Left message ->
              modify_ \next ->
                next
                  { financeDetailSnapshot = map (mapDetailSnapshotTransaction (replaceTransactionNoteText noteId previousText)) next.financeDetailSnapshot
                  , financeDetailMutationState =
                      map
                        (_ { isSubmittingNoteEdit = false, noteError = Just message, editingNoteInput = previousText })
                        next.financeDetailMutationState
                  }
            Right updatedNote ->
              modify_ \next ->
                next
                  { financeDetailSnapshot = map (mapDetailSnapshotTransaction (replaceTransactionNote updatedNote)) next.financeDetailSnapshot
                  , financeDetailMutationState =
                      map
                        (_ { isSubmittingNoteEdit = false, editingNoteId = Nothing, editingNoteInput = "", noteError = Nothing, hasSuccessfulMutations = true })
                        next.financeDetailMutationState
                  }
handleAction (SubmitFinanceDetailNoteDelete noteId) = do
  st <- get
  case st.financeDetailSnapshot, st.financeDetailMutationState of
    Just { transaction: FinanceTransaction tx }, Just detailState ->
      if any (_ == noteId) detailState.deletingNoteIds then
        pure unit
      else do
        modify_ \next ->
          next
            { financeDetailSnapshot = map (mapDetailSnapshotTransaction (removeTransactionNote noteId)) next.financeDetailSnapshot
            , financeDetailMutationState =
                map
                  ( \current ->
                      current
                        { deletingNoteIds = current.deletingNoteIds <> [ noteId ]
                        , noteError = Nothing
                        }
                  )
                  next.financeDetailMutationState
            }
        result <- liftAff (submitFinanceDetailNoteDelete tx.id noteId)
        handleAction (FinanceDetailNoteDeleteSubmitted noteId result)
    _, _ ->
      pure unit
handleAction (FinanceDetailNoteDeleteSubmitted noteId result) =
  case result of
    Left message ->
      modify_ \st ->
        st
          { financeDetailMutationState =
              map
                ( \detailState ->
                    detailState
                      { deletingNoteIds = filter (_ /= noteId) detailState.deletingNoteIds
                      , noteError = Just message
                      }
                )
                st.financeDetailMutationState
          }
    Right _ ->
      modify_ \st ->
        st
          { financeDetailMutationState =
              map
                ( \detailState ->
                    detailState
                      { deletingNoteIds = filter (_ /= noteId) detailState.deletingNoteIds
                      , hasSuccessfulMutations = true
                      }
                )
                st.financeDetailMutationState
          }
handleAction OpenFinanceSplitEditor = do
  st <- get
  case st.financeDetailSnapshot, st.financeDetailMutationState of
    Just { transaction: FinanceTransaction tx }, Just _ ->
      modify_ \next ->
        next
          { financeDetailMutationState =
              map
                ( _
                    { splitEditor = Just (initialFinanceSplitEditorState tx)
                    , transferSelector = Nothing
                    , categoryError = Nothing
                    , noteError = Nothing
                    }
                )
                next.financeDetailMutationState
          }
    _, _ ->
      pure unit
handleAction (FinanceSplitRowAmountChanged rowIndex raw) =
  modify_ \st ->
    st
      { financeDetailMutationState =
          map
            ( \mutationState ->
                mutationState
                  { splitEditor =
                      map
                        ( \editor ->
                            editor { rows = updateSplitRows rowIndex (_ { amountInput = raw }) editor.rows }
                        )
                        mutationState.splitEditor
                  }
            )
            st.financeDetailMutationState
      }
handleAction (FinanceSplitRowCategoryChanged rowIndex categoryId) =
  modify_ \st ->
    st
      { financeDetailMutationState =
          map
            ( \mutationState ->
                mutationState
                  { splitEditor =
                      map
                        ( \editor ->
                            editor { rows = updateSplitRows rowIndex (_ { category = categoryId }) editor.rows }
                        )
                        mutationState.splitEditor
                  }
            )
            st.financeDetailMutationState
      }
handleAction AddFinanceSplitRow =
  modify_ \st ->
    st
      { financeDetailMutationState =
          map
            ( \mutationState ->
                mutationState
                  { splitEditor =
                      map
                        ( \editor ->
                            let
                              nextRow =
                                if null editor.rows then
                                  { amountInput: show editor.transactionTotal
                                  , category: if mutationState.categoryDraft == "" then "uncategorized" else mutationState.categoryDraft
                                  }
                                else
                                  { amountInput: "0", category: "uncategorized" }
                            in
                              editor { rows = snoc editor.rows nextRow }
                        )
                        mutationState.splitEditor
                  }
            )
            st.financeDetailMutationState
      }
handleAction (RemoveFinanceSplitRow rowIndex) =
  modify_ \st ->
    st
      { financeDetailMutationState =
          map
            ( \mutationState ->
                mutationState
                  { splitEditor =
                      map
                        ( \editor ->
                            editor
                              { rows =
                                  mapMaybeWithIndex
                                    ( \idx row ->
                                        if idx == rowIndex then Nothing else Just row
                                    )
                                    editor.rows
                              }
                        )
                        mutationState.splitEditor
                  }
            )
            st.financeDetailMutationState
      }
handleAction SaveFinanceSplit = do
  st <- get
  case st.financeDetailSnapshot, st.financeDetailMutationState of
    Just { transaction: FinanceTransaction tx }, Just mutationState ->
      case mutationState.splitEditor of
        Nothing -> pure unit
        Just editor ->
          if editor.isSaving then
            pure unit
          else
            case buildValidSplitPayload editor of
              Left validationError ->
                modify_ \next ->
                  next
                    { financeDetailMutationState =
                        map
                          ( \current ->
                              current
                                { splitEditor =
                                    map (_ { submitError = Just validationError }) current.splitEditor
                                }
                          )
                          next.financeDetailMutationState
                    }
              Right splits -> do
                modify_ \next ->
                  next
                    { financeDetailMutationState =
                        map
                          ( \current ->
                              current
                                { splitEditor =
                                    map (_ { isSaving = true, submitError = Nothing }) current.splitEditor
                                }
                          )
                          next.financeDetailMutationState
                    }
                result <- liftAff (submitFinanceSplit tx.id splits)
                handleAction (FinanceSplitSaved result)
    _, _ ->
      pure unit
handleAction (FinanceSplitSaved result) =
  case result of
    Left message ->
      modify_ \st ->
        st
          { financeDetailMutationState =
              map
                ( \mutationState ->
                    mutationState
                      { splitEditor =
                          map (_ { isSaving = false, submitError = Just message }) mutationState.splitEditor
                      }
                )
                st.financeDetailMutationState
          }
    Right _ ->
      modify_ \st ->
        let
          maybeSavedSplits =
            case st.financeDetailMutationState >>= _.splitEditor of
              Nothing -> Nothing
              Just editor ->
                case buildValidSplitPayload editor of
                  Left _ -> Nothing
                  Right splits -> Just splits
        in
          st
            { financeDetailSnapshot =
                map
                  ( \snapshot ->
                      case maybeSavedSplits of
                        Nothing -> snapshot
                        Just splits -> mapDetailSnapshotTransaction (setTransactionSplits splits) snapshot
                  )
                  st.financeDetailSnapshot
            , financeDetailMutationState =
                map
                  (_ { splitEditor = Nothing, hasSuccessfulMutations = true, categoryError = Nothing })
                  st.financeDetailMutationState
            }
handleAction CancelFinanceSplitEditor =
  modify_ \st ->
    st
      { financeDetailMutationState =
          map
            (_ { splitEditor = Nothing })
            st.financeDetailMutationState
      }
handleAction OpenFinanceTransferSelector = do
  st <- get
  case st.financeDetailSnapshot, st.financeDetailMutationState of
    Just { transaction: FinanceTransaction tx }, Just _ -> do
      let
        query =
          case st.currentRoute of
            Route (FinanceTransactions routeState) ->
              FinanceTransactionsQuery
                { accountId: routeState.accountId
                , from: routeState.from
                , to: routeState.to
                , direction: routeState.direction
                , categoryIn: routeState.categoryIn
                , categoryNotIn: routeState.categoryNotIn
                , amountMin: routeState.amountMin
                , amountMax: routeState.amountMax
                , search: routeState.search
                }
            _ ->
              FinanceTransactionsQuery
                { accountId: Nothing
                , from: Nothing
                , to: Nothing
                , direction: Nothing
                , categoryIn: []
                , categoryNotIn: []
                , amountMin: Nothing
                , amountMax: Nothing
                , search: Nothing
                }
      modify_ \next ->
        next
          { financeDetailMutationState =
              map
                (_ { transferSelector = Just { isLoading: true, candidates: [], selectedTargetId: "", submitError: Nothing, isSubmitting: false } })
                next.financeDetailMutationState
          }
      result <- liftAff (fetchTransferCandidates tx.id query)
      handleAction (FinanceTransferCandidatesLoaded result)
    _, _ ->
      pure unit
handleAction (FinanceTransferCandidatesLoaded result) =
  modify_ \st ->
    st
      { financeDetailMutationState =
          map
            ( \mutationState ->
                mutationState
                  { transferSelector =
                      map
                        ( \selector ->
                            case result of
                              Left message ->
                                selector { isLoading = false, submitError = Just message }
                              Right candidates ->
                                let
                                  firstTargetId = foldMap (\(FinanceTransaction candidate) -> candidate.id) (head candidates)
                                in
                                  selector { isLoading = false, candidates = candidates, selectedTargetId = firstTargetId }
                        )
                        mutationState.transferSelector
                  }
            )
            st.financeDetailMutationState
      }
handleAction (FinanceTransferTargetChanged targetId) =
  modify_ \st ->
    st
      { financeDetailMutationState =
          map
            ( \mutationState ->
                mutationState
                  { transferSelector =
                      map
                        (_ { selectedTargetId = targetId, submitError = Nothing })
                        mutationState.transferSelector
                  }
            )
            st.financeDetailMutationState
      }
handleAction SubmitFinanceTransferLink = do
  st <- get
  case st.financeDetailSnapshot, st.financeDetailMutationState >>= _.transferSelector of
    Just { transaction: FinanceTransaction tx }, Just selector ->
      if selector.isSubmitting then
        pure unit
      else if selector.selectedTargetId == "" then
        modify_ \next ->
          next
            { financeDetailMutationState =
                map
                  ( \mutationState ->
                      mutationState
                        { transferSelector =
                            map
                              (_ { submitError = Just "Select a target transaction first." })
                              mutationState.transferSelector
                        }
                  )
                  next.financeDetailMutationState
            }
      else do
        let targetId = selector.selectedTargetId
        modify_ \next ->
          next
            { financeDetailMutationState =
                map
                  ( \mutationState ->
                      mutationState
                        { transferSelector =
                            map
                              (_ { isSubmitting = true, submitError = Nothing })
                              mutationState.transferSelector
                        }
                  )
                  next.financeDetailMutationState
            }
        result <- liftAff (submitFinanceTransferLink tx.id targetId)
        handleAction (FinanceTransferLinked targetId result)
    _, _ ->
      pure unit
handleAction (FinanceTransferLinked targetId result) =
  case result of
    Left message ->
      modify_ \st ->
        st
          { financeDetailMutationState =
              map
                ( \mutationState ->
                    mutationState
                      { transferSelector =
                          map
                            (_ { isSubmitting = false, submitError = Just message })
                            mutationState.transferSelector
                      }
                )
                st.financeDetailMutationState
          }
    Right _ ->
      modify_ \st ->
        st
          { financeDetailSnapshot =
              map (mapDetailSnapshotTransaction (setTransactionTransfer targetId)) st.financeDetailSnapshot
          , financeDetailMutationState =
              map
                (_ { transferSelector = Nothing, hasSuccessfulMutations = true })
                st.financeDetailMutationState
          }
handleAction CancelFinanceTransferSelector =
  modify_ \st ->
    st
      { financeDetailMutationState =
          map
            (_ { transferSelector = Nothing })
            st.financeDetailMutationState
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
    let initialMutationState = initialFinanceDetailMutationState detailSnapshot
    modify_ _
      { financeOverlay = Just (FinanceDetailOverlay detailSnapshot.transactionId)
      , financeOverlayScrollY = Just scrollY
      , financeDetailSnapshot = Just detailSnapshot
      , financeDetailMutationState = Just initialMutationState
      }
    categoriesResult <- liftAff fetchFinanceDetailCategories
    handleAction (FinanceDetailCategoriesLoaded categoriesResult)
handleAction (HandleReportsOutput (FinanceReports.DrillDownRequested range)) = do
  st <- get
  let
    nextRoute =
      Route
        ( FinanceTransactions
            ( normalizeTransactionsRouteState
                { accountId: Nothing
                , from: Just range.from
                , to: Just range.to
                , direction: Nothing
                , categoryIn: []
                , categoryNotIn: []
                , amountMin: Nothing
                , amountMax: Nothing
                , search: Nothing
                }
            )
        )
  when (st.currentRoute /= nextRoute) do
    modify_ _ { currentRoute = nextRoute }
    navigateWith _.pushState st.nav nextRoute
handleAction (HandleAuthOutput (SignupSucceeded username)) = do
  st <- get
  liftEffect $ AuthSession.storeAuthenticatedUsername username
  let route = routeFromDefined Signin
  modify_ _ { currentRoute = route, authStatus = Unauthenticated, financeOverlay = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing, financeDetailMutationState = Nothing }
  navigateWith _.pushState st.nav route
handleAction (HandleAuthOutput (SigninSucceeded profile@(AuthenticatedProfile { username }))) = do
  st <- get
  liftEffect $ AuthSession.storeAuthenticatedUsername username
  let route = routeFromDefined Note
  modify_ _ { currentRoute = route, authStatus = Authenticated profile, financeOverlay = Nothing, financeOverlayScrollY = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing, financeDetailMutationState = Nothing }
  navigateWith _.pushState st.nav route
handleAction SignOut = do
  st <- get
  _ <- liftAff $ post string "/api/signout" Nothing
  liftEffect AuthSession.clearAuthenticatedUsername
  let route = routeFromDefined Signin
  modify_ _ { authStatus = Unauthenticated, currentRoute = route, financeOverlay = Nothing, financeOverlayScrollY = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing, financeDetailMutationState = Nothing }
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
render { currentRoute, authStatus, financeOverlay, lateItems, financeIsMobile, financeCreateState, financeDetailSnapshot, financeDetailMutationState, financeToastMessage, financeTransactionsSlotNonce } =
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
            , [ currentComponent authStatus financeOverlay route financeTransactionsSlotNonce ]
            , [ renderLateItemsSheet authStatus route lateItems ]
            , [ renderFinanceOverlay route financeOverlay financeIsMobile financeCreateState financeDetailSnapshot financeDetailMutationState ]
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

currentComponent :: AuthStatus -> Maybe FinanceOverlay -> DefinedRoute -> Int -> H.ComponentHTML Action ChildSlots Aff
currentComponent _ _ Note _ = slot_ (Proxy :: _ "notes") unit Notes.component unit
currentComponent _ _ Checklist _ = slot_ (Proxy :: _ "checklists") unit Checklists.component unit
currentComponent _ _ (Calendar calendarRoute) _ =
  slot
    (Proxy :: _ "calendar")
    unit
    Calendar.component
    { initialDay: calendarRoute.day
    , initialItemId: calendarRoute.item
    }
    HandleCalendarOutput
currentComponent _ financeOverlay (FinanceTransactions transactionsRoute) financeTransactionsSlotNonce = renderFinanceShell (FinanceTransactions transactionsRoute) financeOverlay financeTransactionsSlotNonce
currentComponent _ financeOverlay FinanceReports financeTransactionsSlotNonce = renderFinanceShell FinanceReports financeOverlay financeTransactionsSlotNonce
currentComponent (Authenticated (AuthenticatedProfile { username })) _ Admin _ =
  slot_ (Proxy :: _ "admin") unit Admin.component { currentUsername: username }
currentComponent _ _ Admin _ = text ""
currentComponent _ _ Signup _ = slot (Proxy :: _ "signup") unit signupComponent unit HandleAuthOutput
currentComponent _ _ Signin _ = slot (Proxy :: _ "signin") unit signinComponent unit HandleAuthOutput

renderFinanceShell :: DefinedRoute -> Maybe FinanceOverlay -> Int -> H.ComponentHTML Action ChildSlots Aff
renderFinanceShell route financeOverlay financeTransactionsSlotNonce =
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
          financeTransactionsSlotNonce
          FinanceTransactions.component
          transactionsRoute
          HandleTransactionsOutput
      else if routeKey route == routeKey FinanceReports then
        slot
          (Proxy :: _ "financeReports")
          unit
          FinanceReports.component
          unit
          HandleReportsOutput
      else
        renderFinancePlaceholderBody route
    ]
  where
  transactionsRoute =
    case route of
      FinanceTransactions currentRoute -> currentRoute
      _ ->
        { accountId: Nothing
        , from: Nothing
        , to: Nothing
        , direction: Nothing
        , categoryIn: []
        , categoryNotIn: []
        , amountMin: Nothing
        , amountMax: Nothing
        , search: Nothing
        }

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

renderFinanceOverlay :: forall w. DefinedRoute -> Maybe FinanceOverlay -> Boolean -> Maybe FinanceCreateState -> Maybe FinanceTransactions.FinanceDetailSnapshot -> Maybe FinanceDetailMutationState -> HTML w Action
renderFinanceOverlay route financeOverlay financeIsMobile financeCreateState financeDetailSnapshot financeDetailMutationState =
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
          [ renderFinanceDetailOverlayBody transactionId financeDetailSnapshot financeDetailMutationState ]
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
    modify_ _ { financeOverlay = Just FinanceCreateChooserOverlay, financeOverlayScrollY = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing, financeDetailMutationState = Nothing, financeIsMobile = isMobile }

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
  let shouldReloadLedger = fromMaybe false (map _.hasSuccessfulMutations state.financeDetailMutationState)
  let nextSlotNonce = if shouldReloadLedger then state.financeTransactionsSlotNonce + 1 else state.financeTransactionsSlotNonce
  case state.financeOverlay of
    Just (FinanceDetailOverlay _) ->
      case state.financeOverlayScrollY of
        Just scrollY -> do
          liftEffect (WindowScroll.setWindowScrollY scrollY)
          modify_ _ { financeOverlay = Nothing, financeOverlayScrollY = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing, financeDetailMutationState = Nothing, financeTransactionsSlotNonce = nextSlotNonce }
        Nothing ->
          modify_ _ { financeOverlay = Nothing, financeOverlayScrollY = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing, financeDetailMutationState = Nothing, financeTransactionsSlotNonce = nextSlotNonce }
    _ ->
      modify_ _ { financeOverlay = Nothing, financeOverlayScrollY = Nothing, financeCreateState = Nothing, financeDetailSnapshot = Nothing, financeDetailMutationState = Nothing, financeTransactionsSlotNonce = nextSlotNonce }

openFinanceCreateFromIntent :: FinanceCreateIntent -> H.HalogenM State Action ChildSlots Void Aff Unit
openFinanceCreateFromIntent intent = do
  state <- get
  case state.currentRoute of
    Route (FinanceTransactions transactionsRoute) -> do
      let launch = buildFinanceCreateLaunch intent transactionsRoute
      now <- liftEffect nowDateTime
      let nowLocalDateTime = DateTime.formatLocalDateTime now
      let occurredAtInput = defaultOccurredAtInput launch nowLocalDateTime
      modify_ _ { financeOverlay = Just (FinanceCreateOverlay launch), financeOverlayScrollY = Nothing, financeCreateState = Just (initialFinanceCreateState launch occurredAtInput), financeDetailSnapshot = Nothing, financeDetailMutationState = Nothing }
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
  , counterpartyInput: ""
  , descriptionInput: ""
  , suggestions: []
  , isLoadingSuggestions: false
  , suggestionsError: Nothing
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

fetchCounterpartySuggestions :: String -> String -> String -> Aff (Either String (Array FinanceCounterpartySuggestion))
fetchCounterpartySuggestions direction accountId q = do
  let reportDirection = if direction == "sent" then Just ReportSent else Just ReportReceived
  response <-
    getCounterpartySuggestions
      ( FinanceCounterpartySuggestionsQuery
          { q
          , limit: Just 8
          , direction: reportDirection
          , accountId: if accountId == "" then Nothing else Just accountId
          }
      )
  case response of
    Left err ->
      pure (Left ("Unable to load counterparty suggestions: " <> printError err))
    Right successResponse ->
      if statusOk successResponse then
        case lmap show (decodeJson successResponse.body :: Either _ FinanceCounterpartySuggestionsResult) of
          Left decodeError -> pure (Left decodeError)
          Right (FinanceCounterpartySuggestionsResult payload) -> pure (Right payload.items)
      else
        pure (Left ("Unable to load counterparty suggestions: status " <> show (unwrap successResponse.status)))

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
      let
        payload =
          CreateFinanceTransaction
            { accountId: createState.accountId
            , amount
            , occurredAt: Just createState.occurredAtInput
            , counterparty: nonEmptyTrimmed createState.counterpartyInput
            , description: nonEmptyTrimmed createState.descriptionInput
            }
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

nonEmptyTrimmed :: String -> Maybe String
nonEmptyTrimmed raw =
  let
    trimmed = StringCommon.trim raw
  in
    if trimmed == "" then Nothing else Just trimmed

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
        , div [ class_ "d-flex flex-column gap-1" ]
            [ label [ class_ "form-label mb-0" ] [ text "Counterparty" ]
            , input
                [ class_ "form-control finance-create-overlay__counterparty-input"
                , value createState.counterpartyInput
                , disabled createState.isSubmitting
                , onValueChange FinanceCreateCounterpartyChanged
                ]
            , if createState.isLoadingSuggestions then
                div [ class_ "small text-muted" ] [ text "Loading suggestions..." ]
              else if null createState.suggestions then
                text ""
              else
                div [ class_ "d-flex flex-column gap-1" ] (map renderSuggestion createState.suggestions)
            , maybe (text "") (\message -> div [ class_ "text-danger small" ] [ text message ]) createState.suggestionsError
            ]
        , div [ class_ "d-flex flex-column gap-1" ]
            [ label [ class_ "form-label mb-0" ] [ text "Description" ]
            , input
                [ class_ "form-control finance-create-overlay__description-input"
                , value createState.descriptionInput
                , disabled createState.isSubmitting
                , onValueChange FinanceCreateDescriptionChanged
                ]
            ]
        , maybe (text "") (\message -> div [ class_ "alert alert-danger mb-0 finance-create-overlay__submit-error" ] [ text message ]) createState.submitError
        ]
  where
  renderAccountOption (FinanceAccount account) =
    option [ value account.id ] [ text account.name ]

  renderSuggestion (FinanceCounterpartySuggestion suggestion) =
    button
      [ class_ "btn btn-sm btn-outline-secondary text-start finance-create-overlay__counterparty-suggestion"
      , onClick (const (FinanceCreateSuggestionSelected suggestion.value))
      ]
      [ text
          ( suggestion.value
              <> foldMap (\category -> " (" <> category <> ")") suggestion.suggestedCategory
          )
      ]

renderFinanceDetailOverlayBody :: forall w. String -> Maybe FinanceTransactions.FinanceDetailSnapshot -> Maybe FinanceDetailMutationState -> HTML w Action
renderFinanceDetailOverlayBody transactionId maybeSnapshot maybeMutationState =
  case maybeSnapshot, maybeMutationState of
    Nothing, _ ->
      div [ class_ "finance-detail-overlay d-flex flex-column gap-2" ]
        [ div [ class_ "alert alert-warning mb-0" ] [ text "Transaction unavailable in current snapshot." ]
        , div [ class_ "small text-muted" ] [ text ("Requested transaction: " <> transactionId) ]
        ]
    _, Nothing ->
      div [ class_ "finance-detail-overlay d-flex flex-column gap-2" ]
        [ div [ class_ "alert alert-warning mb-0" ] [ text "Transaction detail state unavailable." ] ]
    Just { transaction: FinanceTransaction tx, accountLabel }, Just mutationState ->
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
        , div [ class_ "d-flex flex-column gap-2 finance-detail-overlay__metadata" ]
            [ div [ class_ "fw-semibold" ] [ text "Metadata" ]
            , div [ class_ "d-flex flex-column gap-1" ]
                [ label [ class_ "form-label mb-0" ] [ text "Counterparty" ]
                , input
                    [ class_ "form-control form-control-sm finance-detail-overlay__counterparty-input"
                    , value mutationState.counterpartyDraft
                    , onValueChange FinanceDetailCounterpartyChanged
                    , disabled mutationState.isSubmittingMetadata
                    ]
                ]
            , div [ class_ "d-flex flex-column gap-1" ]
                [ label [ class_ "form-label mb-0" ] [ text "Description" ]
                , input
                    [ class_ "form-control form-control-sm finance-detail-overlay__description-input"
                    , value mutationState.descriptionDraft
                    , onValueChange FinanceDetailDescriptionChanged
                    , disabled mutationState.isSubmittingMetadata
                    ]
                ]
            , button
                [ class_ "btn btn-sm btn-outline-primary finance-detail-overlay__metadata-submit"
                , onClick (const SubmitFinanceDetailMetadata)
                , disabled mutationState.isSubmittingMetadata
                ]
                [ text (if mutationState.isSubmittingMetadata then "Saving..." else "Save metadata") ]
            ]
        , div [ class_ "d-flex flex-column gap-2 finance-detail-overlay__category" ]
            [ div [ class_ "fw-semibold" ] [ text "Categorization" ]
            , renderCategorization tx.splits tx.category
            , renderCategoryMutationControls tx.splits mutationState
            , renderSplitEditor mutationState
            ]
        , div [ class_ "d-flex flex-column gap-1" ]
            [ div [ class_ "fw-semibold" ] [ text "Transfer" ]
            , renderTransfer tx mutationState
            ]
        , div [ class_ "d-flex flex-column gap-2 finance-detail-overlay__notes" ]
            [ div [ class_ "fw-semibold" ] [ text "Notes" ]
            , renderNotesMutation tx.notes mutationState
            ]
        , div [ class_ "d-flex flex-column gap-1" ]
            [ div [ class_ "fw-semibold" ] [ text "Adjustment" ]
            , renderAdjustment tx.adjustment
            ]
        , maybe (text "") (\message -> div [ class_ "alert alert-danger mb-0 finance-detail-overlay__mutation-error" ] [ text message ]) (mutationState.metadataError <|> mutationState.categoryError <|> mutationState.noteError)
        ]
  where
  renderCategorization splits category =
    if not (null splits) then
      ul [ class_ "mb-0 ps-3" ] (map renderSplit splits)
    else
      div [] [ text (maybe "Uncategorized" (\(FinanceTransactionCategory entry) -> entry.id) category) ]

  renderSplit (FinanceTransactionSplitRow split) =
    li [] [ text (split.category <> ": " <> show split.amount) ]

  renderTransfer tx mutationState =
    case tx.transfer of
      Nothing ->
        div [ class_ "d-flex flex-column gap-2" ]
          [ div [] [ text "No transfer link." ]
          , renderTransferMutationControls mutationState
          ]
      Just (FinanceTransferLink transfer) ->
        div [ class_ "d-flex flex-column gap-2" ]
          [ div [] [ text ("Linked transaction: " <> transfer.linkedTransactionId <> " (" <> transfer.linkType <> ")") ] ]

  renderTransferMutationControls mutationState =
    case mutationState.transferSelector of
      Nothing ->
        button
          [ class_ "btn btn-sm btn-outline-secondary finance-detail-overlay__transfer-open"
          , onClick (const OpenFinanceTransferSelector)
          ]
          [ text "Link transfer" ]
      Just selector ->
        let
          submitDisabled = selector.isLoading || selector.isSubmitting || selector.selectedTargetId == ""
        in
          div [ class_ "finance-detail-overlay__transfer-selector d-flex flex-column gap-2" ]
            [ if selector.isLoading then
                div [ class_ "small text-muted" ] [ text "Loading transfer candidates..." ]
              else if null selector.candidates then
                div [ class_ "small text-muted finance-detail-overlay__transfer-empty" ] [ text "No eligible target transaction in current scope." ]
              else
                select
                  [ class_ "form-select form-select-sm finance-detail-overlay__transfer-target"
                  , value selector.selectedTargetId
                  , onValueChange FinanceTransferTargetChanged
                  , disabled selector.isSubmitting
                  ]
                  ( map renderTransferCandidate selector.candidates
                  )
            , maybe (text "") (\message -> div [ class_ "alert alert-danger mb-0 finance-detail-overlay__transfer-error" ] [ text message ]) selector.submitError
            , div [ class_ "d-flex gap-2" ]
                [ button
                    [ class_ "btn btn-sm btn-outline-secondary finance-detail-overlay__transfer-cancel"
                    , onClick (const CancelFinanceTransferSelector)
                    , disabled selector.isSubmitting
                    ]
                    [ text "Cancel" ]
                , button
                    [ class_ "btn btn-sm btn-primary finance-detail-overlay__transfer-submit"
                    , onClick (const SubmitFinanceTransferLink)
                    , disabled submitDisabled
                    ]
                    [ text (if selector.isSubmitting then "Linking..." else "Link transfer") ]
                ]
            ]
    where
    renderTransferCandidate (FinanceTransaction candidate) =
      option [ value candidate.id ]
        [ text (StringCommon.joinWith " | " [ candidate.id, financeDirectionLabel candidate.direction, show candidate.amount, candidate.occurredAt ]) ]

  renderAdjustment maybeAdjustment =
    case maybeAdjustment of
      Nothing ->
        div [] [ text "No adjustment context." ]
      Just (FinanceTransactionAdjustment adjustment) ->
        div [] [ text ("Adjustment kind: " <> adjustment.kind) ]

  renderCategoryMutationControls splits mutationState =
    if mutationState.splitEditor /= Nothing then
      text ""
    else if not (null splits) then
      div [ class_ "small text-muted finance-detail-overlay__category-disabled" ]
        [ text "Single-category change is disabled for split transactions while split state is active." ]
    else
      div [ class_ "d-flex flex-column gap-2" ]
        [ select
            [ class_ "form-select form-select-sm finance-detail-overlay__category-select"
            , value mutationState.categoryDraft
            , onValueChange FinanceDetailCategoryChanged
            , disabled (mutationState.isSubmittingCategory || mutationState.isLoadingCategories)
            ]
            ( [ option [ value "" ] [ text "Select category" ] ] <> map renderCategoryOption mutationState.categories
            )
        , button
            [ class_ "btn btn-sm btn-outline-primary finance-detail-overlay__category-submit"
            , onClick (const SubmitFinanceDetailCategory)
            , disabled (mutationState.isSubmittingCategory || mutationState.isLoadingCategories)
            ]
            [ text (if mutationState.isSubmittingCategory then "Saving..." else "Save category") ]
        ]

  renderSplitEditor mutationState =
    case mutationState.splitEditor of
      Nothing ->
        button
          [ class_ "btn btn-sm btn-outline-secondary finance-detail-overlay__split-open"
          , onClick (const OpenFinanceSplitEditor)
          ]
          [ text "Edit split" ]
      Just editor ->
        let
          summary = splitSummary editor
          saveDisabled = editor.isSaving || not summary.isValid
        in
          div [ class_ "finance-detail-overlay__split-editor d-flex flex-column gap-2" ]
            [ div [ class_ "small text-muted" ] [ text ("Total: " <> show editor.transactionTotal) ]
            , if null editor.rows then
                div [ class_ "small text-muted finance-detail-overlay__split-empty" ] [ text "No split rows yet. Add a row to start splitting this transaction." ]
              else
                div [ class_ "d-flex flex-column gap-2" ] (mapWithIndex (renderSplitRow mutationState.categories editor.isSaving) editor.rows)
            , div [ class_ "small finance-detail-overlay__split-remainder" ]
                [ text ("Remainder: " <> show summary.remainder) ]
            , maybe (text "") (\message -> div [ class_ "alert alert-danger mb-0 finance-detail-overlay__split-error" ] [ text message ]) editor.submitError
            , div [ class_ "d-flex gap-2" ]
                [ button
                    [ class_ "btn btn-sm btn-outline-secondary finance-detail-overlay__split-add-row"
                    , onClick (const AddFinanceSplitRow)
                    , disabled editor.isSaving
                    ]
                    [ text "Add row" ]
                , button
                    [ class_ "btn btn-sm btn-outline-secondary finance-detail-overlay__split-cancel"
                    , onClick (const CancelFinanceSplitEditor)
                    , disabled editor.isSaving
                    ]
                    [ text "Cancel" ]
                , button
                    [ class_ "btn btn-sm btn-primary finance-detail-overlay__split-save"
                    , onClick (const SaveFinanceSplit)
                    , disabled saveDisabled
                    ]
                    [ text (if editor.isSaving then "Saving..." else "Save split") ]
                ]
            , if summary.isValid then text "" else div [ class_ "small text-danger" ] [ text "Split must have at least two rows, positive amounts, valid categories, and exact sum equality." ]
            ]

  renderSplitRow categories isSaving idx row =
    div [ class_ "d-flex gap-2 align-items-center finance-detail-overlay__split-row" ]
      [ input
          [ class_ "form-control form-control-sm finance-detail-overlay__split-row-amount"
          , value row.amountInput
          , onValueChange (FinanceSplitRowAmountChanged idx)
          , disabled isSaving
          ]
      , select
          [ class_ "form-select form-select-sm finance-detail-overlay__split-row-category"
          , value row.category
          , onValueChange (FinanceSplitRowCategoryChanged idx)
          , disabled isSaving
          ]
          ( [ option [ value "" ] [ text "Select category" ] ] <> map renderCategoryOption categories
          )
      , button
          [ class_ "btn btn-sm btn-outline-danger finance-detail-overlay__split-row-remove"
          , onClick (const (RemoveFinanceSplitRow idx))
          , disabled isSaving
          ]
          [ text "Remove" ]
      ]

  renderCategoryOption (FinanceCategory category) =
    option [ value category.id ] [ text category.id ]

  renderNotesMutation notes mutationState =
    div [ class_ "d-flex flex-column gap-2" ]
      [ div [ class_ "d-flex gap-2" ]
          [ input
              [ class_ "form-control form-control-sm finance-detail-overlay__new-note-input"
              , value mutationState.newNoteInput
              , placeholder "Add note"
              , onValueChange FinanceDetailNewNoteChanged
              , disabled mutationState.isAddingNote
              ]
          , button
              [ class_ "btn btn-sm btn-outline-primary finance-detail-overlay__new-note-submit"
              , onClick (const SubmitFinanceDetailNewNote)
              , disabled mutationState.isAddingNote
              ]
              [ text (if mutationState.isAddingNote then "Adding..." else "Add") ]
          ]
      , if null notes then
          div [ class_ "small text-muted" ] [ text "No notes." ]
        else
          ul [ class_ "mb-0 ps-3 d-flex flex-column gap-2" ] (map (renderNoteMutation mutationState) notes)
      ]

  renderNoteMutation mutationState (FinanceTransactionNote note) =
    let
      isEditing = mutationState.editingNoteId == Just note.id
      isDeleting = any (_ == note.id) mutationState.deletingNoteIds
    in
      li [ class_ "d-flex flex-column gap-1 finance-detail-overlay__note-row" ]
        [ if isEditing then
            div [ class_ "d-flex gap-2" ]
              [ input
                  [ class_ "form-control form-control-sm finance-detail-overlay__edit-note-input"
                  , value mutationState.editingNoteInput
                  , onValueChange FinanceDetailNoteEditChanged
                  , disabled mutationState.isSubmittingNoteEdit
                  ]
              , button
                  [ class_ "btn btn-sm btn-outline-primary finance-detail-overlay__edit-note-save"
                  , onClick (const SubmitFinanceDetailNoteEdit)
                  , disabled mutationState.isSubmittingNoteEdit
                  ]
                  [ text (if mutationState.isSubmittingNoteEdit then "Saving..." else "Save") ]
              , button
                  [ class_ "btn btn-sm btn-outline-secondary finance-detail-overlay__edit-note-cancel"
                  , onClick (const CancelFinanceDetailNoteEdit)
                  , disabled mutationState.isSubmittingNoteEdit
                  ]
                  [ text "Cancel" ]
              ]
          else
            div [ class_ "d-flex justify-content-between align-items-center gap-2" ]
              [ span [] [ text ("#" <> note.id <> ": " <> note.text) ]
              , div [ class_ "d-flex gap-1" ]
                  [ button
                      [ class_ "btn btn-sm btn-outline-secondary finance-detail-overlay__note-edit"
                      , onClick (const (StartFinanceDetailNoteEdit note.id))
                      , disabled isDeleting
                      ]
                      [ text "Edit" ]
                  , button
                      [ class_ "btn btn-sm btn-outline-danger finance-detail-overlay__note-delete"
                      , onClick (const (SubmitFinanceDetailNoteDelete note.id))
                      , disabled isDeleting
                      ]
                      [ text (if isDeleting then "Deleting..." else "Delete") ]
                  ]
              ]
        ]

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

initialFinanceDetailMutationState :: FinanceTransactions.FinanceDetailSnapshot -> FinanceDetailMutationState
initialFinanceDetailMutationState { transaction: FinanceTransaction tx } =
  { categories: []
  , isLoadingCategories: true
  , counterpartyDraft: fromMaybe "" tx.counterparty
  , descriptionDraft: fromMaybe "" tx.description
  , isSubmittingMetadata: false
  , metadataError: Nothing
  , categoryDraft: foldMap (\(FinanceTransactionCategory category) -> category.id) tx.category
  , categoryError: Nothing
  , isSubmittingCategory: false
  , newNoteInput: ""
  , noteError: Nothing
  , isAddingNote: false
  , editingNoteId: Nothing
  , editingNoteInput: ""
  , isSubmittingNoteEdit: false
  , deletingNoteIds: []
  , splitEditor: Nothing
  , transferSelector: Nothing
  , hasSuccessfulMutations: false
  }

initialFinanceSplitEditorState :: forall r. { amount :: Number, splits :: Array FinanceTransactionSplitRow, category :: Maybe FinanceTransactionCategory | r } -> FinanceSplitEditorState
initialFinanceSplitEditorState tx =
  { transactionTotal: tx.amount
  , rows: map splitEditorRowFromTransactionSplit tx.splits
  , submitError: Nothing
  , isSaving: false
  }

splitEditorRowFromTransactionSplit :: FinanceTransactionSplitRow -> FinanceSplitEditorRow
splitEditorRowFromTransactionSplit (FinanceTransactionSplitRow split) =
  { amountInput: show split.amount
  , category: split.category
  }

updateSplitRows :: Int -> (FinanceSplitEditorRow -> FinanceSplitEditorRow) -> Array FinanceSplitEditorRow -> Array FinanceSplitEditorRow
updateSplitRows targetIndex updater =
  mapWithIndex (\idx row -> if idx == targetIndex then updater row else row)

mapMaybeWithIndex :: forall a b. (Int -> a -> Maybe b) -> Array a -> Array b
mapMaybeWithIndex mapper rows =
  mapMaybe identity (mapWithIndex mapper rows)

type SplitSummary =
  { remainder :: Number
  , isValid :: Boolean
  }

splitSummary :: FinanceSplitEditorState -> SplitSummary
splitSummary editor =
  case buildValidSplitPayload editor of
    Left _ ->
      { remainder: editor.transactionTotal - sumSplitRows editor.rows
      , isValid: false
      }
    Right _ ->
      { remainder: editor.transactionTotal - sumSplitRows editor.rows
      , isValid: true
      }

sumSplitRows :: Array FinanceSplitEditorRow -> Number
sumSplitRows rows =
  foldl (\acc row -> acc + fromMaybe 0.0 (Number.fromString row.amountInput)) 0.0 rows

buildValidSplitPayload :: FinanceSplitEditorState -> Either String (Array FinanceTransactionSplitRow)
buildValidSplitPayload editor =
  if length editor.rows < 2 then
    Left "At least two split rows are required."
  else
    case traverse parseSplitRow editor.rows of
      Left message -> Left message
      Right rows ->
        let
          total = foldl (\acc (FinanceTransactionSplitRow row) -> acc + row.amount) 0.0 rows
          remainder = editor.transactionTotal - total
        in
          if abs remainder > 0.0 then
            Left "Split rows must sum exactly to transaction total."
          else
            Right rows
  where
  parseSplitRow row =
    if row.category == "" then
      Left "Each split row requires a category."
    else
      case Number.fromString row.amountInput of
        Nothing -> Left "Each split row requires a valid numeric amount."
        Just amount ->
          if amount <= 0.0 then
            Left "Each split row amount must be strictly positive."
          else
            Right (FinanceTransactionSplitRow { amount, category: row.category })

submitFinanceSplit :: String -> Array FinanceTransactionSplitRow -> Aff (Either String Unit)
submitFinanceSplit transactionId splits = do
  result <- splitTransaction transactionId (SplitFinanceTransaction { splits })
  case result of
    Left err ->
      pure (Left ("Unable to save split: " <> printError err))
    Right response ->
      if statusOk response then
        pure (Right unit)
      else
        pure (Left ("Unable to save split: status " <> show (unwrap response.status)))

fetchTransferCandidates :: String -> FinanceTransactionsQuery -> Aff (Either String (Array FinanceTransaction))
fetchTransferCandidates sourceTransactionId query = do
  result <- getTransactions query
  case result of
    Left err ->
      pure (Left ("Unable to load transfer candidates: " <> printError err))
    Right response ->
      if statusOk response then
        case (lmap show (decodeJson response.body :: Either _ (Array FinanceTransaction))) of
          Left decodeError ->
            pure (Left decodeError)
          Right transactions ->
            pure (Right (filterTransferCandidates sourceTransactionId transactions))
      else
        pure (Left ("Unable to load transfer candidates: status " <> show (unwrap response.status)))

filterTransferCandidates :: String -> Array FinanceTransaction -> Array FinanceTransaction
filterTransferCandidates sourceTransactionId =
  filter
    ( \(FinanceTransaction tx) ->
        tx.id /= sourceTransactionId && tx.transfer == Nothing
    )

submitFinanceTransferLink :: String -> String -> Aff (Either String Unit)
submitFinanceTransferLink sourceTransactionId targetTransactionId = do
  result <-
    linkTransfer
      ( LinkFinanceTransfer
          { sourceTransactionId
          , targetTransactionId
          , linkType: "transfer"
          }
      )
  case result of
    Left err ->
      pure (Left ("Unable to link transfer: " <> printError err))
    Right response ->
      if statusOk response then
        pure (Right unit)
      else
        pure (Left ("Unable to link transfer: status " <> show (unwrap response.status)))

fetchFinanceDetailCategories :: Aff (Either String (Array FinanceCategory))
fetchFinanceDetailCategories = do
  result <- getCategories
  case result of
    Left err ->
      pure (Left ("Unable to load categories: " <> printError err))
    Right response ->
      if statusOk response then
        pure
          ( map
              (filter (\(FinanceCategory category) -> category.selectable))
              (lmap show (decodeJson response.body :: Either _ (Array FinanceCategory)))
          )
      else
        pure (Left ("Unable to load categories: status " <> show (unwrap response.status)))

submitFinanceDetailCategory :: String -> String -> Aff (Either String Unit)
submitFinanceDetailCategory transactionId categoryId = do
  result <- categorizeTransaction transactionId (CategorizeFinanceTransaction { category: categoryId })
  case result of
    Left err ->
      pure (Left ("Unable to categorize transaction: " <> printError err))
    Right response ->
      if statusOk response then pure (Right unit)
      else pure (Left ("Unable to categorize transaction: status " <> show (unwrap response.status)))

submitFinanceDetailMetadata :: String -> String -> String -> Aff (Either String FinanceTransaction)
submitFinanceDetailMetadata transactionId counterpartyRaw descriptionRaw = do
  result <-
    updateTransactionMetadata
      transactionId
      ( UpdateFinanceTransactionMetadata
          { counterparty: Just (nonEmptyTrimmed counterpartyRaw)
          , description: Just (nonEmptyTrimmed descriptionRaw)
          }
      )
  case result of
    Left err ->
      pure (Left ("Unable to update metadata: " <> printError err))
    Right response ->
      if statusOk response then
        pure (lmap show (decodeJson response.body :: Either _ FinanceTransaction))
      else
        pure (Left ("Unable to update metadata: status " <> show (unwrap response.status)))

submitFinanceDetailNewNote :: String -> String -> Aff (Either String FinanceTransactionNote)
submitFinanceDetailNewNote transactionId noteText = do
  result <- createTransactionNote transactionId (CreateFinanceTransactionNote { text: noteText })
  case result of
    Left err ->
      pure (Left ("Unable to create note: " <> printError err))
    Right response ->
      if statusOk response then
        pure (lmap show (decodeJson response.body :: Either _ FinanceTransactionNote))
      else
        pure (Left ("Unable to create note: status " <> show (unwrap response.status)))

submitFinanceDetailNoteEdit :: String -> String -> String -> Aff (Either String FinanceTransactionNote)
submitFinanceDetailNoteEdit transactionId noteId noteText = do
  result <- updateTransactionNote transactionId noteId (UpdateFinanceTransactionNote { text: noteText })
  case result of
    Left err ->
      pure (Left ("Unable to update note: " <> printError err))
    Right response ->
      if statusOk response then
        pure (lmap show (decodeJson response.body :: Either _ FinanceTransactionNote))
      else
        pure (Left ("Unable to update note: status " <> show (unwrap response.status)))

submitFinanceDetailNoteDelete :: String -> String -> Aff (Either String Unit)
submitFinanceDetailNoteDelete transactionId noteId = do
  result <- deleteTransactionNote transactionId noteId
  case result of
    Left err ->
      pure (Left ("Unable to delete note: " <> printError err))
    Right response ->
      if statusOk response then pure (Right unit)
      else pure (Left ("Unable to delete note: status " <> show (unwrap response.status)))

mapDetailSnapshotTransaction :: (FinanceTransaction -> FinanceTransaction) -> FinanceTransactions.FinanceDetailSnapshot -> FinanceTransactions.FinanceDetailSnapshot
mapDetailSnapshotTransaction f snapshot =
  snapshot
    { transaction = f snapshot.transaction
    }

setTransactionCategory :: String -> FinanceTransaction -> FinanceTransaction
setTransactionCategory categoryId (FinanceTransaction tx) =
  FinanceTransaction
    ( tx
        { category =
            if categoryId == "" then
              Nothing
            else
              Just (FinanceTransactionCategory { id: categoryId })
        }
    )

setTransactionSplits :: Array FinanceTransactionSplitRow -> FinanceTransaction -> FinanceTransaction
setTransactionSplits splits (FinanceTransaction tx) =
  FinanceTransaction
    ( tx
        { splits = splits
        , category = Nothing
        }
    )

setTransactionTransfer :: String -> FinanceTransaction -> FinanceTransaction
setTransactionTransfer targetTransactionId (FinanceTransaction tx) =
  FinanceTransaction
    ( tx
        { transfer = Just (FinanceTransferLink { linkedTransactionId: targetTransactionId, linkType: "transfer" })
        }
    )

appendTransactionNote :: FinanceTransactionNote -> FinanceTransaction -> FinanceTransaction
appendTransactionNote note (FinanceTransaction tx) =
  FinanceTransaction (tx { notes = snoc tx.notes note })

replaceTransactionNote :: FinanceTransactionNote -> FinanceTransaction -> FinanceTransaction
replaceTransactionNote (FinanceTransactionNote note) (FinanceTransaction tx) =
  FinanceTransaction
    ( tx
        { notes =
            map
              ( \(FinanceTransactionNote current) ->
                  if current.id == note.id then
                    FinanceTransactionNote note
                  else
                    FinanceTransactionNote current
              )
              tx.notes
        }
    )

replaceTransactionNoteText :: String -> String -> FinanceTransaction -> FinanceTransaction
replaceTransactionNoteText noteId nextText (FinanceTransaction tx) =
  FinanceTransaction
    ( tx
        { notes =
            map
              ( \(FinanceTransactionNote note) ->
                  if note.id == noteId then
                    FinanceTransactionNote (note { text = nextText })
                  else
                    FinanceTransactionNote note
              )
              tx.notes
        }
    )

removeTransactionNote :: String -> FinanceTransaction -> FinanceTransaction
removeTransactionNote noteId (FinanceTransaction tx) =
  FinanceTransaction
    ( tx
        { notes =
            filter
              ( \(FinanceTransactionNote note) -> note.id /= noteId
              )
              tx.notes
        }
    )

findDetailNoteText :: String -> Maybe FinanceTransactions.FinanceDetailSnapshot -> Maybe String
findDetailNoteText noteId maybeSnapshot =
  case maybeSnapshot of
    Nothing ->
      Nothing
    Just { transaction: FinanceTransaction tx } ->
      map
        ( \(FinanceTransactionNote note) -> note.text
        )
        (find (\(FinanceTransactionNote note) -> note.id == noteId) tx.notes)

financeCreateStateAfterRouteChange :: Route -> Maybe FinanceOverlay -> Maybe FinanceCreateState -> Maybe FinanceCreateState
financeCreateStateAfterRouteChange route financeOverlay financeCreateState =
  if routeCanOpenFinanceOverlay route then
    case financeOverlay of
      Just (FinanceCreateOverlay _) -> financeCreateState
      _ -> Nothing
  else
    Nothing

financeDetailMutationStateAfterRouteChange :: Route -> Maybe FinanceOverlay -> Maybe FinanceDetailMutationState -> Maybe FinanceDetailMutationState
financeDetailMutationStateAfterRouteChange route financeOverlay financeDetailMutationState =
  if routeCanOpenFinanceOverlay route then
    case financeOverlay of
      Just (FinanceDetailOverlay _) -> financeDetailMutationState
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
