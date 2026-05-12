module Pages.FinanceTransactions
  ( component
  , Output(..)
  , LedgerRemoteState(..)
  , LedgerBodyState(..)
  , LedgerContext
  , FinanceLedgerRow
  , FinanceDetailSnapshot
  , beginLedgerLoad
  , applyLedgerLoadSuccess
  , applyLedgerLoadFailure
  , deriveLedgerBodyState
  , resolveAccountLabel
  , buildLedgerRows
  , buildDetailSnapshot
  ) where

import Prelude hiding (div)

import Affjax (printError)
import Affjax.Web (Response)
import Api.Finance (getAccounts, getCategories, getTransactions)
import Api.FinanceContract
  ( FinanceAccount(..)
  , FinanceAccountsQuery(..)
  , FinanceCategory(..)
  , FinanceReportDirection(..)
  , FinanceTransaction(..)
  , FinanceTransactionCategory(..)
  , FinanceTransactionDirection(..)
  , FinanceTransactionsQuery(..)
  )
import Control.Monad.RWS (get, modify_)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (any, delete, filter, find, null, snoc)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap, foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval) as H
import Halogen (raise)
import Halogen.HTML (button, div, option, select, span, table, tbody, td, text, th, thead, tr, input)
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (value)
import Ui.Utils (class_)

type LedgerContext =
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

type Input = LedgerContext

type State =
  { context :: LedgerContext
  , draftContext :: LedgerContext
  , draftAmountMinInput :: String
  , draftAmountMaxInput :: String
  , draftCategoryInSelection :: String
  , draftCategoryNotInSelection :: String
  , filterError :: Maybe String
  , remoteState :: LedgerRemoteState
  }

data Output
  = RouteSyncRequested LedgerContext
  | OpenTransactionDetail FinanceDetailSnapshot

data LedgerRemoteState
  = LedgerLoading
  | LedgerLoadError String
  | LedgerLoaded
      { transactions :: Array FinanceTransaction
      , accounts :: Array FinanceAccount
      , categories :: Array FinanceCategory
      }

type FinanceLedgerRow =
  { id :: String
  , directionLabel :: String
  , directionClass :: String
  , amountLabel :: String
  , accountLabel :: String
  , occurredAtLabel :: String
  , categoryLabel :: String
  , counterpartyLabel :: String
  , descriptionLabel :: String
  , hasSplit :: Boolean
  , hasTransfer :: Boolean
  , hasNote :: Boolean
  , hasAdjustment :: Boolean
  }

type FinanceDetailSnapshot =
  { transactionId :: String
  , accountLabel :: String
  , transaction :: FinanceTransaction
  }

data LedgerBodyState
  = LedgerBodyLoading
  | LedgerBodyError String
  | LedgerBodyEmpty
  | LedgerBodyNoResults
  | LedgerBodyRows (Array FinanceLedgerRow)

derive instance ledgerRemoteStateEq :: Eq LedgerRemoteState
derive instance ledgerBodyStateEq :: Eq LedgerBodyState

data Action
  = Initialize
  | RetryLoad
  | LedgerLoadedResult (Either String { transactions :: Array FinanceTransaction, accounts :: Array FinanceAccount, categories :: Array FinanceCategory })
  | UpdateAccountId String
  | UpdateFrom String
  | UpdateTo String
  | UpdateDirection String
  | UpdateSearch String
  | UpdateAmountMin String
  | UpdateAmountMax String
  | UpdateCategoryInSelection String
  | AddCategoryIn
  | RemoveCategoryIn String
  | UpdateCategoryNotInSelection String
  | AddCategoryNotIn
  | RemoveCategoryNotIn String
  | ApplyFilters
  | ClearAccountId
  | ClearFrom
  | ClearTo
  | ClearDirection
  | ClearSearch
  | ClearAmountMin
  | ClearAmountMax
  | ClearCategoryIn String
  | ClearCategoryNotIn String
  | ResetFilters
  | OpenRow String

component :: forall q. H.Component q Input Output Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }

initialState :: Input -> State
initialState input =
  { context: input
  , draftContext: input
  , draftAmountMinInput: foldMap show input.amountMin
  , draftAmountMaxInput: foldMap show input.amountMax
  , draftCategoryInSelection: ""
  , draftCategoryNotInSelection: ""
  , filterError: Nothing
  , remoteState: LedgerLoading
  }

beginLedgerLoad :: State -> State
beginLedgerLoad state =
  state { remoteState = LedgerLoading }

applyLedgerLoadSuccess :: Array FinanceTransaction -> Array FinanceAccount -> Array FinanceCategory -> State -> State
applyLedgerLoadSuccess transactions accounts categories state =
  state
    { remoteState = LedgerLoaded { transactions, accounts, categories }
    }

applyLedgerLoadFailure :: String -> State -> State
applyLedgerLoadFailure message state =
  state { remoteState = LedgerLoadError message }

deriveLedgerBodyState :: State -> LedgerBodyState
deriveLedgerBodyState { context, remoteState } =
  case remoteState of
    LedgerLoading ->
      LedgerBodyLoading
    LedgerLoadError message ->
      LedgerBodyError message
    LedgerLoaded { transactions, accounts } ->
      if null transactions then
        if hasActiveContext context then LedgerBodyNoResults else LedgerBodyEmpty
      else
        LedgerBodyRows (buildLedgerRows accounts transactions)

hasActiveContext :: LedgerContext -> Boolean
hasActiveContext context =
  isJust context.accountId || isJust context.from || isJust context.to || isJust context.direction || not (null context.categoryIn) || not (null context.categoryNotIn) || isJust context.amountMin || isJust context.amountMax || isJust context.search

buildLedgerRows :: Array FinanceAccount -> Array FinanceTransaction -> Array FinanceLedgerRow
buildLedgerRows accounts transactions =
  map (buildLedgerRow accounts) transactions

buildDetailSnapshot :: Array FinanceAccount -> Array FinanceTransaction -> String -> Maybe FinanceDetailSnapshot
buildDetailSnapshot accounts transactions transactionId =
  map
    ( \(FinanceTransaction tx) ->
        { transactionId: tx.id
        , accountLabel: resolveAccountLabel accounts tx.accountId
        , transaction: FinanceTransaction tx
        }
    )
    (find (\(FinanceTransaction tx) -> tx.id == transactionId) transactions)

buildLedgerRow :: Array FinanceAccount -> FinanceTransaction -> FinanceLedgerRow
buildLedgerRow accounts (FinanceTransaction tx) =
  { id: tx.id
  , directionLabel: directionLabel tx.direction
  , directionClass: directionClass tx.direction
  , amountLabel: amountLabel tx.direction tx.amount
  , accountLabel: resolveAccountLabel accounts tx.accountId
  , occurredAtLabel: tx.occurredAt
  , categoryLabel: maybe "-" (\(FinanceTransactionCategory category) -> category.id) tx.category
  , counterpartyLabel: fromMaybe "-" tx.counterparty
  , descriptionLabel: fromMaybe "-" tx.description
  , hasSplit: not (null tx.splits)
  , hasTransfer: isJust tx.transfer
  , hasNote: not (null tx.notes)
  , hasAdjustment: isJust tx.adjustment
  }

resolveAccountLabel :: Array FinanceAccount -> String -> String
resolveAccountLabel accounts accountId =
  maybe accountId (\(FinanceAccount account) -> account.name)
    (find (\(FinanceAccount account) -> account.id == accountId) accounts)

directionLabel :: FinanceTransactionDirection -> String
directionLabel = case _ of
  TransactionSent -> "Sent"
  TransactionReceived -> "Received"

directionClass :: FinanceTransactionDirection -> String
directionClass = case _ of
  TransactionSent -> "text-bg-danger"
  TransactionReceived -> "text-bg-success"

amountLabel :: FinanceTransactionDirection -> Number -> String
amountLabel direction amount =
  signPrefix direction <> show amount
  where
  signPrefix TransactionSent = "-"
  signPrefix TransactionReceived = "+"

render :: State -> H.ComponentHTML Action () Aff
render state =
  div [ class_ "finance-ledger-workspace" ]
    [ renderFilterBar state
    , case deriveLedgerBodyState state of
        LedgerBodyLoading ->
          div [ class_ "text-muted finance-ledger-loading" ] [ text "Loading transactions..." ]
        LedgerBodyError message ->
          div [ class_ "alert alert-danger d-flex flex-column align-items-start gap-2 finance-ledger-error" ]
            [ div [] [ text message ]
            , button [ class_ "btn btn-outline-danger btn-sm finance-ledger-retry", onClick (const RetryLoad) ] [ text "Retry" ]
            ]
        LedgerBodyEmpty ->
          renderEmptyState "No transactions yet" "Create your first transaction to start your ledger."
        LedgerBodyNoResults ->
          renderEmptyState "No matching transactions" "Current account/date context has no matching transaction."
        LedgerBodyRows rows ->
          renderRows rows
    ]

renderFilterBar :: State -> H.ComponentHTML Action () Aff
renderFilterBar { context, draftContext, draftAmountMinInput, draftAmountMaxInput, remoteState, draftCategoryInSelection, draftCategoryNotInSelection, filterError } =
  div [ class_ "finance-ledger-filters card shadow-sm border-0 mb-3" ]
    [ div [ class_ "card-body d-flex flex-column gap-2" ]
        [ div [ class_ "d-flex flex-wrap align-items-end gap-2" ]
            [ div [ class_ "d-flex flex-column" ]
                [ span [ class_ "small text-muted" ] [ text "Account" ]
                , renderAccountSelect remoteState draftContext.accountId
                ]
            , div [ class_ "d-flex flex-column" ]
                [ span [ class_ "small text-muted" ] [ text "Direction" ]
                , renderDirectionSelect draftContext.direction
                ]
            , div [ class_ "d-flex flex-column" ]
                [ span [ class_ "small text-muted" ] [ text "From" ]
                , input [ class_ "form-control form-control-sm finance-ledger-filter-from", value (fromMaybe "" draftContext.from), onValueChange UpdateFrom ]
                ]
            , div [ class_ "d-flex flex-column" ]
                [ span [ class_ "small text-muted" ] [ text "To" ]
                , input [ class_ "form-control form-control-sm finance-ledger-filter-to", value (fromMaybe "" draftContext.to), onValueChange UpdateTo ]
                ]
            , div [ class_ "d-flex flex-column" ]
                [ span [ class_ "small text-muted" ] [ text "Amount min (cents)" ]
                , input [ class_ "form-control form-control-sm finance-ledger-filter-amount-min", value draftAmountMinInput, onValueChange UpdateAmountMin ]
                ]
            , div [ class_ "d-flex flex-column" ]
                [ span [ class_ "small text-muted" ] [ text "Amount max (cents)" ]
                , input [ class_ "form-control form-control-sm finance-ledger-filter-amount-max", value draftAmountMaxInput, onValueChange UpdateAmountMax ]
                ]
            , div [ class_ "d-flex flex-column" ]
                [ span [ class_ "small text-muted" ] [ text "Search" ]
                , input [ class_ "form-control form-control-sm finance-ledger-filter-search", value (fromMaybe "" draftContext.search), onValueChange UpdateSearch ]
                ]
            ]
        , renderCategorySelectors remoteState draftCategoryInSelection draftCategoryNotInSelection draftContext.categoryIn draftContext.categoryNotIn
        , maybe (text "") (\message -> div [ class_ "alert alert-danger mb-0 finance-ledger-filter-error" ] [ text message ]) filterError
        , div [ class_ "d-flex flex-wrap gap-2" ]
            [ button [ class_ "btn btn-primary btn-sm finance-ledger-apply", onClick (const ApplyFilters) ] [ text "Apply" ]
            , button [ class_ "btn btn-outline-secondary btn-sm finance-ledger-reset", onClick (const ResetFilters) ] [ text "Reset" ]
            ]
        , div [ class_ "d-flex flex-wrap gap-2" ]
            [ activeContextPill "accountId" context.accountId ClearAccountId
            , activeContextPill "from" context.from ClearFrom
            , activeContextPill "to" context.to ClearTo
            , activeContextPill "direction" (map encodeDirection context.direction) ClearDirection
            , activeContextPill "search" context.search ClearSearch
            , activeContextPill "amountMin" (map show context.amountMin) ClearAmountMin
            , activeContextPill "amountMax" (map show context.amountMax) ClearAmountMax
            ]
        , div [ class_ "d-flex flex-wrap gap-2" ]
            ( map (\value -> arrayPill ("categoryIn=" <> value) (ClearCategoryIn value)) context.categoryIn
                <> map (\value -> arrayPill ("categoryNotIn=" <> value) (ClearCategoryNotIn value)) context.categoryNotIn
            )
        ]
    ]

renderCategorySelectors :: LedgerRemoteState -> String -> String -> Array String -> Array String -> H.ComponentHTML Action () Aff
renderCategorySelectors remoteState selectedIn selectedNotIn selectedInValues selectedNotInValues =
  let
    categories =
      case remoteState of
        LedgerLoaded { categories: allCategories } ->
          map (\(FinanceCategory category) -> category.id) allCategories
        _ ->
          []
  in
    div [ class_ "d-flex flex-wrap align-items-end gap-2" ]
      [ div [ class_ "d-flex flex-column" ]
          [ span [ class_ "small text-muted" ] [ text "Include category" ]
          , select [ class_ "form-select form-select-sm finance-ledger-filter-category-in", value selectedIn, onValueChange UpdateCategoryInSelection ]
              ([ option [ value "" ] [ text "Select category" ] ] <> map (\id -> option [ value id ] [ text id ]) categories)
          ]
      , button [ class_ "btn btn-outline-secondary btn-sm", onClick (const AddCategoryIn) ] [ text "Add include" ]
      , div [ class_ "d-flex flex-wrap gap-1" ] (map (\id -> arrayPill ("in:" <> id) (RemoveCategoryIn id)) selectedInValues)
      , div [ class_ "d-flex flex-column" ]
          [ span [ class_ "small text-muted" ] [ text "Exclude category" ]
          , select [ class_ "form-select form-select-sm finance-ledger-filter-category-not-in", value selectedNotIn, onValueChange UpdateCategoryNotInSelection ]
              ([ option [ value "" ] [ text "Select category" ] ] <> map (\id -> option [ value id ] [ text id ]) categories)
          ]
      , button [ class_ "btn btn-outline-secondary btn-sm", onClick (const AddCategoryNotIn) ] [ text "Add exclude" ]
      , div [ class_ "d-flex flex-wrap gap-1" ] (map (\id -> arrayPill ("not-in:" <> id) (RemoveCategoryNotIn id)) selectedNotInValues)
      ]

arrayPill :: String -> Action -> H.ComponentHTML Action () Aff
arrayPill label action =
  button [ class_ "btn btn-outline-secondary btn-sm finance-ledger-active-filter", onClick (const action) ]
    [ text (label <> " ×") ]

renderDirectionSelect :: Maybe FinanceReportDirection -> H.ComponentHTML Action () Aff
renderDirectionSelect selectedDirection =
  select [ class_ "form-select form-select-sm finance-ledger-filter-direction", value (fromMaybe "" (map encodeDirection selectedDirection)), onValueChange UpdateDirection ]
    [ option [ value "" ] [ text "All directions" ]
    , option [ value "sent" ] [ text "Sent" ]
    , option [ value "received" ] [ text "Received" ]
    ]

encodeDirection :: FinanceReportDirection -> String
encodeDirection = case _ of
  ReportSent -> "sent"
  ReportReceived -> "received"
  ReportAll -> "all"

renderAccountSelect :: LedgerRemoteState -> Maybe String -> H.ComponentHTML Action () Aff
renderAccountSelect remoteState selectedId =
  let
    options =
      case remoteState of
        LedgerLoaded { accounts } ->
          map (\(FinanceAccount account) -> { id: account.id, name: account.name }) accounts
        _ ->
          []
  in
    select [ class_ "form-select form-select-sm finance-ledger-filter-account", value (fromMaybe "" selectedId), onValueChange UpdateAccountId ]
      ( [ option [ value "" ] [ text "All accounts" ] ]
          <> map (\account -> option [ value account.id ] [ text account.name ]) options
      )

activeContextPill :: String -> Maybe String -> Action -> H.ComponentHTML Action () Aff
activeContextPill label maybeValue clearAction =
  case maybeValue of
    Nothing ->
      text ""
    Just valueText ->
      button [ class_ "btn btn-outline-secondary btn-sm finance-ledger-active-filter", onClick (const clearAction) ]
        [ text (label <> "=" <> valueText <> " ×") ]

renderRows :: Array FinanceLedgerRow -> H.ComponentHTML Action () Aff
renderRows rows =
  div [ class_ "table-responsive finance-ledger-table-wrap" ]
    [ table [ class_ "table align-middle finance-ledger-table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Amount" ]
                , th [] [ text "Direction" ]
                , th [] [ text "Account" ]
                , th [] [ text "Occurred At" ]
                , th [] [ text "Category" ]
                , th [] [ text "Counterparty" ]
                , th [] [ text "Description" ]
                , th [] [ text "Facts" ]
                ]
            ]
        , tbody [] (map renderRow rows)
        ]
    ]

renderRow :: FinanceLedgerRow -> H.ComponentHTML Action () Aff
renderRow row =
  tr [ class_ ("finance-ledger-row finance-ledger-row--actionable" <> guard row.hasAdjustment " finance-ledger-row--adjustment"), onClick (const (OpenRow row.id)) ]
    [ td [ class_ "finance-ledger-row__amount" ] [ text row.amountLabel ]
    , td [ class_ "finance-ledger-row__direction" ] [ span [ class_ ("badge " <> row.directionClass) ] [ text row.directionLabel ] ]
    , td [ class_ "finance-ledger-row__account" ] [ text row.accountLabel ]
    , td [ class_ "finance-ledger-row__occurred-at" ] [ text row.occurredAtLabel ]
    , td [ class_ "finance-ledger-row__category" ] [ text row.categoryLabel ]
    , td [ class_ "finance-ledger-row__counterparty" ] [ text row.counterpartyLabel ]
    , td [ class_ "finance-ledger-row__description" ] [ text row.descriptionLabel ]
    , td [ class_ "finance-ledger-row__facts" ] [ renderFacts row ]
    ]

renderFacts :: FinanceLedgerRow -> H.ComponentHTML Action () Aff
renderFacts row =
  if null facts then
    text "-"
  else
    div [ class_ "d-flex flex-wrap gap-1" ] facts
  where
  facts =
    fold
      [ guard row.hasSplit [ badge "split" ]
      , guard row.hasTransfer [ badge "transfer" ]
      , guard row.hasNote [ badge "note" ]
      , guard row.hasAdjustment [ badge "adjustment" ]
      ]
  badge label = span [ class_ "badge text-bg-secondary finance-ledger-fact" ] [ text label ]

renderEmptyState :: String -> String -> H.ComponentHTML Action () Aff
renderEmptyState title subtitle =
  div [ class_ "row entity-empty finance-ledger-empty" ]
    [ div [ class_ "entity-empty-title" ] [ text title ]
    , div [ class_ "entity-empty-subtitle" ] [ text subtitle ]
    ]

handleAction :: Action -> H.HalogenM State Action () Output Aff Unit
handleAction = case _ of
  Initialize -> do
    state <- get
    modify_ (const (beginLedgerLoad state))
    loadLedger
  RetryLoad -> do
    modify_ beginLedgerLoad
    loadLedger
  LedgerLoadedResult result ->
    case result of
      Left message ->
        modify_ (applyLedgerLoadFailure message)
      Right { transactions, accounts, categories } ->
        modify_ (applyLedgerLoadSuccess transactions accounts categories)
  UpdateAccountId raw ->
    modify_ \state ->
      state { draftContext = state.draftContext { accountId = toOptional raw } }
  UpdateFrom raw ->
    modify_ \state ->
      state { draftContext = state.draftContext { from = toOptional raw } }
  UpdateTo raw ->
    modify_ \state ->
      state { draftContext = state.draftContext { to = toOptional raw } }
  UpdateDirection raw ->
    modify_ \state ->
      state { draftContext = state.draftContext { direction = decodeDirection raw }, filterError = Nothing }
  UpdateSearch raw ->
    modify_ \state ->
      state { draftContext = state.draftContext { search = toOptional raw }, filterError = Nothing }
  UpdateAmountMin raw ->
    modify_ _ { draftAmountMinInput = raw, filterError = Nothing }
  UpdateAmountMax raw ->
    modify_ _ { draftAmountMaxInput = raw, filterError = Nothing }
  UpdateCategoryInSelection raw ->
    modify_ _ { draftCategoryInSelection = raw, filterError = Nothing }
  AddCategoryIn ->
    modify_ \state ->
      if state.draftCategoryInSelection == "" || any (_ == state.draftCategoryInSelection) state.draftContext.categoryIn then
        state
      else
        state { draftContext = state.draftContext { categoryIn = snoc state.draftContext.categoryIn state.draftCategoryInSelection }, draftCategoryInSelection = "", filterError = Nothing }
  RemoveCategoryIn value ->
    modify_ \state ->
      state { draftContext = state.draftContext { categoryIn = delete value state.draftContext.categoryIn }, filterError = Nothing }
  UpdateCategoryNotInSelection raw ->
    modify_ _ { draftCategoryNotInSelection = raw, filterError = Nothing }
  AddCategoryNotIn ->
    modify_ \state ->
      if state.draftCategoryNotInSelection == "" || any (_ == state.draftCategoryNotInSelection) state.draftContext.categoryNotIn then
        state
      else
        state { draftContext = state.draftContext { categoryNotIn = snoc state.draftContext.categoryNotIn state.draftCategoryNotInSelection }, draftCategoryNotInSelection = "", filterError = Nothing }
  RemoveCategoryNotIn value ->
    modify_ \state ->
      state { draftContext = state.draftContext { categoryNotIn = delete value state.draftContext.categoryNotIn }, filterError = Nothing }
  ApplyFilters -> do
    state <- get
    case validateAndNormalizeContext state.draftContext state.draftAmountMinInput state.draftAmountMaxInput of
      Left message ->
        modify_ _ { filterError = Just message }
      Right nextContext -> do
        modify_ _ { context = nextContext, draftContext = nextContext, draftAmountMinInput = foldMap show nextContext.amountMin, draftAmountMaxInput = foldMap show nextContext.amountMax, filterError = Nothing }
        raise (RouteSyncRequested nextContext)
        modify_ beginLedgerLoad
        loadLedger
  ClearAccountId ->
    clearOne (_ { accountId = Nothing })
  ClearFrom ->
    clearOne (_ { from = Nothing })
  ClearTo ->
    clearOne (_ { to = Nothing })
  ClearDirection ->
    clearOne (_ { direction = Nothing })
  ClearSearch ->
    clearOne (_ { search = Nothing })
  ClearAmountMin -> do
    modify_ _ { draftAmountMinInput = "" }
    clearOne (_ { amountMin = Nothing })
  ClearAmountMax -> do
    modify_ _ { draftAmountMaxInput = "" }
    clearOne (_ { amountMax = Nothing })
  ClearCategoryIn value ->
    clearOne (\context -> context { categoryIn = delete value context.categoryIn })
  ClearCategoryNotIn value ->
    clearOne (\context -> context { categoryNotIn = delete value context.categoryNotIn })
  ResetFilters -> do
    let cleared = emptyContext
    modify_ _ { context = cleared, draftContext = cleared, draftAmountMinInput = "", draftAmountMaxInput = "", draftCategoryInSelection = "", draftCategoryNotInSelection = "", filterError = Nothing }
    raise (RouteSyncRequested cleared)
    modify_ beginLedgerLoad
    loadLedger
  OpenRow transactionId -> do
    state <- get
    case state.remoteState of
      LedgerLoaded { transactions, accounts } ->
        case buildDetailSnapshot accounts transactions transactionId of
          Just snapshot ->
            raise (OpenTransactionDetail snapshot)
          Nothing ->
            pure unit
      _ ->
        pure unit

clearOne :: (LedgerContext -> LedgerContext) -> H.HalogenM State Action () Output Aff Unit
clearOne updateContext = do
  state <- get
  let nextContext = updateContext state.context
  modify_ _ { context = nextContext, draftContext = nextContext, filterError = Nothing }
  raise (RouteSyncRequested nextContext)
  modify_ beginLedgerLoad
  loadLedger

loadLedger :: H.HalogenM State Action () Output Aff Unit
loadLedger = do
  state <- get
  result <- liftAff (fetchLedgerData state.context)
  handleAction (LedgerLoadedResult result)

fetchLedgerData :: LedgerContext -> Aff (Either String { transactions :: Array FinanceTransaction, accounts :: Array FinanceAccount, categories :: Array FinanceCategory })
fetchLedgerData context = do
  transactionResponse <- getTransactions (FinanceTransactionsQuery context)
  case transactionResponse of
    Left err ->
      pure (Left ("Unable to load transactions: " <> printError err))
    Right response ->
      if not (isSuccessStatus response) then
        pure (Left ("Unable to load transactions: status " <> show (unwrap response.status)))
      else
        case lmap show (decodeJson response.body :: Either _ (Array FinanceTransaction)) of
          Left decodeError ->
            pure (Left ("Unable to decode transactions: " <> decodeError))
          Right transactions -> do
            accountsResult <- fetchAccounts
            categoriesResult <- fetchCategories
            pure
              ( case accountsResult, categoriesResult of
                  Left message, _ -> Left message
                  _, Left message -> Left message
                  Right accounts, Right categories -> Right { transactions, accounts, categories }
              )

fetchAccounts :: Aff (Either String (Array FinanceAccount))
fetchAccounts = do
  accountsResponse <- getAccounts (FinanceAccountsQuery { status: Nothing })
  case accountsResponse of
    Left err ->
      pure (Left ("Unable to load accounts: " <> printError err))
    Right response ->
      if not (isSuccessStatus response) then
        pure (Left ("Unable to load accounts: status " <> show (unwrap response.status)))
      else
        pure (lmap show (decodeJson response.body :: Either _ (Array FinanceAccount)))

fetchCategories :: Aff (Either String (Array FinanceCategory))
fetchCategories = do
  categoriesResponse <- getCategories
  case categoriesResponse of
    Left err ->
      pure (Left ("Unable to load categories: " <> printError err))
    Right response ->
      if not (isSuccessStatus response) then
        pure (Left ("Unable to load categories: status " <> show (unwrap response.status)))
      else
        pure (lmap show (decodeJson response.body :: Either _ (Array FinanceCategory)))

isSuccessStatus :: forall body. Response body -> Boolean
isSuccessStatus response =
  let
    status = unwrap response.status
  in
    status >= 200 && status < 300

normalizeContext :: LedgerContext -> LedgerContext
normalizeContext context =
  { accountId: context.accountId >>= toOptional
  , from: context.from >>= toOptional
  , to: context.to >>= toOptional
  , direction: context.direction
  , categoryIn: context.categoryIn
  , categoryNotIn: context.categoryNotIn
  , amountMin: context.amountMin
  , amountMax: context.amountMax
  , search: context.search >>= toOptional
  }

emptyContext :: LedgerContext
emptyContext =
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

validateAndNormalizeContext :: LedgerContext -> String -> String -> Either String LedgerContext
validateAndNormalizeContext draft rawAmountMin rawAmountMax = do
  amountMin <- parseOptionalInt "amountMin" rawAmountMin
  amountMax <- parseOptionalInt "amountMax" rawAmountMax
  let normalizedCategoryIn = uniqueValues draft.categoryIn
  let normalizedCategoryNotIn = filter (\value -> not (any (_ == value) normalizedCategoryIn)) (uniqueValues draft.categoryNotIn)
  case amountMin, amountMax of
    Just minValue, Just maxValue | minValue > maxValue ->
      Left "amountMin must be lower or equal to amountMax."
    _, _ ->
      Right
        ( normalizeContext draft
            { amountMin = amountMin
            , amountMax = amountMax
            , categoryIn = normalizedCategoryIn
            , categoryNotIn = normalizedCategoryNotIn
            }
        )

parseOptionalInt :: String -> String -> Either String (Maybe Int)
parseOptionalInt field raw =
  if raw == "" then
    Right Nothing
  else
    case Int.fromString raw of
      Nothing -> Left (field <> " must be an integer.")
      Just value -> Right (Just value)

uniqueValues :: Array String -> Array String
uniqueValues =
  foldl
    ( \acc raw ->
        if raw == "" || any (_ == raw) acc then acc else snoc acc raw
    )
    []

decodeDirection :: String -> Maybe FinanceReportDirection
decodeDirection = case _ of
  "" -> Nothing
  "sent" -> Just ReportSent
  "received" -> Just ReportReceived
  "all" -> Just ReportAll
  _ -> Nothing

toOptional :: String -> Maybe String
toOptional raw =
  if raw == "" then Nothing else Just raw

fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe fallback = case _ of
  Just value -> value
  Nothing -> fallback
