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
import Api.Finance (getAccounts, getTransactions)
import Api.FinanceContract
  ( FinanceAccount(..)
  , FinanceAccountsQuery(..)
  , FinanceTransaction(..)
  , FinanceTransactionCategory(..)
  , FinanceTransactionDirection(..)
  , FinanceTransactionsQuery(..)
  )
import Control.Monad.RWS (get, modify_)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (find, null)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (fold)
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
  }

type Input = LedgerContext

type State =
  { context :: LedgerContext
  , draftContext :: LedgerContext
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
      }

type FinanceLedgerRow =
  { id :: String
  , directionLabel :: String
  , directionClass :: String
  , amountLabel :: String
  , accountLabel :: String
  , occurredAtLabel :: String
  , categoryLabel :: String
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
  | LedgerLoadedResult (Either String { transactions :: Array FinanceTransaction, accounts :: Array FinanceAccount })
  | UpdateAccountId String
  | UpdateFrom String
  | UpdateTo String
  | ApplyFilters
  | ClearAccountId
  | ClearFrom
  | ClearTo
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
  , remoteState: LedgerLoading
  }

beginLedgerLoad :: State -> State
beginLedgerLoad state =
  state { remoteState = LedgerLoading }

applyLedgerLoadSuccess :: Array FinanceTransaction -> Array FinanceAccount -> State -> State
applyLedgerLoadSuccess transactions accounts state =
  state
    { remoteState = LedgerLoaded { transactions, accounts }
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
  isJust context.accountId || isJust context.from || isJust context.to

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
renderFilterBar { context, draftContext, remoteState } =
  div [ class_ "finance-ledger-filters card shadow-sm border-0 mb-3" ]
    [ div [ class_ "card-body d-flex flex-column gap-2" ]
        [ div [ class_ "d-flex flex-wrap align-items-end gap-2" ]
            [ div [ class_ "d-flex flex-column" ]
                [ span [ class_ "small text-muted" ] [ text "Account" ]
                , renderAccountSelect remoteState draftContext.accountId
                ]
            , div [ class_ "d-flex flex-column" ]
                [ span [ class_ "small text-muted" ] [ text "From" ]
                , input [ class_ "form-control form-control-sm finance-ledger-filter-from", value (fromMaybe "" draftContext.from), onValueChange UpdateFrom ]
                ]
            , div [ class_ "d-flex flex-column" ]
                [ span [ class_ "small text-muted" ] [ text "To" ]
                , input [ class_ "form-control form-control-sm finance-ledger-filter-to", value (fromMaybe "" draftContext.to), onValueChange UpdateTo ]
                ]
            ]
        , div [ class_ "d-flex flex-wrap gap-2" ]
            [ button [ class_ "btn btn-primary btn-sm finance-ledger-apply", onClick (const ApplyFilters) ] [ text "Apply" ]
            , button [ class_ "btn btn-outline-secondary btn-sm finance-ledger-reset", onClick (const ResetFilters) ] [ text "Reset" ]
            ]
        , div [ class_ "d-flex flex-wrap gap-2" ]
            [ activeContextPill "accountId" context.accountId ClearAccountId
            , activeContextPill "from" context.from ClearFrom
            , activeContextPill "to" context.to ClearTo
            ]
        ]
    ]

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
      Right { transactions, accounts } ->
        modify_ (applyLedgerLoadSuccess transactions accounts)
  UpdateAccountId raw ->
    modify_ \state ->
      state { draftContext = state.draftContext { accountId = toOptional raw } }
  UpdateFrom raw ->
    modify_ \state ->
      state { draftContext = state.draftContext { from = toOptional raw } }
  UpdateTo raw ->
    modify_ \state ->
      state { draftContext = state.draftContext { to = toOptional raw } }
  ApplyFilters -> do
    state <- get
    let nextContext = normalizeContext state.draftContext
    modify_ _ { context = nextContext, draftContext = nextContext }
    raise (RouteSyncRequested nextContext)
    modify_ beginLedgerLoad
    loadLedger
  ClearAccountId ->
    clearOne (_ { accountId = Nothing })
  ClearFrom ->
    clearOne (_ { from = Nothing })
  ClearTo ->
    clearOne (_ { to = Nothing })
  ResetFilters -> do
    let cleared = { accountId: Nothing, from: Nothing, to: Nothing }
    modify_ _ { context = cleared, draftContext = cleared }
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
  modify_ _ { context = nextContext, draftContext = nextContext }
  raise (RouteSyncRequested nextContext)
  modify_ beginLedgerLoad
  loadLedger

loadLedger :: H.HalogenM State Action () Output Aff Unit
loadLedger = do
  state <- get
  result <- liftAff (fetchLedgerData state.context)
  handleAction (LedgerLoadedResult result)

fetchLedgerData :: LedgerContext -> Aff (Either String { transactions :: Array FinanceTransaction, accounts :: Array FinanceAccount })
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
          Right transactions ->
            map
              ( \accountsResult ->
                  case accountsResult of
                    Left message -> Left message
                    Right accounts -> Right { transactions, accounts }
              )
              fetchAccounts

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
  }

toOptional :: String -> Maybe String
toOptional raw =
  if raw == "" then Nothing else Just raw

fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe fallback = case _ of
  Just value -> value
  Nothing -> fallback
