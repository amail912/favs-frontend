module Pages.FinanceTransactions
  ( component
  , LedgerRemoteState(..)
  , LedgerBodyState(..)
  , FinanceLedgerRow
  , beginLedgerLoad
  , applyLedgerLoadSuccess
  , applyLedgerLoadFailure
  , deriveLedgerBodyState
  , resolveAccountLabel
  , buildLedgerRows
  ) where

import Prelude hiding (div)

import Affjax (printError)
import Affjax.Web (Response)
import Api.Finance (getAccounts, getTransactions)
import Api.FinanceContract
  ( FinanceAccount(..)
  , FinanceAccountsQuery(..)
  , FinanceAccountsStatus(..)
  , FinanceTransaction(..)
  , FinanceTransactionCategory(..)
  , FinanceTransactionDirection(..)
  , FinanceTransactionsQuery(..)
  )
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (find, null)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Control.Monad.RWS (get, modify_)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.String.Common as StringCommon
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval) as H
import Halogen.HTML (button, div, span, table, tbody, td, text, th, thead, tr)
import Halogen.HTML.Events (onClick)
import Ui.Utils (class_)

type Input =
  { hasActiveContext :: Boolean
  }

type State =
  { hasActiveContext :: Boolean
  , remoteState :: LedgerRemoteState
  }

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

component :: forall q. H.Component q Input Void Aff
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
initialState { hasActiveContext } =
  { hasActiveContext
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
deriveLedgerBodyState { hasActiveContext, remoteState } =
  case remoteState of
    LedgerLoading ->
      LedgerBodyLoading
    LedgerLoadError message ->
      LedgerBodyError message
    LedgerLoaded { transactions, accounts } ->
      if null transactions then
        if hasActiveContext then LedgerBodyNoResults else LedgerBodyEmpty
      else
        LedgerBodyRows (buildLedgerRows accounts transactions)

buildLedgerRows :: Array FinanceAccount -> Array FinanceTransaction -> Array FinanceLedgerRow
buildLedgerRows accounts transactions =
  map (buildLedgerRow accounts) transactions

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
    [ case deriveLedgerBodyState state of
        LedgerBodyLoading ->
          div [ class_ "text-muted finance-ledger-loading" ] [ text "Loading transactions..." ]
        LedgerBodyError message ->
          div [ class_ "alert alert-danger d-flex flex-column align-items-start gap-2 finance-ledger-error" ]
            [ div_ [ text message ]
            , button [ class_ "btn btn-outline-danger btn-sm finance-ledger-retry", onClick (const RetryLoad) ] [ text "Retry" ]
            ]
        LedgerBodyEmpty ->
          renderEmptyState "No transactions yet" "Create your first transaction to start your ledger."
        LedgerBodyNoResults ->
          renderEmptyState "No matching transactions" "Current account/date context has no matching transaction."
        LedgerBodyRows rows ->
          renderRows rows
    ]

renderRows :: Array FinanceLedgerRow -> H.ComponentHTML Action () Aff
renderRows rows =
  div [ class_ "table-responsive finance-ledger-table-wrap" ]
    [ table [ class_ "table align-middle finance-ledger-table" ]
        [ thead_
            [ tr_
                [ th_ [ text "Amount" ]
                , th_ [ text "Direction" ]
                , th_ [ text "Account" ]
                , th_ [ text "Occurred At" ]
                , th_ [ text "Category" ]
                , th_ [ text "Facts" ]
                ]
            ]
        , tbody_ (map renderRow rows)
        ]
    ]

renderRow :: FinanceLedgerRow -> H.ComponentHTML Action () Aff
renderRow row =
  tr [ class_ "finance-ledger-row" ]
    [ td [ class_ "finance-ledger-row__amount" ] [ text row.amountLabel ]
    , td [ class_ "finance-ledger-row__direction" ] [ span [ class_ ("badge " <> row.directionClass) ] [ text row.directionLabel ] ]
    , td [ class_ "finance-ledger-row__account" ] [ text row.accountLabel ]
    , td [ class_ "finance-ledger-row__occurred-at" ] [ text row.occurredAtLabel ]
    , td [ class_ "finance-ledger-row__category" ] [ text row.categoryLabel ]
    , td [ class_ "finance-ledger-row__facts" ] [ text (factsLabel row) ]
    ]

factsLabel :: FinanceLedgerRow -> String
factsLabel row =
  if null facts then "-" else StringCommon.joinWith ", " facts
  where
  facts =
    fold
      [ guard row.hasSplit [ "split" ]
      , guard row.hasTransfer [ "transfer" ]
      , guard row.hasNote [ "note" ]
      , guard row.hasAdjustment [ "adjustment" ]
      ]

renderEmptyState :: String -> String -> H.ComponentHTML Action () Aff
renderEmptyState title subtitle =
  div [ class_ "row entity-empty finance-ledger-empty" ]
    [ div [ class_ "entity-empty-title" ] [ text title ]
    , div [ class_ "entity-empty-subtitle" ] [ text subtitle ]
    ]

handleAction :: Action -> H.HalogenM State Action () Void Aff Unit
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

loadLedger :: H.HalogenM State Action () Void Aff Unit
loadLedger = do
  result <- liftAff fetchLedgerData
  handleAction (LedgerLoadedResult result)

fetchLedgerData :: Aff (Either String { transactions :: Array FinanceTransaction, accounts :: Array FinanceAccount })
fetchLedgerData = do
  transactionResponse <- getTransactions (FinanceTransactionsQuery { accountId: Nothing, from: Nothing, to: Nothing })
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
            map (\accounts -> Right { transactions, accounts }) fetchAccounts

fetchAccounts :: Aff (Array FinanceAccount)
fetchAccounts = do
  accountsResponse <- getAccounts (FinanceAccountsQuery { status: Just AccountsAll })
  case accountsResponse of
    Left _ ->
      pure []
    Right response ->
      if not (isSuccessStatus response) then
        pure []
      else
        case lmap show (decodeJson response.body :: Either _ (Array FinanceAccount)) of
          Left _ -> pure []
          Right accounts -> pure accounts

isSuccessStatus :: Response Json -> Boolean
isSuccessStatus response =
  let
    statusCode = unwrap response.status
  in
    statusCode >= 200 && statusCode < 300

div_ :: forall i. Array (H.ComponentHTML i () Aff) -> H.ComponentHTML i () Aff
div_ = div []

thead_ :: forall i. Array (H.ComponentHTML i () Aff) -> H.ComponentHTML i () Aff
thead_ = thead []

tbody_ :: forall i. Array (H.ComponentHTML i () Aff) -> H.ComponentHTML i () Aff
tbody_ = tbody []

tr_ :: forall i. Array (H.ComponentHTML i () Aff) -> H.ComponentHTML i () Aff
tr_ = tr []

th_ :: forall i. Array (H.ComponentHTML i () Aff) -> H.ComponentHTML i () Aff
th_ = th []
