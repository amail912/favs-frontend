module Api.Finance
  ( getAccounts
  , getCategories
  , getTransactions
  , createSentTransaction
  , createReceivedTransaction
  , categorizeTransaction
  , splitTransaction
  , linkTransfer
  , createTransactionNote
  , updateTransactionNote
  , updateTransactionMetadata
  , deleteTransactionNote
  , getCounterpartySuggestions
  , getAggregateReport
  , encodeTransactionsQuery
  , encodeAccountsQuery
  , encodeCounterpartySuggestionsQuery
  , encodeReportQuery
  ) where

import Prelude

import Affjax (defaultRequest)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (json, string)
import Affjax.Web as Affjax
import Api.Common (JsonResponse, TextResponse, jsonBody)
import Api.FinanceContract
  ( CategorizeFinanceTransaction
  , CreateFinanceTransaction
  , CreateFinanceTransactionNote
  , FinanceCounterpartySuggestionsQuery(..)
  , FinanceAccountsQuery(..)
  , FinanceAccountsStatus(..)
  , FinanceReportDirection(..)
  , FinanceReportQuery(..)
  , FinanceTransactionsQuery(..)
  , LinkFinanceTransfer
  , SplitFinanceTransaction
  , UpdateFinanceTransactionMetadata
  , UpdateFinanceTransactionNote
  , accountsPath
  , categoriesPath
  , categorizeTransactionPath
  , linkTransferPath
  , reportPath
  , splitTransactionPath
  , transactionNotePath
  , transactionMetadataPath
  , transactionNotesPath
  , counterpartySuggestPath
  , transactionsPath
  , transactionsReceivedPath
  , transactionsSentPath
  )
import Data.Array (concat, null)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.String.Common (joinWith)
import Effect.Aff (Aff)

getAccounts :: FinanceAccountsQuery -> Aff JsonResponse
getAccounts query =
  Affjax.get json (accountsPath <> encodeAccountsQuery query)

getCategories :: Aff JsonResponse
getCategories =
  Affjax.get json categoriesPath

getTransactions :: FinanceTransactionsQuery -> Aff JsonResponse
getTransactions query =
  Affjax.get json (transactionsPath <> encodeTransactionsQuery query)

createSentTransaction :: String -> CreateFinanceTransaction -> Aff JsonResponse
createSentTransaction idempotencyKey payload =
  Affjax.request
    ( defaultRequest
        { method = Left POST
        , url = transactionsSentPath
        , headers = [ RequestHeader "Idempotency-Key" idempotencyKey ]
        , content = jsonBody payload
        , responseFormat = json
        }
    )

createReceivedTransaction :: String -> CreateFinanceTransaction -> Aff JsonResponse
createReceivedTransaction idempotencyKey payload =
  Affjax.request
    ( defaultRequest
        { method = Left POST
        , url = transactionsReceivedPath
        , headers = [ RequestHeader "Idempotency-Key" idempotencyKey ]
        , content = jsonBody payload
        , responseFormat = json
        }
    )

categorizeTransaction :: String -> CategorizeFinanceTransaction -> Aff JsonResponse
categorizeTransaction transactionId payload =
  Affjax.post json (categorizeTransactionPath transactionId) (jsonBody payload)

splitTransaction :: String -> SplitFinanceTransaction -> Aff JsonResponse
splitTransaction transactionId payload =
  Affjax.post json (splitTransactionPath transactionId) (jsonBody payload)

linkTransfer :: LinkFinanceTransfer -> Aff JsonResponse
linkTransfer payload =
  Affjax.post json linkTransferPath (jsonBody payload)

createTransactionNote :: String -> CreateFinanceTransactionNote -> Aff JsonResponse
createTransactionNote transactionId payload =
  Affjax.post json (transactionNotesPath transactionId) (jsonBody payload)

updateTransactionNote :: String -> String -> UpdateFinanceTransactionNote -> Aff JsonResponse
updateTransactionNote transactionId noteId payload =
  Affjax.put json (transactionNotePath transactionId noteId) (jsonBody payload)

updateTransactionMetadata :: String -> UpdateFinanceTransactionMetadata -> Aff JsonResponse
updateTransactionMetadata transactionId payload =
  Affjax.post json (transactionMetadataPath transactionId) (jsonBody payload)

deleteTransactionNote :: String -> String -> Aff TextResponse
deleteTransactionNote transactionId noteId =
  Affjax.delete string (transactionNotePath transactionId noteId)

getCounterpartySuggestions :: FinanceCounterpartySuggestionsQuery -> Aff JsonResponse
getCounterpartySuggestions query =
  Affjax.get json (counterpartySuggestPath <> encodeCounterpartySuggestionsQuery query)

getAggregateReport :: FinanceReportQuery -> Aff JsonResponse
getAggregateReport query =
  Affjax.get json (reportPath <> encodeReportQuery query)

encodeAccountsQuery :: FinanceAccountsQuery -> String
encodeAccountsQuery (FinanceAccountsQuery query) =
  case query.status of
    Nothing -> ""
    Just status -> "?status=" <> encodeAccountsStatus status

encodeTransactionsQuery :: FinanceTransactionsQuery -> String
encodeTransactionsQuery (FinanceTransactionsQuery query) =
  encodeQueryString
    [ map (\value -> "accountId=" <> value) query.accountId
    , map (\value -> "from=" <> value) query.from
    , map (\value -> "to=" <> value) query.to
    ]

encodeReportQuery :: FinanceReportQuery -> String
encodeReportQuery (FinanceReportQuery query) =
  encodeQueryString
    ( [ Just ("from=" <> query.from)
      , Just ("to=" <> query.to)
      , map (\direction -> "direction=" <> encodeReportDirection direction) query.direction
      ]
        <> map (\value -> Just ("accountIn=" <> value)) query.accountIn
        <> map (\value -> Just ("accountNotIn=" <> value)) query.accountNotIn
        <> map (\value -> Just ("categoryIn=" <> value)) query.categoryIn
        <> map (\value -> Just ("categoryNotIn=" <> value)) query.categoryNotIn
    )

encodeCounterpartySuggestionsQuery :: FinanceCounterpartySuggestionsQuery -> String
encodeCounterpartySuggestionsQuery (FinanceCounterpartySuggestionsQuery query) =
  encodeQueryString
    [ Just ("q=" <> query.q)
    , map (\value -> "limit=" <> show value) query.limit
    , map (\direction -> "direction=" <> encodeReportDirection direction) query.direction
    , map (\value -> "accountId=" <> value) query.accountId
    ]

encodeQueryString :: Array (Maybe String) -> String
encodeQueryString entries =
  let
    parts = concat (map maybeToArray entries)
  in
    if null parts then
      ""
    else
      "?" <> joinWith "&" parts

maybeToArray :: Maybe String -> Array String
maybeToArray = case _ of
  Nothing -> []
  Just value -> [ value ]

encodeAccountsStatus :: FinanceAccountsStatus -> String
encodeAccountsStatus = case _ of
  AccountsActive -> "active"
  AccountsClosed -> "closed"
  AccountsAll -> "all"

encodeReportDirection :: FinanceReportDirection -> String
encodeReportDirection = case _ of
  ReportSent -> "sent"
  ReportReceived -> "received"
  ReportAll -> "all"
