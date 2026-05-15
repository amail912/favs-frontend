module Api.FinanceContract
  ( FinanceAccountsStatus(..)
  , FinanceAccountsQuery(..)
  , FinanceAccount(..)
  , FinanceCategory(..)
  , FinanceTransactionDirection(..)
  , FinanceTransferLink(..)
  , FinanceTransactionCategory(..)
  , FinanceTransactionSplitRow(..)
  , FinanceTransactionNote(..)
  , FinanceTransactionAdjustment(..)
  , FinanceTransaction(..)
  , FinanceTransactionsQuery(..)
  , CreateFinanceAccount(..)
  , CreateFinanceTransaction(..)
  , UpdateFinanceTransactionMetadata(..)
  , CategorizeFinanceTransaction(..)
  , SplitFinanceTransaction(..)
  , LinkFinanceTransfer(..)
  , CreateFinanceTransactionNote(..)
  , UpdateFinanceTransactionNote(..)
  , FinanceReportDirection(..)
  , FinanceReportQuery(..)
  , FinanceAggregateReport(..)
  , FinanceAnalyticsQuery(..)
  , FinanceAnalyticsSummary(..)
  , FinanceAnalyticsCategoryBreakdownRow(..)
  , FinanceAnalyticsCashflowSeriesRow(..)
  , FinanceAnalyticsAccountBalanceRow(..)
  , FinanceAnalyticsReport(..)
  , FinanceCounterpartySuggestionsQuery(..)
  , FinanceCounterpartySuggestion(..)
  , FinanceCounterpartySuggestionsResult(..)
  , basePath
  , accountsPath
  , categoriesPath
  , transactionsPath
  , transactionsSentPath
  , transactionsReceivedPath
  , categorizeTransactionPath
  , splitTransactionPath
  , linkTransferPath
  , transactionNotesPath
  , transactionNotePath
  , transactionMetadataPath
  , counterpartySuggestPath
  , reportPath
  , analyticsReportPath
  ) where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

basePath :: String
basePath = "/api/v1/finance"

accountsPath :: String
accountsPath = basePath <> "/accounts"

categoriesPath :: String
categoriesPath = basePath <> "/categories"

transactionsPath :: String
transactionsPath = basePath <> "/transactions"

transactionsSentPath :: String
transactionsSentPath = transactionsPath <> "/sent"

transactionsReceivedPath :: String
transactionsReceivedPath = transactionsPath <> "/received"

categorizeTransactionPath :: String -> String
categorizeTransactionPath transactionId =
  transactionsPath <> "/" <> transactionId <> "/categorize"

splitTransactionPath :: String -> String
splitTransactionPath transactionId =
  transactionsPath <> "/" <> transactionId <> "/split"

linkTransferPath :: String
linkTransferPath = transactionsPath <> "/link"

transactionNotesPath :: String -> String
transactionNotesPath transactionId =
  transactionsPath <> "/" <> transactionId <> "/notes"

transactionNotePath :: String -> String -> String
transactionNotePath transactionId noteId =
  transactionNotesPath transactionId <> "/" <> noteId

transactionMetadataPath :: String -> String
transactionMetadataPath transactionId =
  transactionsPath <> "/" <> transactionId <> "/metadata"

counterpartySuggestPath :: String
counterpartySuggestPath = basePath <> "/counterparties/suggest"

reportPath :: String
reportPath = basePath <> "/report"

analyticsReportPath :: String
analyticsReportPath = reportPath <> "/analytics"

data FinanceAccountsStatus = AccountsActive | AccountsClosed | AccountsAll

derive instance financeAccountsStatusEq :: Eq FinanceAccountsStatus
derive instance financeAccountsStatusGeneric :: Generic FinanceAccountsStatus _

instance showFinanceAccountsStatus :: Show FinanceAccountsStatus where
  show = genericShow

instance encodeFinanceAccountsStatus :: EncodeJson FinanceAccountsStatus where
  encodeJson = case _ of
    AccountsActive -> encodeJson "active"
    AccountsClosed -> encodeJson "closed"
    AccountsAll -> encodeJson "all"

instance decodeFinanceAccountsStatus :: DecodeJson FinanceAccountsStatus where
  decodeJson json = do
    value <- decodeJson json
    case value of
      "active" -> pure AccountsActive
      "closed" -> pure AccountsClosed
      "all" -> pure AccountsAll
      _ -> Left (TypeMismatch "FinanceAccountsStatus")

newtype FinanceAccountsQuery = FinanceAccountsQuery
  { status :: Maybe FinanceAccountsStatus }

newtype FinanceAccount = FinanceAccount
  { id :: String
  , name :: String
  , status :: String
  }

newtype FinanceCategory = FinanceCategory
  { id :: String
  , name :: String
  , parentId :: Maybe String
  , owner :: String
  , selectable :: Boolean
  }

data FinanceTransactionDirection = TransactionSent | TransactionReceived

derive instance financeTransactionDirectionEq :: Eq FinanceTransactionDirection
derive instance financeTransactionDirectionGeneric :: Generic FinanceTransactionDirection _

instance showFinanceTransactionDirection :: Show FinanceTransactionDirection where
  show = genericShow

instance encodeFinanceTransactionDirection :: EncodeJson FinanceTransactionDirection where
  encodeJson = case _ of
    TransactionSent -> encodeJson "sent"
    TransactionReceived -> encodeJson "received"

instance decodeFinanceTransactionDirection :: DecodeJson FinanceTransactionDirection where
  decodeJson json = do
    value <- decodeJson json
    case value of
      "sent" -> pure TransactionSent
      "received" -> pure TransactionReceived
      _ -> Left (TypeMismatch "FinanceTransactionDirection")

newtype FinanceTransferLink = FinanceTransferLink
  { linkedTransactionId :: String
  , linkType :: String
  }

newtype FinanceTransactionCategory = FinanceTransactionCategory
  { id :: String }

newtype FinanceTransactionSplitRow = FinanceTransactionSplitRow
  { amount :: Number
  , category :: String
  }

newtype FinanceTransactionNote = FinanceTransactionNote
  { id :: String
  , text :: String
  }

newtype FinanceTransactionAdjustment = FinanceTransactionAdjustment
  { kind :: String }

newtype FinanceTransaction = FinanceTransaction
  { id :: String
  , direction :: FinanceTransactionDirection
  , accountId :: String
  , amount :: Number
  , occurredAt :: String
  , recordedAt :: String
  , counterparty :: Maybe String
  , description :: Maybe String
  , transfer :: Maybe FinanceTransferLink
  , category :: Maybe FinanceTransactionCategory
  , splits :: Array FinanceTransactionSplitRow
  , notes :: Array FinanceTransactionNote
  , adjustment :: Maybe FinanceTransactionAdjustment
  }

newtype FinanceTransactionsQuery = FinanceTransactionsQuery
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

newtype CreateFinanceAccount = CreateFinanceAccount
  { name :: String
  }

newtype CreateFinanceTransaction = CreateFinanceTransaction
  { accountId :: String
  , amount :: Number
  , occurredAt :: Maybe String
  , counterparty :: Maybe String
  , description :: Maybe String
  }

newtype UpdateFinanceTransactionMetadata = UpdateFinanceTransactionMetadata
  { counterparty :: Maybe (Maybe String)
  , description :: Maybe (Maybe String)
  }

newtype CategorizeFinanceTransaction = CategorizeFinanceTransaction
  { category :: String }

newtype SplitFinanceTransaction = SplitFinanceTransaction
  { splits :: Array FinanceTransactionSplitRow }

newtype LinkFinanceTransfer = LinkFinanceTransfer
  { sourceTransactionId :: String
  , targetTransactionId :: String
  , linkType :: String
  }

newtype CreateFinanceTransactionNote = CreateFinanceTransactionNote
  { text :: String }

newtype UpdateFinanceTransactionNote = UpdateFinanceTransactionNote
  { text :: String }

data FinanceReportDirection = ReportSent | ReportReceived | ReportAll

derive instance financeReportDirectionEq :: Eq FinanceReportDirection
derive instance financeReportDirectionOrd :: Ord FinanceReportDirection
derive instance financeReportDirectionGeneric :: Generic FinanceReportDirection _

instance showFinanceReportDirection :: Show FinanceReportDirection where
  show = genericShow

instance encodeFinanceReportDirection :: EncodeJson FinanceReportDirection where
  encodeJson = case _ of
    ReportSent -> encodeJson "sent"
    ReportReceived -> encodeJson "received"
    ReportAll -> encodeJson "all"

newtype FinanceReportQuery = FinanceReportQuery
  { from :: String
  , to :: String
  , direction :: Maybe FinanceReportDirection
  , accountIn :: Array String
  , accountNotIn :: Array String
  , categoryIn :: Array String
  , categoryNotIn :: Array String
  }

newtype FinanceAggregateReport = FinanceAggregateReport
  { total :: Number
  , count :: Int
  , transactionIds :: Array String
  }

newtype FinanceAnalyticsQuery = FinanceAnalyticsQuery
  { from :: String
  , to :: String
  , direction :: Maybe FinanceReportDirection
  , accountId :: Maybe String
  , categoryIn :: Array String
  , categoryNotIn :: Array String
  , amountMin :: Maybe Int
  , amountMax :: Maybe Int
  , search :: Maybe String
  }

newtype FinanceAnalyticsSummary = FinanceAnalyticsSummary
  { total :: Number
  , count :: Int
  }

newtype FinanceAnalyticsCategoryBreakdownRow = FinanceAnalyticsCategoryBreakdownRow
  { categoryId :: String
  , total :: Number
  , count :: Int
  }

newtype FinanceAnalyticsCashflowSeriesRow = FinanceAnalyticsCashflowSeriesRow
  { bucketStart :: String
  , bucketEnd :: String
  , total :: Number
  , count :: Int
  }

newtype FinanceAnalyticsAccountBalanceRow = FinanceAnalyticsAccountBalanceRow
  { accountId :: String
  , total :: Number
  }

newtype FinanceAnalyticsReport = FinanceAnalyticsReport
  { summary :: FinanceAnalyticsSummary
  , categoryBreakdown :: Array FinanceAnalyticsCategoryBreakdownRow
  , cashflowSeries :: Array FinanceAnalyticsCashflowSeriesRow
  , accountBalances :: Array FinanceAnalyticsAccountBalanceRow
  }

newtype FinanceCounterpartySuggestionsQuery = FinanceCounterpartySuggestionsQuery
  { q :: String
  , limit :: Maybe Int
  , direction :: Maybe FinanceReportDirection
  , accountId :: Maybe String
  }

newtype FinanceCounterpartySuggestion = FinanceCounterpartySuggestion
  { value :: String
  , usageCount :: Int
  , lastUsedAt :: String
  , suggestedCategory :: Maybe String
  }

newtype FinanceCounterpartySuggestionsResult = FinanceCounterpartySuggestionsResult
  { items :: Array FinanceCounterpartySuggestion
  }

derive instance financeAccountEq :: Eq FinanceAccount
derive instance financeCategoryEq :: Eq FinanceCategory
derive instance financeTransferLinkEq :: Eq FinanceTransferLink
derive instance financeTransactionCategoryEq :: Eq FinanceTransactionCategory
derive instance financeTransactionSplitRowEq :: Eq FinanceTransactionSplitRow
derive instance financeTransactionNoteEq :: Eq FinanceTransactionNote
derive instance financeTransactionAdjustmentEq :: Eq FinanceTransactionAdjustment
derive instance financeTransactionEq :: Eq FinanceTransaction
derive instance createFinanceAccountEq :: Eq CreateFinanceAccount
derive instance createFinanceTransactionEq :: Eq CreateFinanceTransaction
derive instance categorizeFinanceTransactionEq :: Eq CategorizeFinanceTransaction
derive instance splitFinanceTransactionEq :: Eq SplitFinanceTransaction
derive instance linkFinanceTransferEq :: Eq LinkFinanceTransfer
derive instance createFinanceTransactionNoteEq :: Eq CreateFinanceTransactionNote
derive instance updateFinanceTransactionNoteEq :: Eq UpdateFinanceTransactionNote
derive instance updateFinanceTransactionMetadataEq :: Eq UpdateFinanceTransactionMetadata
derive instance financeAggregateReportEq :: Eq FinanceAggregateReport
derive instance financeAnalyticsSummaryEq :: Eq FinanceAnalyticsSummary
derive instance financeAnalyticsCategoryBreakdownRowEq :: Eq FinanceAnalyticsCategoryBreakdownRow
derive instance financeAnalyticsCashflowSeriesRowEq :: Eq FinanceAnalyticsCashflowSeriesRow
derive instance financeAnalyticsAccountBalanceRowEq :: Eq FinanceAnalyticsAccountBalanceRow
derive instance financeAnalyticsReportEq :: Eq FinanceAnalyticsReport
derive instance financeCounterpartySuggestionEq :: Eq FinanceCounterpartySuggestion
derive instance financeCounterpartySuggestionsResultEq :: Eq FinanceCounterpartySuggestionsResult

instance decodeFinanceAccount :: DecodeJson FinanceAccount where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    name <- obj .: "name"
    status <- obj .: "status"
    pure (FinanceAccount { id, name, status })

instance decodeFinanceCategory :: DecodeJson FinanceCategory where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    name <- obj .: "name"
    parentId <- obj .:? "parentId"
    owner <- obj .: "owner"
    selectable <- obj .: "selectable"
    pure (FinanceCategory { id, name, parentId, owner, selectable })

instance decodeFinanceTransferLink :: DecodeJson FinanceTransferLink where
  decodeJson json = do
    obj <- decodeJson json
    linkedTransactionId <- obj .: "linkedTransactionId"
    linkType <- obj .: "linkType"
    pure (FinanceTransferLink { linkedTransactionId, linkType })

instance decodeFinanceTransactionCategory :: DecodeJson FinanceTransactionCategory where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    pure (FinanceTransactionCategory { id })

instance decodeFinanceTransactionSplitRow :: DecodeJson FinanceTransactionSplitRow where
  decodeJson json = do
    obj <- decodeJson json
    amount <- obj .: "amount"
    category <- obj .: "category"
    pure (FinanceTransactionSplitRow { amount, category })

instance decodeFinanceTransactionNote :: DecodeJson FinanceTransactionNote where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    text <- obj .: "text"
    pure (FinanceTransactionNote { id, text })

instance decodeFinanceTransactionAdjustment :: DecodeJson FinanceTransactionAdjustment where
  decodeJson json = do
    obj <- decodeJson json
    kind <- obj .: "kind"
    pure (FinanceTransactionAdjustment { kind })

instance decodeFinanceTransaction :: DecodeJson FinanceTransaction where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    direction <- obj .: "direction"
    accountId <- obj .: "accountId"
    amount <- obj .: "amount"
    occurredAt <- obj .: "occurredAt"
    recordedAt <- obj .: "recordedAt"
    counterparty <- obj .:? "counterparty"
    description <- obj .:? "description"
    transfer <- obj .:? "transfer"
    category <- obj .:? "category"
    splits <- obj .: "splits"
    notes <- obj .: "notes"
    adjustment <- obj .:? "adjustment"
    pure
      ( FinanceTransaction
          { id
          , direction
          , accountId
          , amount
          , occurredAt
          , recordedAt
          , counterparty
          , description
          , transfer
          , category
          , splits
          , notes
          , adjustment
          }
      )

instance decodeFinanceAggregateReport :: DecodeJson FinanceAggregateReport where
  decodeJson json = do
    obj <- decodeJson json
    total <- obj .: "total"
    count <- obj .: "count"
    transactionIds <- obj .: "transactionIds"
    pure (FinanceAggregateReport { total, count, transactionIds })

instance decodeFinanceAnalyticsSummary :: DecodeJson FinanceAnalyticsSummary where
  decodeJson json = do
    obj <- decodeJson json
    total <- obj .: "total"
    count <- obj .: "count"
    pure (FinanceAnalyticsSummary { total, count })

instance decodeFinanceAnalyticsCategoryBreakdownRow :: DecodeJson FinanceAnalyticsCategoryBreakdownRow where
  decodeJson json = do
    obj <- decodeJson json
    categoryId <- obj .: "categoryId"
    total <- obj .: "total"
    count <- obj .: "count"
    pure (FinanceAnalyticsCategoryBreakdownRow { categoryId, total, count })

instance decodeFinanceAnalyticsCashflowSeriesRow :: DecodeJson FinanceAnalyticsCashflowSeriesRow where
  decodeJson json = do
    obj <- decodeJson json
    bucketStart <- obj .: "bucketStart"
    bucketEnd <- obj .: "bucketEnd"
    total <- obj .: "total"
    count <- obj .: "count"
    pure (FinanceAnalyticsCashflowSeriesRow { bucketStart, bucketEnd, total, count })

instance decodeFinanceAnalyticsAccountBalanceRow :: DecodeJson FinanceAnalyticsAccountBalanceRow where
  decodeJson json = do
    obj <- decodeJson json
    accountId <- obj .: "accountId"
    total <- obj .: "total"
    pure (FinanceAnalyticsAccountBalanceRow { accountId, total })

instance decodeFinanceAnalyticsReport :: DecodeJson FinanceAnalyticsReport where
  decodeJson json = do
    obj <- decodeJson json
    summary <- obj .: "summary"
    categoryBreakdown <- obj .: "categoryBreakdown"
    cashflowSeries <- obj .: "cashflowSeries"
    accountBalances <- obj .: "accountBalances"
    pure (FinanceAnalyticsReport { summary, categoryBreakdown, cashflowSeries, accountBalances })

instance decodeFinanceCounterpartySuggestion :: DecodeJson FinanceCounterpartySuggestion where
  decodeJson json = do
    obj <- decodeJson json
    value <- obj .: "value"
    usageCount <- obj .: "usageCount"
    lastUsedAt <- obj .: "lastUsedAt"
    suggestedCategory <- obj .:? "suggestedCategory"
    pure (FinanceCounterpartySuggestion { value, usageCount, lastUsedAt, suggestedCategory })

instance decodeFinanceCounterpartySuggestionsResult :: DecodeJson FinanceCounterpartySuggestionsResult where
  decodeJson json = do
    obj <- decodeJson json
    items <- obj .: "items"
    pure (FinanceCounterpartySuggestionsResult { items })

instance encodeFinanceCounterpartySuggestion :: EncodeJson FinanceCounterpartySuggestion where
  encodeJson (FinanceCounterpartySuggestion payload) =
    "value" := payload.value
      ~> "usageCount" := payload.usageCount
      ~> "lastUsedAt" := payload.lastUsedAt
      ~> "suggestedCategory" := payload.suggestedCategory
      ~> jsonEmptyObject

instance encodeFinanceCounterpartySuggestionsResult :: EncodeJson FinanceCounterpartySuggestionsResult where
  encodeJson (FinanceCounterpartySuggestionsResult payload) =
    "items" := payload.items ~> jsonEmptyObject

instance encodeFinanceAccount :: EncodeJson FinanceAccount where
  encodeJson (FinanceAccount payload) =
    "id" := payload.id
      ~> "name" := payload.name
      ~> "status" := payload.status
      ~> jsonEmptyObject

instance encodeFinanceCategory :: EncodeJson FinanceCategory where
  encodeJson (FinanceCategory payload) =
    "id" := payload.id
      ~> "name" := payload.name
      ~> "parentId" := payload.parentId
      ~> "owner" := payload.owner
      ~> "selectable" := payload.selectable
      ~> jsonEmptyObject

instance encodeFinanceTransferLink :: EncodeJson FinanceTransferLink where
  encodeJson (FinanceTransferLink payload) =
    "linkedTransactionId" := payload.linkedTransactionId
      ~> "linkType" := payload.linkType
      ~> jsonEmptyObject

instance encodeFinanceTransactionCategory :: EncodeJson FinanceTransactionCategory where
  encodeJson (FinanceTransactionCategory payload) =
    "id" := payload.id ~> jsonEmptyObject

instance encodeFinanceTransactionNote :: EncodeJson FinanceTransactionNote where
  encodeJson (FinanceTransactionNote payload) =
    "id" := payload.id
      ~> "text" := payload.text
      ~> jsonEmptyObject

instance encodeFinanceTransactionAdjustment :: EncodeJson FinanceTransactionAdjustment where
  encodeJson (FinanceTransactionAdjustment payload) =
    "kind" := payload.kind ~> jsonEmptyObject

instance encodeFinanceTransaction :: EncodeJson FinanceTransaction where
  encodeJson (FinanceTransaction payload) =
    "id" := payload.id
      ~> "direction" := payload.direction
      ~> "accountId" := payload.accountId
      ~> "amount" := payload.amount
      ~> "occurredAt" := payload.occurredAt
      ~> "recordedAt" := payload.recordedAt
      ~> "counterparty" := payload.counterparty
      ~> "description" := payload.description
      ~> "transfer" := payload.transfer
      ~> "category" := payload.category
      ~> "splits" := payload.splits
      ~> "notes" := payload.notes
      ~> "adjustment" := payload.adjustment
      ~> jsonEmptyObject

instance encodeFinanceAggregateReport :: EncodeJson FinanceAggregateReport where
  encodeJson (FinanceAggregateReport payload) =
    "total" := payload.total
      ~> "count" := payload.count
      ~> "transactionIds" := payload.transactionIds
      ~> jsonEmptyObject

instance encodeFinanceAnalyticsSummary :: EncodeJson FinanceAnalyticsSummary where
  encodeJson (FinanceAnalyticsSummary payload) =
    "total" := payload.total
      ~> "count" := payload.count
      ~> jsonEmptyObject

instance encodeFinanceAnalyticsCategoryBreakdownRow :: EncodeJson FinanceAnalyticsCategoryBreakdownRow where
  encodeJson (FinanceAnalyticsCategoryBreakdownRow payload) =
    "categoryId" := payload.categoryId
      ~> "total" := payload.total
      ~> "count" := payload.count
      ~> jsonEmptyObject

instance encodeFinanceAnalyticsCashflowSeriesRow :: EncodeJson FinanceAnalyticsCashflowSeriesRow where
  encodeJson (FinanceAnalyticsCashflowSeriesRow payload) =
    "bucketStart" := payload.bucketStart
      ~> "bucketEnd" := payload.bucketEnd
      ~> "total" := payload.total
      ~> "count" := payload.count
      ~> jsonEmptyObject

instance encodeFinanceAnalyticsAccountBalanceRow :: EncodeJson FinanceAnalyticsAccountBalanceRow where
  encodeJson (FinanceAnalyticsAccountBalanceRow payload) =
    "accountId" := payload.accountId
      ~> "total" := payload.total
      ~> jsonEmptyObject

instance encodeFinanceAnalyticsReport :: EncodeJson FinanceAnalyticsReport where
  encodeJson (FinanceAnalyticsReport payload) =
    "summary" := payload.summary
      ~> "categoryBreakdown" := payload.categoryBreakdown
      ~> "cashflowSeries" := payload.cashflowSeries
      ~> "accountBalances" := payload.accountBalances
      ~> jsonEmptyObject

instance encodeCreateFinanceTransaction :: EncodeJson CreateFinanceTransaction where
  encodeJson (CreateFinanceTransaction payload) =
    "accountId" := payload.accountId
      ~> "amount" := payload.amount
      ~> "occurredAt" := payload.occurredAt
      ~> "counterparty" := payload.counterparty
      ~> "description" := payload.description
      ~> jsonEmptyObject

instance encodeCreateFinanceAccount :: EncodeJson CreateFinanceAccount where
  encodeJson (CreateFinanceAccount payload) =
    "name" := payload.name
      ~> jsonEmptyObject

instance encodeUpdateFinanceTransactionMetadata :: EncodeJson UpdateFinanceTransactionMetadata where
  encodeJson (UpdateFinanceTransactionMetadata payload) =
    "counterparty" := payload.counterparty
      ~> "description" := payload.description
      ~> jsonEmptyObject

instance encodeCategorizeFinanceTransaction :: EncodeJson CategorizeFinanceTransaction where
  encodeJson (CategorizeFinanceTransaction payload) =
    "category" := payload.category ~> jsonEmptyObject

instance encodeFinanceTransactionSplitRow :: EncodeJson FinanceTransactionSplitRow where
  encodeJson (FinanceTransactionSplitRow payload) =
    "amount" := payload.amount
      ~> "category" := payload.category
      ~> jsonEmptyObject

instance encodeSplitFinanceTransaction :: EncodeJson SplitFinanceTransaction where
  encodeJson (SplitFinanceTransaction payload) =
    "splits" := payload.splits ~> jsonEmptyObject

instance encodeLinkFinanceTransfer :: EncodeJson LinkFinanceTransfer where
  encodeJson (LinkFinanceTransfer payload) =
    "sourceTransactionId" := payload.sourceTransactionId
      ~> "targetTransactionId" := payload.targetTransactionId
      ~> "linkType" := payload.linkType
      ~> jsonEmptyObject

instance encodeCreateFinanceTransactionNote :: EncodeJson CreateFinanceTransactionNote where
  encodeJson (CreateFinanceTransactionNote payload) =
    "text" := payload.text ~> jsonEmptyObject

instance encodeUpdateFinanceTransactionNote :: EncodeJson UpdateFinanceTransactionNote where
  encodeJson (UpdateFinanceTransactionNote payload) =
    "text" := payload.text ~> jsonEmptyObject
