module Test.Api.FinanceSpec (spec) where

import Prelude

import Api.Finance (encodeAccountsQuery, encodeReportQuery, encodeTransactionsQuery)
import Api.FinanceContract
  ( CategorizeFinanceTransaction(..)
  , CreateFinanceTransaction(..)
  , CreateFinanceTransactionNote(..)
  , FinanceAccount(..)
  , FinanceAccountsQuery(..)
  , FinanceAccountsStatus(..)
  , FinanceAggregateReport(..)
  , FinanceCategory(..)
  , FinanceReportDirection(..)
  , FinanceReportQuery(..)
  , FinanceTransaction(..)
  , FinanceTransactionAdjustment(..)
  , FinanceTransactionCategory(..)
  , FinanceTransactionDirection(..)
  , FinanceTransactionNote(..)
  , FinanceTransactionSplitRow(..)
  , FinanceTransactionsQuery(..)
  , FinanceTransferLink(..)
  , LinkFinanceTransfer(..)
  , SplitFinanceTransaction(..)
  , UpdateFinanceTransactionNote(..)
  )
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (contains)
import Data.String.Pattern (Pattern(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec =
  describe "Finance API contract" do
    it "round-trips account decoding shape" do
      let account = FinanceAccount { id: "acc-1", name: "Main", status: "active" }
      case decodeJson (encodeJson account) of
        Right decoded ->
          if decoded == account then pure unit else fail "Decoded account did not match encoded value"
        Left err -> fail $ "Failed to decode account shape: " <> show err

    it "round-trips category decoding shape" do
      let category = FinanceCategory { id: "cat-1", name: "Food", parentId: Nothing, owner: "user", selectable: true }
      case decodeJson (encodeJson category) of
        Right decoded ->
          if decoded == category then pure unit else fail "Decoded category did not match encoded value"
        Left err -> fail $ "Failed to decode category shape: " <> show err

    it "round-trips transaction decoding shape without unsupported fields" do
      let
        transaction =
          FinanceTransaction
            { id: "tx-1"
            , direction: TransactionSent
            , accountId: "acc-1"
            , amount: 42.5
            , occurredAt: "2026-05-01T10:00:00Z"
            , recordedAt: "2026-05-01T10:00:01Z"
            , transfer: Just (FinanceTransferLink { linkedTransactionId: "tx-2", linkType: "transfer" })
            , category: Just (FinanceTransactionCategory { id: "cat-1" })
            , splits: [ FinanceTransactionSplitRow { amount: 42.5, category: "cat-1" } ]
            , notes: [ FinanceTransactionNote { id: "note-1", text: "memo" } ]
            , adjustment: Just (FinanceTransactionAdjustment { kind: "balance-snapshot" })
            }
      case decodeJson (encodeJson transaction) of
        Right decoded ->
          if decoded == transaction then pure unit else fail "Decoded transaction did not match encoded value"
        Left err -> fail $ "Failed to decode transaction shape: " <> show err
      let raw = stringify (encodeJson transaction)
      contains (Pattern "\"counterparty\"") raw `shouldEqual` false
      contains (Pattern "\"description\"") raw `shouldEqual` false

    it "encodes sent and received create payloads with optional occurredAt" do
      let
        createSent = CreateFinanceTransaction { accountId: "acc-1", amount: 10.0, occurredAt: Just "2026-05-01T10:00:00Z" }
        createReceived = CreateFinanceTransaction { accountId: "acc-1", amount: 12.0, occurredAt: Nothing }
      let encodedSent = stringify (encodeJson createSent)
      contains (Pattern "\"accountId\":\"acc-1\"") encodedSent `shouldEqual` true
      contains (Pattern "\"amount\":10") encodedSent `shouldEqual` true
      contains (Pattern "\"occurredAt\":\"2026-05-01T10:00:00Z\"") encodedSent `shouldEqual` true
      let encodedReceived = stringify (encodeJson createReceived)
      contains (Pattern "\"accountId\":\"acc-1\"") encodedReceived `shouldEqual` true
      contains (Pattern "\"amount\":12") encodedReceived `shouldEqual` true
      contains (Pattern "\"occurredAt\":null") encodedReceived `shouldEqual` true

    it "encodes categorize, split, transfer link, and notes payloads" do
      stringify (encodeJson (CategorizeFinanceTransaction { category: "cat-1" }))
        `shouldEqual` "{\"category\":\"cat-1\"}"
      let encodedSplit = stringify (encodeJson (SplitFinanceTransaction { splits: [ FinanceTransactionSplitRow { amount: 9.0, category: "cat-1" }, FinanceTransactionSplitRow { amount: 1.0, category: "cat-2" } ] }))
      contains (Pattern "\"splits\":[") encodedSplit `shouldEqual` true
      contains (Pattern "\"amount\":9") encodedSplit `shouldEqual` true
      contains (Pattern "\"category\":\"cat-1\"") encodedSplit `shouldEqual` true
      contains (Pattern "\"amount\":1") encodedSplit `shouldEqual` true
      contains (Pattern "\"category\":\"cat-2\"") encodedSplit `shouldEqual` true
      let encodedLink = stringify (encodeJson (LinkFinanceTransfer { sourceTransactionId: "tx-1", targetTransactionId: "tx-2", linkType: "transfer" }))
      contains (Pattern "\"sourceTransactionId\":\"tx-1\"") encodedLink `shouldEqual` true
      contains (Pattern "\"targetTransactionId\":\"tx-2\"") encodedLink `shouldEqual` true
      contains (Pattern "\"linkType\":\"transfer\"") encodedLink `shouldEqual` true
      stringify (encodeJson (CreateFinanceTransactionNote { text: "hello" }))
        `shouldEqual` "{\"text\":\"hello\"}"
      stringify (encodeJson (UpdateFinanceTransactionNote { text: "updated" }))
        `shouldEqual` "{\"text\":\"updated\"}"

    it "encodes transactions query with only accountId/from/to keys" do
      let
        query = encodeTransactionsQuery (FinanceTransactionsQuery { accountId: Just "acc-1", from: Just "2026-05-01T00:00:00Z", to: Just "2026-05-08T00:00:00Z" })
      query `shouldEqual` "?accountId=acc-1&from=2026-05-01T00:00:00Z&to=2026-05-08T00:00:00Z"

    it "encodes account status query values" do
      encodeAccountsQuery (FinanceAccountsQuery { status: Nothing }) `shouldEqual` ""
      encodeAccountsQuery (FinanceAccountsQuery { status: Just AccountsActive }) `shouldEqual` "?status=active"
      encodeAccountsQuery (FinanceAccountsQuery { status: Just AccountsClosed }) `shouldEqual` "?status=closed"
      encodeAccountsQuery (FinanceAccountsQuery { status: Just AccountsAll }) `shouldEqual` "?status=all"

    it "decodes aggregate report with total count and transaction ids only" do
      let
        report = FinanceAggregateReport { total: 125.5, count: 2, transactionIds: [ "tx-1", "tx-2" ] }
      case decodeJson (encodeJson report) of
        Right decoded ->
          if decoded == report then pure unit else fail "Decoded report did not match encoded value"
        Left err -> fail $ "Failed to decode aggregate report: " <> show err
      let encoded = stringify (encodeJson report)
      contains (Pattern "\"total\":125.5") encoded `shouldEqual` true
      contains (Pattern "\"count\":2") encoded `shouldEqual` true
      contains (Pattern "\"transactionIds\":[\"tx-1\",\"tx-2\"]") encoded `shouldEqual` true

    it "encodes aggregate report query with supported backend parameters" do
      let
        query =
          encodeReportQuery
            ( FinanceReportQuery
                { from: "2026-05-01T00:00:00Z"
                , to: "2026-05-31T00:00:00Z"
                , direction: Just ReportSent
                , accountIn: [ "acc-1" ]
                , accountNotIn: [ "acc-9" ]
                , categoryIn: [ "cat-1", "cat-2" ]
                , categoryNotIn: [ "cat-9" ]
                }
            )
      query
        `shouldEqual`
          "?from=2026-05-01T00:00:00Z&to=2026-05-31T00:00:00Z&direction=sent&accountIn=acc-1&accountNotIn=acc-9&categoryIn=cat-1&categoryIn=cat-2&categoryNotIn=cat-9"
