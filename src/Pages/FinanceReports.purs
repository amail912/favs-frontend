module Pages.FinanceReports
  ( component
  , Output(..)
  , ReportRemoteState(..)
  , ReportBodyState(..)
  , ReportRange
  , beginReportLoad
  , applyReportLoadSuccess
  , applyReportLoadFailure
  , deriveReportBodyState
  , buildAggregateQuery
  ) where

import Prelude hiding (div)

import Affjax (printError)
import Api.Finance (getAccounts, getAnalyticsReport, getCategories)
import Api.FinanceContract
  ( FinanceAccount(..)
  , FinanceAccountsQuery(..)
  , FinanceAnalyticsAccountBalanceRow(..)
  , FinanceAnalyticsCashflowSeriesRow(..)
  , FinanceAnalyticsCategoryBreakdownRow(..)
  , FinanceAnalyticsQuery(..)
  , FinanceAnalyticsReport(..)
  , FinanceAnalyticsSummary(..)
  , FinanceCategory(..)
  , FinanceReportDirection(..)
  )
import Control.Monad.RWS (get, modify_)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (any, delete, null, snoc)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String.Common (joinWith)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval) as H
import Halogen (raise)
import Halogen.HTML (button, div, h1, input, label, option, select, text)
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (disabled, value)
import Ui.Utils (class_)

type ReportRange =
  { from :: String
  , to :: String
  }

type Input = Unit

data Output = DrillDownRequested ReportRange

type ReportFilters =
  { from :: String
  , to :: String
  , direction :: Maybe FinanceReportDirection
  , accountId :: Maybe String
  , categoryIn :: Array String
  , categoryNotIn :: Array String
  , amountMinInput :: String
  , amountMaxInput :: String
  , search :: String
  }

type State =
  { filters :: ReportFilters
  , selectedCategoryIn :: String
  , selectedCategoryNotIn :: String
  , appliedRange :: Maybe ReportRange
  , remoteState :: ReportRemoteState
  , validationError :: Maybe String
  , accounts :: Array FinanceAccount
  , categories :: Array FinanceCategory
  }

data ReportRemoteState
  = ReportIdle
  | ReportLoading
  | ReportLoadError String
  | ReportLoaded FinanceAnalyticsReport

data ReportBodyState
  = ReportBodyIdle
  | ReportBodyLoading
  | ReportBodyError String
  | ReportBodyEmpty
  | ReportBodySummary { total :: Number, count :: Int }

derive instance reportRemoteStateEq :: Eq ReportRemoteState
derive instance reportBodyStateEq :: Eq ReportBodyState

data Action
  = Initialize
  | AccountsLoaded (Either String (Array FinanceAccount))
  | CategoriesLoaded (Either String (Array FinanceCategory))
  | UpdateFrom String
  | UpdateTo String
  | UpdateDirection String
  | UpdateAccountId String
  | UpdateAmountMin String
  | UpdateAmountMax String
  | UpdateSearch String
  | UpdateCategoryInSelection String
  | AddCategoryIn
  | RemoveCategoryIn String
  | UpdateCategoryNotInSelection String
  | AddCategoryNotIn
  | RemoveCategoryNotIn String
  | ApplyReport
  | ReportLoadedResult (Either String FinanceAnalyticsReport)
  | DrillDownToLedger

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
initialState _ =
  { filters: emptyFilters
  , selectedCategoryIn: ""
  , selectedCategoryNotIn: ""
  , appliedRange: Nothing
  , remoteState: ReportIdle
  , validationError: Nothing
  , accounts: []
  , categories: []
  }

beginReportLoad :: State -> State
beginReportLoad state =
  state
    { remoteState = ReportLoading
    , validationError = Nothing
    }

applyReportLoadSuccess :: FinanceAnalyticsReport -> State -> State
applyReportLoadSuccess report state =
  state { remoteState = ReportLoaded report }

applyReportLoadFailure :: String -> State -> State
applyReportLoadFailure message state =
  state { remoteState = ReportLoadError message }

deriveReportBodyState :: State -> ReportBodyState
deriveReportBodyState state =
  case state.remoteState of
    ReportIdle ->
      ReportBodyIdle
    ReportLoading ->
      ReportBodyLoading
    ReportLoadError message ->
      ReportBodyError message
    ReportLoaded (FinanceAnalyticsReport report) ->
      let
        FinanceAnalyticsSummary summary = report.summary
      in
        if summary.count == 0 then
          ReportBodyEmpty
        else
          ReportBodySummary { total: summary.total, count: summary.count }

buildAggregateQuery :: ReportRange -> FinanceAnalyticsQuery
buildAggregateQuery range =
  FinanceAnalyticsQuery
    { from: range.from
    , to: range.to
    , direction: Nothing
    , accountId: Nothing
    , categoryIn: []
    , categoryNotIn: []
    , amountMin: Nothing
    , amountMax: Nothing
    , search: Nothing
    }

handleAction :: Action -> H.HalogenM State Action () Output Aff Unit
handleAction = case _ of
  Initialize -> do
    accountsResult <- liftAff fetchAccounts
    categoriesResult <- liftAff fetchCategories
    handleAction (AccountsLoaded accountsResult)
    handleAction (CategoriesLoaded categoriesResult)
  AccountsLoaded result ->
    case result of
      Left _ -> pure unit
      Right accounts ->
        modify_ _ { accounts = accounts }
  CategoriesLoaded result ->
    case result of
      Left _ -> pure unit
      Right categories ->
        modify_ _ { categories = categories }
  UpdateFrom raw ->
    modify_ \st -> st { filters = st.filters { from = raw }, validationError = Nothing }
  UpdateTo raw ->
    modify_ \st -> st { filters = st.filters { to = raw }, validationError = Nothing }
  UpdateDirection raw ->
    modify_ \st -> st { filters = st.filters { direction = decodeDirection raw }, validationError = Nothing }
  UpdateAccountId raw ->
    modify_ \st -> st { filters = st.filters { accountId = toOptional raw }, validationError = Nothing }
  UpdateAmountMin raw ->
    modify_ \st -> st { filters = st.filters { amountMinInput = raw }, validationError = Nothing }
  UpdateAmountMax raw ->
    modify_ \st -> st { filters = st.filters { amountMaxInput = raw }, validationError = Nothing }
  UpdateSearch raw ->
    modify_ \st -> st { filters = st.filters { search = raw }, validationError = Nothing }
  UpdateCategoryInSelection raw ->
    modify_ _ { selectedCategoryIn = raw, validationError = Nothing }
  AddCategoryIn ->
    modify_ \st ->
      if st.selectedCategoryIn == "" || any (_ == st.selectedCategoryIn) st.filters.categoryIn then
        st
      else
        st { filters = st.filters { categoryIn = snoc st.filters.categoryIn st.selectedCategoryIn }, selectedCategoryIn = "", validationError = Nothing }
  RemoveCategoryIn value ->
    modify_ \st ->
      st { filters = st.filters { categoryIn = delete value st.filters.categoryIn }, validationError = Nothing }
  UpdateCategoryNotInSelection raw ->
    modify_ _ { selectedCategoryNotIn = raw, validationError = Nothing }
  AddCategoryNotIn ->
    modify_ \st ->
      if st.selectedCategoryNotIn == "" || any (_ == st.selectedCategoryNotIn) st.filters.categoryNotIn then
        st
      else
        st { filters = st.filters { categoryNotIn = snoc st.filters.categoryNotIn st.selectedCategoryNotIn }, selectedCategoryNotIn = "", validationError = Nothing }
  RemoveCategoryNotIn value ->
    modify_ \st ->
      st { filters = st.filters { categoryNotIn = delete value st.filters.categoryNotIn }, validationError = Nothing }
  ApplyReport -> do
    st <- get
    case buildValidatedQuery st.filters of
      Left message ->
        modify_ _ { validationError = Just message }
      Right query@(FinanceAnalyticsQuery valid) -> do
        modify_ \next ->
          let
            loadingState = beginReportLoad next
          in
            loadingState { appliedRange = Just { from: valid.from, to: valid.to } }
        result <- liftAff (fetchAnalyticsReport query)
        handleAction (ReportLoadedResult result)
  ReportLoadedResult result ->
    case result of
      Left message ->
        modify_ (applyReportLoadFailure message)
      Right report ->
        modify_ (applyReportLoadSuccess report)
  DrillDownToLedger -> do
    st <- get
    case st.appliedRange, deriveReportBodyState st of
      Just range, ReportBodySummary _ ->
        raise (DrillDownRequested range)
      _, _ ->
        pure unit

fetchAccounts :: Aff (Either String (Array FinanceAccount))
fetchAccounts = do
  result <- getAccounts (FinanceAccountsQuery { status: Nothing })
  case result of
    Left err ->
      pure (Left ("Unable to load accounts: " <> printError err))
    Right response ->
      if unwrap response.status >= 200 && unwrap response.status < 300 then
        pure (lmap show (decodeJson response.body :: Either _ (Array FinanceAccount)))
      else
        pure (Left ("Unable to load accounts: status " <> show (unwrap response.status)))

fetchCategories :: Aff (Either String (Array FinanceCategory))
fetchCategories = do
  result <- getCategories
  case result of
    Left err ->
      pure (Left ("Unable to load categories: " <> printError err))
    Right response ->
      if unwrap response.status >= 200 && unwrap response.status < 300 then
        pure (lmap show (decodeJson response.body :: Either _ (Array FinanceCategory)))
      else
        pure (Left ("Unable to load categories: status " <> show (unwrap response.status)))

fetchAnalyticsReport :: FinanceAnalyticsQuery -> Aff (Either String FinanceAnalyticsReport)
fetchAnalyticsReport query = do
  result <- getAnalyticsReport query
  case result of
    Left err ->
      pure (Left ("Unable to load report analytics: " <> printError err))
    Right response ->
      if unwrap response.status >= 200 && unwrap response.status < 300 then
        pure (lmap show (decodeJson response.body :: Either _ FinanceAnalyticsReport))
      else
        pure (Left ("Unable to load report analytics: status " <> show (unwrap response.status)))

buildValidatedQuery :: ReportFilters -> Either String FinanceAnalyticsQuery
buildValidatedQuery filters = do
  if filters.from == "" || filters.to == "" then
    Left "Both from and to are required."
  else
    pure unit
  amountMin <- parseOptionalInt "amountMin" filters.amountMinInput
  amountMax <- parseOptionalInt "amountMax" filters.amountMaxInput
  let categoryIn = unique filters.categoryIn
  let categoryNotIn = unique filters.categoryNotIn
  case amountMin, amountMax of
    Just minValue, Just maxValue | minValue > maxValue ->
      Left "amountMin must be lower or equal to amountMax."
    _, _ ->
      if hasOverlap categoryIn categoryNotIn then
        Left "categoryIn and categoryNotIn must not overlap."
      else
        Right
          ( FinanceAnalyticsQuery
              { from: filters.from
              , to: filters.to
              , direction: filters.direction
              , accountId: filters.accountId
              , categoryIn
              , categoryNotIn
              , amountMin
              , amountMax
              , search: toOptional filters.search
              }
          )

parseOptionalInt :: String -> String -> Either String (Maybe Int)
parseOptionalInt field raw =
  if raw == "" then
    Right Nothing
  else
    case Int.fromString raw of
      Just value -> Right (Just value)
      Nothing -> Left (field <> " must be an integer.")

hasOverlap :: Array String -> Array String -> Boolean
hasOverlap left right =
  any (\value -> any (_ == value) right) left

unique :: Array String -> Array String
unique =
  foldl
    ( \acc value ->
        if value == "" || any (_ == value) acc then acc else snoc acc value
    )
    []

emptyFilters :: ReportFilters
emptyFilters =
  { from: ""
  , to: ""
  , direction: Nothing
  , accountId: Nothing
  , categoryIn: []
  , categoryNotIn: []
  , amountMinInput: ""
  , amountMaxInput: ""
  , search: ""
  }

decodeDirection :: String -> Maybe FinanceReportDirection
decodeDirection = case _ of
  "" -> Nothing
  "sent" -> Just ReportSent
  "received" -> Just ReportReceived
  "all" -> Just ReportAll
  _ -> Nothing

encodeDirection :: FinanceReportDirection -> String
encodeDirection = case _ of
  ReportSent -> "sent"
  ReportReceived -> "received"
  ReportAll -> "all"

toOptional :: String -> Maybe String
toOptional raw =
  if raw == "" then Nothing else Just raw

render :: State -> H.ComponentHTML Action () Aff
render state =
  div [ class_ "finance-reports-workspace d-flex flex-column gap-3" ]
    [ h1 [ class_ "h4 mb-0" ] [ text "Analytics reports" ]
    , div [ class_ "card shadow-sm border-0" ]
        [ div [ class_ "card-body d-flex flex-column gap-2" ]
            [ div [ class_ "d-flex flex-column gap-1" ]
                [ label [ class_ "form-label mb-0" ] [ text "From" ]
                , input
                    [ class_ "form-control finance-reports-filter-from"
                    , value state.filters.from
                    , onValueChange UpdateFrom
                    ]
                ]
            , div [ class_ "d-flex flex-column gap-1" ]
                [ label [ class_ "form-label mb-0" ] [ text "To" ]
                , input
                    [ class_ "form-control finance-reports-filter-to"
                    , value state.filters.to
                    , onValueChange UpdateTo
                    ]
                ]
            , div [ class_ "d-flex flex-column gap-1" ]
                [ label [ class_ "form-label mb-0" ] [ text "Direction" ]
                , select [ class_ "form-select finance-reports-filter-direction", value (foldMap encodeDirection state.filters.direction), onValueChange UpdateDirection ]
                    [ option [ value "" ] [ text "All" ]
                    , option [ value "sent" ] [ text "Sent" ]
                    , option [ value "received" ] [ text "Received" ]
                    ]
                ]
            , div [ class_ "d-flex flex-column gap-1" ]
                [ label [ class_ "form-label mb-0" ] [ text "Account" ]
                , select [ class_ "form-select finance-reports-filter-account", value (foldMap identity state.filters.accountId), onValueChange UpdateAccountId ]
                    ([ option [ value "" ] [ text "All accounts" ] ] <> map renderAccountOption state.accounts)
                ]
            , div [ class_ "d-flex gap-2" ]
                [ div [ class_ "d-flex flex-column gap-1 flex-fill" ]
                    [ label [ class_ "form-label mb-0" ] [ text "Amount min (cents)" ]
                    , input [ class_ "form-control finance-reports-filter-amount-min", value state.filters.amountMinInput, onValueChange UpdateAmountMin ]
                    ]
                , div [ class_ "d-flex flex-column gap-1 flex-fill" ]
                    [ label [ class_ "form-label mb-0" ] [ text "Amount max (cents)" ]
                    , input [ class_ "form-control finance-reports-filter-amount-max", value state.filters.amountMaxInput, onValueChange UpdateAmountMax ]
                    ]
                ]
            , div [ class_ "d-flex flex-column gap-1" ]
                [ label [ class_ "form-label mb-0" ] [ text "Search" ]
                , input [ class_ "form-control finance-reports-filter-search", value state.filters.search, onValueChange UpdateSearch ]
                ]
            , renderCategoryFilter "Include category" "finance-reports-filter-category-in" state.selectedCategoryIn state.categories UpdateCategoryInSelection AddCategoryIn state.filters.categoryIn RemoveCategoryIn
            , renderCategoryFilter "Exclude category" "finance-reports-filter-category-not-in" state.selectedCategoryNotIn state.categories UpdateCategoryNotInSelection AddCategoryNotIn state.filters.categoryNotIn RemoveCategoryNotIn
            , maybe (text "") (\message -> div [ class_ "alert alert-danger mb-0 finance-reports-validation" ] [ text message ]) state.validationError
            , button
                [ class_ "btn btn-primary finance-reports-apply"
                , onClick (const ApplyReport)
                , disabled (state.remoteState == ReportLoading)
                ]
                [ text (if state.remoteState == ReportLoading then "Loading..." else "Run report") ]
            ]
        ]
    , renderBody state
    ]

renderAccountOption :: FinanceAccount -> H.ComponentHTML Action () Aff
renderAccountOption (FinanceAccount account) =
  option [ value account.id ] [ text account.name ]

renderCategoryFilter
  :: String
  -> String
  -> String
  -> Array FinanceCategory
  -> (String -> Action)
  -> Action
  -> Array String
  -> (String -> Action)
  -> H.ComponentHTML Action () Aff
renderCategoryFilter labelText selectClass selected categories onSelect onAdd selectedValues onRemove =
  div [ class_ "d-flex flex-column gap-1" ]
    [ label [ class_ "form-label mb-0" ] [ text labelText ]
    , div [ class_ "d-flex gap-2" ]
        [ select [ class_ ("form-select " <> selectClass), value selected, onValueChange onSelect ]
            ([ option [ value "" ] [ text "Select category" ] ] <> map renderCategoryOption categories)
        , button [ class_ "btn btn-outline-secondary", onClick (const onAdd) ] [ text "Add" ]
        ]
    , div [ class_ "d-flex flex-wrap gap-1" ] (map (\categoryId -> renderTag categoryId (onRemove categoryId)) selectedValues)
    ]

renderCategoryOption :: FinanceCategory -> H.ComponentHTML Action () Aff
renderCategoryOption (FinanceCategory category) =
  option [ value category.id ] [ text category.id ]

renderTag :: String -> Action -> H.ComponentHTML Action () Aff
renderTag labelText removeAction =
  button [ class_ "btn btn-outline-secondary btn-sm", onClick (const removeAction) ]
    [ text (labelText <> " ×") ]

renderBody :: State -> H.ComponentHTML Action () Aff
renderBody state =
  case deriveReportBodyState state of
    ReportBodyIdle ->
      div [ class_ "text-muted finance-reports-idle" ] [ text "Run an analytics report." ]
    ReportBodyLoading ->
      div [ class_ "text-muted finance-reports-loading" ] [ text "Loading report..." ]
    ReportBodyError message ->
      div [ class_ "alert alert-danger mb-0 finance-reports-error" ] [ text message ]
    ReportBodyEmpty ->
      div [ class_ "alert alert-secondary mb-0 finance-reports-empty" ] [ text "No transaction matches this report scope." ]
    ReportBodySummary summary ->
      case state.remoteState of
        ReportLoaded report ->
          renderAnalyticsSections summary report
        _ ->
          text ""

renderAnalyticsSections :: { total :: Number, count :: Int } -> FinanceAnalyticsReport -> H.ComponentHTML Action () Aff
renderAnalyticsSections summary (FinanceAnalyticsReport report) =
  div [ class_ "d-flex flex-column gap-3" ]
    [ div [ class_ "card shadow-sm border-0 finance-reports-result" ]
        [ div [ class_ "card-body d-flex flex-column gap-2" ]
            [ div [ class_ "finance-reports-total" ] [ text ("Total: " <> show summary.total) ]
            , div [ class_ "finance-reports-count" ] [ text ("Count: " <> show summary.count) ]
            , button
                [ class_ "btn btn-outline-primary finance-reports-drilldown"
                , onClick (const DrillDownToLedger)
                ]
                [ text "Open matching ledger scope" ]
            ]
        ]
    , renderCategoryBreakdown report.categoryBreakdown
    , renderCashflowSeries report.cashflowSeries
    , renderAccountBalances report.accountBalances
    ]

renderCategoryBreakdown :: Array FinanceAnalyticsCategoryBreakdownRow -> H.ComponentHTML Action () Aff
renderCategoryBreakdown rows =
  div [ class_ "card shadow-sm border-0 finance-reports-category-breakdown" ]
    [ div [ class_ "card-body d-flex flex-column gap-2" ]
        [ h1 [ class_ "h6 mb-0" ] [ text "Category breakdown" ]
        , if null rows then
            div [ class_ "text-muted" ] [ text "No category breakdown data." ]
          else
            div [ class_ "d-flex flex-column gap-1" ] (map renderCategoryRow rows)
        ]
    ]

renderCategoryRow :: FinanceAnalyticsCategoryBreakdownRow -> H.ComponentHTML Action () Aff
renderCategoryRow (FinanceAnalyticsCategoryBreakdownRow row) =
  div [ class_ "finance-reports-category-row" ]
    [ text (row.categoryId <> " | total=" <> show row.total <> " | count=" <> show row.count) ]

renderCashflowSeries :: Array FinanceAnalyticsCashflowSeriesRow -> H.ComponentHTML Action () Aff
renderCashflowSeries rows =
  div [ class_ "card shadow-sm border-0 finance-reports-cashflow-series" ]
    [ div [ class_ "card-body d-flex flex-column gap-2" ]
        [ h1 [ class_ "h6 mb-0" ] [ text "Cashflow series" ]
        , if null rows then
            div [ class_ "text-muted" ] [ text "No cashflow series data." ]
          else
            div [ class_ "d-flex flex-column gap-1" ] (map renderCashflowRow rows)
        ]
    ]

renderCashflowRow :: FinanceAnalyticsCashflowSeriesRow -> H.ComponentHTML Action () Aff
renderCashflowRow (FinanceAnalyticsCashflowSeriesRow row) =
  let
    label = joinWith " | " [ row.bucketStart <> " -> " <> row.bucketEnd, "total=" <> show row.total, "count=" <> show row.count ]
  in
    div [ class_ "finance-reports-cashflow-row" ]
      [ text label ]

renderAccountBalances :: Array FinanceAnalyticsAccountBalanceRow -> H.ComponentHTML Action () Aff
renderAccountBalances rows =
  div [ class_ "card shadow-sm border-0 finance-reports-account-balances" ]
    [ div [ class_ "card-body d-flex flex-column gap-2" ]
        [ h1 [ class_ "h6 mb-0" ] [ text "Account balances" ]
        , if null rows then
            div [ class_ "text-muted" ] [ text "No account balance data." ]
          else
            div [ class_ "d-flex flex-column gap-1" ] (map renderAccountBalanceRow rows)
        ]
    ]

renderAccountBalanceRow :: FinanceAnalyticsAccountBalanceRow -> H.ComponentHTML Action () Aff
renderAccountBalanceRow (FinanceAnalyticsAccountBalanceRow row) =
  div [ class_ "finance-reports-account-balance-row" ]
    [ text (row.accountId <> " | total=" <> show row.total) ]
