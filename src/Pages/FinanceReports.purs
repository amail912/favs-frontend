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
import Api.Finance (getAggregateReport)
import Api.FinanceContract (FinanceAggregateReport(..), FinanceReportQuery(..))
import Control.Monad.RWS (get, modify_)
import Data.Argonaut.Decode (decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval) as H
import Halogen (raise)
import Halogen.HTML (button, div, h1, input, label, text)
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (disabled, value)
import Ui.Utils (class_)

type ReportRange =
  { from :: String
  , to :: String
  }

type Input = Unit

data Output = DrillDownRequested ReportRange

type State =
  { draftRange :: ReportRange
  , appliedRange :: Maybe ReportRange
  , remoteState :: ReportRemoteState
  , validationError :: Maybe String
  }

data ReportRemoteState
  = ReportIdle
  | ReportLoading
  | ReportLoadError String
  | ReportLoaded FinanceAggregateReport

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
  | UpdateFrom String
  | UpdateTo String
  | ApplyReport
  | ReportLoadedResult (Either String FinanceAggregateReport)
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
  { draftRange: { from: "", to: "" }
  , appliedRange: Nothing
  , remoteState: ReportIdle
  , validationError: Nothing
  }

beginReportLoad :: State -> State
beginReportLoad state =
  state
    { remoteState = ReportLoading
    , validationError = Nothing
    }

applyReportLoadSuccess :: FinanceAggregateReport -> State -> State
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
    ReportLoaded (FinanceAggregateReport aggregate) ->
      if aggregate.count == 0 then
        ReportBodyEmpty
      else
        ReportBodySummary { total: aggregate.total, count: aggregate.count }

buildAggregateQuery :: ReportRange -> FinanceReportQuery
buildAggregateQuery range =
  FinanceReportQuery
    { from: range.from
    , to: range.to
    , direction: Nothing
    , accountIn: []
    , accountNotIn: []
    , categoryIn: []
    , categoryNotIn: []
    }

validateRange :: ReportRange -> Maybe String
validateRange range
  | range.from == "" || range.to == "" = Just "Both from and to are required."
  | otherwise = Nothing

handleAction :: Action -> H.HalogenM State Action () Output Aff Unit
handleAction = case _ of
  Initialize ->
    pure unit
  UpdateFrom raw ->
    modify_ \st -> st { draftRange = st.draftRange { from = raw }, validationError = Nothing }
  UpdateTo raw ->
    modify_ \st -> st { draftRange = st.draftRange { to = raw }, validationError = Nothing }
  ApplyReport -> do
    st <- get
    case validateRange st.draftRange of
      Just message ->
        modify_ _ { validationError = Just message }
      Nothing -> do
        modify_ \next ->
          let
            loadingState = beginReportLoad next
          in
            loadingState { appliedRange = Just next.draftRange }
        result <- liftAff (fetchReportAggregate st.draftRange)
        handleAction (ReportLoadedResult result)
  ReportLoadedResult result ->
    case result of
      Left message ->
        modify_ (applyReportLoadFailure message)
      Right aggregate ->
        modify_ (applyReportLoadSuccess aggregate)
  DrillDownToLedger -> do
    st <- get
    case st.appliedRange, deriveReportBodyState st of
      Just range, ReportBodySummary _ ->
        raise (DrillDownRequested range)
      _, _ ->
        pure unit

fetchReportAggregate :: ReportRange -> Aff (Either String FinanceAggregateReport)
fetchReportAggregate range = do
  result <- getAggregateReport (buildAggregateQuery range)
  case result of
    Left err ->
      pure (Left ("Unable to load report: " <> printError err))
    Right response ->
      if unwrap response.status >= 200 && unwrap response.status < 300 then
        pure (lmap show (decodeJson response.body :: Either _ FinanceAggregateReport))
      else
        pure (Left ("Unable to load report: status " <> show (unwrap response.status)))

render :: State -> H.ComponentHTML Action () Aff
render state =
  div [ class_ "finance-reports-workspace d-flex flex-column gap-3" ]
    [ h1 [ class_ "h4 mb-0" ] [ text "Aggregate reports" ]
    , div [ class_ "card shadow-sm border-0" ]
        [ div [ class_ "card-body d-flex flex-column gap-2" ]
            [ div [ class_ "d-flex flex-column gap-1" ]
                [ label [ class_ "form-label mb-0" ] [ text "From" ]
                , input
                    [ class_ "form-control finance-reports-filter-from"
                    , value state.draftRange.from
                    , onValueChange UpdateFrom
                    ]
                ]
            , div [ class_ "d-flex flex-column gap-1" ]
                [ label [ class_ "form-label mb-0" ] [ text "To" ]
                , input
                    [ class_ "form-control finance-reports-filter-to"
                    , value state.draftRange.to
                    , onValueChange UpdateTo
                    ]
                ]
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

renderBody :: State -> H.ComponentHTML Action () Aff
renderBody state =
  case deriveReportBodyState state of
    ReportBodyIdle ->
      div [ class_ "text-muted finance-reports-idle" ] [ text "Run a date-scoped aggregate report." ]
    ReportBodyLoading ->
      div [ class_ "text-muted finance-reports-loading" ] [ text "Loading report..." ]
    ReportBodyError message ->
      div [ class_ "alert alert-danger mb-0 finance-reports-error" ] [ text message ]
    ReportBodyEmpty ->
      div [ class_ "alert alert-secondary mb-0 finance-reports-empty" ] [ text "No transaction matches this date range." ]
    ReportBodySummary summary ->
      div [ class_ "card shadow-sm border-0 finance-reports-result" ]
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
