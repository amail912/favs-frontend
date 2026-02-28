module Calendar.Export
  ( ExportState
  , ExportAction(..)
  , exportInitialState
  , handleExportAction
  , renderExportPanel
  ) where

import Prelude hiding (div)

import Calendar.Exports
  ( exportFormatValue
  , exportItemsToCsv
  , exportItemsToIcs
  , filterItemsForExport
  , parseExportFormat
  , parseExportItemType
  , parseExportStatus
  )
import Calendar.Helpers (toOptionalString)
import Calendar.Model (CalendarItem, ExportFormat(..))
import Calendar.Commands (Command)
import Control.Monad.State.Trans (StateT, get, modify_)
import Control.Monad.Writer.Trans (WriterT)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Record (prop)
import Effect.Aff (Aff)
import Halogen.HTML (HTML, button, div, input, option, section, select, text, textarea)
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (placeholder, type_, value)
import Type.Proxy (Proxy(..))
import DOM.HTML.Indexed.InputType (InputType(..))
import Ui.AgendaRender (renderPanelHeader)
import Ui.Utils (class_)


type ExportState =
  { exportFormat :: ExportFormat
  , exportTypeFilter :: String
  , exportStatusFilter :: String
  , exportCategoryFilter :: String
  , exportStartDate :: String
  , exportEndDate :: String
  , exportOutput :: String
  }


exportInitialState :: ExportState
exportInitialState =
  { exportFormat: ExportCSV
  , exportTypeFilter: ""
  , exportStatusFilter: ""
  , exportCategoryFilter: ""
  , exportStartDate: ""
  , exportEndDate: ""
  , exportOutput: ""
  }


_exportFormatS :: Lens' ExportState ExportFormat
_exportFormatS = prop (Proxy :: _ "exportFormat")

_exportTypeFilterS :: Lens' ExportState String
_exportTypeFilterS = prop (Proxy :: _ "exportTypeFilter")

_exportStatusFilterS :: Lens' ExportState String
_exportStatusFilterS = prop (Proxy :: _ "exportStatusFilter")

_exportCategoryFilterS :: Lens' ExportState String
_exportCategoryFilterS = prop (Proxy :: _ "exportCategoryFilter")

_exportStartDateS :: Lens' ExportState String
_exportStartDateS = prop (Proxy :: _ "exportStartDate")

_exportEndDateS :: Lens' ExportState String
_exportEndDateS = prop (Proxy :: _ "exportEndDate")

_exportOutputS :: Lens' ExportState String
_exportOutputS = prop (Proxy :: _ "exportOutput")


data ExportAction
  = ExportFormatChangedAction String
  | ExportTypeFilterChangedAction String
  | ExportStatusFilterChangedAction String
  | ExportCategoryFilterChangedAction String
  | ExportStartDateChangedAction String
  | ExportEndDateChangedAction String
  | ExportGenerate
  | ExportClearOutput


handleExportAction :: Array CalendarItem -> ExportAction -> StateT ExportState (WriterT (Array Command) Aff) Unit
handleExportAction items = case _ of
  ExportFormatChangedAction raw ->
    modify_ (_exportFormatS .~ parseExportFormat raw)
  ExportTypeFilterChangedAction raw ->
    modify_ (_exportTypeFilterS .~ raw)
  ExportStatusFilterChangedAction raw ->
    modify_ (_exportStatusFilterS .~ raw)
  ExportCategoryFilterChangedAction raw ->
    modify_ (_exportCategoryFilterS .~ raw)
  ExportStartDateChangedAction raw ->
    modify_ (_exportStartDateS .~ raw)
  ExportEndDateChangedAction raw ->
    modify_ (_exportEndDateS .~ raw)
  ExportGenerate -> do
    exportState <- get
    let
      filter =
        { itemType: parseExportItemType (exportState ^. _exportTypeFilterS)
        , status: parseExportStatus (exportState ^. _exportStatusFilterS)
        , category: toOptionalString (exportState ^. _exportCategoryFilterS)
        , startDate: toOptionalString (exportState ^. _exportStartDateS)
        , endDate: toOptionalString (exportState ^. _exportEndDateS)
        }
      filtered = filterItemsForExport filter items
      output =
        case exportState ^. _exportFormatS of
          ExportCSV -> exportItemsToCsv filtered
          ExportICS -> exportItemsToIcs filtered
    modify_ (_exportOutputS .~ output)
  ExportClearOutput ->
    modify_ (_exportOutputS .~ "")


renderExportPanel
  :: forall w
   . ExportFormat
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> HTML w ExportAction
renderExportPanel format typeFilter statusFilter categoryFilter startDate endDate output =
  section [ class_ "calendar-export" ]
    [ renderPanelHeader
        "calendar-export"
        "Export"
        "Filtres: type, categorie, statut, periode."
        []
    , div [ class_ "calendar-export-controls" ]
        [ div [ class_ "calendar-export-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Format" ]
            , select
                [ class_ "form-select calendar-sort-select"
                , onValueChange ExportFormatChangedAction
                , value (exportFormatValue format)
                ]
                [ option [ value "csv" ] [ text "CSV" ]
                , option [ value "ics" ] [ text "ICS" ]
                ]
            ]
        , div [ class_ "calendar-export-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Type" ]
            , select
                [ class_ "form-select calendar-sort-select"
                , onValueChange ExportTypeFilterChangedAction
                , value typeFilter
                ]
                [ option [ value "" ] [ text "Tous" ]
                , option [ value "INTENTION" ] [ text "Intention" ]
                , option [ value "BLOC_PLANIFIE" ] [ text "Bloc planifie" ]
                ]
            ]
        , div [ class_ "calendar-export-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Statut" ]
            , select
                [ class_ "form-select calendar-sort-select"
                , onValueChange ExportStatusFilterChangedAction
                , value statusFilter
                ]
                [ option [ value "" ] [ text "Tous" ]
                , option [ value "TODO" ] [ text "TODO" ]
                , option [ value "EN_COURS" ] [ text "EN_COURS" ]
                , option [ value "FAIT" ] [ text "FAIT" ]
                , option [ value "ANNULE" ] [ text "ANNULE" ]
                ]
            ]
        , div [ class_ "calendar-export-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Categorie" ]
            , input
                [ class_ "form-control calendar-input"
                , placeholder "Ex: Sport"
                , value categoryFilter
                , onValueChange ExportCategoryFilterChangedAction
                ]
            ]
        , div [ class_ "calendar-export-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Debut" ]
            , input
                [ class_ "form-control calendar-input"
                , type_ InputDate
                , value startDate
                , onValueChange ExportStartDateChangedAction
                ]
            ]
        , div [ class_ "calendar-export-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Fin" ]
            , input
                [ class_ "form-control calendar-input"
                , type_ InputDate
                , value endDate
                , onValueChange ExportEndDateChangedAction
                ]
            ]
        ]
    , div [ class_ "calendar-export-actions" ]
        [ button [ class_ "btn btn-sm btn-primary", onClick (const ExportGenerate) ] [ text "Generer" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ExportClearOutput) ] [ text "Effacer" ]
        ]
    , if output == "" then text ""
      else
        textarea
          [ class_ "form-control calendar-export-textarea"
          , value output
          ]
    ]
