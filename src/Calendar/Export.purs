module Calendar.Export
  ( ExportState(..)
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
import Control.Monad.State.Trans (StateT, get, modify_)
import Control.Monad.Writer.Trans (WriterT)
import Data.Lens (Lens', lens, (.~), (^.))
import Effect.Aff (Aff)
import Halogen.HTML (HTML, button, div, input, option, section, select, text, textarea)
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (placeholder, type_, value)
import DOM.HTML.Indexed.InputType (InputType(..))
import Ui.AgendaRender (renderPanelHeader)
import Ui.Utils (class_)

newtype ExportState = ExportState
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
  ExportState
    { exportFormat: ExportCSV
    , exportTypeFilter: ""
    , exportStatusFilter: ""
    , exportCategoryFilter: ""
    , exportStartDate: ""
    , exportEndDate: ""
    , exportOutput: ""
    }

_exportFormatS :: Lens' ExportState ExportFormat
_exportFormatS =
  lens
    (\(ExportState state) -> state.exportFormat)
    (\(ExportState state) exportFormat -> ExportState (state { exportFormat = exportFormat }))

_exportTypeFilterS :: Lens' ExportState String
_exportTypeFilterS =
  lens
    (\(ExportState state) -> state.exportTypeFilter)
    (\(ExportState state) exportTypeFilter -> ExportState (state { exportTypeFilter = exportTypeFilter }))

_exportStatusFilterS :: Lens' ExportState String
_exportStatusFilterS =
  lens
    (\(ExportState state) -> state.exportStatusFilter)
    (\(ExportState state) exportStatusFilter -> ExportState (state { exportStatusFilter = exportStatusFilter }))

_exportCategoryFilterS :: Lens' ExportState String
_exportCategoryFilterS =
  lens
    (\(ExportState state) -> state.exportCategoryFilter)
    (\(ExportState state) exportCategoryFilter -> ExportState (state { exportCategoryFilter = exportCategoryFilter }))

_exportStartDateS :: Lens' ExportState String
_exportStartDateS =
  lens
    (\(ExportState state) -> state.exportStartDate)
    (\(ExportState state) exportStartDate -> ExportState (state { exportStartDate = exportStartDate }))

_exportEndDateS :: Lens' ExportState String
_exportEndDateS =
  lens
    (\(ExportState state) -> state.exportEndDate)
    (\(ExportState state) exportEndDate -> ExportState (state { exportEndDate = exportEndDate }))

_exportOutputS :: Lens' ExportState String
_exportOutputS =
  lens
    (\(ExportState state) -> state.exportOutput)
    (\(ExportState state) exportOutput -> ExportState (state { exportOutput = exportOutput }))

data ExportAction
  = ExportFormatChangedAction String
  | ExportTypeFilterChangedAction String
  | ExportStatusFilterChangedAction String
  | ExportCategoryFilterChangedAction String
  | ExportStartDateChangedAction String
  | ExportEndDateChangedAction String
  | ExportGenerate
  | ExportClearOutput

handleExportAction :: Array CalendarItem -> ExportAction -> StateT ExportState (WriterT (Array Void) Aff) Unit
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
        "Filtres: type, catégorie, statut, période."
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
                , option [ value "BLOC_PLANIFIE" ] [ text "Bloc planifié" ]
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
            [ div [ class_ "calendar-notifications-label" ] [ text "Catégorie" ]
            , input
                [ class_ "form-control calendar-input"
                , placeholder "Ex: Sport"
                , value categoryFilter
                , onValueChange ExportCategoryFilterChangedAction
                ]
            ]
        , div [ class_ "calendar-export-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Début" ]
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
        [ button [ class_ "btn btn-sm btn-primary", onClick (const ExportGenerate) ] [ text "Générer" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ExportClearOutput) ] [ text "Effacer" ]
        ]
    , if output == "" then text ""
      else
        textarea
          [ class_ "form-control calendar-export-textarea"
          , value output
          ]
    ]
