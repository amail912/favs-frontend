module Calendar.Calendar.Controls
  ( ControlsAction(..)
  , applyControlsAction
  , renderSortPicker
  ) where

import Prelude hiding (div)

import Calendar.Calendar.State (CalendarState, _showConflictsOnlyS, _sortModeS)
import Calendar.Helpers (parseSortMode, sortModeValue)
import Calendar.Model (SortMode)
import Data.Lens ((.~), (%~))
import Halogen.HTML (HTML, div, option, select, text)
import Halogen.HTML.Events (onValueChange)
import Halogen.HTML.Properties (value)
import Ui.Utils (class_)

data ControlsAction
  = ControlsSortChanged String
  | ControlsToggleConflictFilter

applyControlsAction :: ControlsAction -> CalendarState -> CalendarState
applyControlsAction action dataState =
  case action of
    ControlsSortChanged raw ->
      (_sortModeS .~ parseSortMode raw) dataState
    ControlsToggleConflictFilter ->
      (_showConflictsOnlyS %~ not) dataState

renderSortPicker :: forall w. SortMode -> HTML w ControlsAction
renderSortPicker sortMode =
  div [ class_ "calendar-sort" ]
    [ text "Trier:"
    , select
        [ class_ "form-select calendar-sort-select"
        , onValueChange ControlsSortChanged
        , value (sortModeValue sortMode)
        ]
        [ option [ value "time" ] [ text "Horaire" ]
        , option [ value "status" ] [ text "Statut" ]
        , option [ value "category" ] [ text "Catégorie" ]
        , option [ value "conflict" ] [ text "Conflit" ]
        ]
    ]
