module Calendar.Calendar.Agenda.Range
  ( RangeAction(..)
  , renderRangeView
  ) where

import Prelude hiding (div)

import Calendar.Calendar.Agenda.List (ListAction, agendaList)
import Calendar.Helpers (isItemOnDate, sortItems)
import Calendar.Model (CalendarItem, SortMode(..))
import Data.Array (filter, null)
import Halogen.HTML (HTML, div, section, text)
import Ui.Utils (class_)

data RangeAction
  = RangeListAction ListAction

renderRangeView
  :: forall w
   . String
  -> Array String
  -> Array String
  -> Array CalendarItem
  -> Boolean
  -> HTML w RangeAction
renderRangeView label dates conflictIds items isMobile =
  if null dates then emptyAgendaRange label
  else
    div [ class_ "calendar-range" ]
      (map (renderDateSection label conflictIds items isMobile) dates)

renderDateSection
  :: forall w
   . String
  -> Array String
  -> Array CalendarItem
  -> Boolean
  -> String
  -> HTML w RangeAction
renderDateSection _ conflictIds items isMobile dateStr =
  let
    itemsForDate = filter (isItemOnDate dateStr) items
    sorted = sortItems SortByTime conflictIds itemsForDate
  in
    section [ class_ "calendar-date-section" ]
      [ div [ class_ "calendar-date-title" ] [ text dateStr ]
      , if null sorted then div [ class_ "calendar-date-empty" ] [ text "Aucun item" ]
        else map RangeListAction (agendaList conflictIds sorted isMobile)
      ]

emptyAgendaRange :: forall w action. String -> HTML w action
emptyAgendaRange label =
  div [ class_ "row entity-empty calendar-empty" ]
    [ div [ class_ "entity-empty-title" ] [ text $ "Aucun item sur la " <> label ]
    , div [ class_ "entity-empty-subtitle" ] [ text "Ajoutez une intention pour demarrer." ]
    ]
