module Test.Support.Builders
  ( agendaContent
  , serverAgendaItem
  ) where

import Agenda.Model (CalendarItem(..), CalendarItemContent, ItemStatus(..), ItemType)
import Data.Maybe (Maybe(..))

agendaContent :: ItemType -> String -> String -> String -> CalendarItemContent
agendaContent itemType title windowStart windowEnd =
  { itemType
  , title
  , windowStart
  , windowEnd
  , status: Todo
  , sourceItemId: Nothing
  , actualDurationMinutes: Nothing
  , category: Nothing
  , recurrenceRule: Nothing
  , recurrenceExceptionDates: []
  }

serverAgendaItem :: String -> CalendarItemContent -> CalendarItem
serverAgendaItem id content =
  ServerCalendarItem { id, content }
