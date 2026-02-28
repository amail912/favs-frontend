module Test.Support.Builders
  ( calendarContent
  , serverCalendarItem
  ) where

import Calendar.Model (CalendarItem(..), CalendarItemContent, ItemStatus(..), ItemType)
import Data.Maybe (Maybe(..))

calendarContent :: ItemType -> String -> String -> String -> CalendarItemContent
calendarContent itemType title windowStart windowEnd =
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

serverCalendarItem :: String -> CalendarItemContent -> CalendarItem
serverCalendarItem id content =
  ServerCalendarItem { id, content }
