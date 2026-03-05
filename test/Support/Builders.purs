module Test.Support.Builders
  ( calendarContent
  , serverCalendarItem
  , unsafeDate
  , unsafeDateTime
  ) where

import Prelude

import Pages.Calendar (CalendarItem(..), CalendarItemContent, ItemStatus(..), ItemType)
import Data.Date (Date)
import Data.DateTime (DateTime(..))
import Data.Maybe (Maybe(..))
import Data.Int as Int
import Data.String.CodeUnits as String
import Helpers.DateTime as DateTime
import Partial.Unsafe (unsafeCrashWith)

calendarContent :: ItemType -> String -> String -> String -> CalendarItemContent
calendarContent itemType title windowStart windowEnd =
  { itemType
  , title
  , windowStart: unsafeDateTime windowStart
  , windowEnd: unsafeDateTime windowEnd
  , status: Todo
  , sourceItemId: Nothing
  , actualDurationMinutes: Nothing
  , category: Nothing
  , recurrenceRule: Nothing
  , recurrenceExceptionDates: []
  }

unsafeDateTime :: String -> DateTime
unsafeDateTime raw =
  case parseDateTime raw of
    Nothing -> unsafeCrashWith ("Invalid DateTime: " <> raw)
    Just dateTime -> dateTime
  where
  parseDateTime input = do
    dateValue <- DateTime.parseLocalDate (String.slice 0 10 input)
    hourInt <- Int.fromString (String.slice 11 13 input)
    minuteInt <- Int.fromString (String.slice 14 16 input)
    timeValue <- DateTime.timeFromParts hourInt minuteInt
    pure $ DateTime dateValue timeValue

unsafeDate :: String -> Date
unsafeDate raw =
  case DateTime.parseLocalDate raw of
    Nothing -> unsafeCrashWith ("Invalid Date: " <> raw)
    Just dateValue -> dateValue

serverCalendarItem :: String -> CalendarItemContent -> CalendarItem
serverCalendarItem id content =
  ServerCalendarItem { id, content }
