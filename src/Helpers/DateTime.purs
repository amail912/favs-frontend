module Helpers.DateTime
  ( parseLocalDateTime
  , formatLocalDateTime
  , formatDisplayDateTime
  , formatDisplayDateTimeRaw
  , parseLocalDate
  , formatLocalDate
  , formatDisplayDate
  , formatDisplayDateRaw
  , formatCalendarDayDateLabel
  , formatCalendarDayDateLabelWithReference
  , parseLocalTime
  , formatLocalTime
  , formatDisplayTime
  , formatDisplayTimeRaw
  , timeFromParts
  , formatLocalTimeParts
  , isLocalDateTime
  , isLocalDate
  , isLocalTime
  ) where

import Prelude

import Data.Date (Date, year)
import Data.DateTime (DateTime(..), date, time)
import Data.Either (either)
import Data.Enum (toEnum)
import Data.Formatter.DateTime (formatDateTime, unformatDateTime)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Time (Time(..))

localDateTimePattern :: String
localDateTimePattern = "YYYY-MM-DDTHH:mm"

localDatePattern :: String
localDatePattern = "YYYY-MM-DD"

localTimePattern :: String
localTimePattern = "HH:mm"

parseLocalDateTime :: String -> Maybe DateTime
parseLocalDateTime raw =
  unformatDateTime localDateTimePattern raw
    # either (const Nothing) Just

formatLocalDateTime :: DateTime -> String
formatLocalDateTime dt =
  formatDateTime localDateTimePattern dt
    # either (const "") identity

formatDisplayDateTime :: DateTime -> String
formatDisplayDateTime = formatDisplayDateTimeRaw <<< formatLocalDateTime

formatDisplayDateTimeRaw :: String -> String
formatDisplayDateTimeRaw raw =
  case parseLocalDateTime raw of
    Just _ -> formatFrenchDateTimeImpl raw
    Nothing -> raw

parseLocalDate :: String -> Maybe Date
parseLocalDate raw =
  unformatDateTime localDatePattern raw
    # either (const Nothing) (Just <<< date)

formatLocalDate :: Date -> String
formatLocalDate date' =
  fromMaybe "" do
    midnight <- timeFromParts 0 0
    pure $ formatDateTime localDatePattern (DateTime date' midnight)
      # either (const "") identity

formatDisplayDate :: Date -> String
formatDisplayDate = formatDisplayDateRaw <<< formatLocalDate

formatDisplayDateRaw :: String -> String
formatDisplayDateRaw raw =
  case parseLocalDate raw of
    Just _ -> formatFrenchDateImpl raw
    Nothing -> raw

formatCalendarDayDateLabel :: String -> String
formatCalendarDayDateLabel raw =
  formatCalendarDayDateLabelWithReference raw currentLocalDate

formatCalendarDayDateLabelWithReference :: String -> String -> String
formatCalendarDayDateLabelWithReference raw referenceRaw =
  case { selected: parseLocalDate raw, reference: parseLocalDate referenceRaw } of
    { selected: Just selectedDate, reference: Just referenceDate } ->
      formatFrenchDayLabelImpl (year selectedDate /= year referenceDate) raw
    _ ->
      raw

parseLocalTime :: String -> Maybe Time
parseLocalTime raw =
  unformatDateTime localTimePattern raw
    # either (const Nothing) (Just <<< time)

formatLocalTime :: Time -> String
formatLocalTime time' =
  case parseLocalDate "2000-01-01" of
    Nothing -> ""
    Just date' ->
      formatDateTime localTimePattern (DateTime date' time')
        # either (const "") identity

formatDisplayTime :: Time -> String
formatDisplayTime = formatDisplayTimeRaw <<< formatLocalTime

formatDisplayTimeRaw :: String -> String
formatDisplayTimeRaw raw =
  case parseLocalTime raw of
    Just _ -> formatFrenchTimeImpl raw
    Nothing -> raw

timeFromParts :: Int -> Int -> Maybe Time
timeFromParts hour minute = do
  hour' <- toEnum hour
  minute' <- toEnum minute
  second' <- toEnum 0
  millisecond' <- toEnum 0
  pure $ Time hour' minute' second' millisecond'

formatLocalTimeParts :: Int -> Int -> String
formatLocalTimeParts hour minute =
  timeFromParts hour minute
    # map formatLocalTime
    # fromMaybe ""

isLocalDateTime :: String -> Boolean
isLocalDateTime raw = isJust (parseLocalDateTime raw)

isLocalDate :: String -> Boolean
isLocalDate raw = isJust (parseLocalDate raw)

isLocalTime :: String -> Boolean
isLocalTime raw = isJust (parseLocalTime raw)

foreign import currentLocalDate :: String

foreign import formatFrenchDayLabelImpl :: Boolean -> String -> String
foreign import formatFrenchDateTimeImpl :: String -> String
foreign import formatFrenchDateImpl :: String -> String
foreign import formatFrenchTimeImpl :: String -> String
