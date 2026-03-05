module Helpers.DateTime
  ( parseLocalDateTime
  , formatLocalDateTime
  , parseLocalDate
  , formatLocalDate
  , parseLocalTime
  , formatLocalTime
  , timeFromParts
  , formatLocalTimeParts
  , isLocalDateTime
  , isLocalDate
  , isLocalTime
  ) where

import Prelude

import Data.Date (Date)
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
