module Notifications.LateItems
  ( LateItem
  , deriveLateItems
  , visibleLateItems
  , hasMoreLateItems
  , normalizeQuickCompleteDurationMinutes
  , validateQuickCompleteDurationInput
  ) where

import Prelude

import Data.Array (drop, filter, length, mapMaybe, sortBy, take)
import Data.DateTime (DateTime)
import Data.DateTime (date) as DateTime
import Data.Int as Int
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Common as StringCommon
import Helpers.DateTime (formatDisplayDateTime, formatLocalDate) as DateTime
import Pages.Calendar (CalendarItem(..), CalendarItemContent(..), ItemStatus(..), calendarItemPrimaryText, durationMinutesBetween)

type LateItem =
  { id :: Maybe String
  , title :: String
  , day :: String
  , end :: DateTime
  , endDisplay :: String
  , plannedDurationMinutes :: Int
  }

deriveLateItems :: DateTime -> Array CalendarItem -> Array LateItem
deriveLateItems now =
  sortBy compareEndDesc
    <<< mapMaybe toLateItem
    <<< filter (isLateItem now)
  where
  compareEndDesc left right = compare right.end left.end

visibleLateItems :: Int -> Array LateItem -> Array LateItem
visibleLateItems limit items = take (max 0 limit) items

hasMoreLateItems :: Int -> Array LateItem -> Boolean
hasMoreLateItems limit items = length (drop (max 0 limit) items) > 0

isLateItem :: DateTime -> CalendarItem -> Boolean
isLateItem now item =
  case item of
    ServerCalendarItem { content: TaskCalendarItemContent content } ->
      now > content.windowEnd && not (isExcludedStatus content.status)
    NewCalendarItem { content: TaskCalendarItemContent content } ->
      now > content.windowEnd && not (isExcludedStatus content.status)
    _ ->
      false

isExcludedStatus :: ItemStatus -> Boolean
isExcludedStatus status =
  case status of
    Done -> true
    Canceled -> true
    _ -> false

toLateItem :: CalendarItem -> Maybe LateItem
toLateItem item =
  case item of
    ServerCalendarItem { id, content: TaskCalendarItemContent content } ->
      Just (mkLateItem (Just id) item content.windowEnd (durationMinutesBetween content.windowStart content.windowEnd))
    NewCalendarItem { content: TaskCalendarItemContent content } ->
      Just (mkLateItem Nothing item content.windowEnd (durationMinutesBetween content.windowStart content.windowEnd))
    _ ->
      Nothing

mkLateItem :: Maybe String -> CalendarItem -> DateTime -> Int -> LateItem
mkLateItem id item end plannedDurationMinutes =
  { id
  , title: calendarItemPrimaryText item
  , day: DateTime.formatLocalDate (DateTime.date end)
  , end
  , endDisplay: DateTime.formatDisplayDateTime end
  , plannedDurationMinutes
  }

normalizeQuickCompleteDurationMinutes :: Int -> { value :: Int, wasRounded :: Boolean }
normalizeQuickCompleteDurationMinutes raw =
  let
    normalizedRaw = max 5 raw
    roundedToNearest = max 5 (Int.round (Int.toNumber normalizedRaw / 5.0) * 5)
  in
    { value: roundedToNearest
    , wasRounded: roundedToNearest /= raw
    }

validateQuickCompleteDurationInput :: String -> Either String Int
validateQuickCompleteDurationInput raw =
  case Int.fromString (StringCommon.trim raw) of
    Nothing ->
      Left "La durée doit être un nombre entier."
    Just minutes | minutes <= 0 ->
      Left "La durée doit être supérieure à 0."
    Just minutes | Int.rem minutes 5 /= 0 ->
      Left "La durée doit être un multiple de 5 minutes."
    Just minutes ->
      Right minutes
