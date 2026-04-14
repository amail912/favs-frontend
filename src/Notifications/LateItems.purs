module Notifications.LateItems
  ( LateItem
  , deriveLateItems
  , visibleLateItems
  , hasMoreLateItems
  ) where

import Prelude

import Data.Array (drop, filter, length, mapMaybe, sortBy, take)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Helpers.DateTime as DateTime
import Pages.Calendar (CalendarItem(..), CalendarItemContent(..), ItemStatus(..), calendarItemPrimaryText)

type LateItem =
  { id :: Maybe String
  , title :: String
  , end :: DateTime
  , endDisplay :: String
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
      Just (mkLateItem (Just id) item content.windowEnd)
    NewCalendarItem { content: TaskCalendarItemContent content } ->
      Just (mkLateItem Nothing item content.windowEnd)
    _ ->
      Nothing

mkLateItem :: Maybe String -> CalendarItem -> DateTime -> LateItem
mkLateItem id item end =
  { id
  , title: calendarItemPrimaryText item
  , end
  , endDisplay: DateTime.formatDisplayDateTime end
  }
