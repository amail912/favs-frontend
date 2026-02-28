module Calendar.Helpers
  ( isDateTimeLocal
  , parseDateTimeLocal
  , parseDateLocal
  , parseTimeLocal
  , parsePositiveInt
  , isTimeLocal
  , combineDateWithTime
  , shiftMinutes
  , durationMinutesBetween
  , durationMinutesBetweenDateTime
  , suggestDurationMinutes
  , formatDate
  , formatDateOnly
  , formatDateTimeLocal
  , formatTime
  , pad2
  , timeLabel
  , datePart
  , toOptionalString
  , calendarItemContent
  , isItemOnDate
  , generateDateRange
  , generateMonthDates
  , addDaysToDate
  , addDays
  , addMonths
  , isUnplannedIntention
  , plannedIntentionIds
  , isConflict
  , sortItems
  , sortModeValue
  , parseSortMode
  , minuteOfDay
  ) where

import Prelude

import Calendar.Model (CalendarItem(..), CalendarItemContent, ItemStatus(..), ItemType(..), SortMode(..))
import Data.Array (elem, mapMaybe, sortBy)
import Data.Date (Date, canonicalDate, day, exactDate, month, year)
import Data.DateTime (DateTime(..), adjust, date, diff, time)
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (all)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String.CodeUnits as String
import Data.String.Common as StringCommon
import Data.Time (Time(..), hour, minute)
import Data.Time.Duration (Days(..), Minutes(..))
import Data.Formatter.DateTime (unformatDateTime)
import Effect (Effect)
import Effect.Now (nowDateTime)

isDateTimeLocal :: String -> Boolean
isDateTimeLocal raw =
  case unformatDateTime "YYYY-MM-DDTHH:mm" raw of
    Right _ -> true
    Left _ -> false

parseDateTimeLocal :: String -> Maybe DateTime
parseDateTimeLocal raw = do
  yearNum <- parseInt (slice 0 4)
  monthNum <- parseInt (slice 5 7)
  dayNum <- parseInt (slice 8 10)
  hourNum <- parseInt (slice 11 13)
  minuteNum <- parseInt (slice 14 16)
  month <- toEnum monthNum
  day <- toEnum dayNum
  hour <- toEnum hourNum
  minute <- toEnum minuteNum
  yearEnum <- toEnum yearNum
  date <- exactDate yearEnum month day
  second <- toEnum 0
  millisecond <- toEnum 0
  pure $ DateTime date (Time hour minute second millisecond)
  where
  slice start end = String.slice start end raw
  parseInt str = Int.fromString str

parseDateLocal :: String -> Maybe Date
parseDateLocal raw = do
  yearNum <- parseInt (String.slice 0 4 raw)
  monthNum <- parseInt (String.slice 5 7 raw)
  dayNum <- parseInt (String.slice 8 10 raw)
  month' <- toEnum monthNum
  day' <- toEnum dayNum
  year' <- toEnum yearNum
  exactDate year' month' day'
  where
  parseInt str = Int.fromString str

parseTimeLocal :: String -> Maybe Time
parseTimeLocal raw = do
  hourNum <- parseInt (String.slice 0 2 raw)
  minuteNum <- parseInt (String.slice 3 5 raw)
  hour <- toEnum hourNum
  minute <- toEnum minuteNum
  second <- toEnum 0
  millisecond <- toEnum 0
  pure $ Time hour minute second millisecond
  where
  parseInt str = Int.fromString str

parsePositiveInt :: String -> Maybe Int
parsePositiveInt raw =
  Int.fromString (StringCommon.trim raw) >>= \val ->
    if val > 0 then Just val else Nothing

isTimeLocal :: String -> Boolean
isTimeLocal raw =
  String.length raw == 5
    && matchesAt 2 ':'
    && allDigitsAt [ 0, 1, 3, 4 ]
  where
  matchesAt idx expected =
    case String.charAt idx raw of
      Just ch -> ch == expected
      Nothing -> false
  allDigitsAt = all (\idx -> maybe false isDigitChar (String.charAt idx raw))
  isDigitChar ch = ch >= '0' && ch <= '9'

combineDateWithTime :: String -> String -> Maybe String
combineDateWithTime dateTimeRaw timeRaw = do
  dt <- parseDateTimeLocal dateTimeRaw
  t <- parseTimeLocal timeRaw
  pure $ formatDateTimeLocal (DateTime (date dt) t)

shiftMinutes :: Int -> String -> Maybe String
shiftMinutes offset start = do
  dt <- parseDateTimeLocal start
  newDt <- adjust (Minutes (Int.toNumber offset)) dt
  pure $ formatDateTimeLocal newDt

durationMinutesBetween :: String -> String -> Maybe Int
durationMinutesBetween start end = do
  startDt <- parseDateTimeLocal start
  endDt <- parseDateTimeLocal end
  let
    Minutes n = diff endDt startDt
    minutes = Int.floor n
  pure $ max 1 minutes

durationMinutesBetweenDateTime :: String -> DateTime -> Maybe Int
durationMinutesBetweenDateTime start now = do
  startDt <- parseDateTimeLocal start
  let
    Minutes n = diff now startDt
    minutes = Int.floor n
  pure $ max 1 minutes

suggestDurationMinutes :: String -> Effect (Maybe Int)
suggestDurationMinutes start = do
  now <- nowDateTime
  pure $ durationMinutesBetweenDateTime start now

formatDate :: DateTime -> String
formatDate dt =
  formatDateOnly (date dt)

formatDateOnly :: Date -> String
formatDateOnly dt =
  let
    y = Int.toStringAs Int.decimal (fromEnum (year dt))
    m = pad2 (fromEnum (month dt))
    d = pad2 (fromEnum (day dt))
  in
    y <> "-" <> m <> "-" <> d

formatDateTimeLocal :: DateTime -> String
formatDateTimeLocal dt =
  formatDate dt <> "T" <> formatTime (time dt)

formatTime :: Time -> String
formatTime t =
  let
    h = pad2 (fromEnum (hour t))
    m = pad2 (fromEnum (minute t))
  in
    h <> ":" <> m

pad2 :: Int -> String
pad2 n =
  let
    raw = Int.toStringAs Int.decimal n
  in
    if String.length raw == 1 then "0" <> raw else raw

timeLabel :: String -> String
timeLabel raw =
  if String.length raw >= 16 then String.slice 11 16 raw else raw

datePart :: String -> String
datePart raw =
  if String.length raw >= 10 then String.slice 0 10 raw else raw

toOptionalString :: String -> Maybe String
toOptionalString raw =
  let
    trimmed = StringCommon.trim raw
  in
    if trimmed == "" then Nothing else Just trimmed

calendarItemContent :: CalendarItem -> CalendarItemContent
calendarItemContent (NewCalendarItem { content }) = content
calendarItemContent (ServerCalendarItem { content }) = content

isItemOnDate :: String -> CalendarItem -> Boolean
isItemOnDate dateStr item =
  datePart (calendarItemContent item).windowStart == dateStr

generateDateRange :: String -> Int -> Array String
generateDateRange start count =
  case parseDateLocal start of
    Nothing -> []
    Just date ->
      let
        go current remaining acc =
          if remaining <= 0 then acc
          else
            let
              next = addDaysToDate 1 current
            in
              case next of
                Nothing -> acc <> [ formatDateOnly current ]
                Just nextDate -> go nextDate (remaining - 1) (acc <> [ formatDateOnly current ])
      in
        go date count []

generateMonthDates :: String -> Array String
generateMonthDates start =
  case parseDateLocal start of
    Nothing -> []
    Just date ->
      case toEnum 1 of
        Nothing -> []
        Just day1 ->
          let
            first = canonicalDate (year date) (month date) day1
            targetMonth = month date
            go current acc =
              if month current /= targetMonth then acc
              else case addDaysToDate 1 current of
                Nothing -> acc <> [ formatDateOnly current ]
                Just nextDate -> go nextDate (acc <> [ formatDateOnly current ])
          in
            go first []

addDaysToDate :: Int -> Date -> Maybe Date
addDaysToDate days date' = do
  t <- parseTimeLocal "00:00"
  let dt = DateTime date' t
  newDt <- addDays days dt
  pure $ date newDt

addDays :: Int -> DateTime -> Maybe DateTime
addDays n dt = adjust (Days (Int.toNumber n)) dt

addMonths :: Int -> DateTime -> DateTime
addMonths n (DateTime d t) =
  let
    y = fromEnum (year d)
    m = fromEnum (month d)
    dNum = fromEnum (day d)
    total = (m - 1) + n
    newYear = y + Int.quot total 12
    newMonth = (Int.rem total 12) + 1
    newDate =
      case { year: toEnum newYear, month: toEnum newMonth, day: toEnum dNum } of
        { year: Just y', month: Just m', day: Just d' } -> canonicalDate y' m' d'
        _ -> d
  in
    DateTime newDate t

isUnplannedIntention :: Array CalendarItem -> CalendarItem -> Boolean
isUnplannedIntention items item =
  case item of
    ServerCalendarItem { id, content } | content.itemType == Intention ->
      not (elem id (plannedIntentionIds items))
    _ -> false

plannedIntentionIds :: Array CalendarItem -> Array String
plannedIntentionIds items =
  mapMaybe extractSource items
  where
  extractSource (ServerCalendarItem { content }) | content.itemType == ScheduledBlock = content.sourceItemId
  extractSource _ = Nothing

isConflict :: Array String -> CalendarItem -> Boolean
isConflict conflictIds (ServerCalendarItem { id }) = elem id conflictIds
isConflict _ _ = false

sortItems :: SortMode -> Array String -> Array CalendarItem -> Array CalendarItem
sortItems mode conflictIds items =
  case mode of
    SortByStatus -> sortBy compareStatus items
    SortByCategory -> sortBy compareCategory items
    SortByConflict -> sortBy (compareConflict conflictIds) items
    SortByTime -> sortBy compareTime items
  where
  compareTime a b = compare (calendarItemContent a).windowStart (calendarItemContent b).windowStart

  compareStatus a b = compare (statusRank (calendarItemContent a).status) (statusRank (calendarItemContent b).status)

  compareCategory a b = compare (categoryKey (calendarItemContent a).category) (categoryKey (calendarItemContent b).category)

  compareConflict ids a b = compare (conflictRank ids a) (conflictRank ids b)

  conflictRank ids item = if isConflict ids item then 0 else 1

  statusRank Todo = 0
  statusRank EnCours = 1
  statusRank Fait = 2
  statusRank Annule = 3

  categoryKey Nothing = "~~~"
  categoryKey (Just value) = value

sortModeValue :: SortMode -> String
sortModeValue SortByTime = "time"
sortModeValue SortByStatus = "status"
sortModeValue SortByCategory = "category"
sortModeValue SortByConflict = "conflict"

parseSortMode :: String -> SortMode
parseSortMode raw =
  case raw of
    "status" -> SortByStatus
    "category" -> SortByCategory
    "conflict" -> SortByConflict
    _ -> SortByTime

minuteOfDay :: String -> Maybe Int
minuteOfDay raw = do
  dt <- parseDateTimeLocal raw
  let t = time dt
  pure $ (fromEnum (hour t) * 60) + fromEnum (minute t)
