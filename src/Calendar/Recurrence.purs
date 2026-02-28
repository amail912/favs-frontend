module Calendar.Recurrence
  ( generateOccurrencesForMonth
  , nextOccurrence
  ) where

import Prelude

import Calendar.Helpers (addDays, addMonths, formatDate, parseDateTimeLocal)
import Calendar.Model (RecurrenceRule(..))
import Data.Array (elem, filter)
import Data.Date (month, year)
import Data.DateTime (DateTime, date)
import Data.Maybe (Maybe(..))


generateOccurrencesForMonth :: RecurrenceRule -> Array String -> String -> Array String
generateOccurrencesForMonth rule exceptions start =
  case parseDateTimeLocal start of
    Nothing -> []
    Just startDt ->
      let
        targetMonth = month (date startDt)
        targetYear = year (date startDt)
        sameMonth dt = month (date dt) == targetMonth && year (date dt) == targetYear

        collectOccurrences current acc =
          if not (sameMonth current) then acc
          else case nextOccurrence rule current of
            Nothing -> acc <> [ current ]
            Just next -> collectOccurrences next (acc <> [ current ])

        occurrences = collectOccurrences startDt []
      in
        occurrences
          # map formatDate
          # filter (\dateStr -> not (elem dateStr exceptions))

nextOccurrence :: RecurrenceRule -> DateTime -> Maybe DateTime
nextOccurrence rule dt =
  case rule of
    RecurrenceDaily -> addDays 1 dt
    RecurrenceWeekly -> addDays 7 dt
    RecurrenceEveryXDays interval -> addDays interval dt
    RecurrenceMonthly -> Just (addMonths 1 dt)
    RecurrenceYearly -> Just (addMonths 12 dt)
