module Calendar.Calendar.Draft
  ( toNewIntention
  ) where

import Prelude

import Calendar.Helpers (parsePositiveInt, toOptionalString)
import Calendar.Model (CalendarItem(..), IntentionDraft)
import Calendar.RecurrenceEditor (draftToRecurrence)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Common as StringCommon

toNewIntention :: IntentionDraft -> Either String CalendarItem
toNewIntention draft = do
  recurrence <- case draftToRecurrence draft.recurrence of
    Left err -> Left err
    Right ok -> Right ok
  actualDuration <- parseDuration draft.actualDurationMinutes
  Right
    ( NewCalendarItem
        { content:
            { itemType: draft.itemType
            , title: draft.title
            , windowStart: draft.windowStart
            , windowEnd: draft.windowEnd
            , status: draft.status
            , sourceItemId: Nothing
            , actualDurationMinutes: actualDuration
            , category: toOptionalString draft.category
            , recurrenceRule: recurrence.rule
            , recurrenceExceptionDates: recurrence.exceptions
            }
        }
    )
  where
  parseDuration raw =
    if StringCommon.trim raw == "" then
      Right Nothing
    else
      case parsePositiveInt raw of
        Just minutes -> Right (Just minutes)
        Nothing -> Left "Durée réelle invalide."
