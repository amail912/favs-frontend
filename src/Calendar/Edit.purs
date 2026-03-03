module Calendar.Edit
  ( EditDraft
  , EditError(..)
  , buildEditDraft
  , applyEditDraft
  ) where

import Prelude

import Calendar.Helpers (parsePositiveInt, toOptionalString, validateIntention)
import Calendar.Model (CalendarItem(..), IntentionDraft, ItemStatus, ItemType, RecurrenceDraft, ValidationError)
import Calendar.RecurrenceEditor (draftFromRecurrence, draftToRecurrence)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Common as StringCommon

type EditDraft =
  { itemId :: String
  , itemType :: ItemType
  , title :: String
  , windowStart :: String
  , windowEnd :: String
  , category :: String
  , status :: ItemStatus
  , actualDurationMinutes :: String
  , recurrence :: RecurrenceDraft
  }

data EditError
  = EditValidation ValidationError
  | EditRecurrence String
  | EditDuration String
  | EditUnsupported

derive instance editErrorEq :: Eq EditError
derive instance editErrorGeneric :: Generic EditError _
instance editErrorShow :: Show EditError where
  show = genericShow

buildEditDraft :: CalendarItem -> Maybe EditDraft
buildEditDraft item =
  case item of
    ServerCalendarItem { id, content } ->
      Just
        { itemId: id
        , itemType: content.itemType
        , title: content.title
        , windowStart: content.windowStart
        , windowEnd: content.windowEnd
        , category: case content.category of
            Nothing -> ""
            Just value -> value
        , status: content.status
        , actualDurationMinutes: case content.actualDurationMinutes of
            Nothing -> ""
            Just minutes -> show minutes
        , recurrence: draftFromRecurrence content.recurrenceRule content.recurrenceExceptionDates
        }
    _ -> Nothing

applyEditDraft :: EditDraft -> CalendarItem -> Either EditError CalendarItem
applyEditDraft draft item = do
  let
    intentionDraft :: IntentionDraft
    intentionDraft =
      { itemType: draft.itemType
      , title: draft.title
      , windowStart: draft.windowStart
      , windowEnd: draft.windowEnd
      , category: draft.category
      , status: draft.status
      , actualDurationMinutes: draft.actualDurationMinutes
      , recurrence: draft.recurrence
      }
  case validateIntention intentionDraft of
    Left err -> Left (EditValidation err)
    Right _ -> do
      recurrence <- case draftToRecurrence draft.recurrence of
        Left err -> Left (EditRecurrence err)
        Right ok -> Right ok
      actualDuration <- parseDuration draft.actualDurationMinutes
      case item of
        ServerCalendarItem payload ->
          Right
            ( ServerCalendarItem
                payload
                  { content =
                      payload.content
                        { title = draft.title
                        , windowStart = draft.windowStart
                        , windowEnd = draft.windowEnd
                        , category = toOptionalString draft.category
                        , status = draft.status
                        , actualDurationMinutes = actualDuration
                        , recurrenceRule = recurrence.rule
                        , recurrenceExceptionDates = recurrence.exceptions
                        }
                  }
            )
        _ -> Left EditUnsupported
  where
  parseDuration raw =
    if StringCommon.trim raw == "" then
      Right Nothing
    else
      case parsePositiveInt raw of
        Just minutes -> Right (Just minutes)
        Nothing -> Left (EditDuration "Durée réelle invalide.")
