module Calendar.Model
  ( CalendarItem(..)
  , CalendarItemContent
  , IntentionDraft
  , ItemStatus(..)
  , ItemType(..)
  , SortMode(..)
  , AgendaView(..)
  , RecurrenceRule(..)
  , StepDependency(..)
  , RoutineTemplate
  , RoutineTemplateStep
  , RoutineInstance
  , RoutineInstanceStep
  , ValidationError(..)
  , NotificationDefaults
  , NotificationOverride
  , ReminderTime
  , TaskTemplate
  , TemplateDraft
  , CsvImportError
  , CsvImportResult
  , IcsImportError
  , IcsImportResult
  , ExportFormat(..)
  , ExportFilter
  , defaultNotificationDefaults
  , emptyTemplateDraft
  ) where

import Prelude

import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)

data ItemType = Intention | ScheduledBlock

derive instance itemTypeGeneric :: Generic ItemType _
derive instance itemTypeEq :: Eq ItemType
instance itemTypeShow :: Show ItemType where
  show = genericShow

data ItemStatus = Todo | EnCours | Fait | Annule

derive instance itemStatusGeneric :: Generic ItemStatus _
derive instance itemStatusEq :: Eq ItemStatus
instance itemStatusShow :: Show ItemStatus where
  show = genericShow

data RecurrenceRule
  = RecurrenceDaily
  | RecurrenceWeekly
  | RecurrenceMonthly
  | RecurrenceYearly
  | RecurrenceEveryXDays Int

derive instance recurrenceRuleGeneric :: Generic RecurrenceRule _
derive instance recurrenceRuleEq :: Eq RecurrenceRule
instance recurrenceRuleShow :: Show RecurrenceRule where
  show = genericShow

data StepDependency
  = StartAfterEnd { stepId :: String, offsetMinutes :: Int }
  | StartBeforeStart { stepId :: String, offsetMinutes :: Int }

derive instance stepDependencyGeneric :: Generic StepDependency _
derive instance stepDependencyEq :: Eq StepDependency
instance stepDependencyShow :: Show StepDependency where
  show = genericShow

type RoutineTemplate =
  { id :: String
  , name :: String
  , steps :: Array RoutineTemplateStep
  }

type RoutineTemplateStep =
  { id :: String
  , title :: String
  , windowStart :: String
  , windowEnd :: String
  , dependsOn :: Maybe StepDependency
  }

type RoutineInstance =
  { templateId :: String
  , steps :: Array RoutineInstanceStep
  }

type RoutineInstanceStep =
  { id :: String
  , title :: String
  , windowStart :: String
  , windowEnd :: String
  , sourceStepId :: String
  }

type CalendarItemContent =
  { itemType :: ItemType
  , title :: String
  , windowStart :: String
  , windowEnd :: String
  , status :: ItemStatus
  , sourceItemId :: Maybe String
  , actualDurationMinutes :: Maybe Int
  , category :: Maybe String
  , recurrenceRule :: Maybe RecurrenceRule
  , recurrenceExceptionDates :: Array String
  }

type NotificationDefaults =
  { startDayTime :: String
  , beforeEndHours :: Int
  }

type NotificationOverride =
  { itemId :: String
  , startDayTime :: Maybe String
  , beforeEndHours :: Maybe Int
  }

type ReminderTime =
  { label :: String
  , at :: String
  }

defaultNotificationDefaults :: NotificationDefaults
defaultNotificationDefaults =
  { startDayTime: "06:00"
  , beforeEndHours: 24
  }

type TaskTemplate =
  { id :: String
  , title :: String
  , durationMinutes :: Int
  , category :: String
  }

type TemplateDraft =
  { title :: String
  , durationMinutes :: String
  , category :: String
  }

emptyTemplateDraft :: TemplateDraft
emptyTemplateDraft =
  { title: ""
  , durationMinutes: ""
  , category: ""
  }

type CsvImportError =
  { rowNumber :: Int
  , message :: String
  }

type CsvImportResult =
  { items :: Array CalendarItem
  , errors :: Array CsvImportError
  }

type IcsImportError =
  { eventIndex :: Int
  , message :: String
  }

type IcsImportResult =
  { items :: Array CalendarItem
  , errors :: Array IcsImportError
  }

data ExportFormat = ExportCSV | ExportICS

derive instance exportFormatGeneric :: Generic ExportFormat _
derive instance exportFormatEq :: Eq ExportFormat
instance exportFormatShow :: Show ExportFormat where
  show = genericShow

type ExportFilter =
  { itemType :: Maybe ItemType
  , status :: Maybe ItemStatus
  , category :: Maybe String
  , startDate :: Maybe String
  , endDate :: Maybe String
  }

data CalendarItem
  = NewCalendarItem { content :: CalendarItemContent }
  | ServerCalendarItem { content :: CalendarItemContent, id :: String }

derive instance calendarItemGeneric :: Generic CalendarItem _
derive instance calendarItemEq :: Eq CalendarItem
instance calendarItemShow :: Show CalendarItem where
  show = genericShow

type IntentionDraft =
  { title :: String
  , windowStart :: String
  , windowEnd :: String
  , category :: String
  }

data ValidationError
  = TitleEmpty
  | WindowStartInvalid
  | WindowEndInvalid
  | WindowOrderInvalid

derive instance validationErrorGeneric :: Generic ValidationError _
derive instance validationErrorEq :: Eq ValidationError
instance validationErrorShow :: Show ValidationError where
  show = genericShow

data SortMode
  = SortByTime
  | SortByStatus
  | SortByCategory
  | SortByConflict

derive instance sortModeGeneric :: Generic SortMode _
derive instance sortModeEq :: Eq SortMode
instance sortModeShow :: Show SortMode where
  show = genericShow

data AgendaView
  = ViewDay
  | ViewWeek
  | ViewMonth

derive instance agendaViewGeneric :: Generic AgendaView _
derive instance agendaViewEq :: Eq AgendaView
instance agendaViewShow :: Show AgendaView where
  show = genericShow

instance itemTypeEncodeJson :: EncodeJson ItemType where
  encodeJson Intention = encodeJson "INTENTION"
  encodeJson ScheduledBlock = encodeJson "BLOC_PLANIFIE"

instance itemTypeDecodeJson :: DecodeJson ItemType where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "INTENTION" -> pure Intention
      "BLOC_PLANIFIE" -> pure ScheduledBlock
      _ -> Left $ UnexpectedValue json

instance itemStatusEncodeJson :: EncodeJson ItemStatus where
  encodeJson Todo = encodeJson "TODO"
  encodeJson EnCours = encodeJson "EN_COURS"
  encodeJson Fait = encodeJson "FAIT"
  encodeJson Annule = encodeJson "ANNULE"

instance itemStatusDecodeJson :: DecodeJson ItemStatus where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "TODO" -> pure Todo
      "EN_COURS" -> pure EnCours
      "FAIT" -> pure Fait
      "ANNULE" -> pure Annule
      _ -> Left $ UnexpectedValue json

instance recurrenceRuleEncodeJson :: EncodeJson RecurrenceRule where
  encodeJson rule =
    case rule of
      RecurrenceDaily ->
        "type" := "DAILY" ~> jsonEmptyObject
      RecurrenceWeekly ->
        "type" := "WEEKLY" ~> jsonEmptyObject
      RecurrenceMonthly ->
        "type" := "MONTHLY" ~> jsonEmptyObject
      RecurrenceYearly ->
        "type" := "YEARLY" ~> jsonEmptyObject
      RecurrenceEveryXDays interval ->
        "type" := "EVERY_X_DAYS"
          ~> "interval_days" := interval
          ~> jsonEmptyObject

instance recurrenceRuleDecodeJson :: DecodeJson RecurrenceRule where
  decodeJson json = do
    obj <- decodeJson json
    kind <- obj .: "type"
    case kind of
      "DAILY" -> pure RecurrenceDaily
      "WEEKLY" -> pure RecurrenceWeekly
      "MONTHLY" -> pure RecurrenceMonthly
      "YEARLY" -> pure RecurrenceYearly
      "EVERY_X_DAYS" -> RecurrenceEveryXDays <$> obj .: "interval_days"
      _ -> Left $ UnexpectedValue json

instance calendarItemDecodeJson :: DecodeJson CalendarItem where
  decodeJson json = do
    obj <- decodeJson json
    itemType <- obj .: "type"
    title <- obj .: "titre"
    windowStart <- obj .: "fenetre_debut"
    windowEnd <- obj .: "fenetre_fin"
    status <- obj .: "statut"
    sourceItemId <- obj .:? "source_item_id"
    actualDurationMinutes <- obj .:? "duree_reelle_minutes"
    category <- obj .:? "categorie"
    recurrenceRule <- obj .:? "recurrence_rule"
    recurrenceExceptionDates <- obj .:? "recurrence_exception_dates"
    let
      content =
        { itemType
        , title
        , windowStart
        , windowEnd
        , status
        , sourceItemId
        , actualDurationMinutes
        , category
        , recurrenceRule
        , recurrenceExceptionDates: fromMaybe [] recurrenceExceptionDates
        }
    either (const $ pure $ NewCalendarItem { content })
      (\id -> pure $ ServerCalendarItem { content, id })
      (obj .: "id")

instance calendarItemEncodeJson :: EncodeJson CalendarItem where
  encodeJson (NewCalendarItem { content }) =
    encodeCalendarContent content
  encodeJson (ServerCalendarItem { content, id }) =
    "id" := id
      ~> encodeCalendarContent content

encodeCalendarContent :: CalendarItemContent -> Json
encodeCalendarContent { itemType, title, windowStart, windowEnd, status, sourceItemId, actualDurationMinutes, category, recurrenceRule, recurrenceExceptionDates } =
  withRecurrence $ withCategory $ withDuration $ withSourceItem $
    "type" := itemType
      ~> "titre" := title
      ~> "fenetre_debut" := windowStart
      ~> "fenetre_fin" := windowEnd
      ~> "statut" := status
      ~> jsonEmptyObject
  where
  withSourceItem base =
    case sourceItemId of
      Just sourceId -> "source_item_id" := sourceId ~> base
      Nothing -> base
  withDuration base =
    case actualDurationMinutes of
      Just minutes -> "duree_reelle_minutes" := minutes ~> base
      Nothing -> base
  withCategory base =
    case category of
      Just value -> "categorie" := value ~> base
      Nothing -> base
  withRecurrence base =
    case recurrenceRule of
      Just rule ->
        "recurrence_rule" := encodeJson rule
          ~> "recurrence_exception_dates" := recurrenceExceptionDates
          ~> base
      Nothing -> base
