module Agenda
  ( component
  , CalendarItem(..)
  , CalendarItemContent
  , IntentionDraft
  , ItemStatus(..)
  , ItemType(..)
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
  , SortMode(..)
  , RecurrenceRule(..)
  , StepDependency(..)
  , RoutineTemplate
  , RoutineTemplateStep
  , RoutineInstance
  , RoutineInstanceStep
  , ValidationError(..)
  , applyOfflineMutation
  , durationMinutesBetween
  , detectConflictGroups
  , detectConflictIds
  , generateOccurrencesForMonth
  , instantiateRoutine
  , defaultNotificationDefaults
  , emptyTemplateDraft
  , applyTemplateToDraft
  , addTemplate
  , updateTemplate
  , removeTemplate
  , templateSummary
  , parseCsvImport
  , parseIcsImport
  , filterItemsForExport
  , exportItemsToCsv
  , exportItemsToIcs
  , reminderTimesForIntention
  , sortItems
  , toNewIntention
  , toScheduledBlock
  , validateIntention
  ) where

import Prelude hiding (div)

import Affjax (Error, printError)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (json)
import Affjax.Web (Response, post)
import Affjax.Web (get) as Affjax
import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Monad.RWS (get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (catMaybes, elem, filter, find, findIndex, foldM, index, last, length, mapMaybe, mapWithIndex, nub, null, sortBy, uncons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (all, foldl)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String
import Data.String.Common as StringCommon
import Data.String (toLower)
import Data.String.Pattern (Pattern(..))
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Date (Date, canonicalDate, day, exactDate, month, year)
import Data.DateTime (DateTime(..), adjust, date, diff, time)
import Data.Enum (fromEnum, toEnum)
import Data.Time (Time(..), hour, minute)
import Data.Time.Duration (Days(..), Minutes(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Now (nowDateTime)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval) as H
import Halogen.HTML (HTML, button, div, h2, input, li, option, section, select, text, textarea, ul)
import Halogen.HTML.Events (onClick, onDragEnd, onDragOver, onDragStart, onDrop, onValueChange)
import Halogen.HTML.Properties (IProp, draggable, placeholder, type_, value)
import Utils (class_)
import Web.Event.Event (preventDefault)
import Web.HTML.Event.DragEvent (DragEvent, toEvent)

type NoOutput = Void
type AgendaAppM = H.HalogenM State Action () NoOutput Aff
type ErrorAgendaAppM = ExceptT FatalError AgendaAppM

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

emptyDraft :: IntentionDraft
emptyDraft =
  { title: ""
  , windowStart: ""
  , windowEnd: ""
  , category: ""
  }

toNewIntention :: IntentionDraft -> CalendarItem
toNewIntention { title, windowStart, windowEnd, category } =
  NewCalendarItem
    { content:
        { itemType: Intention
        , title
        , windowStart
        , windowEnd
        , status: Todo
        , sourceItemId: Nothing
        , actualDurationMinutes: Nothing
        , category: toOptional category
        , recurrenceRule: Nothing
        , recurrenceExceptionDates: []
        }
    }
  where
  toOptional raw =
    let trimmed = StringCommon.trim raw
    in if trimmed == "" then Nothing else Just trimmed

toScheduledBlock :: String -> CalendarItemContent -> CalendarItem
toScheduledBlock sourceId content =
  NewCalendarItem
    { content:
        content
          { itemType = ScheduledBlock
          , status = Todo
          , sourceItemId = Just sourceId
          }
    }

validateIntention :: IntentionDraft -> Either ValidationError IntentionDraft
validateIntention draft =
  case unit of
    _ | StringCommon.trim draft.title == "" -> Left TitleEmpty
    _ | not (isDateTimeLocal draft.windowStart) -> Left WindowStartInvalid
    _ | not (isDateTimeLocal draft.windowEnd) -> Left WindowEndInvalid
    _ | draft.windowEnd <= draft.windowStart -> Left WindowOrderInvalid
    _ -> Right draft

isDateTimeLocal :: String -> Boolean
isDateTimeLocal raw =
  String.length raw == 16
    && matchesAt 4 '-'
    && matchesAt 7 '-'
    && matchesAt 10 'T'
    && matchesAt 13 ':'
    && allDigitsAt [ 0, 1, 2, 3, 5, 6, 8, 9, 11, 12, 14, 15 ]
  where
  matchesAt :: Int -> Char -> Boolean
  matchesAt idx expected =
    case String.charAt idx raw of
      Just ch -> ch == expected
      Nothing -> false

  allDigitsAt :: Array Int -> Boolean
  allDigitsAt = all (\idx -> maybe false isDigitChar (String.charAt idx raw))

  isDigitChar :: Char -> Boolean
  isDigitChar ch = ch >= '0' && ch <= '9'

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

type State =
  { items :: Array CalendarItem
  , draft :: IntentionDraft
  , validationError :: Maybe ValidationError
  , showConflictsOnly :: Boolean
  , conflictResolution :: Maybe ConflictResolution
  , offlineMode :: Boolean
  , pendingSync :: Array CalendarItem
  , syncConflict :: Maybe (Array CalendarItem)
  , validationPanel :: Maybe ValidationPanel
  , sortMode :: SortMode
  , draggingId :: Maybe String
  , notificationDefaults :: NotificationDefaults
  , notificationOverrides :: Array NotificationOverride
  , notificationPanelOpen :: Boolean
  , notificationEditor :: Maybe NotificationEditor
  , templates :: Array TaskTemplate
  , templateDraft :: TemplateDraft
  , editingTemplateId :: Maybe String
  , csvInput :: String
  , csvImportResult :: Maybe CsvImportResult
  , icsInput :: String
  , icsImportResult :: Maybe IcsImportResult
  , exportFormat :: ExportFormat
  , exportTypeFilter :: String
  , exportStatusFilter :: String
  , exportCategoryFilter :: String
  , exportStartDate :: String
  , exportEndDate :: String
  , exportOutput :: String
  , viewMode :: AgendaView
  , focusDate :: String
  }

data Action
  = Initialize
  | DraftTitleChanged String
  | DraftStartChanged String
  | DraftEndChanged String
  | DraftCategoryChanged String
  | SubmitIntention
  | PlanifyFrom String CalendarItemContent
  | ToggleConflictFilter
  | ToggleOffline
  | SortChanged String
  | DragStart String
  | DragOver String DragEvent
  | DropOn String
  | DragEnd
  | OpenValidation String CalendarItemContent
  | ValidationMinutesChanged String
  | ConfirmValidation
  | CancelValidation
  | OpenConflictResolution (Array String)
  | ChooseResolutionStrategy ResolutionStrategy
  | ConfirmResolution
  | CancelResolution
  | ResolveSyncKeepLocal
  | ResolveSyncDiscardLocal
  | ToggleNotificationPanel
  | DefaultStartTimeChanged String
  | DefaultBeforeEndChanged String
  | OpenNotificationEditor String
  | NotificationStartTimeChanged String
  | NotificationBeforeEndChanged String
  | SaveNotificationOverride
  | ResetNotificationOverride String
  | CancelNotificationOverride
  | TemplateTitleChanged String
  | TemplateDurationChanged String
  | TemplateCategoryChanged String
  | SubmitTemplate
  | EditTemplate String
  | CancelTemplateEdit
  | DeleteTemplate String
  | UseTemplate String
  | CsvInputChanged String
  | ParseCsvInput
  | ApplyCsvImport
  | ClearCsvImport
  | IcsInputChanged String
  | ParseIcsInput
  | ApplyIcsImport
  | ClearIcsImport
  | ExportFormatChanged String
  | ExportTypeFilterChanged String
  | ExportStatusFilterChanged String
  | ExportCategoryFilterChanged String
  | ExportStartDateChanged String
  | ExportEndDateChanged String
  | GenerateExport
  | ClearExportOutput
  | ViewChanged String
  | FocusDateChanged String

component :: forall q i. H.Component q i NoOutput Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = pure Initialize
                                     }
    }

initialState :: forall i. i -> State
initialState = const
  { items: []
  , draft: emptyDraft
  , validationError: Nothing
  , showConflictsOnly: false
  , conflictResolution: Nothing
  , offlineMode: false
  , pendingSync: []
  , syncConflict: Nothing
  , validationPanel: Nothing
  , sortMode: SortByTime
  , draggingId: Nothing
  , notificationDefaults: defaultNotificationDefaults
  , notificationOverrides: []
  , notificationPanelOpen: false
  , notificationEditor: Nothing
  , templates: []
  , templateDraft: emptyTemplateDraft
  , editingTemplateId: Nothing
  , csvInput: ""
  , csvImportResult: Nothing
  , icsInput: ""
  , icsImportResult: Nothing
  , exportFormat: ExportCSV
  , exportTypeFilter: ""
  , exportStatusFilter: ""
  , exportCategoryFilter: ""
  , exportStartDate: ""
  , exportEndDate: ""
  , exportOutput: ""
  , viewMode: ViewDay
  , focusDate: ""
  }

handleAction :: Action -> AgendaAppM Unit
handleAction action = handleError $
  case action of
    Initialize -> do
      now <- liftEffect nowDateTime
      lift $ modify_ _ { focusDate = formatDate now }
      refreshItems
    DraftTitleChanged title ->
      lift $ modify_ \st -> st { draft = st.draft { title = title }, validationError = Nothing }
    DraftStartChanged windowStart ->
      lift $ modify_ \st -> st { draft = st.draft { windowStart = windowStart }, validationError = Nothing }
    DraftEndChanged windowEnd ->
      lift $ modify_ \st -> st { draft = st.draft { windowEnd = windowEnd }, validationError = Nothing }
    DraftCategoryChanged category ->
      lift $ modify_ \st -> st { draft = st.draft { category = category }, validationError = Nothing }
    SubmitIntention -> do
      st <- get
      case validateIntention st.draft of
        Left err -> lift $ modify_ _ { validationError = Just err }
        Right validDraft -> do
          let item = toNewIntention validDraft
          if st.offlineMode then do
            let
              result = applyOfflineMutation true item st.items st.pendingSync
            lift $ modify_ _ { items = result.items
                            , pendingSync = result.pending
                            , draft = emptyDraft
                            , validationError = Nothing
                            }
          else do
            _ <- createItem item
            lift $ modify_ _ { draft = emptyDraft, validationError = Nothing }
            refreshItems
    PlanifyFrom sourceId content -> do
      st <- get
      let item = toScheduledBlock sourceId content
      if st.offlineMode then do
        let
          result = applyOfflineMutation true item st.items st.pendingSync
        lift $ modify_ _ { items = result.items, pendingSync = result.pending }
      else do
        _ <- createItem item
        refreshItems
    ToggleConflictFilter ->
      lift $ modify_ \st -> st { showConflictsOnly = not st.showConflictsOnly }
    ToggleOffline -> do
      st <- get
      if st.offlineMode then do
        lift $ modify_ _ { offlineMode = false }
        syncPending
      else lift $ modify_ _ { offlineMode = true }
    SortChanged raw ->
      lift $ modify_ \st -> st { sortMode = parseSortMode raw }
    DragStart itemId ->
      lift $ modify_ _ { draggingId = Just itemId }
    DragOver _ ev ->
      liftEffect $ preventDefault (toEvent ev)
    DropOn targetId -> do
      st <- get
      case st.draggingId of
        Nothing -> pure unit
        Just draggingId -> do
          let reordered = moveItemBefore draggingId targetId st.items
          lift $ modify_ _ { items = reordered, draggingId = Nothing }
    DragEnd ->
      lift $ modify_ _ { draggingId = Nothing }
    OpenValidation itemId content -> do
      suggested <- liftEffect $ suggestDurationMinutes content.windowStart
      lift $ modify_ _ { validationPanel = Just { itemId, proposedMinutes: suggested, inputValue: "" } }
    ValidationMinutesChanged raw ->
      lift $ modify_ \st -> st { validationPanel = st.validationPanel <#> \panel -> panel { inputValue = raw } }
    ConfirmValidation -> do
      st <- get
      case st.validationPanel of
        Nothing -> pure unit
        Just panel -> do
          let duration = parsePositiveInt panel.inputValue <|> panel.proposedMinutes
          case duration of
            Nothing -> pure unit
            Just minutes -> do
              _ <- validateItem panel.itemId minutes
              lift $ modify_ _ { validationPanel = Nothing }
              refreshItems
    CancelValidation ->
      lift $ modify_ _ { validationPanel = Nothing }
    OpenConflictResolution groupIds ->
      lift $ modify_ \st -> st { conflictResolution = Just { groupIds, pendingStrategy: Nothing } }
    ChooseResolutionStrategy strategy ->
      lift $ modify_ \st -> st { conflictResolution = st.conflictResolution <#> \res -> res { pendingStrategy = Just strategy } }
    ConfirmResolution ->
      lift $ modify_ \st -> st { conflictResolution = Nothing }
    CancelResolution ->
      lift $ modify_ \st -> st { conflictResolution = Nothing }
    ResolveSyncKeepLocal ->
      lift $ modify_ \st -> st { syncConflict = Nothing, offlineMode = true }
    ResolveSyncDiscardLocal -> do
      lift $ modify_ \st -> st { syncConflict = Nothing, pendingSync = [] }
      refreshItems
    ToggleNotificationPanel ->
      lift $ modify_ \st -> st { notificationPanelOpen = not st.notificationPanelOpen }
    DefaultStartTimeChanged raw ->
      lift $ modify_ \st ->
        if isTimeLocal raw then st { notificationDefaults = st.notificationDefaults { startDayTime = raw } } else st
    DefaultBeforeEndChanged raw ->
      lift $ modify_ \st ->
        case parsePositiveInt raw of
          Just hours -> st { notificationDefaults = st.notificationDefaults { beforeEndHours = hours } }
          Nothing -> st
    OpenNotificationEditor itemId -> do
      st <- get
      let
        existing = lookupNotificationOverride itemId st.notificationOverrides
        startTime = fromMaybe st.notificationDefaults.startDayTime (existing >>= _.startDayTime)
        beforeEnd = fromMaybe st.notificationDefaults.beforeEndHours (existing >>= _.beforeEndHours)
      lift $ modify_ _ { notificationEditor = Just { itemId, startTime, beforeEndRaw: show beforeEnd } }
    NotificationStartTimeChanged raw ->
      lift $ modify_ \st ->
        st { notificationEditor = st.notificationEditor <#> \editor -> editor { startTime = raw } }
    NotificationBeforeEndChanged raw ->
      lift $ modify_ \st ->
        st { notificationEditor = st.notificationEditor <#> \editor -> editor { beforeEndRaw = raw } }
    SaveNotificationOverride -> do
      st <- get
      case st.notificationEditor of
        Nothing -> pure unit
        Just editor -> do
          let
            cleanedTime = if isTimeLocal editor.startTime then Just editor.startTime else Nothing
            cleanedHours = parsePositiveInt editor.beforeEndRaw
          lift $ modify_ _ { notificationOverrides = upsertNotificationOverride editor.itemId cleanedTime cleanedHours st.notificationOverrides
                          , notificationEditor = Nothing
                          }
    ResetNotificationOverride itemId ->
      lift $ modify_ \st -> st { notificationOverrides = removeNotificationOverride itemId st.notificationOverrides
                              , notificationEditor = Nothing
                              }
    CancelNotificationOverride ->
      lift $ modify_ _ { notificationEditor = Nothing }
    TemplateTitleChanged title ->
      lift $ modify_ \st -> st { templateDraft = st.templateDraft { title = title } }
    TemplateDurationChanged duration ->
      lift $ modify_ \st -> st { templateDraft = st.templateDraft { durationMinutes = duration } }
    TemplateCategoryChanged category ->
      lift $ modify_ \st -> st { templateDraft = st.templateDraft { category = category } }
    SubmitTemplate -> do
      st <- get
      let
        draft = st.templateDraft
        duration = parsePositiveInt draft.durationMinutes
      case duration of
        Nothing -> pure unit
        Just minutes -> do
          let
            template =
              { id: fromMaybe "" st.editingTemplateId
              , title: StringCommon.trim draft.title
              , durationMinutes: minutes
              , category: draft.category
              }
          if template.title == "" then pure unit
          else do
            let
              nextTemplates =
                case st.editingTemplateId of
                  Nothing -> addTemplate template st.templates
                  Just _ -> updateTemplate template st.templates
            lift $ modify_ _ { templates = nextTemplates
                            , templateDraft = emptyTemplateDraft
                            , editingTemplateId = Nothing
                            }
    EditTemplate templateId -> do
      st <- get
      case find (\tpl -> tpl.id == templateId) st.templates of
        Nothing -> pure unit
        Just template ->
          lift $ modify_ _ { templateDraft =
                              { title: template.title
                              , durationMinutes: show template.durationMinutes
                              , category: template.category
                              }
                          , editingTemplateId = Just template.id
                          }
    CancelTemplateEdit ->
      lift $ modify_ _ { templateDraft = emptyTemplateDraft, editingTemplateId = Nothing }
    DeleteTemplate templateId ->
      lift $ modify_ \st -> st { templates = removeTemplate templateId st.templates
                              , templateDraft = emptyTemplateDraft
                              , editingTemplateId = Nothing
                              }
    UseTemplate templateId -> do
      st <- get
      case find (\tpl -> tpl.id == templateId) st.templates of
        Nothing -> pure unit
        Just template -> do
          now <- liftEffect nowDateTime
          let startStr = formatDateTimeLocal now
          let endStr = fromMaybe startStr (shiftMinutes template.durationMinutes startStr)
          lift $ modify_ \st' -> st' { draft = applyTemplateToDraft template startStr endStr }
    CsvInputChanged raw ->
      lift $ modify_ \st -> st { csvInput = raw }
    ParseCsvInput -> do
      st <- get
      let result = parseCsvImport st.csvInput
      lift $ modify_ _ { csvImportResult = Just result }
    ApplyCsvImport -> do
      st <- get
      case st.csvImportResult of
        Nothing -> pure unit
        Just result ->
          if null result.items then pure unit
          else if st.offlineMode then do
            let
              initial = { items: st.items, pending: st.pendingSync }
              final = foldl (\acc item -> applyOfflineMutation true item acc.items acc.pending) initial result.items
            lift $ modify_ _ { items = final.items
                            , pendingSync = final.pending
                            , csvInput = ""
                            , csvImportResult = Nothing
                            }
          else
            lift $ modify_ _ { items = st.items <> result.items
                            , csvInput = ""
                            , csvImportResult = Nothing
                            }
    ClearCsvImport ->
      lift $ modify_ _ { csvInput = "", csvImportResult = Nothing }
    IcsInputChanged raw ->
      lift $ modify_ \st -> st { icsInput = raw }
    ParseIcsInput -> do
      st <- get
      let result = parseIcsImport st.icsInput
      lift $ modify_ _ { icsImportResult = Just result }
    ApplyIcsImport -> do
      st <- get
      case st.icsImportResult of
        Nothing -> pure unit
        Just result ->
          if null result.items then pure unit
          else if st.offlineMode then do
            let
              initial = { items: st.items, pending: st.pendingSync }
              final = foldl (\acc item -> applyOfflineMutation true item acc.items acc.pending) initial result.items
            lift $ modify_ _ { items = final.items
                            , pendingSync = final.pending
                            , icsInput = ""
                            , icsImportResult = Nothing
                            }
          else
            lift $ modify_ _ { items = st.items <> result.items
                            , icsInput = ""
                            , icsImportResult = Nothing
                            }
    ClearIcsImport ->
      lift $ modify_ _ { icsInput = "", icsImportResult = Nothing }
    ExportFormatChanged raw ->
      lift $ modify_ \st -> st { exportFormat = parseExportFormat raw }
    ExportTypeFilterChanged raw ->
      lift $ modify_ \st -> st { exportTypeFilter = raw }
    ExportStatusFilterChanged raw ->
      lift $ modify_ \st -> st { exportStatusFilter = raw }
    ExportCategoryFilterChanged raw ->
      lift $ modify_ \st -> st { exportCategoryFilter = raw }
    ExportStartDateChanged raw ->
      lift $ modify_ \st -> st { exportStartDate = raw }
    ExportEndDateChanged raw ->
      lift $ modify_ \st -> st { exportEndDate = raw }
    GenerateExport -> do
      st <- get
      let
        filter =
          { itemType: parseExportItemType st.exportTypeFilter
          , status: parseExportStatus st.exportStatusFilter
          , category: toOptionalString st.exportCategoryFilter
          , startDate: toOptionalString st.exportStartDate
          , endDate: toOptionalString st.exportEndDate
          }
        filtered = filterItemsForExport filter st.items
        output =
          case st.exportFormat of
            ExportCSV -> exportItemsToCsv filtered
            ExportICS -> exportItemsToIcs filtered
      lift $ modify_ _ { exportOutput = output }
    ClearExportOutput ->
      lift $ modify_ _ { exportOutput = "" }
    ViewChanged raw ->
      lift $ modify_ \st -> st { viewMode = parseAgendaView raw }
    FocusDateChanged raw ->
      lift $ modify_ \st -> st { focusDate = raw }

handleError :: ErrorAgendaAppM Unit -> AgendaAppM Unit
handleError m = do
  res <- runExceptT m
  either logShow pure res

refreshItems :: ErrorAgendaAppM Unit
refreshItems = do
  jsonResponse <- withExceptT toFatalError $ ExceptT $ liftAff $ Affjax.get json "/api/v1/calendar-items"
  items <- (_.body >>> decodeJson >>> lmap toFatalError >>> pure >>> ExceptT) jsonResponse
  lift $ modify_ \st -> st { items = items }

createItem :: CalendarItem -> ErrorAgendaAppM (Response Json)
createItem item = withExceptT toFatalError $ ExceptT $ liftAff $ post json "/api/v1/calendar-items" (Just $ Json $ encodeJson item)

validateItem :: String -> Int -> ErrorAgendaAppM (Response Json)
validateItem itemId minutes =
  withExceptT toFatalError $ ExceptT $ liftAff $
    post json ("/api/v1/calendar-items/" <> itemId <> "/validate")
      (Just $ Json $ "duree_reelle_minutes" := minutes ~> jsonEmptyObject)

statusOk :: forall a. Response a -> Boolean
statusOk r = unwrap r.status >= 200 && unwrap r.status < 300

syncPending :: ErrorAgendaAppM Unit
syncPending = do
  st <- get
  if null st.pendingSync then refreshItems
  else do
    ok <- foldM
      (\acc item ->
        if not acc then pure false
        else do
          resp <- createItem item
          pure (statusOk resp)
      )
      true
      st.pendingSync
    if ok then do
      lift $ modify_ _ { pendingSync = [], syncConflict = Nothing }
      refreshItems
    else lift $ modify_ _ { syncConflict = Just st.pendingSync }

render :: forall m. State -> H.ComponentHTML Action () m
render { items, draft, validationError, showConflictsOnly, conflictResolution, offlineMode, syncConflict, validationPanel, sortMode, notificationDefaults, notificationOverrides, notificationPanelOpen, notificationEditor, templates, templateDraft, editingTemplateId, csvInput, csvImportResult, icsInput, icsImportResult, exportFormat, exportTypeFilter, exportStatusFilter, exportCategoryFilter, exportStartDate, exportEndDate, exportOutput, viewMode, focusDate } =
  let
    conflictIds = detectConflictIds items
    conflictGroups = detectConflictGroups items
    itemsToShow =
      if showConflictsOnly
        then filter (isConflict conflictIds) items
        else items
    sortedItems = sortItems sortMode conflictIds itemsToShow
    unplannedIntentions = filter (isUnplannedIntention items) items
  in
  div [ class_ "entity-page agenda-page" ]
    [ section [ class_ "agenda-header" ]
        [ h2 [ class_ "agenda-title" ] [ text (viewTitle viewMode) ]
        , div [ class_ "agenda-subtitle" ] [ text "Capture rapide des intentions a planifier." ]
        , button
            [ class_ $ "btn btn-sm agenda-filter" <> if showConflictsOnly then " btn-outline-primary" else " btn-outline-secondary"
            , onClick (const ToggleConflictFilter)
            ]
            [ text "Filtrer: en conflit" ]
        , renderOfflineToggle offlineMode
        , renderSortPicker sortMode
        , renderConflictActions conflictGroups
        , renderViewSelector viewMode focusDate
        ]
    , renderForm draft validationError
    , renderNotificationsPanel notificationPanelOpen notificationDefaults notificationOverrides notificationEditor unplannedIntentions
    , renderTemplatesPanel templates templateDraft editingTemplateId
    , renderCsvImportPanel csvInput csvImportResult
    , renderIcsImportPanel icsInput icsImportResult
    , renderExportPanel exportFormat exportTypeFilter exportStatusFilter exportCategoryFilter exportStartDate exportEndDate exportOutput
    , renderAgendaView viewMode focusDate conflictIds sortedItems
    , maybe (text "") (renderConflictResolution items) conflictResolution
    , maybe (text "") renderSyncConflict syncConflict
    , maybe (text "") renderValidationPanel validationPanel
    ]

renderForm :: forall w. IntentionDraft -> Maybe ValidationError -> HTML w Action
renderForm draft validationError =
  section [ class_ "agenda-form" ]
    [ input
        [ class_ "form-control agenda-input"
        , placeholder "Titre de l'intention"
        , onValueChange DraftTitleChanged
        , value draft.title
        ]
    , div [ class_ "agenda-time-row" ]
        [ input
            [ class_ "form-control agenda-input"
            , type_ InputDatetimeLocal
            , placeholder "Debut"
            , onValueChange DraftStartChanged
            , value draft.windowStart
            ]
        , input
            [ class_ "form-control agenda-input"
            , type_ InputDatetimeLocal
            , placeholder "Fin"
            , onValueChange DraftEndChanged
            , value draft.windowEnd
            ]
        ]
    , input
        [ class_ "form-control agenda-input"
        , placeholder "Categorie (optionnelle)"
        , onValueChange DraftCategoryChanged
        , value draft.category
        ]
    , maybe (text "") renderValidationError validationError
    , button [ class_ "btn btn-primary agenda-submit", onClick (const SubmitIntention) ] [ text "Creer l'intention" ]
    ]

renderValidationError :: forall w. ValidationError -> HTML w Action
renderValidationError err =
  div [ class_ "agenda-error" ]
    [ text $ case err of
        TitleEmpty -> "Le titre est obligatoire."
        WindowStartInvalid -> "La date de debut est invalide."
        WindowEndInvalid -> "La date de fin est invalide."
        WindowOrderInvalid -> "La fin doit etre apres le debut."
    ]

emptyAgenda :: forall w i. HTML w i
emptyAgenda =
  div [ class_ "row entity-empty agenda-empty" ]
    [ div [ class_ "entity-empty-title" ] [ text "Aucune intention aujourd'hui" ]
    , div [ class_ "entity-empty-subtitle" ] [ text "Ajoutez une intention pour demarrer votre journee." ]
    ]

renderAgendaView :: forall w. AgendaView -> String -> Array String -> Array CalendarItem -> HTML w Action
renderAgendaView viewMode focusDate conflictIds items =
  case viewMode of
    ViewDay ->
      if null items then emptyAgenda else agendaList conflictIds items
    ViewWeek ->
      renderRangeView "Semaine" (generateDateRange focusDate 7) conflictIds items
    ViewMonth ->
      renderRangeView "Mois" (generateMonthDates focusDate) conflictIds items

renderRangeView :: forall w. String -> Array String -> Array String -> Array CalendarItem -> HTML w Action
renderRangeView label dates conflictIds items =
  if null dates then emptyAgenda
  else
    div [ class_ "agenda-range" ]
      (map (renderDateSection label conflictIds items) dates)

renderDateSection :: forall w. String -> Array String -> Array CalendarItem -> String -> HTML w Action
renderDateSection _ conflictIds items dateStr =
  let
    itemsForDate = filter (isItemOnDate dateStr) items
    sorted = sortItems SortByTime conflictIds itemsForDate
  in
    section [ class_ "agenda-date-section" ]
      [ div [ class_ "agenda-date-title" ] [ text dateStr ]
      , if null sorted
          then div [ class_ "agenda-date-empty" ] [ text "Aucun item" ]
          else agendaList conflictIds sorted
      ]

agendaList :: forall w. Array String -> Array CalendarItem -> HTML w Action
agendaList conflictIds items =
  ul [ class_ "list-group entity-list agenda-list" ] (mapWithIndex (renderItem conflictIds) items)

renderItem :: forall w. Array String -> Int -> CalendarItem -> HTML w Action
renderItem conflictIds _ item =
  let
    content = calendarItemContent item
    conflictClass = if isConflict conflictIds item then " agenda-card--conflict" else ""
    dragProps = dragHandlers item
  in
    li ([ class_ $ "row list-group-item entity-card agenda-card" <> conflictClass ] <> dragProps)
      [ div [ class_ "col entity-card-body" ]
          [ div [ class_ "agenda-card-time" ] [ text (timeLabel content.windowStart) ]
          , div [ class_ "agenda-card-title" ] [ text content.title ]
          , div [ class_ "agenda-card-window" ]
              [ text $ content.windowStart <> " → " <> content.windowEnd ]
          , renderCategory content.category
          , renderValidationAction item content
          , renderPlanifyAction item content
          ]
      ]

renderPlanifyAction :: forall w. CalendarItem -> CalendarItemContent -> HTML w Action
renderPlanifyAction (ServerCalendarItem { id, content }) _ | content.itemType == Intention =
  button [ class_ "btn btn-sm btn-outline-primary agenda-planify", onClick (const $ PlanifyFrom id content) ]
    [ text "Planifier" ]
renderPlanifyAction _ _ = text ""

renderValidationAction :: forall w. CalendarItem -> CalendarItemContent -> HTML w Action
renderValidationAction (ServerCalendarItem { id, content }) _ | content.status /= Fait =
  button [ class_ "btn btn-sm btn-outline-success agenda-validate", onClick (const $ OpenValidation id content) ]
    [ text "Valider" ]
renderValidationAction _ _ = text ""

renderCategory :: forall w. Maybe String -> HTML w Action
renderCategory category =
  case category of
    Nothing -> text ""
    Just value -> div [ class_ "agenda-card-category" ] [ text value ]

type NotificationEditor =
  { itemId :: String
  , startTime :: String
  , beforeEndRaw :: String
  }

renderNotificationsPanel :: forall w. Boolean -> NotificationDefaults -> Array NotificationOverride -> Maybe NotificationEditor -> Array CalendarItem -> HTML w Action
renderNotificationsPanel isOpen defaults overrides editor intentions =
  if null intentions then text ""
  else
    section [ class_ "agenda-notifications" ]
      [ div [ class_ "agenda-notifications-header" ]
          [ div []
              [ div [ class_ "agenda-notifications-title" ] [ text "Rappels des intentions non planifiees" ]
              , div [ class_ "agenda-notifications-subtitle" ] [ text "Les rappels par defaut s'appliquent aux intentions non planifiees." ]
              ]
          , button
              [ class_ $ "btn btn-sm agenda-notifications-toggle" <> if isOpen then " btn-outline-primary" else " btn-outline-secondary"
              , onClick (const ToggleNotificationPanel)
              ]
              [ text $ if isOpen then "Masquer" else "Configurer" ]
          ]
      , if isOpen then renderNotificationDefaults defaults else text ""
      , if isOpen then renderNotificationList defaults overrides editor intentions else text ""
      ]

renderNotificationDefaults :: forall w. NotificationDefaults -> HTML w Action
renderNotificationDefaults defaults =
  div [ class_ "agenda-notifications-defaults" ]
    [ div [ class_ "agenda-notifications-section-title" ] [ text "Rappels par defaut" ]
    , div [ class_ "agenda-notifications-controls" ]
        [ div [ class_ "agenda-notifications-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Jour de debut" ]
            , input
                [ class_ "form-control agenda-input"
                , type_ InputTime
                , value defaults.startDayTime
                , onValueChange DefaultStartTimeChanged
                ]
            ]
        , div [ class_ "agenda-notifications-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Avant fin (heures)" ]
            , input
                [ class_ "form-control agenda-input"
                , type_ InputNumber
                , value (show defaults.beforeEndHours)
                , onValueChange DefaultBeforeEndChanged
                ]
            ]
        ]
    ]

renderNotificationList :: forall w. NotificationDefaults -> Array NotificationOverride -> Maybe NotificationEditor -> Array CalendarItem -> HTML w Action
renderNotificationList defaults overrides editor intentions =
  div [ class_ "agenda-notifications-list" ]
    (map (renderNotificationItem defaults overrides editor) intentions)

renderNotificationItem :: forall w. NotificationDefaults -> Array NotificationOverride -> Maybe NotificationEditor -> CalendarItem -> HTML w Action
renderNotificationItem defaults overrides editor item =
  case item of
    ServerCalendarItem { id, content } | content.itemType == Intention ->
      let
        override = lookupNotificationOverride id overrides
        reminders = reminderTimesForIntention defaults override content
        editorForItem = editor >>= \current -> if current.itemId == id then Just current else Nothing
        hasOverride = case override of
          Nothing -> false
          Just _ -> true
      in
        div [ class_ "agenda-notification-item" ]
          [ div [ class_ "agenda-notification-header" ]
              [ div []
                  [ div [ class_ "agenda-notification-title" ] [ text content.title ]
                  , div [ class_ "agenda-notification-window" ] [ text $ content.windowStart <> " → " <> content.windowEnd ]
                  ]
              , div [ class_ "agenda-notification-actions" ]
                  [ div [ class_ $ "agenda-notification-badge" <> if hasOverride then " agenda-notification-badge--custom" else "" ]
                      [ text $ if hasOverride then "Personnalise" else "Par defaut" ]
                  , button
                      [ class_ "btn btn-sm btn-outline-secondary"
                      , onClick (const $ OpenNotificationEditor id)
                      ]
                      [ text "Personnaliser" ]
                  ]
              ]
          , renderReminderTimes reminders
          , maybe (text "") (renderNotificationEditor id) editorForItem
          ]
    _ -> text ""

renderReminderTimes :: forall w. Array ReminderTime -> HTML w Action
renderReminderTimes reminders =
  div [ class_ "agenda-notification-times" ]
    (map (\reminder -> div [ class_ "agenda-notification-time" ] [ text $ reminder.label <> ": " <> reminder.at ]) reminders)

renderNotificationEditor :: forall w. String -> NotificationEditor -> HTML w Action
renderNotificationEditor itemId editor =
  div [ class_ "agenda-notification-editor" ]
    [ div [ class_ "agenda-notifications-section-title" ] [ text "Surcharge de rappel" ]
    , div [ class_ "agenda-notifications-controls" ]
        [ div [ class_ "agenda-notifications-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Jour de debut" ]
            , input
                [ class_ "form-control agenda-input"
                , type_ InputTime
                , value editor.startTime
                , onValueChange NotificationStartTimeChanged
                ]
            ]
        , div [ class_ "agenda-notifications-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Avant fin (heures)" ]
            , input
                [ class_ "form-control agenda-input"
                , type_ InputNumber
                , value editor.beforeEndRaw
                , onValueChange NotificationBeforeEndChanged
                ]
            ]
        ]
    , div [ class_ "agenda-notification-editor-actions" ]
        [ button [ class_ "btn btn-sm btn-success", onClick (const SaveNotificationOverride) ] [ text "Enregistrer" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const CancelNotificationOverride) ] [ text "Annuler" ]
        , button [ class_ "btn btn-sm btn-outline-danger", onClick (const $ ResetNotificationOverride itemId) ] [ text "Reinitialiser" ]
        ]
    ]

renderTemplatesPanel :: forall w. Array TaskTemplate -> TemplateDraft -> Maybe String -> HTML w Action
renderTemplatesPanel templates draft editingId =
  section [ class_ "agenda-templates" ]
    [ div [ class_ "agenda-templates-header" ]
        [ div [ class_ "agenda-templates-title" ] [ text "Templates de taches" ]
        , div [ class_ "agenda-templates-subtitle" ] [ text "Creez des templates reutilisables pour accelerer la saisie." ]
        ]
    , div [ class_ "agenda-templates-form" ]
        [ input
            [ class_ "form-control agenda-input"
            , placeholder "Titre du template"
            , value draft.title
            , onValueChange TemplateTitleChanged
            ]
        , div [ class_ "agenda-templates-row" ]
            [ input
                [ class_ "form-control agenda-input"
                , type_ InputNumber
                , placeholder "Duree (minutes)"
                , value draft.durationMinutes
                , onValueChange TemplateDurationChanged
                ]
            , input
                [ class_ "form-control agenda-input"
                , placeholder "Categorie (optionnelle)"
                , value draft.category
                , onValueChange TemplateCategoryChanged
                ]
            ]
        , div [ class_ "agenda-templates-actions" ]
            ([ button
                 [ class_ "btn btn-sm btn-primary"
                 , onClick (const SubmitTemplate)
                 ]
                 [ text $ if editingId == Nothing then "Ajouter" else "Mettre a jour" ]
             ] <> if editingId == Nothing then [] else
               [ button
                   [ class_ "btn btn-sm btn-outline-secondary"
                   , onClick (const CancelTemplateEdit)
                   ]
                   [ text "Annuler" ]
               ])
        ]
    , renderTemplatesList templates
    ]

renderTemplatesList :: forall w. Array TaskTemplate -> HTML w Action
renderTemplatesList templates =
  if null templates then
    div [ class_ "agenda-templates-empty" ] [ text "Aucun template pour l'instant." ]
  else
    div [ class_ "agenda-templates-list" ] (map renderTemplateCard templates)

renderTemplateCard :: forall w. TaskTemplate -> HTML w Action
renderTemplateCard template =
  div [ class_ "agenda-template-card" ]
    [ div [ class_ "agenda-template-main" ]
        [ div [ class_ "agenda-template-title" ] [ text template.title ]
        , div [ class_ "agenda-template-summary" ] [ text (templateSummary template) ]
        ]
    , div [ class_ "agenda-template-actions" ]
        [ button [ class_ "btn btn-sm btn-outline-primary", onClick (const $ UseTemplate template.id) ] [ text "Utiliser" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const $ EditTemplate template.id) ] [ text "Editer" ]
        , button [ class_ "btn btn-sm btn-outline-danger", onClick (const $ DeleteTemplate template.id) ] [ text "Supprimer" ]
        ]
    ]

renderCsvImportPanel :: forall w. String -> Maybe CsvImportResult -> HTML w Action
renderCsvImportPanel csvInput result =
  section [ class_ "agenda-import" ]
    [ div [ class_ "agenda-import-header" ]
        [ div [ class_ "agenda-import-title" ] [ text "Import CSV" ]
        , div [ class_ "agenda-import-subtitle" ]
            [ text "Colonnes minimales: type, titre, fenetre_debut, fenetre_fin." ]
        ]
    , textarea
        [ class_ "form-control agenda-import-textarea"
        , placeholder "Collez votre CSV ici..."
        , value csvInput
        , onValueChange CsvInputChanged
        ]
    , div [ class_ "agenda-import-actions" ]
        [ button [ class_ "btn btn-sm btn-outline-primary", onClick (const ParseCsvInput) ] [ text "Analyser" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ClearCsvImport) ] [ text "Effacer" ]
        , button [ class_ "btn btn-sm btn-success", onClick (const ApplyCsvImport) ] [ text "Ajouter a la liste" ]
        ]
    , maybe (text "") renderCsvImportResult result
    ]

renderCsvImportResult :: forall w. CsvImportResult -> HTML w Action
renderCsvImportResult result =
  let
    okCount = length result.items
    errorCount = length result.errors
  in
    div [ class_ "agenda-import-result" ]
      [ div [ class_ "agenda-import-summary" ]
          [ text $ "Valides: " <> show okCount <> " • Erreurs: " <> show errorCount ]
      , if null result.errors then text "" else renderCsvImportErrors result.errors
      ]

renderCsvImportErrors :: forall w. Array CsvImportError -> HTML w Action
renderCsvImportErrors errors =
  div [ class_ "agenda-import-errors" ]
    (map renderCsvImportError errors)

renderCsvImportError :: forall w. CsvImportError -> HTML w Action
renderCsvImportError err =
  div [ class_ "agenda-import-error" ]
    [ text $ "Ligne " <> show err.rowNumber <> ": " <> err.message ]

renderIcsImportPanel :: forall w. String -> Maybe IcsImportResult -> HTML w Action
renderIcsImportPanel icsInput result =
  section [ class_ "agenda-import" ]
    [ div [ class_ "agenda-import-header" ]
        [ div [ class_ "agenda-import-title" ] [ text "Import ICS" ]
        , div [ class_ "agenda-import-subtitle" ]
            [ text "Support basique: SUMMARY, DTSTART, DTEND." ]
        ]
    , textarea
        [ class_ "form-control agenda-import-textarea"
        , placeholder "Collez votre fichier ICS ici..."
        , value icsInput
        , onValueChange IcsInputChanged
        ]
    , div [ class_ "agenda-import-actions" ]
        [ button [ class_ "btn btn-sm btn-outline-primary", onClick (const ParseIcsInput) ] [ text "Analyser" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ClearIcsImport) ] [ text "Effacer" ]
        , button [ class_ "btn btn-sm btn-success", onClick (const ApplyIcsImport) ] [ text "Ajouter a la liste" ]
        ]
    , maybe (text "") renderIcsImportResult result
    ]

renderIcsImportResult :: forall w. IcsImportResult -> HTML w Action
renderIcsImportResult result =
  let
    okCount = length result.items
    errorCount = length result.errors
  in
    div [ class_ "agenda-import-result" ]
      [ div [ class_ "agenda-import-summary" ]
          [ text $ "Valides: " <> show okCount <> " • Erreurs: " <> show errorCount ]
      , if null result.errors then text "" else renderIcsImportErrors result.errors
      ]

renderIcsImportErrors :: forall w. Array IcsImportError -> HTML w Action
renderIcsImportErrors errors =
  div [ class_ "agenda-import-errors" ]
    (map renderIcsImportError errors)

renderIcsImportError :: forall w. IcsImportError -> HTML w Action
renderIcsImportError err =
  div [ class_ "agenda-import-error" ]
    [ text $ "Evenement " <> show err.eventIndex <> ": " <> err.message ]

renderExportPanel
  :: forall w
   . ExportFormat
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> HTML w Action
renderExportPanel format typeFilter statusFilter categoryFilter startDate endDate output =
  section [ class_ "agenda-export" ]
    [ div [ class_ "agenda-export-header" ]
        [ div [ class_ "agenda-export-title" ] [ text "Export" ]
        , div [ class_ "agenda-export-subtitle" ]
            [ text "Filtres: type, categorie, statut, periode." ]
        ]
    , div [ class_ "agenda-export-controls" ]
        [ div [ class_ "agenda-export-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Format" ]
            , select
                [ class_ "form-select agenda-sort-select"
                , onValueChange ExportFormatChanged
                , value (exportFormatValue format)
                ]
                [ option [ value "csv" ] [ text "CSV" ]
                , option [ value "ics" ] [ text "ICS" ]
                ]
            ]
        , div [ class_ "agenda-export-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Type" ]
            , select
                [ class_ "form-select agenda-sort-select"
                , onValueChange ExportTypeFilterChanged
                , value typeFilter
                ]
                [ option [ value "" ] [ text "Tous" ]
                , option [ value "INTENTION" ] [ text "Intention" ]
                , option [ value "BLOC_PLANIFIE" ] [ text "Bloc planifie" ]
                ]
            ]
        , div [ class_ "agenda-export-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Statut" ]
            , select
                [ class_ "form-select agenda-sort-select"
                , onValueChange ExportStatusFilterChanged
                , value statusFilter
                ]
                [ option [ value "" ] [ text "Tous" ]
                , option [ value "TODO" ] [ text "TODO" ]
                , option [ value "EN_COURS" ] [ text "EN_COURS" ]
                , option [ value "FAIT" ] [ text "FAIT" ]
                , option [ value "ANNULE" ] [ text "ANNULE" ]
                ]
            ]
        , div [ class_ "agenda-export-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Categorie" ]
            , input
                [ class_ "form-control agenda-input"
                , placeholder "Ex: Sport"
                , value categoryFilter
                , onValueChange ExportCategoryFilterChanged
                ]
            ]
        , div [ class_ "agenda-export-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Debut" ]
            , input
                [ class_ "form-control agenda-input"
                , type_ InputDate
                , value startDate
                , onValueChange ExportStartDateChanged
                ]
            ]
        , div [ class_ "agenda-export-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Fin" ]
            , input
                [ class_ "form-control agenda-input"
                , type_ InputDate
                , value endDate
                , onValueChange ExportEndDateChanged
                ]
            ]
        ]
    , div [ class_ "agenda-export-actions" ]
        [ button [ class_ "btn btn-sm btn-primary", onClick (const GenerateExport) ] [ text "Generer" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ClearExportOutput) ] [ text "Effacer" ]
        ]
    , if output == "" then text "" else
        textarea
          [ class_ "form-control agenda-export-textarea"
          , value output
          ]
    ]

timeLabel :: String -> String
timeLabel raw =
  if String.length raw >= 16 then String.slice 11 16 raw else raw

dragHandlers ::
  forall r.
  CalendarItem ->
  Array
    (IProp
       ( draggable :: Boolean
       , onDragStart :: DragEvent
       , onDragOver :: DragEvent
       , onDrop :: DragEvent
       , onDragEnd :: DragEvent
       | r
       )
       Action
    )
dragHandlers (ServerCalendarItem { id }) =
  [ draggable true
  , onDragStart (const $ DragStart id)
  , onDragOver (\ev -> DragOver id ev)
  , onDrop (const $ DropOn id)
  , onDragEnd (const DragEnd)
  ]
dragHandlers _ = []

isConflict :: Array String -> CalendarItem -> Boolean
isConflict conflictIds (ServerCalendarItem { id }) = elem id conflictIds
isConflict _ _ = false

type ConflictBlock =
  { id :: String
  , start :: String
  , end :: String
  }

type ConflictResolution =
  { groupIds :: Array String
  , pendingStrategy :: Maybe ResolutionStrategy
  }

type ValidationPanel =
  { itemId :: String
  , proposedMinutes :: Maybe Int
  , inputValue :: String
  }

data ResolutionStrategy
  = StrategyShift30
  | StrategySwap

derive instance resolutionStrategyGeneric :: Generic ResolutionStrategy _
derive instance resolutionStrategyEq :: Eq ResolutionStrategy
instance resolutionStrategyShow :: Show ResolutionStrategy where
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

type OfflineMutationResult =
  { items :: Array CalendarItem
  , pending :: Array CalendarItem
  }

applyOfflineMutation :: Boolean -> CalendarItem -> Array CalendarItem -> Array CalendarItem -> OfflineMutationResult
applyOfflineMutation offline item items pending =
  if offline
    then { items: items <> [ item ], pending: pending <> [ item ] }
    else { items, pending }

durationMinutesBetween :: String -> String -> Maybe Int
durationMinutesBetween start end = do
  startDt <- parseDateTimeLocal start
  endDt <- parseDateTimeLocal end
  let Minutes n = diff endDt startDt
      minutes = Int.floor n
  pure $ max 1 minutes

suggestDurationMinutes :: String -> Effect (Maybe Int)
suggestDurationMinutes start = do
  now <- nowDateTime
  pure $ durationMinutesBetweenDateTime start now

durationMinutesBetweenDateTime :: String -> DateTime -> Maybe Int
durationMinutesBetweenDateTime start now = do
  startDt <- parseDateTimeLocal start
  let Minutes n = diff now startDt
      minutes = Int.floor n
  pure $ max 1 minutes

parseDateTimeLocal :: String -> Maybe DateTime
parseDateTimeLocal raw = do
  year <- parseInt (slice 0 4)
  monthNum <- parseInt (slice 5 7)
  dayNum <- parseInt (slice 8 10)
  hourNum <- parseInt (slice 11 13)
  minuteNum <- parseInt (slice 14 16)
  month <- toEnum monthNum
  day <- toEnum dayNum
  hour <- toEnum hourNum
  minute <- toEnum minuteNum
  yearEnum <- toEnum year
  date <- exactDate yearEnum month day
  second <- toEnum 0
  millisecond <- toEnum 0
  pure $ DateTime date (Time hour minute second millisecond)
  where
  slice start end = String.slice start end raw
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

combineDateWithTime :: String -> String -> Maybe String
combineDateWithTime dateTimeRaw timeRaw = do
  dt <- parseDateTimeLocal dateTimeRaw
  t <- parseTimeLocal timeRaw
  pure $ formatDateTimeLocal (DateTime (date dt) t)

reminderTimesForIntention :: NotificationDefaults -> Maybe NotificationOverride -> CalendarItemContent -> Array ReminderTime
reminderTimesForIntention defaults override content =
  if content.itemType /= Intention then []
  else
    let
      startTime = fromMaybe defaults.startDayTime (override >>= _.startDayTime)
      beforeEndHours = fromMaybe defaults.beforeEndHours (override >>= _.beforeEndHours)
      startReminder = combineDateWithTime content.windowStart startTime <#> \at -> { label: "Jour de debut", at }
      beforeEndReminder = shiftMinutes (negate (beforeEndHours * 60)) content.windowEnd <#> \at -> { label: show beforeEndHours <> "h avant fin", at }
    in
      catMaybes [ startReminder, beforeEndReminder ]

lookupNotificationOverride :: String -> Array NotificationOverride -> Maybe NotificationOverride
lookupNotificationOverride itemId overrides =
  find (\override -> override.itemId == itemId) overrides

upsertNotificationOverride :: String -> Maybe String -> Maybe Int -> Array NotificationOverride -> Array NotificationOverride
upsertNotificationOverride itemId startTime beforeEnd overrides =
  let
    cleaned =
      case { start: startTime, end: beforeEnd } of
        { start: Nothing, end: Nothing } -> Nothing
        { start, end } -> Just { itemId, startDayTime: start, beforeEndHours: end }
  in
    case cleaned of
      Nothing -> removeNotificationOverride itemId overrides
      Just entry ->
        case find (\override -> override.itemId == itemId) overrides of
          Nothing -> overrides <> [ entry ]
          Just _ -> map (\override -> if override.itemId == itemId then entry else override) overrides

removeNotificationOverride :: String -> Array NotificationOverride -> Array NotificationOverride
removeNotificationOverride itemId overrides =
  filter (\override -> override.itemId /= itemId) overrides

applyTemplateToDraft :: TaskTemplate -> String -> String -> IntentionDraft
applyTemplateToDraft template windowStart windowEnd =
  { title: template.title
  , windowStart
  , windowEnd
  , category: template.category
  }

addTemplate :: TaskTemplate -> Array TaskTemplate -> Array TaskTemplate
addTemplate template templates =
  let
    nextId = if template.id == "" then nextTemplateId templates else template.id
  in
    templates <> [ template { id = nextId } ]

updateTemplate :: TaskTemplate -> Array TaskTemplate -> Array TaskTemplate
updateTemplate template templates =
  map (\existing -> if existing.id == template.id then template else existing) templates

removeTemplate :: String -> Array TaskTemplate -> Array TaskTemplate
removeTemplate templateId templates =
  filter (\template -> template.id /= templateId) templates

templateSummary :: TaskTemplate -> String
templateSummary template =
  let
    duration = show template.durationMinutes <> " min"
    category = StringCommon.trim template.category
  in
    if category == "" then duration else duration <> " • " <> category

nextTemplateId :: Array TaskTemplate -> String
nextTemplateId templates =
  let
    existing = map _.id templates
    findId n =
      let candidate = "tpl-" <> show n
      in if elem candidate existing then findId (n + 1) else candidate
  in
    findId 1

type CsvHeader =
  { typeIdx :: Int
  , titleIdx :: Int
  , startIdx :: Int
  , endIdx :: Int
  , categoryIdx :: Maybe Int
  , statusIdx :: Maybe Int
  }

parseCsvImport :: String -> CsvImportResult
parseCsvImport raw =
  let
    lines = map stripCR (StringCommon.split (Pattern "\n") raw)
    indexed = mapWithIndex (\idx line -> { row: idx + 1, raw: line }) lines
    nonEmpty = filter (\line -> StringCommon.trim line.raw /= "") indexed
  in
    case uncons nonEmpty of
      Nothing -> { items: [], errors: [ { rowNumber: 1, message: "CSV vide." } ] }
      Just { head: headerLine, tail: dataLines } ->
        case resolveCsvHeader headerLine.raw of
          Left err -> { items: [], errors: [ { rowNumber: headerLine.row, message: err } ] }
          Right header -> parseCsvRows header dataLines

resolveCsvHeader :: String -> Either String CsvHeader
resolveCsvHeader rawHeader =
  let
    headers = map normalizeHeader (splitCsvLine rawHeader)
    findFor names = findIndex (\name -> elem name names) headers
    typeIdx = findFor [ "type" ]
    titleIdx = findFor [ "titre", "title" ]
    startIdx = findFor [ "fenetre_debut", "window_start", "start" ]
    endIdx = findFor [ "fenetre_fin", "window_end", "end" ]
    categoryIdx = findFor [ "categorie", "category" ]
    statusIdx = findFor [ "statut", "status" ]
  in
    case { typeIdx, titleIdx, startIdx, endIdx } of
      { typeIdx: Just t, titleIdx: Just ti, startIdx: Just s, endIdx: Just e } ->
        Right { typeIdx: t, titleIdx: ti, startIdx: s, endIdx: e, categoryIdx, statusIdx }
      _ ->
        Left "Colonnes minimales manquantes: type, titre, fenetre_debut, fenetre_fin."

parseCsvRows :: CsvHeader -> Array { row :: Int, raw :: String } -> CsvImportResult
parseCsvRows header rows =
  foldl parseRow { items: [], errors: [] } rows
  where
  parseRow acc row =
    let
      fields = splitCsvLine row.raw
      fieldAt idx = index fields idx
    in
      case { typeVal: fieldAt header.typeIdx
           , titleVal: fieldAt header.titleIdx
           , startVal: fieldAt header.startIdx
           , endVal: fieldAt header.endIdx
           } of
        { typeVal: Just typeVal
        , titleVal: Just titleVal
        , startVal: Just startVal
        , endVal: Just endVal
        } ->
          case parseCsvItem header fields typeVal titleVal startVal endVal of
            Left err -> acc { errors = acc.errors <> [ { rowNumber: row.row, message: err } ] }
            Right item -> acc { items = acc.items <> [ item ] }
        _ ->
          acc { errors = acc.errors <> [ { rowNumber: row.row, message: "Colonnes manquantes pour cette ligne." } ] }

parseCsvItem :: CsvHeader -> Array String -> String -> String -> String -> String -> Either String CalendarItem
parseCsvItem header fields typeVal titleVal startVal endVal = do
  itemType <- parseCsvItemType typeVal
  status <- parseCsvStatus (lookupField header.statusIdx fields)
  let
    title = StringCommon.trim titleVal
    windowStart = StringCommon.trim startVal
    windowEnd = StringCommon.trim endVal
    category = lookupField header.categoryIdx fields >>= toOptionalString
  if title == "" then Left "Le titre est vide."
  else if not (isDateTimeLocal windowStart) then Left "Debut invalide (format YYYY-MM-DDTHH:MM)."
  else if not (isDateTimeLocal windowEnd) then Left "Fin invalide (format YYYY-MM-DDTHH:MM)."
  else if windowEnd <= windowStart then Left "La fin doit etre apres le debut."
  else
    Right $ NewCalendarItem
      { content:
          { itemType
          , title
          , windowStart
          , windowEnd
          , status
          , sourceItemId: Nothing
          , actualDurationMinutes: Nothing
          , category
          , recurrenceRule: Nothing
          , recurrenceExceptionDates: []
          }
      }

lookupField :: Maybe Int -> Array String -> Maybe String
lookupField idx fields = idx >>= \i -> index fields i

parseCsvItemType :: String -> Either String ItemType
parseCsvItemType raw =
  case normalizeHeader raw of
    "intention" -> Right Intention
    "bloc_planifie" -> Right ScheduledBlock
    "scheduled_block" -> Right ScheduledBlock
    _ -> Left "Type invalide (INTENTION ou BLOC_PLANIFIE)."

parseCsvStatus :: Maybe String -> Either String ItemStatus
parseCsvStatus raw =
  case raw of
    Nothing -> Right Todo
    Just value | StringCommon.trim value == "" -> Right Todo
    Just value ->
      case normalizeHeader value of
        "todo" -> Right Todo
        "en_cours" -> Right EnCours
        "fait" -> Right Fait
        "annule" -> Right Annule
        _ -> Left "Statut invalide (TODO, EN_COURS, FAIT, ANNULE)."

normalizeHeader :: String -> String
normalizeHeader = toLower <<< StringCommon.trim

toOptionalString :: String -> Maybe String
toOptionalString raw =
  let trimmed = StringCommon.trim raw
  in if trimmed == "" then Nothing else Just trimmed

splitCsvLine :: String -> Array String
splitCsvLine raw =
  parseChars (String.toCharArray raw) { field: "", fields: [], inQuote: false }
  where
  parseChars chars state =
    case uncons chars of
      Nothing -> state.fields <> [ state.field ]
      Just { head, tail } ->
        if head == '"' then
          case uncons tail of
            Just { head: next, tail: rest } | state.inQuote && next == '"' ->
              parseChars rest state { field = state.field <> String.singleton '"' }
            _ ->
              parseChars tail state { inQuote = not state.inQuote }
        else if head == ',' && not state.inQuote then
          parseChars tail state { fields = state.fields <> [ state.field ], field = "" }
        else
          parseChars tail state { field = state.field <> String.singleton head }

stripCR :: String -> String
stripCR line =
  let len = String.length line
  in if len > 0 && String.charAt (len - 1) line == Just '\r'
     then String.slice 0 (len - 1) line
     else line

type IcsEventDraft =
  { summary :: Maybe String
  , dtStart :: Maybe String
  , dtEnd :: Maybe String
  }

parseIcsImport :: String -> IcsImportResult
parseIcsImport raw =
  let
    lines = map stripCR (StringCommon.split (Pattern "\n") raw)
    initial =
      { current: Nothing
      , items: []
      , errors: []
      , index: 0
      }
    final = foldl parseIcsLine initial lines
  in
    { items: final.items, errors: final.errors }
  where
  parseIcsLine state line =
    case StringCommon.trim line of
      "BEGIN:VEVENT" ->
        state { current = Just { summary: Nothing, dtStart: Nothing, dtEnd: Nothing } }
      "END:VEVENT" ->
        case state.current of
          Nothing -> state
          Just event ->
            let nextIndex = state.index + 1
            in case buildIcsItem nextIndex event of
                Left err -> state { errors = state.errors <> [ err ], current = Nothing, index = nextIndex }
                Right item -> state { items = state.items <> [ item ], current = Nothing, index = nextIndex }
      _ ->
        case state.current of
          Nothing -> state
          Just event ->
            let
              key = extractIcsKey line
              value = extractIcsValue line
              updated =
                case key of
                  "SUMMARY" -> event { summary = toOptionalString value }
                  "DTSTART" -> event { dtStart = Just value }
                  "DTEND" -> event { dtEnd = Just value }
                  _ -> event
            in
              state { current = Just updated }

buildIcsItem :: Int -> IcsEventDraft -> Either IcsImportError CalendarItem
buildIcsItem index event = do
  title <- maybe (Left { eventIndex: index, message: "SUMMARY manquant." }) Right event.summary
  startRaw <- maybe (Left { eventIndex: index, message: "DTSTART manquant." }) Right event.dtStart
  endRaw <- maybe (Left { eventIndex: index, message: "DTEND manquant." }) Right event.dtEnd
  windowStart <- maybe (Left { eventIndex: index, message: "DTSTART invalide." }) Right (parseIcsDateTime startRaw)
  windowEnd <- maybe (Left { eventIndex: index, message: "DTEND invalide." }) Right (parseIcsDateTime endRaw)
  if windowEnd <= windowStart then Left { eventIndex: index, message: "La fin doit etre apres le debut." }
  else
    Right $ NewCalendarItem
      { content:
          { itemType: ScheduledBlock
          , title
          , windowStart
          , windowEnd
          , status: Todo
          , sourceItemId: Nothing
          , actualDurationMinutes: Nothing
          , category: Nothing
          , recurrenceRule: Nothing
          , recurrenceExceptionDates: []
          }
      }

extractIcsKey :: String -> String
extractIcsKey line =
  case uncons (StringCommon.split (Pattern ":") line) of
    Nothing -> ""
    Just { head: first } ->
      case uncons (StringCommon.split (Pattern ";") first) of
        Nothing -> ""
        Just { head: key } -> StringCommon.trim key

extractIcsValue :: String -> String
extractIcsValue line =
  fromMaybe "" (last (StringCommon.split (Pattern ":") line))

parseIcsDateTime :: String -> Maybe String
parseIcsDateTime raw =
  let
    trimmed = StringCommon.trim raw
    cleaned = if endsWithChar 'Z' trimmed then String.slice 0 (String.length trimmed - 1) trimmed else trimmed
  in
    if String.length cleaned < 13 then Nothing
    else if String.charAt 8 cleaned /= Just 'T' then Nothing
    else
      let
        y = String.slice 0 4 cleaned
        m = String.slice 4 6 cleaned
        d = String.slice 6 8 cleaned
        hh = String.slice 9 11 cleaned
        mm = String.slice 11 13 cleaned
      in
        Just $ y <> "-" <> m <> "-" <> d <> "T" <> hh <> ":" <> mm
  where
  endsWithChar ch str =
    let len = String.length str
    in len > 0 && String.charAt (len - 1) str == Just ch

exportFormatValue :: ExportFormat -> String
exportFormatValue ExportCSV = "csv"
exportFormatValue ExportICS = "ics"

parseExportFormat :: String -> ExportFormat
parseExportFormat raw =
  if raw == "ics" then ExportICS else ExportCSV

parseExportItemType :: String -> Maybe ItemType
parseExportItemType raw =
  case raw of
    "INTENTION" -> Just Intention
    "BLOC_PLANIFIE" -> Just ScheduledBlock
    _ -> Nothing

parseExportStatus :: String -> Maybe ItemStatus
parseExportStatus raw =
  case raw of
    "TODO" -> Just Todo
    "EN_COURS" -> Just EnCours
    "FAIT" -> Just Fait
    "ANNULE" -> Just Annule
    _ -> Nothing

filterItemsForExport :: ExportFilter -> Array CalendarItem -> Array CalendarItem
filterItemsForExport criteria items =
  filter (matchesFilter criteria) items

matchesFilter :: ExportFilter -> CalendarItem -> Boolean
matchesFilter criteria item =
  let
    content = calendarItemContent item
    matchesType = maybe true (\target -> content.itemType == target) criteria.itemType
    matchesStatus = maybe true (\target -> content.status == target) criteria.status
    matchesCategory =
      case criteria.category of
        Nothing -> true
        Just target ->
          case content.category of
            Nothing -> false
            Just value -> normalizeHeader value == normalizeHeader target
    dateKey = datePart content.windowStart
    matchesStart =
      case criteria.startDate of
        Nothing -> true
        Just start -> dateKey >= start
    matchesEnd =
      case criteria.endDate of
        Nothing -> true
        Just end -> dateKey <= end
  in
    matchesType && matchesStatus && matchesCategory && matchesStart && matchesEnd

datePart :: String -> String
datePart raw =
  if String.length raw >= 10 then String.slice 0 10 raw else raw

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
            let next = addDaysToDate 1 current
            in case next of
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

addDaysToDate :: Int -> Date -> Maybe Date
addDaysToDate days date' = do
  t <- parseTimeLocal "00:00"
  let dt = DateTime date' t
  newDt <- addDays days dt
  pure $ date newDt

exportItemsToCsv :: Array CalendarItem -> String
exportItemsToCsv items =
  let
    header = "type,titre,fenetre_debut,fenetre_fin,statut,categorie"
    rows = map exportCsvRow items
  in
    StringCommon.joinWith "\n" ([ header ] <> rows)

exportCsvRow :: CalendarItem -> String
exportCsvRow item =
  let
    content = calendarItemContent item
    category = fromMaybe "" content.category
  in
    StringCommon.joinWith "," $
      map csvEscape
        [ exportItemType content.itemType
        , content.title
        , content.windowStart
        , content.windowEnd
        , exportItemStatus content.status
        , category
        ]

csvEscape :: String -> String
csvEscape value =
  "\"" <> escapeQuotes value <> "\""

escapeQuotes :: String -> String
escapeQuotes raw =
  foldl
    (\acc ch -> if ch == '"' then acc <> "\"\"" else acc <> String.singleton ch)
    ""
    (String.toCharArray raw)

exportItemsToIcs :: Array CalendarItem -> String
exportItemsToIcs items =
  let
    header = [ "BEGIN:VCALENDAR", "VERSION:2.0", "PRODID:-//FAVS//EN" ]
    events = items >>= exportIcsEvent
  in
    StringCommon.joinWith "\n" (header <> events <> [ "END:VCALENDAR" ])

exportIcsEvent :: CalendarItem -> Array String
exportIcsEvent item =
  let
    content = calendarItemContent item
    start = toIcsDateTime content.windowStart
    end = toIcsDateTime content.windowEnd
    categoryLine =
      case content.category of
        Nothing -> []
        Just value -> [ "CATEGORIES:" <> value ]
  in
    [ "BEGIN:VEVENT"
    , "SUMMARY:" <> content.title
    , "DTSTART:" <> start
    , "DTEND:" <> end
    ]
      <> categoryLine
      <> [ "END:VEVENT" ]

toIcsDateTime :: String -> String
toIcsDateTime raw =
  if String.length raw >= 16 then
    String.slice 0 4 raw <>
    String.slice 5 7 raw <>
    String.slice 8 10 raw <>
    "T" <>
    String.slice 11 13 raw <>
    String.slice 14 16 raw
  else raw

exportItemType :: ItemType -> String
exportItemType Intention = "INTENTION"
exportItemType ScheduledBlock = "BLOC_PLANIFIE"

exportItemStatus :: ItemStatus -> String
exportItemStatus Todo = "TODO"
exportItemStatus EnCours = "EN_COURS"
exportItemStatus Fait = "FAIT"
exportItemStatus Annule = "ANNULE"

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

instantiateRoutine :: RoutineTemplate -> RoutineInstance
instantiateRoutine template =
  let
    steps = map toInstance template.steps
    withDeps = applyDependencies template.steps steps
  in
    { templateId: template.id
    , steps: withDeps
    }
  where
  toInstance step =
    { id: step.id
    , title: step.title
    , windowStart: step.windowStart
    , windowEnd: step.windowEnd
    , sourceStepId: step.id
    }

applyDependencies :: Array RoutineTemplateStep -> Array RoutineInstanceStep -> Array RoutineInstanceStep
applyDependencies templateSteps instanceSteps =
  map (applyDependency templateSteps instanceSteps) instanceSteps

applyDependency :: Array RoutineTemplateStep -> Array RoutineInstanceStep -> RoutineInstanceStep -> RoutineInstanceStep
applyDependency templateSteps instanceSteps step =
  case find (\templateStep -> templateStep.id == step.sourceStepId) templateSteps of
    Nothing -> step
    Just templateStep ->
      case templateStep.dependsOn of
        Nothing -> step
        Just dependency ->
          case dependency of
            StartAfterEnd { stepId, offsetMinutes } ->
              updateFromBase stepId offsetMinutes _.windowEnd
            StartBeforeStart { stepId, offsetMinutes } ->
              updateFromBase stepId (-offsetMinutes) _.windowStart
  where
  updateFromBase baseId offset selectBase =
    case find (\candidate -> candidate.id == baseId) instanceSteps of
      Nothing -> step
      Just base ->
        let
          duration = durationMinutesBetween step.windowStart step.windowEnd
          newStart = shiftMinutes offset (selectBase base)
          newEnd = newStart >>= \start -> duration >>= \mins -> shiftMinutes mins start
        in
          case { start: newStart, end: newEnd } of
            { start: Just start, end: Just end } -> step { windowStart = start, windowEnd = end }
            _ -> step

shiftMinutes :: Int -> String -> Maybe String
shiftMinutes offset start = do
  dt <- parseDateTimeLocal start
  newDt <- adjust (Minutes (Int.toNumber offset)) dt
  pure $ formatDateTimeLocal newDt

nextOccurrence :: RecurrenceRule -> DateTime -> Maybe DateTime
nextOccurrence rule dt =
  case rule of
    RecurrenceDaily -> addDays 1 dt
    RecurrenceWeekly -> addDays 7 dt
    RecurrenceEveryXDays interval -> addDays interval dt
    RecurrenceMonthly -> Just (addMonths 1 dt)
    RecurrenceYearly -> Just (addMonths 12 dt)

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
  let raw = Int.toStringAs Int.decimal n
  in if String.length raw == 1 then "0" <> raw else raw

detectConflictIds :: Array CalendarItem -> Array String
detectConflictIds items =
  nub $ go (mapMaybe toConflictBlock items) []
  where
  toConflictBlock :: CalendarItem -> Maybe ConflictBlock
  toConflictBlock (ServerCalendarItem { id, content }) | content.itemType == ScheduledBlock =
    Just { id, start: content.windowStart, end: content.windowEnd }
  toConflictBlock _ = Nothing

  overlaps a b = a.start < b.end && b.start < a.end

  go blocks acc =
    case uncons blocks of
      Nothing -> acc
      Just { head: current, tail: rest } ->
        let
          acc' =
            foldl
              (\currentAcc other ->
                if overlaps current other
                  then currentAcc <> [ current.id, other.id ]
                  else currentAcc
              )
              acc
              rest
        in
          go rest acc'

detectConflictGroups :: Array CalendarItem -> Array (Array String)
detectConflictGroups items =
  filter (\group -> length group > 1) $ components allIds []
  where
  blocks = mapMaybe toConflictBlock items
  allIds = map _.id blocks

  toConflictBlock :: CalendarItem -> Maybe ConflictBlock
  toConflictBlock (ServerCalendarItem { id, content }) | content.itemType == ScheduledBlock =
    Just { id, start: content.windowStart, end: content.windowEnd }
  toConflictBlock _ = Nothing

  components ids visited =
    case uncons ids of
      Nothing -> []
      Just { head: current, tail } ->
        if elem current visited then components tail visited
        else
          let
            group = bfs [ current ] []
            newVisited = visited <> group
          in
            [ group ] <> components tail newVisited

  bfs queue visited =
    case uncons queue of
      Nothing -> visited
      Just { head: current, tail } ->
        if elem current visited then bfs tail visited
        else
          let
            next = neighbors current
          in
            bfs (tail <> next) (visited <> [ current ])

  neighbors id =
    case find (\block -> block.id == id) blocks of
      Nothing -> []
      Just current ->
        map _.id $ filter (\block -> block.id /= id && overlaps current block) blocks

  overlaps a b = a.start < b.end && b.start < a.end

renderConflictActions :: forall w. Array (Array String) -> HTML w Action
renderConflictActions conflictGroups =
  if null conflictGroups then text ""
  else
    div [ class_ "agenda-conflict-actions" ]
      [ button
          [ class_ "btn btn-sm btn-outline-danger agenda-conflict-button"
          , onClick (const $ OpenConflictResolution (headOrEmpty conflictGroups))
          ]
          [ text "Resoudre un conflit" ]
      ]
  where
  headOrEmpty groups =
    case uncons groups of
      Just { head } -> head
      Nothing -> []

renderOfflineToggle :: forall w. Boolean -> HTML w Action
renderOfflineToggle offlineMode =
  div [ class_ "agenda-offline-toggle" ]
    [ button
        [ class_ $ "btn btn-sm " <> if offlineMode then "btn-outline-warning" else "btn-outline-secondary"
        , onClick (const ToggleOffline)
        ]
        [ text $ if offlineMode then "Mode hors ligne actif" else "Passer hors ligne" ]
    ]

renderSortPicker :: forall w. SortMode -> HTML w Action
renderSortPicker sortMode =
  div [ class_ "agenda-sort" ]
    [ text "Trier:"
    , select
        [ class_ "form-select agenda-sort-select"
        , onValueChange SortChanged
        , value (sortModeValue sortMode)
        ]
        [ option [ value "time" ] [ text "Horaire" ]
        , option [ value "status" ] [ text "Statut" ]
        , option [ value "category" ] [ text "Categorie" ]
        , option [ value "conflict" ] [ text "Conflit" ]
        ]
    ]

renderViewSelector :: forall w. AgendaView -> String -> HTML w Action
renderViewSelector viewMode focusDate =
  div [ class_ "agenda-view-selector" ]
    [ div [ class_ "agenda-view-buttons" ]
        [ button
            [ class_ $ "btn btn-sm " <> if viewMode == ViewDay then "btn-primary" else "btn-outline-secondary"
            , onClick (const $ ViewChanged "day")
            ]
            [ text "Jour" ]
        , button
            [ class_ $ "btn btn-sm " <> if viewMode == ViewWeek then "btn-primary" else "btn-outline-secondary"
            , onClick (const $ ViewChanged "week")
            ]
            [ text "Semaine" ]
        , button
            [ class_ $ "btn btn-sm " <> if viewMode == ViewMonth then "btn-primary" else "btn-outline-secondary"
            , onClick (const $ ViewChanged "month")
            ]
            [ text "Mois" ]
        ]
    , input
        [ class_ "form-control agenda-input agenda-view-date"
        , type_ InputDate
        , value focusDate
        , onValueChange FocusDateChanged
        ]
    ]

viewTitle :: AgendaView -> String
viewTitle viewMode =
  case viewMode of
    ViewDay -> "Vue Jour"
    ViewWeek -> "Vue Semaine"
    ViewMonth -> "Vue Mois"

parseAgendaView :: String -> AgendaView
parseAgendaView raw =
  case raw of
    "week" -> ViewWeek
    "month" -> ViewMonth
    _ -> ViewDay

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

moveItemBefore :: String -> String -> Array CalendarItem -> Array CalendarItem
moveItemBefore dragId targetId items =
  case { from: indexOf dragId items, to: indexOf targetId items } of
    { from: Just fromIdx, to: Just toIdx } ->
      let
        without = deleteAtIndex fromIdx items
        adjustedTo = if fromIdx < toIdx then toIdx - 1 else toIdx
        draggedItem = index items fromIdx
      in
        insertAtIndex adjustedTo draggedItem without
    _ -> items
  where
  indexOf id = findIndex (matchesId id)
  matchesId id (ServerCalendarItem { id: candidate }) = id == candidate
  matchesId _ _ = false

  findIndex predicate arr =
    case uncons arr of
      Nothing -> Nothing
      Just { head, tail } ->
        if predicate head then Just 0
        else map (_ + 1) (findIndex predicate tail)

  deleteAtIndex idx arr =
    case uncons arr of
      Nothing -> []
      Just { head, tail } ->
        if idx == 0 then tail
        else [ head ] <> deleteAtIndex (idx - 1) tail

  insertAtIndex idx maybeItem arr =
    case maybeItem of
      Nothing -> arr
      Just item ->
        if idx <= 0 then [ item ] <> arr
        else case uncons arr of
          Nothing -> [ item ]
          Just { head, tail } -> [ head ] <> insertAtIndex (idx - 1) (Just item) tail

renderSyncConflict :: forall w. Array CalendarItem -> HTML w Action
renderSyncConflict pending =
  div [ class_ "agenda-sync-conflict" ]
    [ div [ class_ "agenda-conflict-title" ] [ text "Conflit de synchronisation" ]
    , div [ class_ "agenda-conflict-subtitle" ]
        [ text "Choisissez comment resoudre la synchronisation des changements locaux." ]
    , ul [ class_ "agenda-conflict-list" ] (map (renderConflictItem pending) pendingIds)
    , div [ class_ "agenda-conflict-confirmation-actions" ]
        [ button [ class_ "btn btn-sm btn-danger", onClick (const ResolveSyncDiscardLocal) ] [ text "Abandonner local" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ResolveSyncKeepLocal) ] [ text "Conserver local" ]
        ]
    ]
  where
  pendingIds = mapMaybe extractId pending
  extractId (ServerCalendarItem { id }) = Just id
  extractId _ = Nothing

renderValidationPanel :: forall w. ValidationPanel -> HTML w Action
renderValidationPanel panel =
  div [ class_ "agenda-validation-panel" ]
    [ div [ class_ "agenda-conflict-title" ] [ text "Valider la tache" ]
    , div [ class_ "agenda-conflict-subtitle" ]
        [ text "Saisissez la duree reelle (minutes) ou acceptez la proposition." ]
    , maybe (text "") (\minutes -> div [ class_ "agenda-validation-proposal" ] [ text $ "Proposition: " <> show minutes <> " min" ]) panel.proposedMinutes
    , input
        [ class_ "form-control agenda-input"
        , placeholder "Duree reelle (minutes)"
        , onValueChange ValidationMinutesChanged
        , value panel.inputValue
        ]
    , div [ class_ "agenda-conflict-confirmation-actions" ]
        [ button [ class_ "btn btn-sm btn-success", onClick (const ConfirmValidation) ] [ text "Confirmer" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const CancelValidation) ] [ text "Annuler" ]
        ]
    ]

renderConflictResolution :: forall w. Array CalendarItem -> ConflictResolution -> HTML w Action
renderConflictResolution items resolution =
  div [ class_ "agenda-conflict-panel" ]
    [ div [ class_ "agenda-conflict-title" ] [ text "Resolution de conflit" ]
    , div [ class_ "agenda-conflict-subtitle" ] [ text "Choisissez une strategie puis confirmez." ]
    , ul [ class_ "agenda-conflict-list" ] (map (renderConflictItem items) resolution.groupIds)
    , div [ class_ "agenda-conflict-strategies" ]
        [ button
            [ class_ "btn btn-sm btn-outline-primary"
            , onClick (const $ ChooseResolutionStrategy StrategyShift30)
            ]
            [ text "Decaler de 30 min" ]
        , button
            [ class_ "btn btn-sm btn-outline-primary"
            , onClick (const $ ChooseResolutionStrategy StrategySwap)
            ]
            [ text "Echanger" ]
        ]
    , renderConfirmation resolution.pendingStrategy
    , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const CancelResolution) ] [ text "Fermer" ]
    ]

renderConflictItem :: forall w. Array CalendarItem -> String -> HTML w Action
renderConflictItem items itemId =
  case find (matchId itemId) items of
    Just item ->
      let
        content = calendarItemContent item
      in
        li [ class_ "agenda-conflict-item" ]
          [ div [ class_ "agenda-conflict-item-title" ] [ text content.title ]
          , div [ class_ "agenda-conflict-item-window" ]
              [ text $ content.windowStart <> " → " <> content.windowEnd ]
          ]
    Nothing -> text ""
  where
  matchId id (ServerCalendarItem { id: candidate }) = id == candidate
  matchId _ _ = false

renderConfirmation :: forall w. Maybe ResolutionStrategy -> HTML w Action
renderConfirmation pending =
  case pending of
    Nothing -> text ""
    Just strategy ->
      div [ class_ "agenda-conflict-confirmation" ]
        [ div [ class_ "agenda-conflict-confirmation-text" ]
            [ text $ "Confirmer la strategie: " <> show strategy <> " ?" ]
        , div [ class_ "agenda-conflict-confirmation-actions" ]
            [ button [ class_ "btn btn-sm btn-danger", onClick (const ConfirmResolution) ] [ text "Confirmer" ]
            , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const CancelResolution) ] [ text "Annuler" ]
            ]
        ]

calendarItemContent :: CalendarItem -> CalendarItemContent
calendarItemContent (NewCalendarItem { content }) = content
calendarItemContent (ServerCalendarItem { content }) = content

data FatalError
  = DecodeError JsonDecodeError
  | NetworkError Error
  | CustomFatalError String

instance fatalErrorShowInstance :: Show FatalError where
  show (DecodeError err) = "DecodeError: " <> show err
  show (NetworkError err) = "NetworkError: " <> printError err
  show (CustomFatalError err) = "CustomError: " <> err

class ToFatalError a where
  toFatalError :: a -> FatalError

instance jsonDecodeErrorToFatalErrorInstance :: ToFatalError JsonDecodeError where
  toFatalError = DecodeError

instance affjaxErrorToFatalErrorInstance :: ToFatalError Error where
  toFatalError = NetworkError
