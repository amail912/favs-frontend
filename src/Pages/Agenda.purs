module Pages.Agenda
  ( component
  , applyOfflineMutation
  , durationMinutesBetween
  , detectConflictGroups
  , detectConflictIds
  , generateOccurrencesForMonth
  , instantiateRoutine
  , applyTemplateToDraft
  , addTemplate
  , updateTemplate
  , removeTemplate
  , templateSummary
  , reminderTimesForIntention
  , sortItems
  , toNewIntention
  , toScheduledBlock
  , validateIntention
  ) where

import Prelude hiding (div)

import Affjax.Web (Response)
import Api.Agenda (createItemResponse, getItemsResponse, updateItemResponse, validateItemResponse)
import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), withExceptT)
import Control.Monad.RWS (get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (catMaybes, elem, filter, find, findIndex, foldM, index, length, mapMaybe, mapWithIndex, nub, null, sortBy, uncons, updateAt)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (all, any, foldl)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Lens (Lens', (.~), (%~), (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String
import Data.String.Common as StringCommon
import Data.String.Pattern (Pattern(..))
import Data.Formatter.DateTime (unformatDateTime)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Date (Date, canonicalDate, day, exactDate, month, year)
import Data.DateTime (DateTime(..), adjust, date, diff, time)
import Data.Enum (enumFromTo, fromEnum, toEnum)
import Data.Time (Time(..), hour, minute)
import Data.Time.Duration (Days(..), Minutes(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Ui.Errors (FatalError, handleError, toFatalError)
import Effect.Now (nowDateTime)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval) as H
import Halogen.HTML (HTML, button, details, div, h2, input, li, option, section, select, summary, text, textarea, ul, span)
import Halogen.HTML.Events (onClick, onDragEnd, onDragOver, onDragStart, onDrop, onValueChange, onKeyDown)
import Halogen.HTML.Properties (IProp, attr, draggable, placeholder, style, type_, value)
import Halogen.HTML.Core (AttrName(..))
import Ui.AgendaRender (renderPanelHeader)
import Ui.Modal (renderModal)
import Ui.Utils (class_)
import Web.Event.Event (preventDefault)
import Web.Event.Event (currentTarget, target) as Event
import Web.HTML.Event.DragEvent (DragEvent, toEvent)
import Web.HTML.HTMLElement as HTMLElement
import Web.DOM.Element (getBoundingClientRect)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.KeyboardEvent as KE
import Type.Proxy (Proxy(..))
import Agenda.Model
  ( AgendaView(..)
  , CalendarItem(..)
  , CalendarItemContent
  , CsvImportError
  , CsvImportResult
  , ExportFormat(..)
  , IcsImportError
  , IcsImportResult
  , IntentionDraft
  , ItemStatus(..)
  , ItemType(..)
  , NotificationDefaults
  , NotificationOverride
  , RecurrenceRule(..)
  , ReminderTime
  , RoutineInstance
  , RoutineInstanceStep
  , RoutineTemplate
  , RoutineTemplateStep
  , SortMode(..)
  , StepDependency(..)
  , TaskTemplate
  , TemplateDraft
  , ValidationError(..)
  , defaultNotificationDefaults
  , emptyTemplateDraft
  )
import Agenda.Imports (parseCsvImport, parseIcsImport)
import Agenda.Drag (computeDropMinuteIndex, indexToMinutes, indexToTimeLabel)
import Agenda.Exports
  ( exportFormatValue
  , parseExportFormat
  , parseExportItemType
  , parseExportStatus
  , filterItemsForExport
  , exportItemsToCsv
  , exportItemsToIcs
  )

type NoOutput = Void
type AgendaAppM = H.HalogenM State Action () NoOutput Aff
type ErrorAgendaAppM = ExceptT FatalError AgendaAppM

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
    let
      trimmed = StringCommon.trim raw
    in
      if trimmed == "" then Nothing else Just trimmed

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
  case unformatDateTime "YYYY-MM-DDTHH:mm" raw of
    Right _ -> true
    Left _ -> false

type DataState =
  { items :: Array CalendarItem
  , draft :: IntentionDraft
  , validationError :: Maybe ValidationError
  , showConflictsOnly :: Boolean
  , conflictResolution :: Maybe ConflictResolution
  , sortMode :: SortMode
  }

type SyncState =
  { offlineMode :: Boolean
  , pendingSync :: Array CalendarItem
  , syncConflict :: Maybe (Array CalendarItem)
  , updateError :: Maybe String
  }

type DragState =
  { draggingId :: Maybe String
  , dragHoverIndex :: Maybe Int
  , dragOffsetMinutes :: Maybe Int
  }

type NotificationState =
  { notificationDefaults :: NotificationDefaults
  , notificationOverrides :: Array NotificationOverride
  , notificationPanelOpen :: Boolean
  , notificationEditor :: Maybe NotificationEditor
  }

type TemplateState =
  { templates :: Array TaskTemplate
  , templateDraft :: TemplateDraft
  , editingTemplateId :: Maybe String
  }

type ImportState =
  { csvInput :: String
  , csvImportResult :: Maybe CsvImportResult
  , icsInput :: String
  , icsImportResult :: Maybe IcsImportResult
  }

type ExportState =
  { exportFormat :: ExportFormat
  , exportTypeFilter :: String
  , exportStatusFilter :: String
  , exportCategoryFilter :: String
  , exportStartDate :: String
  , exportEndDate :: String
  , exportOutput :: String
  }

type ViewState =
  { viewMode :: AgendaView
  , focusDate :: String
  , activeModal :: Maybe AgendaModal
  , validationPanel :: Maybe ValidationPanel
  }

type State =
  { data :: DataState
  , sync :: SyncState
  , drag :: DragState
  , notifications :: NotificationState
  , templates :: TemplateState
  , imports :: ImportState
  , exports :: ExportState
  , view :: ViewState
  }

_data :: Lens' State DataState
_data = prop (Proxy :: _ "data")

_sync :: Lens' State SyncState
_sync = prop (Proxy :: _ "sync")

_drag :: Lens' State DragState
_drag = prop (Proxy :: _ "drag")

_notifications :: Lens' State NotificationState
_notifications = prop (Proxy :: _ "notifications")

_templates :: Lens' State TemplateState
_templates = prop (Proxy :: _ "templates")

_imports :: Lens' State ImportState
_imports = prop (Proxy :: _ "imports")

_exports :: Lens' State ExportState
_exports = prop (Proxy :: _ "exports")

_view :: Lens' State ViewState
_view = prop (Proxy :: _ "view")

_dataItems :: Lens' State (Array CalendarItem)
_dataItems = _data <<< prop (Proxy :: _ "items")

_dataDraft :: Lens' State IntentionDraft
_dataDraft = _data <<< prop (Proxy :: _ "draft")

_dataValidationError :: Lens' State (Maybe ValidationError)
_dataValidationError = _data <<< prop (Proxy :: _ "validationError")

_syncOfflineMode :: Lens' State Boolean
_syncOfflineMode = _sync <<< prop (Proxy :: _ "offlineMode")

_syncPendingSync :: Lens' State (Array CalendarItem)
_syncPendingSync = _sync <<< prop (Proxy :: _ "pendingSync")

_syncConflict :: Lens' State (Maybe (Array CalendarItem))
_syncConflict = _sync <<< prop (Proxy :: _ "syncConflict")

_syncUpdateError :: Lens' State (Maybe String)
_syncUpdateError = _sync <<< prop (Proxy :: _ "updateError")

_draggingId :: Lens' State (Maybe String)
_draggingId = _drag <<< prop (Proxy :: _ "draggingId")

_dragHoverIndex :: Lens' State (Maybe Int)
_dragHoverIndex = _drag <<< prop (Proxy :: _ "dragHoverIndex")

_dragOffsetMinutes :: Lens' State (Maybe Int)
_dragOffsetMinutes = _drag <<< prop (Proxy :: _ "dragOffsetMinutes")

_notificationDefaults :: Lens' State NotificationDefaults
_notificationDefaults = _notifications <<< prop (Proxy :: _ "notificationDefaults")

_notificationOverrides :: Lens' State (Array NotificationOverride)
_notificationOverrides = _notifications <<< prop (Proxy :: _ "notificationOverrides")

_notificationPanelOpen :: Lens' State Boolean
_notificationPanelOpen = _notifications <<< prop (Proxy :: _ "notificationPanelOpen")

_notificationEditor :: Lens' State (Maybe NotificationEditor)
_notificationEditor = _notifications <<< prop (Proxy :: _ "notificationEditor")

_templatesList :: Lens' State (Array TaskTemplate)
_templatesList = _templates <<< prop (Proxy :: _ "templates")

_templateDraft :: Lens' State TemplateDraft
_templateDraft = _templates <<< prop (Proxy :: _ "templateDraft")

_editingTemplateId :: Lens' State (Maybe String)
_editingTemplateId = _templates <<< prop (Proxy :: _ "editingTemplateId")

_csvInput :: Lens' State String
_csvInput = _imports <<< prop (Proxy :: _ "csvInput")

_csvImportResult :: Lens' State (Maybe CsvImportResult)
_csvImportResult = _imports <<< prop (Proxy :: _ "csvImportResult")

_icsInput :: Lens' State String
_icsInput = _imports <<< prop (Proxy :: _ "icsInput")

_icsImportResult :: Lens' State (Maybe IcsImportResult)
_icsImportResult = _imports <<< prop (Proxy :: _ "icsImportResult")

_exportFormat :: Lens' State ExportFormat
_exportFormat = _exports <<< prop (Proxy :: _ "exportFormat")

_exportTypeFilter :: Lens' State String
_exportTypeFilter = _exports <<< prop (Proxy :: _ "exportTypeFilter")

_exportStatusFilter :: Lens' State String
_exportStatusFilter = _exports <<< prop (Proxy :: _ "exportStatusFilter")

_exportCategoryFilter :: Lens' State String
_exportCategoryFilter = _exports <<< prop (Proxy :: _ "exportCategoryFilter")

_exportStartDate :: Lens' State String
_exportStartDate = _exports <<< prop (Proxy :: _ "exportStartDate")

_exportEndDate :: Lens' State String
_exportEndDate = _exports <<< prop (Proxy :: _ "exportEndDate")

_exportOutput :: Lens' State String
_exportOutput = _exports <<< prop (Proxy :: _ "exportOutput")

_viewMode :: Lens' State AgendaView
_viewMode = _view <<< prop (Proxy :: _ "viewMode")

_viewFocusDate :: Lens' State String
_viewFocusDate = _view <<< prop (Proxy :: _ "focusDate")

_viewActiveModal :: Lens' State (Maybe AgendaModal)
_viewActiveModal = _view <<< prop (Proxy :: _ "activeModal")

_viewValidationPanel :: Lens' State (Maybe ValidationPanel)
_viewValidationPanel = _view <<< prop (Proxy :: _ "validationPanel")

data Action
  = InitAction InitAction
  | DataAction DataAction
  | SyncAction SyncAction
  | DragAction DragAction
  | ViewAction ViewAction
  | NotificationAction NotificationAction
  | TemplateAction TemplateAction
  | ImportAction ImportAction
  | ExportAction ExportAction

data InitAction = InitInitialize

data DataAction
  = DataDraftTitleChanged String
  | DataDraftStartChanged String
  | DataDraftEndChanged String
  | DataDraftCategoryChanged String
  | DataToggleConflictFilter
  | DataSortChanged String
  | DataOpenConflictResolution (Array String)
  | DataChooseResolutionStrategy ResolutionStrategy
  | DataConfirmResolution
  | DataCancelResolution

data SyncAction
  = SyncDraftTitleKeyDown String
  | SyncSubmitIntention
  | SyncPlanifyFrom String CalendarItemContent
  | SyncToggleOffline
  | SyncResolveKeepLocal
  | SyncResolveDiscardLocal
  | SyncDismissUpdateError

data DragAction
  = DragStart String DragEvent
  | DragOver String DragEvent
  | DropOn String
  | DragEnd
  | DragOverCalendar DragEvent
  | DropOnCalendar DragEvent

data ViewAction
  = ViewOpenValidation String CalendarItemContent
  | ViewValidationMinutesChanged String
  | ViewConfirmValidation
  | ViewCancelValidation
  | ViewChangedAction String
  | ViewFocusDateChanged String
  | ViewOpenModal AgendaModal
  | ViewCloseModal

data NotificationAction
  = NotificationTogglePanel
  | NotificationDefaultStartTimeChanged String
  | NotificationDefaultBeforeEndChanged String
  | NotificationOpenEditor String
  | NotificationStartTimeChanged String
  | NotificationBeforeEndChanged String
  | NotificationSaveOverride
  | NotificationResetOverride String
  | NotificationCancelOverride

data TemplateAction
  = TemplateTitleChangedAction String
  | TemplateDurationChangedAction String
  | TemplateCategoryChangedAction String
  | TemplateSubmit
  | TemplateEdit String
  | TemplateCancelEdit
  | TemplateDelete String
  | TemplateUse String

data ImportAction
  = ImportCsvInputChanged String
  | ImportParseCsvInput
  | ImportApplyCsv
  | ImportClearCsv
  | ImportIcsInputChanged String
  | ImportParseIcsInput
  | ImportApplyIcs
  | ImportClearIcs

data ExportAction
  = ExportFormatChangedAction String
  | ExportTypeFilterChangedAction String
  | ExportStatusFilterChangedAction String
  | ExportCategoryFilterChangedAction String
  | ExportStartDateChangedAction String
  | ExportEndDateChangedAction String
  | ExportGenerate
  | ExportClearOutput

data AgendaModal
  = ModalNotifications
  | ModalTemplates
  | ModalImportCsv
  | ModalImportIcs
  | ModalExport
  | ModalTools
  | ModalFilters
  | ModalDateTime

derive instance eqAgendaModal :: Eq AgendaModal

component :: forall q i. H.Component q i NoOutput Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = pure (InitAction InitInitialize)
        }
    }

initialState :: forall i. i -> State
initialState = const
  { data:
      { items: []
      , draft: emptyDraft
      , validationError: Nothing
      , showConflictsOnly: false
      , conflictResolution: Nothing
      , sortMode: SortByTime
      }
  , sync:
      { offlineMode: false
      , pendingSync: []
      , syncConflict: Nothing
      , updateError: Nothing
      }
  , drag:
      { draggingId: Nothing
      , dragHoverIndex: Nothing
      , dragOffsetMinutes: Nothing
      }
  , notifications:
      { notificationDefaults: defaultNotificationDefaults
      , notificationOverrides: []
      , notificationPanelOpen: false
      , notificationEditor: Nothing
      }
  , templates:
      { templates: []
      , templateDraft: emptyTemplateDraft
      , editingTemplateId: Nothing
      }
  , imports:
      { csvInput: ""
      , csvImportResult: Nothing
      , icsInput: ""
      , icsImportResult: Nothing
      }
  , exports:
      { exportFormat: ExportCSV
      , exportTypeFilter: ""
      , exportStatusFilter: ""
      , exportCategoryFilter: ""
      , exportStartDate: ""
      , exportEndDate: ""
      , exportOutput: ""
      }
  , view:
      { viewMode: ViewDay
      , focusDate: ""
      , activeModal: Nothing
      , validationPanel: Nothing
      }
  }

handleAction :: Action -> AgendaAppM Unit
handleAction action = handleError $
  case action of
    InitAction initAction -> handleInitAction initAction
    DataAction dataAction -> handleDataAction dataAction
    SyncAction syncAction -> handleSyncAction syncAction
    DragAction dragAction -> handleDragAction dragAction
    ViewAction viewAction -> handleViewAction viewAction
    NotificationAction notificationAction -> handleNotificationAction notificationAction
    TemplateAction templateAction -> handleTemplateAction templateAction
    ImportAction importAction -> handleImportAction importAction
    ExportAction exportAction -> handleExportAction exportAction

handleInitAction :: InitAction -> ErrorAgendaAppM Unit
handleInitAction = case _ of
  InitInitialize -> do
    now <- liftEffect nowDateTime
    lift $ modify_ (_viewFocusDate .~ formatDate now)
    refreshItems

handleDataAction :: DataAction -> ErrorAgendaAppM Unit
handleDataAction action =
  lift $ modify_ (_data %~ applyDataAction action)

applyDataAction :: DataAction -> DataState -> DataState
applyDataAction action dataState =
  case action of
    DataDraftTitleChanged title ->
      dataState
        { draft = dataState.draft { title = title }
        , validationError = Nothing
        }
    DataDraftStartChanged windowStart ->
      dataState
        { draft = dataState.draft { windowStart = windowStart }
        , validationError = Nothing
        }
    DataDraftEndChanged windowEnd ->
      dataState
        { draft = dataState.draft { windowEnd = windowEnd }
        , validationError = Nothing
        }
    DataDraftCategoryChanged category ->
      dataState
        { draft = dataState.draft { category = category }
        , validationError = Nothing
        }
    DataToggleConflictFilter ->
      dataState { showConflictsOnly = not dataState.showConflictsOnly }
    DataSortChanged raw ->
      dataState { sortMode = parseSortMode raw }
    DataOpenConflictResolution groupIds ->
      dataState { conflictResolution = Just { groupIds, pendingStrategy: Nothing } }
    DataChooseResolutionStrategy strategy ->
      dataState
        { conflictResolution =
            map (\res -> res { pendingStrategy = Just strategy }) dataState.conflictResolution
        }
    DataConfirmResolution ->
      dataState { conflictResolution = Nothing }
    DataCancelResolution ->
      dataState { conflictResolution = Nothing }

handleSyncAction :: SyncAction -> ErrorAgendaAppM Unit
handleSyncAction = case _ of
  SyncDraftTitleKeyDown key ->
    when (key == "Enter") submitIntention
  SyncSubmitIntention ->
    submitIntention
  SyncPlanifyFrom sourceId content -> do
    st <- get
    let item = toScheduledBlock sourceId content
    if st ^. _syncOfflineMode then do
      let
        result = applyOfflineMutation true item (st ^. _dataItems) (st ^. _syncPendingSync)
      lift $ modify_ ((_dataItems .~ result.items) <<< (_syncPendingSync .~ result.pending))
    else do
      _ <- createItem item
      refreshItems
  SyncToggleOffline -> do
    st <- get
    if st ^. _syncOfflineMode then do
      lift $ modify_ (_syncOfflineMode .~ false)
      syncPending
    else lift $ modify_ (_syncOfflineMode .~ true)
  SyncResolveKeepLocal ->
    lift $ modify_ ((_syncConflict .~ Nothing) <<< (_syncOfflineMode .~ true))
  SyncResolveDiscardLocal -> do
    lift $ modify_ ((_syncConflict .~ Nothing) <<< (_syncPendingSync .~ []))
    refreshItems
  SyncDismissUpdateError ->
    lift $ modify_ (_syncUpdateError .~ Nothing)

handleDragAction :: DragAction -> ErrorAgendaAppM Unit
handleDragAction = case _ of
  DragStart itemId ev -> do
    st <- get
    let
      duration =
        find
          ( \item -> case item of
              ServerCalendarItem { id } -> id == itemId
              _ -> false
          )
          (st ^. _dataItems) >>= \item ->
          durationMinutesBetween (calendarItemContent item).windowStart (calendarItemContent item).windowEnd
    offset <- liftEffect $ dragOffsetFromEvent ev duration
    lift $ modify_ ((_draggingId .~ Just itemId) <<< (_dragOffsetMinutes .~ offset))
  DragOver _ ev ->
    liftEffect $ preventDefault (toEvent ev)
  DropOn targetId -> do
    st <- get
    case st ^. _draggingId of
      Nothing -> pure unit
      Just draggingId -> do
        let reordered = moveItemBefore draggingId targetId (st ^. _dataItems)
        lift $ modify_ ((_dataItems .~ reordered) <<< (_draggingId .~ Nothing))
  DragEnd ->
    lift $ modify_
      ((_draggingId .~ Nothing) <<< (_dragHoverIndex .~ Nothing) <<< (_dragOffsetMinutes .~ Nothing))
  DragOverCalendar ev -> do
    liftEffect $ preventDefault (toEvent ev)
    idx <- liftEffect $ dragMinuteIndexFromEvent ev
    st <- get
    let
      offset = fromMaybe 0 (st ^. _dragOffsetMinutes)
      adjusted = idx <#> \minuteIndex -> computeDropMinuteIndex minuteIndex offset
    lift $ modify_ (_dragHoverIndex .~ adjusted)
  DropOnCalendar ev -> do
    liftEffect $ preventDefault (toEvent ev)
    idx <- liftEffect $ dragMinuteIndexFromEvent ev
    st <- get
    case st ^. _draggingId of
      Nothing -> pure unit
      Just draggingId -> do
        let
          baseDateTime = st ^. _viewFocusDate <> "T00:00"
          offset = fromMaybe 0 (st ^. _dragOffsetMinutes)
          minuteIndex = fromMaybe 0 idx
          adjustedIndex = computeDropMinuteIndex minuteIndex offset
          totalMinutes = indexToMinutes adjustedIndex
          hour = Int.quot totalMinutes 60
          minute = Int.rem totalMinutes 60
          newStart = combineDateWithTime baseDateTime (pad2 hour <> ":" <> pad2 minute)
          updated =
            newStart >>= \start ->
              find
                ( \item -> case item of
                    ServerCalendarItem { id } -> id == draggingId
                    _ -> false
                )
                (st ^. _dataItems) >>= \item ->
                durationMinutesBetween (calendarItemContent item).windowStart (calendarItemContent item).windowEnd >>= \mins ->
                  shiftMinutes mins start >>= \end ->
                    Just { start, end }
        case updated of
          Nothing ->
            lift $ modify_
              ((_draggingId .~ Nothing) <<< (_dragHoverIndex .~ Nothing) <<< (_dragOffsetMinutes .~ Nothing))
          Just { start, end } -> do
            let
              result = updateItemWindowById draggingId start end (st ^. _dataItems)
            case result.updated of
              Nothing ->
                lift $ modify_
                  ((_draggingId .~ Nothing) <<< (_dragHoverIndex .~ Nothing) <<< (_dragOffsetMinutes .~ Nothing))
              Just updatedItem -> do
                if st ^. _syncOfflineMode then
                  lift $ modify_
                    ( (_dataItems .~ result.items)
                        <<< (_syncPendingSync %~ upsertPendingItem updatedItem)
                        <<< (_syncUpdateError .~ Nothing)
                        <<< (_draggingId .~ Nothing)
                        <<< (_dragHoverIndex .~ Nothing)
                        <<< (_dragOffsetMinutes .~ Nothing)
                    )
                else do
                  resp <- updateItem draggingId updatedItem
                  if statusOk resp then do
                    lift $ modify_
                      ( (_draggingId .~ Nothing)
                          <<< (_dragHoverIndex .~ Nothing)
                          <<< (_dragOffsetMinutes .~ Nothing)
                          <<< (_syncUpdateError .~ Nothing)
                      )
                    refreshItems
                  else do
                    lift $ modify_
                      ( (_draggingId .~ Nothing)
                          <<< (_dragHoverIndex .~ Nothing)
                          <<< (_dragOffsetMinutes .~ Nothing)
                          <<< (_syncUpdateError .~ Just (updateErrorMessage (unwrap resp.status)))
                      )
                    refreshItems

handleViewAction :: ViewAction -> ErrorAgendaAppM Unit
handleViewAction = case _ of
  ViewOpenValidation itemId content -> do
    suggested <- liftEffect $ suggestDurationMinutes content.windowStart
    lift $ modify_ (_viewValidationPanel .~ Just { itemId, proposedMinutes: suggested, inputValue: "" })
  ViewValidationMinutesChanged raw ->
    lift $ modify_ (_viewValidationPanel %~ map (\panel -> panel { inputValue = raw }))
  ViewConfirmValidation -> do
    st <- get
    case st ^. _viewValidationPanel of
      Nothing -> pure unit
      Just panel -> do
        let duration = parsePositiveInt panel.inputValue <|> panel.proposedMinutes
        case duration of
          Nothing -> pure unit
          Just minutes -> do
            _ <- validateItem panel.itemId minutes
            lift $ modify_ (_viewValidationPanel .~ Nothing)
            refreshItems
  ViewCancelValidation ->
    lift $ modify_ (_viewValidationPanel .~ Nothing)
  ViewChangedAction raw ->
    lift $ modify_ (_viewMode .~ parseAgendaView raw)
  ViewFocusDateChanged raw ->
    lift $ modify_ (_viewFocusDate .~ raw)
  ViewOpenModal modal ->
    lift $ modify_ (_viewActiveModal .~ Just modal)
  ViewCloseModal ->
    lift $ modify_ (_viewActiveModal .~ Nothing)

handleNotificationAction :: NotificationAction -> ErrorAgendaAppM Unit
handleNotificationAction = case _ of
  NotificationTogglePanel ->
    lift $ modify_ (_notificationPanelOpen %~ not)
  NotificationDefaultStartTimeChanged raw ->
    if isTimeLocal raw then lift $ modify_ (_notificationDefaults %~ _ { startDayTime = raw })
    else pure unit
  NotificationDefaultBeforeEndChanged raw ->
    case parsePositiveInt raw of
      Just hours -> lift $ modify_ (_notificationDefaults %~ _ { beforeEndHours = hours })
      Nothing -> pure unit
  NotificationOpenEditor itemId -> do
    st <- get
    let
      existing = lookupNotificationOverride itemId (st ^. _notificationOverrides)
      startTime =
        fromMaybe (st ^. _notificationDefaults).startDayTime (existing >>= _.startDayTime)
      beforeEnd =
        fromMaybe (st ^. _notificationDefaults).beforeEndHours (existing >>= _.beforeEndHours)
    lift $ modify_ (_notificationEditor .~ Just { itemId, startTime, beforeEndRaw: show beforeEnd })
  NotificationStartTimeChanged raw ->
    lift $ modify_ (_notificationEditor %~ map (\editor -> editor { startTime = raw }))
  NotificationBeforeEndChanged raw ->
    lift $ modify_ (_notificationEditor %~ map (\editor -> editor { beforeEndRaw = raw }))
  NotificationSaveOverride -> do
    st <- get
    case st ^. _notificationEditor of
      Nothing -> pure unit
      Just editor -> do
        let
          cleanedTime = if isTimeLocal editor.startTime then Just editor.startTime else Nothing
          cleanedHours = parsePositiveInt editor.beforeEndRaw
        lift $ modify_
          ( (_notificationOverrides %~ upsertNotificationOverride editor.itemId cleanedTime cleanedHours)
              <<< (_notificationEditor .~ Nothing)
          )
  NotificationResetOverride itemId ->
    lift $ modify_
      ((_notificationOverrides %~ removeNotificationOverride itemId) <<< (_notificationEditor .~ Nothing))
  NotificationCancelOverride ->
    lift $ modify_ (_notificationEditor .~ Nothing)

handleTemplateAction :: TemplateAction -> ErrorAgendaAppM Unit
handleTemplateAction = case _ of
  TemplateTitleChangedAction title ->
    lift $ modify_ (_templateDraft %~ _ { title = title })
  TemplateDurationChangedAction duration ->
    lift $ modify_ (_templateDraft %~ _ { durationMinutes = duration })
  TemplateCategoryChangedAction category ->
    lift $ modify_ (_templateDraft %~ _ { category = category })
  TemplateSubmit -> do
    st <- get
    let
      draft = st ^. _templateDraft
      duration = parsePositiveInt draft.durationMinutes
    case duration of
      Nothing -> pure unit
      Just minutes -> do
        let
          template =
            { id: fromMaybe "" (st ^. _editingTemplateId)
            , title: StringCommon.trim draft.title
            , durationMinutes: minutes
            , category: draft.category
            }
        if template.title == "" then pure unit
        else do
          let
            nextTemplates =
              case st ^. _editingTemplateId of
                Nothing -> addTemplate template (st ^. _templatesList)
                Just _ -> updateTemplate template (st ^. _templatesList)
          lift $ modify_
            ( (_templatesList .~ nextTemplates)
                <<< (_templateDraft .~ emptyTemplateDraft)
                <<< (_editingTemplateId .~ Nothing)
            )
  TemplateEdit templateId -> do
    st <- get
    case find (\tpl -> tpl.id == templateId) (st ^. _templatesList) of
      Nothing -> pure unit
      Just template ->
        lift $ modify_
          ( ( _templateDraft .~
                { title: template.title
                , durationMinutes: show template.durationMinutes
                , category: template.category
                }
            )
              <<< (_editingTemplateId .~ Just template.id)
          )
  TemplateCancelEdit ->
    lift $ modify_
      ((_templateDraft .~ emptyTemplateDraft) <<< (_editingTemplateId .~ Nothing))
  TemplateDelete templateId ->
    lift $ modify_
      ( (_templatesList %~ removeTemplate templateId)
          <<< (_templateDraft .~ emptyTemplateDraft)
          <<< (_editingTemplateId .~ Nothing)
      )
  TemplateUse templateId -> do
    st <- get
    case find (\tpl -> tpl.id == templateId) (st ^. _templatesList) of
      Nothing -> pure unit
      Just template -> do
        now <- liftEffect nowDateTime
        let startStr = formatDateTimeLocal now
        let endStr = fromMaybe startStr (shiftMinutes template.durationMinutes startStr)
        lift $ modify_ (_dataDraft .~ applyTemplateToDraft template startStr endStr)

handleImportAction :: ImportAction -> ErrorAgendaAppM Unit
handleImportAction = case _ of
  ImportCsvInputChanged raw ->
    lift $ modify_ (_csvInput .~ raw)
  ImportParseCsvInput -> do
    st <- get
    let result = parseCsvImport (st ^. _csvInput)
    lift $ modify_ (_csvImportResult .~ Just result)
  ImportApplyCsv -> do
    st <- get
    case st ^. _csvImportResult of
      Nothing -> pure unit
      Just result ->
        if null result.items then pure unit
        else if st ^. _syncOfflineMode then do
          let
            initial = { items: st ^. _dataItems, pending: st ^. _syncPendingSync }
            final = foldl (\acc item -> applyOfflineMutation true item acc.items acc.pending) initial result.items
          lift $ modify_
            ( (_dataItems .~ final.items)
                <<< (_syncPendingSync .~ final.pending)
                <<< (_csvInput .~ "")
                <<< (_csvImportResult .~ Nothing)
            )
        else
          lift $ modify_
            ( (_dataItems %~ (_ <> result.items))
                <<< (_csvInput .~ "")
                <<< (_csvImportResult .~ Nothing)
            )
  ImportClearCsv ->
    lift $ modify_ ((_csvInput .~ "") <<< (_csvImportResult .~ Nothing))
  ImportIcsInputChanged raw ->
    lift $ modify_ (_icsInput .~ raw)
  ImportParseIcsInput -> do
    st <- get
    let result = parseIcsImport (st ^. _icsInput)
    lift $ modify_ (_icsImportResult .~ Just result)
  ImportApplyIcs -> do
    st <- get
    case st ^. _icsImportResult of
      Nothing -> pure unit
      Just result ->
        if null result.items then pure unit
        else if st ^. _syncOfflineMode then do
          let
            initial = { items: st ^. _dataItems, pending: st ^. _syncPendingSync }
            final = foldl (\acc item -> applyOfflineMutation true item acc.items acc.pending) initial result.items
          lift $ modify_
            ( (_dataItems .~ final.items)
                <<< (_syncPendingSync .~ final.pending)
                <<< (_icsInput .~ "")
                <<< (_icsImportResult .~ Nothing)
            )
        else
          lift $ modify_
            ( (_dataItems %~ (_ <> result.items))
                <<< (_icsInput .~ "")
                <<< (_icsImportResult .~ Nothing)
            )
  ImportClearIcs ->
    lift $ modify_ ((_icsInput .~ "") <<< (_icsImportResult .~ Nothing))

handleExportAction :: ExportAction -> ErrorAgendaAppM Unit
handleExportAction = case _ of
  ExportFormatChangedAction raw ->
    lift $ modify_ (_exportFormat .~ parseExportFormat raw)
  ExportTypeFilterChangedAction raw ->
    lift $ modify_ (_exportTypeFilter .~ raw)
  ExportStatusFilterChangedAction raw ->
    lift $ modify_ (_exportStatusFilter .~ raw)
  ExportCategoryFilterChangedAction raw ->
    lift $ modify_ (_exportCategoryFilter .~ raw)
  ExportStartDateChangedAction raw ->
    lift $ modify_ (_exportStartDate .~ raw)
  ExportEndDateChangedAction raw ->
    lift $ modify_ (_exportEndDate .~ raw)
  ExportGenerate -> do
    st <- get
    let
      filter =
        { itemType: parseExportItemType (st ^. _exportTypeFilter)
        , status: parseExportStatus (st ^. _exportStatusFilter)
        , category: toOptionalString (st ^. _exportCategoryFilter)
        , startDate: toOptionalString (st ^. _exportStartDate)
        , endDate: toOptionalString (st ^. _exportEndDate)
        }
      filtered = filterItemsForExport filter (st ^. _dataItems)
      output =
        case st ^. _exportFormat of
          ExportCSV -> exportItemsToCsv filtered
          ExportICS -> exportItemsToIcs filtered
    lift $ modify_ (_exportOutput .~ output)
  ExportClearOutput ->
    lift $ modify_ (_exportOutput .~ "")

refreshItems :: ErrorAgendaAppM Unit
refreshItems = do
  jsonResponse <- withExceptT toFatalError $ ExceptT $ liftAff getItemsResponse
  items <- (_.body >>> decodeJson >>> lmap toFatalError >>> pure >>> ExceptT) jsonResponse
  lift $ modify_ (_dataItems .~ items)

createItem :: CalendarItem -> ErrorAgendaAppM (Response Json)
createItem item = withExceptT toFatalError $ ExceptT $ liftAff $ createItemResponse item

updateItem :: String -> CalendarItem -> ErrorAgendaAppM (Response Json)
updateItem itemId item = withExceptT toFatalError $ ExceptT $ liftAff $ updateItemResponse itemId item

validateItem :: String -> Int -> ErrorAgendaAppM (Response Json)
validateItem itemId minutes =
  withExceptT toFatalError $ ExceptT $ liftAff $ validateItemResponse itemId minutes

statusOk :: forall a. Response a -> Boolean
statusOk r = unwrap r.status >= 200 && unwrap r.status < 300

syncPending :: ErrorAgendaAppM Unit
syncPending = do
  st <- get
  if null (st ^. _syncPendingSync) then refreshItems
  else do
    ok <- foldM
      ( \acc item ->
          if not acc then pure false
          else do
            resp <- createItem item
            pure (statusOk resp)
      )
      true
      (st ^. _syncPendingSync)
    if ok then do
      lift $ modify_ ((_syncPendingSync .~ []) <<< (_syncConflict .~ Nothing))
      refreshItems
    else lift $ modify_ (_syncConflict .~ Just (st ^. _syncPendingSync))

submitIntention :: ErrorAgendaAppM Unit
submitIntention = do
  st <- get
  case validateIntention (st ^. _dataDraft) of
    Left err -> lift $ modify_ (_dataValidationError .~ Just err)
    Right validDraft -> do
      let item = toNewIntention validDraft
      if st ^. _syncOfflineMode then do
        let
          result = applyOfflineMutation true item (st ^. _dataItems) (st ^. _syncPendingSync)
        lift $ modify_
          ( (_dataItems .~ result.items)
              <<< (_dataDraft .~ emptyDraft)
              <<< (_dataValidationError .~ Nothing)
              <<< (_syncPendingSync .~ result.pending)
          )
      else do
        _ <- createItem item
        lift $ modify_ ((_dataDraft .~ emptyDraft) <<< (_dataValidationError .~ Nothing))
        refreshItems

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  let
    dataState = st ^. _data
    syncState = st ^. _sync
    dragState = st ^. _drag
    notifications = st ^. _notifications
    templates = st ^. _templates
    imports = st ^. _imports
    exports = st ^. _exports
    viewState = st ^. _view
    { items, draft, validationError, showConflictsOnly, conflictResolution, sortMode } = dataState
    { offlineMode, syncConflict, updateError } = syncState
    { draggingId, dragHoverIndex } = dragState
    { notificationDefaults, notificationOverrides, notificationPanelOpen, notificationEditor } = notifications
    { templates: templateItems, templateDraft, editingTemplateId } = templates
    { csvInput, csvImportResult, icsInput, icsImportResult } = imports
    { exportFormat, exportTypeFilter, exportStatusFilter, exportCategoryFilter, exportStartDate, exportEndDate, exportOutput } = exports
    { viewMode, focusDate, activeModal, validationPanel } = viewState
    conflictIds = detectConflictIds items
    conflictGroups = detectConflictGroups items
    itemsToShow =
      if showConflictsOnly then filter (isConflict conflictIds) items
      else items
    sortedItems = sortItems sortMode conflictIds itemsToShow
    unplannedIntentions = filter (isUnplannedIntention items) items
  in
    div [ class_ "entity-page agenda-page" ]
      [ section [ class_ "agenda-header" ]
          [ h2 [ class_ "agenda-title" ] [ text (viewTitle viewMode) ]
          , div [ class_ "agenda-subtitle" ] [ text "Capture rapide des intentions a planifier." ]
          , div [ class_ "agenda-controls" ]
              [ button
                  [ class_ $ "btn btn-sm agenda-filter" <> if showConflictsOnly then " btn-outline-primary" else " btn-outline-secondary"
                  , onClick (const (DataAction DataToggleConflictFilter))
                  ]
                  [ text "Filtrer: en conflit" ]
              , renderOfflineToggle offlineMode
              , renderSortPicker sortMode
              , renderConflictActions conflictGroups
              ]
          , renderViewSelector viewMode focusDate
          , renderMobileTools viewMode
          ]
      , div [ class_ $ "agenda-layout" <> if viewMode == ViewDay then " agenda-layout--calendar" else "" ]
          [ div [ class_ "agenda-main" ]
              [ maybe (text "") renderUpdateError updateError
              , maybe (text "") renderValidationPanel validationPanel
              , section [ class_ $ "agenda-list-panel" <> if viewMode == ViewDay then " agenda-list-panel--calendar" else "" ]
                  [ if viewMode == ViewDay then div [ class_ "agenda-calendar-form-header" ] [ renderForm draft validationError ]
                    else text ""
                  , renderAgendaView viewMode focusDate conflictIds sortedItems draggingId dragHoverIndex
                  ]
              , maybe (text "") (renderConflictResolution items) conflictResolution
              , maybe (text "") renderSyncConflict syncConflict
              ]
          , div [ class_ "agenda-side" ]
              [ renderNotificationsPanel notificationPanelOpen notificationDefaults notificationOverrides notificationEditor unplannedIntentions
              , renderAccordion "Templates de taches" "agenda-accordion templates" $ renderTemplatesPanel templateItems templateDraft editingTemplateId
              , renderAccordion "Import CSV" "agenda-accordion import-csv" $ renderCsvImportPanel csvInput csvImportResult
              , renderAccordion "Import ICS" "agenda-accordion import-ics" $ renderIcsImportPanel icsInput icsImportResult
              , renderAccordion "Export" "agenda-accordion export" $ renderExportPanel exportFormat exportTypeFilter exportStatusFilter exportCategoryFilter exportStartDate exportEndDate exportOutput
              ]
          ]
      , renderAgendaModals activeModal showConflictsOnly conflictGroups offlineMode sortMode notificationDefaults notificationOverrides notificationEditor draft unplannedIntentions templateItems templateDraft editingTemplateId csvInput csvImportResult icsInput icsImportResult exportFormat exportTypeFilter exportStatusFilter exportCategoryFilter exportStartDate exportEndDate exportOutput
      ]

renderAccordion :: forall w i. String -> String -> HTML w i -> HTML w i
renderAccordion title extraClass content =
  div [ class_ ("agenda-accordion-wrap " <> extraClass) ]
    [ details [ class_ "agenda-accordion-details" ]
        [ summary [ class_ "agenda-accordion-summary" ] [ text title ]
        , div [ class_ "agenda-accordion-content" ] [ content ]
        ]
    ]

renderForm :: forall w. IntentionDraft -> Maybe ValidationError -> HTML w Action
renderForm draft validationError =
  section [ class_ "agenda-form" ]
    [ input
        [ class_ "form-control agenda-input"
        , placeholder "Titre de l'intention"
        , onValueChange (DataAction <<< DataDraftTitleChanged)
        , onKeyDown (\ev -> SyncAction (SyncDraftTitleKeyDown (KE.key ev)))
        , value draft.title
        ]
    , div [ class_ "agenda-time-row" ]
        [ input
            [ class_ "form-control agenda-input"
            , type_ InputDatetimeLocal
            , attr (AttrName "lang") "fr"
            , placeholder "Debut"
            , onValueChange (DataAction <<< DataDraftStartChanged)
            , value draft.windowStart
            ]
        , input
            [ class_ "form-control agenda-input"
            , type_ InputDatetimeLocal
            , attr (AttrName "lang") "fr"
            , placeholder "Fin"
            , onValueChange (DataAction <<< DataDraftEndChanged)
            , value draft.windowEnd
            ]
        ]
    , input
        [ class_ "form-control agenda-input"
        , placeholder "Categorie (optionnelle)"
        , onValueChange (DataAction <<< DataDraftCategoryChanged)
        , value draft.category
        ]
    , div [ class_ "agenda-datetime-row" ]
        [ button
            [ class_ "btn btn-outline-secondary agenda-datetime-button"
            , onClick (const $ ViewAction (ViewOpenModal ModalDateTime))
            ]
            [ text "Dates & heures" ]
        , div [ class_ "agenda-datetime-summary" ]
            [ text $ summarizeDateRange draft.windowStart draft.windowEnd ]
        ]
    , maybe (text "") renderValidationError validationError
    , button [ class_ "btn btn-primary agenda-submit", onClick (const (SyncAction SyncSubmitIntention)) ] [ text "Creer l'intention" ]
    ]

renderDateTimeContent :: forall w. IntentionDraft -> HTML w Action
renderDateTimeContent draft =
  div [ class_ "agenda-modal-stack" ]
    [ div [ class_ "agenda-modal-field" ]
        [ div [ class_ "agenda-notifications-label" ] [ text "Debut" ]
        , input
            [ class_ "form-control agenda-input"
            , type_ InputDatetimeLocal
            , attr (AttrName "lang") "fr"
            , placeholder "Debut"
            , onValueChange (DataAction <<< DataDraftStartChanged)
            , value draft.windowStart
            ]
        ]
    , div [ class_ "agenda-modal-field" ]
        [ div [ class_ "agenda-notifications-label" ] [ text "Fin" ]
        , input
            [ class_ "form-control agenda-input"
            , type_ InputDatetimeLocal
            , attr (AttrName "lang") "fr"
            , placeholder "Fin"
            , onValueChange (DataAction <<< DataDraftEndChanged)
            , value draft.windowEnd
            ]
        ]
    ]

summarizeDateRange :: String -> String -> String
summarizeDateRange start end =
  if start == "" && end == "" then "Aucune date selectionnee"
  else if end == "" then "Debut: " <> formatDateTimeFr start
  else if start == "" then "Fin: " <> formatDateTimeFr end
  else formatDateTimeFr start <> " → " <> formatDateTimeFr end

formatDateTimeFr :: String -> String
formatDateTimeFr raw =
  if raw == "" then ""
  else
    let
      parts = StringCommon.split (Pattern "T") raw
      rawDatePart = fromMaybe raw (index parts 0)
      timePart = fromMaybe "" (index parts 1)
      dateFr = formatDateFr rawDatePart
      timeFr = if timePart == "" then "" else String.take 5 timePart
    in
      if timeFr == "" then dateFr else dateFr <> " " <> timeFr

formatDateFr :: String -> String
formatDateFr rawDate =
  case StringCommon.split (Pattern "-") rawDate of
    [ yearPart, monthPart, dayPart ] -> dayPart <> "/" <> monthPart <> "/" <> yearPart
    _ -> rawDate

renderValidationError :: forall w. ValidationError -> HTML w Action
renderValidationError err =
  div [ class_ "agenda-error" ]
    [ text $ case err of
        TitleEmpty -> "Le titre est obligatoire."
        WindowStartInvalid -> "La date de debut est invalide."
        WindowEndInvalid -> "La date de fin est invalide."
        WindowOrderInvalid -> "La fin doit etre apres le debut."
    ]

renderUpdateError :: forall w. String -> HTML w Action
renderUpdateError message =
  div [ class_ "agenda-error agenda-error--update" ]
    [ text message
    , button
        [ class_ "btn btn-sm btn-outline-secondary agenda-error-dismiss"
        , onClick (const (SyncAction SyncDismissUpdateError))
        ]
        [ text "OK" ]
    ]

updateErrorMessage :: Int -> String
updateErrorMessage status =
  "Echec de mise a jour de l'item (HTTP " <> show status <> ")."

emptyAgenda :: forall w i. HTML w i
emptyAgenda =
  div [ class_ "row entity-empty agenda-empty" ]
    [ div [ class_ "entity-empty-title" ] [ text "Aucune intention aujourd'hui" ]
    , div [ class_ "entity-empty-subtitle" ] [ text "Ajoutez une intention pour demarrer votre journee." ]
    , div [ class_ "agenda-empty-cta" ]
        [ span [ class_ "badge rounded-pill text-bg-primary" ] [ text "Astuce" ]
        , span [ class_ "text-muted" ] [ text "Commencez par un titre et appuyez sur Entrée." ]
        ]
    ]

renderAgendaView :: forall w. AgendaView -> String -> Array String -> Array CalendarItem -> Maybe String -> Maybe Int -> HTML w Action
renderAgendaView viewMode focusDate conflictIds items draggingId dragHoverIndex =
  case viewMode of
    ViewDay ->
      renderDayCalendar focusDate conflictIds items draggingId dragHoverIndex
    ViewWeek ->
      renderRangeView "Semaine" (generateDateRange focusDate 7) conflictIds items
    ViewMonth ->
      renderRangeView "Mois" (generateMonthDates focusDate) conflictIds items

type TimelineBlock =
  { item :: CalendarItem
  , startMin :: Int
  , endMin :: Int
  }

type TimelineLayout =
  { item :: CalendarItem
  , startMin :: Int
  , duration :: Int
  , columnIndex :: Int
  , columnCount :: Int
  }

renderDayCalendar :: forall w. String -> Array String -> Array CalendarItem -> Maybe String -> Maybe Int -> HTML w Action
renderDayCalendar focusDate conflictIds items draggingId dragHoverIndex =
  let
    itemsForDate = filter (isItemOnDate focusDate) items
    sorted = sortItems SortByTime conflictIds itemsForDate
    layout = buildTimelineLayout sorted
  in
    if null itemsForDate then emptyAgenda
    else
      div [ class_ "agenda-calendar" ]
        [ div [ class_ "agenda-calendar-header" ]
            [ div [ class_ "agenda-calendar-title" ] [ text focusDate ]
            , div [ class_ "agenda-calendar-count" ] [ text $ show (length itemsForDate) <> " items" ]
            ]
        , div [ class_ "agenda-calendar-body" ]
            [ div [ class_ "agenda-calendar-hours" ]
                (map renderHourLabel (enumFromTo 0 23) <> [ renderHourLabelEnd ])
            , div
                [ class_ "agenda-calendar-grid"
                , onDragOver (\ev -> DragAction (DragOverCalendar ev))
                , onDrop (\ev -> DragAction (DropOnCalendar ev))
                ]
                [ div [ class_ "agenda-calendar-lines" ]
                    (map renderHourLine (enumFromTo 0 23))
                , maybe (text "") renderDropIndicator dragHoverIndex
                , div
                    [ class_ $ "agenda-calendar-items" <> if draggingId == Nothing then "" else " agenda-calendar-items--dragging" ]
                    (map (renderTimelineItem conflictIds) layout)
                ]
            ]
        ]

renderHourLabel :: forall w i. Int -> HTML w i
renderHourLabel h =
  div [ class_ "agenda-calendar-hour" ] [ text $ pad2 h <> ":00" ]

renderHourLabelEnd :: forall w i. HTML w i
renderHourLabelEnd =
  div [ class_ "agenda-calendar-hour agenda-calendar-hour--end" ] [ text "24:00" ]

renderHourLine :: forall w. Int -> HTML w Action
renderHourLine _ =
  div
    [ class_ "agenda-calendar-line" ]
    []

renderDropIndicator :: forall w. Int -> HTML w Action
renderDropIndicator idx =
  let
    totalMinutes = indexToMinutes idx
    label = indexToTimeLabel idx
    inlineStyle = "top: calc(" <> show totalMinutes <> " * var(--agenda-minute-height));"
  in
    div
      [ class_ "agenda-calendar-drop-indicator"
      , style inlineStyle
      ]
      [ div [ class_ "agenda-calendar-drop-label" ] [ text label ] ]

renderTimelineItem :: forall w. Array String -> TimelineLayout -> HTML w Action
renderTimelineItem conflictIds layout =
  let
    content = calendarItemContent layout.item
    typeClass =
      case content.itemType of
        ScheduledBlock -> " agenda-calendar-item--scheduled"
        Intention -> " agenda-calendar-item--intention"
    conflictClass = if isConflict conflictIds layout.item then " agenda-calendar-item--conflict" else ""
    inlineStyle =
      " --start:" <> show layout.startMin <> ";"
        <> " --duration:"
        <> show layout.duration
        <> ";"
        <> " --column:"
        <> show layout.columnIndex
        <> ";"
        <> " --columns:"
        <> show layout.columnCount
        <> ";"
    dragProps = dragCalendarHandlers layout.item
  in
    div
      ( [ class_ $ "agenda-calendar-item" <> typeClass <> conflictClass
        , style inlineStyle
        ] <> dragProps
      )
      [ div [ class_ "agenda-calendar-meta" ]
          [ div [ class_ "agenda-calendar-item-time" ]
              [ text $ timeLabel content.windowStart <> " → " <> timeLabel content.windowEnd ]
          , div [ class_ "agenda-calendar-item-title" ] [ text content.title ]
          , div [ class_ "agenda-calendar-footer" ]
              [ renderCategory content.category
              , div [ class_ "agenda-calendar-actions" ]
                  [ renderValidationAction layout.item content
                  , renderPlanifyAction layout.item content
                  ]
              ]
          ]
      ]

buildTimelineLayout :: Array CalendarItem -> Array TimelineLayout
buildTimelineLayout items =
  let
    blocks = sortBy compareStart (mapMaybe toTimelineBlock items)
    groups = groupTimelineBlocks blocks
  in
    foldl (\acc group -> acc <> assignColumns group) [] groups
  where
  compareStart a b = compare a.startMin b.startMin

groupTimelineBlocks :: Array TimelineBlock -> Array (Array TimelineBlock)
groupTimelineBlocks blocks =
  case uncons blocks of
    Nothing -> []
    Just { head, tail } -> go tail [ head ] head.endMin []
  where
  go remaining current maxEnd acc =
    case uncons remaining of
      Nothing -> acc <> [ current ]
      Just { head: next, tail } ->
        if next.startMin < maxEnd then
          go tail (current <> [ next ]) (max maxEnd next.endMin) acc
        else
          go tail [ next ] next.endMin (acc <> [ current ])

assignColumns :: Array TimelineBlock -> Array TimelineLayout
assignColumns group =
  let
    sorted = sortBy (\a b -> compare a.startMin b.startMin) group
    initial = { columns: [], maxColumns: 0, placements: [] }
    step acc block =
      let
        openIndex = findIndex (\endMin -> endMin <= block.startMin) acc.columns
        columnIndex = case openIndex of
          Just idx -> idx
          Nothing -> length acc.columns
        columns' = case openIndex of
          Just idx -> fromMaybe acc.columns (updateAt idx block.endMin acc.columns)
          Nothing -> acc.columns <> [ block.endMin ]
        maxColumns' = max acc.maxColumns (length columns')
        placements' = acc.placements <> [ { block, columnIndex } ]
      in
        { columns: columns', maxColumns: maxColumns', placements: placements' }
    result = foldl step initial sorted
  in
    map
      ( \placement ->
          { item: placement.block.item
          , startMin: placement.block.startMin
          , duration: max 1 (placement.block.endMin - placement.block.startMin)
          , columnIndex: placement.columnIndex
          , columnCount: result.maxColumns
          }
      )
      result.placements

toTimelineBlock :: CalendarItem -> Maybe TimelineBlock
toTimelineBlock item = do
  let content = calendarItemContent item
  startMin <- minuteOfDay content.windowStart
  endMinRaw <- minuteOfDay content.windowEnd
  let
    startClamped = clamp 0 1439 startMin
    endAdjusted = if endMinRaw <= startMin then 1440 else endMinRaw
    endClamped = clamp (startClamped + 1) 1440 endAdjusted
  if endClamped <= 0 || startClamped >= 1440 then Nothing
  else Just { item, startMin: startClamped, endMin: endClamped }

minuteOfDay :: String -> Maybe Int
minuteOfDay raw = do
  dt <- parseDateTimeLocal raw
  let t = time dt
  pure $ (fromEnum (hour t) * 60) + fromEnum (minute t)

renderRangeView :: forall w. String -> Array String -> Array String -> Array CalendarItem -> HTML w Action
renderRangeView label dates conflictIds items =
  if null dates then emptyAgendaRange label
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
      , if null sorted then div [ class_ "agenda-date-empty" ] [ text "Aucun item" ]
        else agendaList conflictIds sorted
      ]

emptyAgendaRange :: forall w i. String -> HTML w i
emptyAgendaRange label =
  div [ class_ "row entity-empty agenda-empty" ]
    [ div [ class_ "entity-empty-title" ] [ text $ "Aucun item sur la " <> label ]
    , div [ class_ "entity-empty-subtitle" ] [ text "Ajoutez une intention pour demarrer." ]
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
  button [ class_ "btn btn-sm btn-outline-primary agenda-planify", onClick (const $ SyncAction (SyncPlanifyFrom id content)) ]
    [ text "Planifier" ]
renderPlanifyAction _ _ = text ""

renderValidationAction :: forall w. CalendarItem -> CalendarItemContent -> HTML w Action
renderValidationAction (ServerCalendarItem { id, content }) _ | content.status /= Fait =
  button [ class_ "btn btn-sm btn-outline-success agenda-validate", onClick (const $ ViewAction (ViewOpenValidation id content)) ]
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
      [ renderPanelHeader
          "agenda-notifications"
          "Rappels des intentions non planifiees"
          "Les rappels par defaut s'appliquent aux intentions non planifiees."
          [ button
              [ class_ $ "btn btn-sm agenda-notifications-toggle" <> if isOpen then " btn-outline-primary" else " btn-outline-secondary"
              , onClick (const (NotificationAction NotificationTogglePanel))
              ]
              [ text $ if isOpen then "Masquer" else "Configurer" ]
          ]
      , if isOpen then renderNotificationDefaults defaults else text ""
      , if isOpen then renderNotificationList defaults overrides editor intentions else text ""
      ]

renderNotificationsContent :: forall w. NotificationDefaults -> Array NotificationOverride -> Maybe NotificationEditor -> Array CalendarItem -> HTML w Action
renderNotificationsContent defaults overrides editor intentions =
  if null intentions then
    div [ class_ "agenda-modal-empty" ]
      [ text "Aucune intention non planifiee." ]
  else
    div [ class_ "agenda-notifications-modal" ]
      [ renderNotificationDefaults defaults
      , renderNotificationList defaults overrides editor intentions
      ]

renderFiltersContent :: forall w. Boolean -> Array (Array String) -> Boolean -> SortMode -> HTML w Action
renderFiltersContent showConflictsOnly conflictGroups offlineMode sortMode =
  div [ class_ "agenda-modal-stack" ]
    [ button
        [ class_ $ "btn btn-sm agenda-filter" <> if showConflictsOnly then " btn-outline-primary" else " btn-outline-secondary"
        , onClick (const (DataAction DataToggleConflictFilter))
        ]
        [ text "Filtrer: en conflit" ]
    , renderOfflineToggle offlineMode
    , renderSortPicker sortMode
    , renderConflictActions conflictGroups
    ]

renderToolsContent :: forall w. HTML w Action
renderToolsContent =
  div [ class_ "agenda-modal-stack" ]
    [ button [ class_ "btn btn-outline-secondary", onClick (const $ ViewAction (ViewOpenModal ModalNotifications)) ] [ text "Rappels" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const $ ViewAction (ViewOpenModal ModalTemplates)) ] [ text "Templates" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const $ ViewAction (ViewOpenModal ModalImportCsv)) ] [ text "Import CSV" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const $ ViewAction (ViewOpenModal ModalImportIcs)) ] [ text "Import ICS" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const $ ViewAction (ViewOpenModal ModalExport)) ] [ text "Export" ]
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
                , onValueChange (NotificationAction <<< NotificationDefaultStartTimeChanged)
                ]
            ]
        , div [ class_ "agenda-notifications-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Avant fin (heures)" ]
            , input
                [ class_ "form-control agenda-input"
                , type_ InputNumber
                , value (show defaults.beforeEndHours)
                , onValueChange (NotificationAction <<< NotificationDefaultBeforeEndChanged)
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
                      , onClick (const $ NotificationAction (NotificationOpenEditor id))
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
                , onValueChange (NotificationAction <<< NotificationStartTimeChanged)
                ]
            ]
        , div [ class_ "agenda-notifications-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Avant fin (heures)" ]
            , input
                [ class_ "form-control agenda-input"
                , type_ InputNumber
                , value editor.beforeEndRaw
                , onValueChange (NotificationAction <<< NotificationBeforeEndChanged)
                ]
            ]
        ]
    , div [ class_ "agenda-notification-editor-actions" ]
        [ button [ class_ "btn btn-sm btn-success", onClick (const (NotificationAction NotificationSaveOverride)) ] [ text "Enregistrer" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (NotificationAction NotificationCancelOverride)) ] [ text "Annuler" ]
        , button [ class_ "btn btn-sm btn-outline-danger", onClick (const $ NotificationAction (NotificationResetOverride itemId)) ] [ text "Reinitialiser" ]
        ]
    ]

renderTemplatesPanel :: forall w. Array TaskTemplate -> TemplateDraft -> Maybe String -> HTML w Action
renderTemplatesPanel templates draft editingId =
  section [ class_ "agenda-templates" ]
    [ renderPanelHeader
        "agenda-templates"
        "Templates de taches"
        "Creez des templates reutilisables pour accelerer la saisie."
        []
    , div [ class_ "agenda-templates-form" ]
        [ input
            [ class_ "form-control agenda-input"
            , placeholder "Titre du template"
            , value draft.title
            , onValueChange (TemplateAction <<< TemplateTitleChangedAction)
            ]
        , div [ class_ "agenda-templates-row" ]
            [ input
                [ class_ "form-control agenda-input"
                , type_ InputNumber
                , placeholder "Duree (minutes)"
                , value draft.durationMinutes
                , onValueChange (TemplateAction <<< TemplateDurationChangedAction)
                ]
            , input
                [ class_ "form-control agenda-input"
                , placeholder "Categorie (optionnelle)"
                , value draft.category
                , onValueChange (TemplateAction <<< TemplateCategoryChangedAction)
                ]
            ]
        , div [ class_ "agenda-templates-actions" ]
            ( [ button
                  [ class_ "btn btn-sm btn-primary"
                  , onClick (const (TemplateAction TemplateSubmit))
                  ]
                  [ text $ if editingId == Nothing then "Ajouter" else "Mettre a jour" ]
              ] <>
                if editingId == Nothing then []
                else
                  [ button
                      [ class_ "btn btn-sm btn-outline-secondary"
                      , onClick (const (TemplateAction TemplateCancelEdit))
                      ]
                      [ text "Annuler" ]
                  ]
            )
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
        [ button [ class_ "btn btn-sm btn-outline-primary", onClick (const $ TemplateAction (TemplateUse template.id)) ] [ text "Utiliser" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const $ TemplateAction (TemplateEdit template.id)) ] [ text "Editer" ]
        , button [ class_ "btn btn-sm btn-outline-danger", onClick (const $ TemplateAction (TemplateDelete template.id)) ] [ text "Supprimer" ]
        ]
    ]

renderCsvImportPanel :: forall w. String -> Maybe CsvImportResult -> HTML w Action
renderCsvImportPanel csvInput result =
  section [ class_ "agenda-import" ]
    [ renderPanelHeader
        "agenda-import"
        "Import CSV"
        "Colonnes minimales: type, titre, fenetre_debut, fenetre_fin."
        []
    , textarea
        [ class_ "form-control agenda-import-textarea"
        , placeholder "Collez votre CSV ici..."
        , value csvInput
        , onValueChange (ImportAction <<< ImportCsvInputChanged)
        ]
    , div [ class_ "agenda-import-actions" ]
        [ button [ class_ "btn btn-sm btn-outline-primary", onClick (const (ImportAction ImportParseCsvInput)) ] [ text "Analyser" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (ImportAction ImportClearCsv)) ] [ text "Effacer" ]
        , button [ class_ "btn btn-sm btn-success", onClick (const (ImportAction ImportApplyCsv)) ] [ text "Ajouter a la liste" ]
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
    [ renderPanelHeader
        "agenda-import"
        "Import ICS"
        "Support basique: SUMMARY, DTSTART, DTEND."
        []
    , textarea
        [ class_ "form-control agenda-import-textarea"
        , placeholder "Collez votre fichier ICS ici..."
        , value icsInput
        , onValueChange (ImportAction <<< ImportIcsInputChanged)
        ]
    , div [ class_ "agenda-import-actions" ]
        [ button [ class_ "btn btn-sm btn-outline-primary", onClick (const (ImportAction ImportParseIcsInput)) ] [ text "Analyser" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (ImportAction ImportClearIcs)) ] [ text "Effacer" ]
        , button [ class_ "btn btn-sm btn-success", onClick (const (ImportAction ImportApplyIcs)) ] [ text "Ajouter a la liste" ]
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
    [ renderPanelHeader
        "agenda-export"
        "Export"
        "Filtres: type, categorie, statut, periode."
        []
    , div [ class_ "agenda-export-controls" ]
        [ div [ class_ "agenda-export-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Format" ]
            , select
                [ class_ "form-select agenda-sort-select"
                , onValueChange (ExportAction <<< ExportFormatChangedAction)
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
                , onValueChange (ExportAction <<< ExportTypeFilterChangedAction)
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
                , onValueChange (ExportAction <<< ExportStatusFilterChangedAction)
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
                , onValueChange (ExportAction <<< ExportCategoryFilterChangedAction)
                ]
            ]
        , div [ class_ "agenda-export-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Debut" ]
            , input
                [ class_ "form-control agenda-input"
                , type_ InputDate
                , value startDate
                , onValueChange (ExportAction <<< ExportStartDateChangedAction)
                ]
            ]
        , div [ class_ "agenda-export-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Fin" ]
            , input
                [ class_ "form-control agenda-input"
                , type_ InputDate
                , value endDate
                , onValueChange (ExportAction <<< ExportEndDateChangedAction)
                ]
            ]
        ]
    , div [ class_ "agenda-export-actions" ]
        [ button [ class_ "btn btn-sm btn-primary", onClick (const (ExportAction ExportGenerate)) ] [ text "Generer" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (ExportAction ExportClearOutput)) ] [ text "Effacer" ]
        ]
    , if output == "" then text ""
      else
        textarea
          [ class_ "form-control agenda-export-textarea"
          , value output
          ]
    ]

timeLabel :: String -> String
timeLabel raw =
  if String.length raw >= 16 then String.slice 11 16 raw else raw

dragHandlers
  :: forall r
   . CalendarItem
  -> Array
       ( IProp
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
  , onDragStart (\ev -> DragAction $ DragStart id ev)
  , onDragOver (\ev -> DragAction $ DragOver id ev)
  , onDrop (const $ DragAction $ DropOn id)
  , onDragEnd (const (DragAction DragEnd))
  ]
dragHandlers _ = []

dragCalendarHandlers
  :: forall r
   . CalendarItem
  -> Array
       ( IProp
           ( draggable :: Boolean
           , onDragStart :: DragEvent
           , onDragEnd :: DragEvent
           | r
           )
           Action
       )
dragCalendarHandlers (ServerCalendarItem { id, content }) =
  if content.itemType == Intention then
    [ draggable true
    , onDragStart (\ev -> DragAction (DragStart id ev))
    , onDragEnd (const (DragAction DragEnd))
    ]
  else []
dragCalendarHandlers _ = []

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

type OfflineMutationResult =
  { items :: Array CalendarItem
  , pending :: Array CalendarItem
  }

applyOfflineMutation :: Boolean -> CalendarItem -> Array CalendarItem -> Array CalendarItem -> OfflineMutationResult
applyOfflineMutation offline item items pending =
  if offline then { items: items <> [ item ], pending: pending <> [ item ] }
  else { items, pending }

durationMinutesBetween :: String -> String -> Maybe Int
durationMinutesBetween start end = do
  startDt <- parseDateTimeLocal start
  endDt <- parseDateTimeLocal end
  let
    Minutes n = diff endDt startDt
    minutes = Int.floor n
  pure $ max 1 minutes

suggestDurationMinutes :: String -> Effect (Maybe Int)
suggestDurationMinutes start = do
  now <- nowDateTime
  pure $ durationMinutesBetweenDateTime start now

durationMinutesBetweenDateTime :: String -> DateTime -> Maybe Int
durationMinutesBetweenDateTime start now = do
  startDt <- parseDateTimeLocal start
  let
    Minutes n = diff now startDt
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

dragOffsetFromEvent :: DragEvent -> Maybe Int -> Effect (Maybe Int)
dragOffsetFromEvent ev duration = do
  let
    dur = fromMaybe 0 duration
    event = toEvent ev
    mouse = MouseEvent.fromEvent event
    clientY = maybe 0 MouseEvent.clientY mouse
    targetEl =
      (Event.currentTarget event <|> Event.target event)
        >>= HTMLElement.fromEventTarget
        <#> HTMLElement.toElement
  case targetEl of
    Nothing -> pure (Just 0)
    Just el -> do
      rect <- getBoundingClientRect el
      let
        safeDuration = max 1 dur
        minuteHeight = rect.height / Int.toNumber safeDuration
        offsetPx = Int.toNumber clientY - rect.top
        rawMinutes = if minuteHeight <= 0.0 then 0.0 else offsetPx / minuteHeight
        minutes = Int.floor rawMinutes
        clamped = max 0 (min (safeDuration - 1) minutes)
        snapped = Int.quot clamped 5 * 5
      pure (Just snapped)

dragMinuteIndexFromEvent :: DragEvent -> Effect (Maybe Int)
dragMinuteIndexFromEvent ev = do
  let
    event = toEvent ev
    mouse = MouseEvent.fromEvent event
    clientY = maybe 0 MouseEvent.clientY mouse
    targetEl =
      (Event.currentTarget event <|> Event.target event)
        >>= HTMLElement.fromEventTarget
        <#> HTMLElement.toElement
  case targetEl of
    Nothing -> pure Nothing
    Just el -> do
      rect <- getBoundingClientRect el
      let
        minuteHeight = rect.height / 1440.0
        offsetPx = Int.toNumber clientY - rect.top
        rawMinutes = if minuteHeight <= 0.0 then 0.0 else offsetPx / minuteHeight
        minutes = max 0 (min 1439 (Int.floor rawMinutes))
        index = Int.quot minutes 5
      pure (Just index)

upsertPendingItem :: CalendarItem -> Array CalendarItem -> Array CalendarItem
upsertPendingItem item pending =
  case item of
    ServerCalendarItem { id } ->
      let
        hasSame =
          any
            ( \candidate -> case candidate of
                ServerCalendarItem { id: candidateId } -> candidateId == id
                _ -> false
            )
            pending
      in
        if hasSame then
          map
            ( \candidate -> case candidate of
                ServerCalendarItem payload | payload.id == id -> item
                _ -> candidate
            )
            pending
        else
          pending <> [ item ]
    _ -> pending

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
      let
        candidate = "tpl-" <> show n
      in
        if elem candidate existing then findId (n + 1) else candidate
  in
    findId 1

toOptionalString :: String -> Maybe String
toOptionalString raw =
  let
    trimmed = StringCommon.trim raw
  in
    if trimmed == "" then Nothing else Just trimmed

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
  let
    raw = Int.toStringAs Int.decimal n
  in
    if String.length raw == 1 then "0" <> raw else raw

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
              ( \currentAcc other ->
                  if overlaps current other then currentAcc <> [ current.id, other.id ]
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
          , onClick (const $ DataAction (DataOpenConflictResolution (headOrEmpty conflictGroups)))
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
        , onClick (const (SyncAction SyncToggleOffline))
        ]
        [ text $ if offlineMode then "Mode hors ligne actif" else "Passer hors ligne" ]
    ]

renderSortPicker :: forall w. SortMode -> HTML w Action
renderSortPicker sortMode =
  div [ class_ "agenda-sort" ]
    [ text "Trier:"
    , select
        [ class_ "form-select agenda-sort-select"
        , onValueChange (DataAction <<< DataSortChanged)
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
            , onClick (const $ ViewAction (ViewChangedAction "day"))
            ]
            [ text "Jour" ]
        , button
            [ class_ $ "btn btn-sm " <> if viewMode == ViewWeek then "btn-primary" else "btn-outline-secondary"
            , onClick (const $ ViewAction (ViewChangedAction "week"))
            ]
            [ text "Semaine" ]
        , button
            [ class_ $ "btn btn-sm " <> if viewMode == ViewMonth then "btn-primary" else "btn-outline-secondary"
            , onClick (const $ ViewAction (ViewChangedAction "month"))
            ]
            [ text "Mois" ]
        ]
    , div [ class_ "agenda-view-date-field" ]
        [ div [ class_ "agenda-notifications-label" ] [ text "Date de reference" ]
        , input
            [ class_ "form-control agenda-input agenda-view-date"
            , type_ InputDate
            , attr (AttrName "lang") "fr"
            , value focusDate
            , onValueChange (ViewAction <<< ViewFocusDateChanged)
            ]
        ]
    ]

renderMobileTools :: forall w. AgendaView -> HTML w Action
renderMobileTools viewMode =
  if viewMode /= ViewDay then text ""
  else
    div [ class_ "agenda-mobile-tools" ]
      [ button [ class_ "btn btn-sm btn-outline-secondary", onClick (const $ ViewAction (ViewOpenModal ModalFilters)) ] [ text "Filtres" ]
      , button [ class_ "btn btn-sm btn-primary", onClick (const $ ViewAction (ViewOpenModal ModalTools)) ] [ text "Outils" ]
      ]

renderAgendaModals
  :: forall w
   . Maybe AgendaModal
  -> Boolean
  -> Array (Array String)
  -> Boolean
  -> SortMode
  -> NotificationDefaults
  -> Array NotificationOverride
  -> Maybe NotificationEditor
  -> IntentionDraft
  -> Array CalendarItem
  -> Array TaskTemplate
  -> TemplateDraft
  -> Maybe String
  -> String
  -> Maybe CsvImportResult
  -> String
  -> Maybe IcsImportResult
  -> ExportFormat
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> HTML w Action
renderAgendaModals activeModal showConflictsOnly conflictGroups offlineMode sortMode defaults overrides editor draft intentions templates templateDraft editingTemplateId csvInput csvImportResult icsInput icsImportResult exportFormat exportTypeFilter exportStatusFilter exportCategoryFilter exportStartDate exportEndDate exportOutput =
  div []
    [ renderModal "Filtres" (activeModal == Just ModalFilters) (ViewAction ViewCloseModal)
        [ renderFiltersContent showConflictsOnly conflictGroups offlineMode sortMode ]
    , renderModal "Outils" (activeModal == Just ModalTools) (ViewAction ViewCloseModal)
        [ renderToolsContent ]
    , renderModal "Dates et heures" (activeModal == Just ModalDateTime) (ViewAction ViewCloseModal)
        [ renderDateTimeContent draft ]
    , renderModal "Rappels" (activeModal == Just ModalNotifications) (ViewAction ViewCloseModal)
        [ renderNotificationsContent defaults overrides editor intentions ]
    , renderModal "Templates de taches" (activeModal == Just ModalTemplates) (ViewAction ViewCloseModal)
        [ renderTemplatesPanel templates templateDraft editingTemplateId ]
    , renderModal "Import CSV" (activeModal == Just ModalImportCsv) (ViewAction ViewCloseModal)
        [ renderCsvImportPanel csvInput csvImportResult ]
    , renderModal "Import ICS" (activeModal == Just ModalImportIcs) (ViewAction ViewCloseModal)
        [ renderIcsImportPanel icsInput icsImportResult ]
    , renderModal "Export" (activeModal == Just ModalExport) (ViewAction ViewCloseModal)
        [ renderExportPanel exportFormat exportTypeFilter exportStatusFilter exportCategoryFilter exportStartDate exportEndDate exportOutput ]
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
        [ button [ class_ "btn btn-sm btn-danger", onClick (const (SyncAction SyncResolveDiscardLocal)) ] [ text "Abandonner local" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (SyncAction SyncResolveKeepLocal)) ] [ text "Conserver local" ]
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
        , onValueChange (ViewAction <<< ViewValidationMinutesChanged)
        , value panel.inputValue
        ]
    , div [ class_ "agenda-conflict-confirmation-actions" ]
        [ button [ class_ "btn btn-sm btn-success", onClick (const (ViewAction ViewConfirmValidation)) ] [ text "Confirmer" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (ViewAction ViewCancelValidation)) ] [ text "Annuler" ]
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
            , onClick (const $ DataAction (DataChooseResolutionStrategy StrategyShift30))
            ]
            [ text "Decaler de 30 min" ]
        , button
            [ class_ "btn btn-sm btn-outline-primary"
            , onClick (const $ DataAction (DataChooseResolutionStrategy StrategySwap))
            ]
            [ text "Echanger" ]
        ]
    , renderConfirmation resolution.pendingStrategy
    , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (DataAction DataCancelResolution)) ] [ text "Fermer" ]
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
            [ button [ class_ "btn btn-sm btn-danger", onClick (const (DataAction DataConfirmResolution)) ] [ text "Confirmer" ]
            , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (DataAction DataCancelResolution)) ] [ text "Annuler" ]
            ]
        ]

calendarItemContent :: CalendarItem -> CalendarItemContent
calendarItemContent (NewCalendarItem { content }) = content
calendarItemContent (ServerCalendarItem { content }) = content

updateItemWindowById
  :: String
  -> String
  -> String
  -> Array CalendarItem
  -> { items :: Array CalendarItem, updated :: Maybe CalendarItem }
updateItemWindowById targetId newStart newEnd items =
  foldl step { items: [], updated: Nothing } items
  where
  step acc item =
    case item of
      ServerCalendarItem payload | payload.id == targetId ->
        let
          updatedItem =
            ServerCalendarItem
              payload
                { content = payload.content
                    { windowStart = newStart
                    , windowEnd = newEnd
                    }
                }
        in
          { items: acc.items <> [ updatedItem ], updated: Just updatedItem }
      _ ->
        { items: acc.items <> [ item ], updated: acc.updated }
