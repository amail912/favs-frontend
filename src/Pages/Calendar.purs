module Pages.Calendar
  ( component
  , decodeCalendarItemsResponse
  , CalendarItem(..)
  , CalendarItemContent
  , IntentionDraft
  , ItemStatus(..)
  , ItemType(..)
  , SortMode(..)
  , ValidationError(..)
  , defaultNotificationDefaults
  , detectConflictGroups
  , detectConflictIds
  , toNewIntention
  , primaryActionFor
  , PrimaryAction(..)
  , buildTimelineLayout
  , toTimelineBlock
  , EditError(..)
  , applyEditDraft
  , buildEditDraft
  , durationMinutesBetween
  , sortItems
  , validateIntention
  , parseCsvImport
  , parseIcsImport
  , reminderTimesForIntention
  , applyOfflineMutation
  , applyTemplateToDraft
  , computeDropMinuteIndex
  , indexToTimeLabel
  ) where

import Prelude hiding (div)
import Affjax.Web (Response)
import Api.Calendar (ValidateItemPayload(..), createItemResponse, getItemsResponse, updateItemResponse, validateItemResponse)
import Calendar.Recurrence (RecurrenceDraft, RecurrenceRule(..), defaultRecurrenceDraft, draftFromRecurrence, draftToRecurrence)
import Calendar.Recurrence as Recurrence
import Calendar.Export (ExportInput(..), ExportItem(..))
import Calendar.Export as Export
import Calendar.Templates as Templates
import Control.Monad.Except (ExceptT(..), withExceptT)
import Control.Monad.State.Trans (StateT, runStateT, get, modify_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Array (filter, foldM, null, length, mapWithIndex, findIndex, foldl, mapMaybe, sortBy, uncons, updateAt, elem, find, findMap, nub, index, last, catMaybes, any)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_, fold)
import Data.Lens (Iso', Lens', iso, view, (.~), (%~), (^.), lens)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap, wrap)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Effect.Now as Now
import Halogen (Component, ComponentHTML, HalogenM, Slot, defaultEval, getRef, mkComponent, mkEval) as H
import Halogen (subscribe)
import Halogen.HTML (HTML, button, details, div, h2, i, section, slot, summary, text, li, span, ul, option, select, input, textarea)
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events (onClick, onDragOver, onDrop, onMouseDown, onTouchCancel, onTouchEnd, onTouchMove, onValueChange, onKeyDown, onDragEnd, onDragStart, onTouchStart)
import Halogen.HTML.Properties (attr, style, IProp, value, placeholder, type_, draggable)
import Halogen.Query.Event as HQE
import Type.Proxy (Proxy(..))
import Ui.Errors (FatalError, handleError, toFatalError)
import Ui.Focus (focusElement)
import Ui.Modal (renderModal) as Modal
import Ui.Utils (class_)
import Web.Event.Event (EventType(..), preventDefault)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KeyboardEventTypes
import Web.UIEvent.MouseEvent as MouseEvent
import Web.TouchEvent.TouchEvent as TouchEvent
import Data.Enum (enumFromTo, fromEnum, toEnum)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Control.Alt ((<|>))
import Control.Monad.Writer.Class (tell)
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Time.Duration (Milliseconds(..), Days(..), Minutes(..))
import Effect (Effect)
import Web.Event.Event (currentTarget, target) as Event
import Web.HTML.Event.DragEvent (DragEvent, toEvent)
import Web.HTML.HTMLElement as HTMLElement
import Web.DOM.Element (getBoundingClientRect)
import Web.DOM.ParentNode as ParentNode
import Web.TouchEvent.Touch (clientY) as Touch
import Web.TouchEvent.TouchList as TouchList
import Ui.Vibration (vibrateIfAvailable)
import Ui.AgendaRender (renderPanelHeader)
import Helpers.DateTime as DateTime
import Web.HTML.HTMLDocument as HTMLDocument
import Data.Date (Date, canonicalDate, month, year)
import Data.DateTime (DateTime(..), adjust, date, diff, time)
import Data.Time (Time, hour, minute)
import Data.String.Common as StringCommon
import Data.String.Pattern (Pattern(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Int as Int

-- foldl comes from Data.Array in this module

-- BEGIN src/Pages/Calendar.purs
type NoOutput = Void
type AgendaAppM = H.HalogenM State Action Slots NoOutput Aff
type ErrorAgendaAppM = ExceptT FatalError AgendaAppM

type State =
  { calendar :: CalendarState
  , sync :: SyncState
  , drag :: DragState
  , notifications :: NotificationState
  , templates :: Templates.TemplateState
  , imports :: ImportState
  , view :: ViewState
  }

data Action
  = Init
  | CreateFormAction CreateFormAction
  | ControlsAction ControlsAction
  | ConflictAction ConflictAction
  | SyncAction SyncAction
  | DragAction DragAction
  | ViewAction ViewAction
  | GlobalKeyDown String
  | GlobalResize
  | NotificationAction NotificationAction
  | TemplatesOutputAction Templates.TemplatesOutput
  | ImportAction ImportAction
  | CreateRecurrenceCmd Recurrence.RecurrenceCommand
  | EditRecurrenceCmd Recurrence.RecurrenceCommand

data RecurrenceSlot
  = RecurrenceCreate
  | RecurrenceEdit

derive instance eqRecurrenceSlot :: Eq RecurrenceSlot
derive instance ordRecurrenceSlot :: Ord RecurrenceSlot
type RecurrenceSlotDef slot = forall query. H.Slot query Recurrence.RecurrenceCommand slot

data TemplatesSlot = TemplatesModal

derive instance eqTemplatesSlot :: Eq TemplatesSlot
derive instance ordTemplatesSlot :: Ord TemplatesSlot
type TemplatesSlotDef slot = forall query. H.Slot query Templates.TemplatesOutput slot

data ExportSlot = ExportModal

derive instance eqExportSlot :: Eq ExportSlot
derive instance ordExportSlot :: Ord ExportSlot
type ExportSlotDef slot = forall query. H.Slot query Void slot

type Slots =
  ( recurrence :: RecurrenceSlotDef RecurrenceSlot
  , templates :: TemplatesSlotDef TemplatesSlot
  , export :: ExportSlotDef ExportSlot
  )

data Command
  = SyncCmd SyncCommand
  | DragCmd DragCommand
  | ViewCmd ViewCommand
  | ImportCmd ImportCommand

class ToCommand cmd where
  toCommand :: cmd -> Command

instance toCommandSyncCommand :: ToCommand SyncCommand where
  toCommand = SyncCmd

instance toCommandDragCommand :: ToCommand DragCommand where
  toCommand = DragCmd

instance toCommandViewCommand :: ToCommand ViewCommand where
  toCommand = ViewCmd

instance toCommandImportCommand :: ToCommand ImportCommand where
  toCommand = ImportCmd

instance toCommandVoid :: ToCommand Void where
  toCommand = absurd

class ToAction a where
  toAction :: a -> Action

instance toActionCreateFormAction :: ToAction CreateFormAction where
  toAction = CreateFormAction

instance toActionControlsAction :: ToAction ControlsAction where
  toAction = ControlsAction

instance toActionConflictAction :: ToAction ConflictAction where
  toAction = ConflictAction

instance toActionSyncAction :: ToAction SyncAction where
  toAction = SyncAction

instance toActionDragAction :: ToAction DragAction where
  toAction = DragAction

instance toActionViewAction :: ToAction ViewAction where
  toAction = ViewAction

instance toActionListAction :: ToAction ListAction where
  toAction (ListViewAction viewAction) = ViewAction viewAction
  toAction (ListSyncAction syncAction) = SyncAction syncAction
  toAction (ListDragAction dragAction) = DragAction dragAction

instance toActionRangeAction :: ToAction RangeAction where
  toAction (RangeListAction listAction) = toAction listAction

instance toActionDayAction :: ToAction DayAction where
  toAction (DayListAction listAction) = toAction listAction
  toAction (DayDragAction dragAction) = DragAction dragAction
  toAction (DayViewAction viewAction) = ViewAction viewAction

renderOfflineTogglePage :: forall w. Boolean -> HTML w Action
renderOfflineTogglePage = map toAction <<< renderOfflineToggle

renderSortPickerPage :: forall w. SortMode -> HTML w Action
renderSortPickerPage = map toAction <<< renderSortPicker

renderConflictActionsPage :: forall w. Array (Array String) -> HTML w Action
renderConflictActionsPage = map toAction <<< renderConflictActions

renderAgendaView
  :: forall w
   . AgendaView
  -> String
  -> Array String
  -> Array CalendarItem
  -> Boolean
  -> Maybe String
  -> Maybe Int
  -> HTML w Action
renderAgendaView viewMode focusDate conflictIds items isMobile draggingId dragHoverIndex =
  case viewMode of
    ViewDay ->
      map toAction (renderDayCalendar focusDate conflictIds items isMobile draggingId dragHoverIndex)
    ViewWeek ->
      map toAction (renderRangeView "Semaine" (generateDateRange focusDate 7) conflictIds items isMobile)
    ViewMonth ->
      map toAction (renderRangeView "Mois" (generateMonthDates focusDate) conflictIds items isMobile)

renderCreateFab :: forall w. HTML w Action
renderCreateFab =
  button
    [ class_ "calendar-fab"
    , attr (AttrName "aria-label") "Nouvel item"
    , onClick (const (ViewAction ViewOpenCreate))
    ]
    [ i [ class_ "bi bi-plus" ] [] ]

renderRecurrenceSlot :: RecurrenceSlot -> RecurrenceDraft -> (Recurrence.RecurrenceCommand -> Action) -> H.ComponentHTML Action Slots Aff
renderRecurrenceSlot slotId draft onOutput =
  slot (Proxy :: _ "recurrence") slotId Recurrence.component draft onOutput

defaultStartDateTime :: String -> String
defaultStartDateTime focusDate =
  if String.length focusDate >= 16 then
    focusDate
  else if String.length focusDate >= 10 then
    String.slice 0 10 focusDate <> "T09:00"
  else
    ""

prefillCreateDraft :: ItemType -> String -> IntentionDraft -> IntentionDraft
prefillCreateDraft itemType focusDate draft
  | itemType /= ScheduledBlock || draft.windowStart /= "" || defaultStartDateTime focusDate == "" = draft
  | otherwise =
      let
        start = defaultStartDateTime focusDate
        end = if draft.windowEnd == "" then fromMaybe start (shiftMinutesRaw 30 start) else draft.windowEnd
      in
        draft
          { windowStart = start
          , windowEnd = end
          }

applyTemplateToDraft :: Templates.TaskTemplate -> String -> String -> IntentionDraft
applyTemplateToDraft template windowStart windowEnd =
  { itemType: Intention
  , title: template.title
  , windowStart
  , windowEnd
  , category: template.category
  , status: Todo
  , actualDurationMinutes: ""
  , recurrence: defaultRecurrenceDraft
  }

itemTypeIso :: Iso' ItemType Export.ItemType
itemTypeIso = iso toExport fromExport
  where
  toExport = case _ of
    Intention -> Export.Intention
    ScheduledBlock -> Export.ScheduledBlock
  fromExport = case _ of
    Export.Intention -> Intention
    Export.ScheduledBlock -> ScheduledBlock

itemStatusIso :: Iso' ItemStatus Export.ItemStatus
itemStatusIso = iso toExport fromExport
  where
  toExport = case _ of
    Todo -> Export.Todo
    EnCours -> Export.EnCours
    Fait -> Export.Fait
    Annule -> Export.Annule
  fromExport = case _ of
    Export.Todo -> Todo
    Export.EnCours -> EnCours
    Export.Fait -> Fait
    Export.Annule -> Annule

toExportItem :: CalendarItem -> ExportItem
toExportItem item =
  let
    content = calendarItemContent item
  in
    ExportItem
      { itemType: view itemTypeIso content.itemType
      , title: content.title
      , windowStart: content.windowStart
      , windowEnd: content.windowEnd
      , status: view itemStatusIso content.status
      , category: content.category
      , sourceItemId: content.sourceItemId
      , actualDurationMinutes: content.actualDurationMinutes
      , recurrenceRule: content.recurrenceRule
      , recurrenceExceptionDates: content.recurrenceExceptionDates
      }

renderTemplatesPanelPage :: Templates.TemplateState -> H.ComponentHTML Action Slots Aff
renderTemplatesPanelPage templatesState =
  slot (Proxy :: _ "templates") TemplatesModal Templates.component templatesState TemplatesOutputAction

renderCsvImportPanelPage :: forall w. String -> Maybe CsvImportResult -> HTML w Action
renderCsvImportPanelPage csvInput result =
  map ImportAction (renderCsvImportPanel csvInput result)

renderIcsImportPanelPage :: forall w. String -> Maybe IcsImportResult -> HTML w Action
renderIcsImportPanelPage icsInput result =
  map ImportAction (renderIcsImportPanel icsInput result)

component :: forall q i. H.Component q i NoOutput Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = pure Init
        }
    }

initialState :: forall i. i -> State
initialState = const
  { calendar: calendarInitialState
  , sync: syncInitialState
  , drag: dragInitialState
  , notifications: notificationInitialState
  , templates: Templates.templateInitialState
  , imports: importInitialState
  , view: viewInitialState
  }

_calendar :: Lens' State CalendarState
_calendar = prop (Proxy :: _ "calendar")

_sync :: Lens' State SyncState
_sync = prop (Proxy :: _ "sync")

_drag :: Lens' State DragState
_drag = prop (Proxy :: _ "drag")

_notifications :: Lens' State NotificationState
_notifications = prop (Proxy :: _ "notifications")

_templates :: Lens' State Templates.TemplateState
_templates = prop (Proxy :: _ "templates")

_imports :: Lens' State ImportState
_imports = prop (Proxy :: _ "imports")

_view :: Lens' State ViewState
_view = prop (Proxy :: _ "view")

_calendarItems :: Lens' State (Array CalendarItem)
_calendarItems = _calendar <<< _items

_calendarDraft :: Lens' State IntentionDraft
_calendarDraft = _calendar <<< _draft

_calendarValidationError :: Lens' State (Maybe String)
_calendarValidationError = _calendar <<< _validationError

_calendarLastCreateType :: Lens' State ItemType
_calendarLastCreateType = _calendar <<< _lastCreateType

_syncOfflineModeState :: Lens' State Boolean
_syncOfflineModeState = _sync <<< _syncOfflineMode

_pendingSync :: Lens' State (Array CalendarItem)
_pendingSync = _sync <<< _syncPendingSync

_syncConflictState :: Lens' State (Maybe (Array CalendarItem))
_syncConflictState = _sync <<< _syncConflict

_syncUpdateErrorState :: Lens' State (Maybe String)
_syncUpdateErrorState = _sync <<< _syncUpdateError

_viewFocusDatePage :: Lens' State String
_viewFocusDatePage = _view <<< _viewFocusDateState

class HasStateLens s where
  getLens :: Lens' State s

instance hasStateLensSync :: HasStateLens SyncState where
  getLens = _sync

instance hasStateLensDrag :: HasStateLens DragState where
  getLens = _drag

instance hasStateLensView :: HasStateLens ViewState where
  getLens = _view

instance hasStateLensImports :: HasStateLens ImportState where
  getLens = _imports

instance hasStateLensNotifications :: HasStateLens NotificationState where
  getLens = _notifications

withSubState :: forall s a cmd. HasStateLens s => ToCommand cmd => StateT s (WriterT (Array cmd) Aff) a -> ErrorAgendaAppM a
withSubState action = do
  st <- get
  let
    lens = getLens
    subState = st ^. lens
  Tuple (Tuple result nextSubState) cmds <- liftAff $ runWriterT (runStateT action subState)
  modify_ (lens .~ nextSubState)
  traverse_ (runCommand <<< toCommand) cmds
  pure result

runSyncCommand :: SyncCommand -> ErrorAgendaAppM Unit
runSyncCommand = case _ of
  SyncSetItems items -> modify_ (_calendarItems .~ items)
  SyncCreateItem item -> createItem item >>= (const refreshItems)
  SyncRefreshItems -> refreshItems
  SyncSubmitIntentionCmd -> submitIntention
  SyncRunPending pending ->
    if null pending then
      refreshItems
    else do
      ok <- foldM
        ( \acc item ->
            if not acc then pure false
            else createItem item >>= (pure <<< statusOk)
        )
        true
        pending
      if ok then do
        modify_ ((_pendingSync .~ []) <<< (_syncConflictState .~ Nothing))
        refreshItems
      else modify_ (_syncConflictState .~ Just pending)

runDragCommand :: DragCommand -> ErrorAgendaAppM Unit
runDragCommand = case _ of
  DragSetItems items -> modify_ (_calendarItems .~ items)
  DragUpsertPending item -> modify_ (_pendingSync %~ upsertPendingItem item)
  DragSetUpdateError err -> modify_ (_syncUpdateErrorState .~ err)
  DragRefreshItems -> refreshItems
  DragUpdateItem itemId updatedItem -> do
    resp <- updateItem itemId updatedItem
    if statusOk resp then modify_ (_syncUpdateErrorState .~ Nothing)
    else modify_ (_syncUpdateErrorState .~ Just (updateErrorMessage (unwrap resp.status)))
    refreshItems

runViewCommand :: ViewCommand -> ErrorAgendaAppM Unit
runViewCommand = case _ of
  ViewValidateItem itemId minutes ->
    validateItem itemId minutes >>= (const refreshItems)
  ViewUpdateItem itemId updatedItem -> do
    st <- get
    if st ^. _syncOfflineModeState then
      modify_ ((_calendarItems %~ replaceItemById itemId updatedItem) <<< (_pendingSync %~ upsertPendingItem updatedItem) <<< (_syncUpdateErrorState .~ Nothing))
    else do
      resp <- updateItem itemId updatedItem
      if statusOk resp then modify_ (_syncUpdateErrorState .~ Nothing)
      else modify_ (_syncUpdateErrorState .~ Just (updateErrorMessage (unwrap resp.status)))
      refreshItems

runImportCommand :: ImportCommand -> ErrorAgendaAppM Unit
runImportCommand (ImportSetItems items) = modify_ (_calendarItems .~ items)
runImportCommand (ImportSetPending pending) = modify_ (_pendingSync .~ pending)

runCommand :: Command -> ErrorAgendaAppM Unit
runCommand = case _ of
  SyncCmd cmd -> runSyncCommand cmd
  DragCmd cmd -> runDragCommand cmd
  ViewCmd cmd -> runViewCommand cmd
  ImportCmd cmd -> runImportCommand cmd

handleSyncActionFlow :: SyncAction -> ErrorAgendaAppM Unit
handleSyncActionFlow syncAction = do
  st <- get
  withSubState $ handleSyncAction (st ^. _calendarItems) syncAction

handleDragActionFlow :: DragAction -> ErrorAgendaAppM Unit
handleDragActionFlow dragAction = do
  st <- get
  let
    ctx =
      { items: st ^. _calendarItems
      , focusDate: st ^. _viewFocusDatePage
      , offlineMode: st ^. _syncOfflineModeState
      }
  withSubState $ handleDragAction ctx dragAction

handleViewActionFlow :: ViewAction -> ErrorAgendaAppM Unit
handleViewActionFlow viewAction = do
  withSubState $ handleViewAction viewAction
  case viewAction of
    ViewOpenModal _ -> focusModal
    ViewOpenCreate -> do
      focusModal
      st <- get
      let
        lastType = st ^. _calendarLastCreateType
        baseDraft = emptyDraft { itemType = lastType }
        nextDraft = prefillCreateDraft lastType (st ^. _viewFocusDatePage) baseDraft
      modify_ ((_calendarDraft .~ nextDraft) <<< (_calendarValidationError .~ Nothing))
    ViewOpenEdit _ -> focusModal
    ViewOpenEditFromDoubleClick _ -> focusModal
    ViewCloseCreate -> do
      st <- get
      let
        lastType = st ^. _calendarLastCreateType
      modify_ ((_calendarDraft .~ (emptyDraft { itemType = lastType })) <<< (_calendarValidationError .~ Nothing))
    _ -> pure unit

handleAction :: Action -> AgendaAppM Unit
handleAction action = handleError $
  case action of
    Init -> initAction
    CreateFormAction formAction -> do
      modify_ (_calendar %~ applyCreateFormAction formAction)
      case formAction of
        CreateFormDraftTypeChanged itemType -> do
          st <- get
          let updatedDraft = prefillCreateDraft itemType (st ^. _viewFocusDatePage) (st ^. _calendarDraft)
          modify_ (_calendarDraft .~ updatedDraft)
        CreateFormSync syncAction ->
          handleSyncActionFlow syncAction
        _ -> pure unit
    ControlsAction controlsAction ->
      modify_ (_calendar %~ applyControlsAction controlsAction)
    ConflictAction conflictAction ->
      modify_ (_calendar %~ applyConflictAction conflictAction)
    SyncAction syncAction -> handleSyncActionFlow syncAction
    DragAction dragAction -> handleDragActionFlow dragAction
    ViewAction viewAction -> do
      case viewAction of
        ViewOpenModal ModalTemplates ->
          modify_ (_templates %~ Templates.resetTemplateDraft)
        _ -> pure unit
      handleViewActionFlow viewAction
    GlobalKeyDown key ->
      if key == "Escape" then do
        st <- get
        case st ^. (_view <<< _viewActiveModalS) of
          Just ModalEditItem -> withSubState $ handleViewAction ViewEditCancel
          Just ModalCreateItem -> do
            withSubState $ handleViewAction ViewCloseCreate
            st' <- get
            let
              lastType = st' ^. _calendarLastCreateType
            modify_ ((_calendarDraft .~ (emptyDraft { itemType = lastType })) <<< (_calendarValidationError .~ Nothing))
          Just _ -> withSubState $ handleViewAction ViewCloseModal
          Nothing -> pure unit
      else pure unit
    GlobalResize -> do
      viewport <- liftEffect $ window >>= Window.innerWidth
      withSubState $ handleViewAction (ViewSetIsMobile (viewport <= 768))
    NotificationAction notificationAction -> withSubState $ handleNotificationAction notificationAction
    TemplatesOutputAction output ->
      case output of
        Templates.TemplatesStateChanged nextState ->
          modify_ (_templates .~ nextState)
        Templates.TemplatesUse template -> do
          now <- liftEffect nowDateTime
          let startStr = formatDateTimeLocal now
          let endStr = fromMaybe startStr (shiftMinutesRaw template.durationMinutes startStr)
          modify_ (_calendarDraft .~ applyTemplateToDraft template startStr endStr)
    CreateRecurrenceCmd (Recurrence.RecurrenceApplied draft) ->
      modify_
        ( (_calendar <<< _draftRecurrenceS .~ draft)
            <<< (_calendarValidationError .~ Nothing)
        )
    EditRecurrenceCmd (Recurrence.RecurrenceApplied draft) ->
      modify_
        ((_view <<< _viewEditPanelS) %~ map (\panel -> panel { draft = panel.draft { recurrence = draft }, validationError = Nothing }))
    ImportAction importAction -> do
      st <- get
      let
        ctx =
          { items: st ^. _calendarItems
          , pending: st ^. _pendingSync
          , offlineMode: st ^. _syncOfflineModeState
          }
      withSubState $ handleImportAction ctx importAction

initAction :: ErrorAgendaAppM Unit
initAction = do
  now <- liftEffect nowDateTime
  modify_ $ _viewFocusDatePage .~ formatDate now
  viewport <- liftEffect $ window >>= Window.innerWidth
  withSubState $ handleViewAction (ViewSetIsMobile (viewport <= 768))
  subscribeToGlobalKeyDown
  subscribeToGlobalResize
  refreshItems

refreshItems :: ErrorAgendaAppM Unit
refreshItems = do
  jsonResponse <- withExceptT toFatalError $ ExceptT $ liftAff getItemsResponse
  items <- decodeCalendarItemsResponse jsonResponse # pure >>> ExceptT
  modify_ (_calendarItems .~ items)

decodeCalendarItemsResponse :: Response Json -> Either FatalError (Array CalendarItem)
decodeCalendarItemsResponse jsonResponse =
  if unwrap jsonResponse.status == 401 then
    Right []
  else
    jsonResponse.body # decodeJson # lmap toFatalError

createItem :: CalendarItem -> ErrorAgendaAppM (Response Json)
createItem item = withExceptT toFatalError $ ExceptT $ liftAff $ createItemResponse item

updateItem :: String -> CalendarItem -> ErrorAgendaAppM (Response Json)
updateItem itemId item = withExceptT toFatalError $ ExceptT $ liftAff $ updateItemResponse itemId item

validateItem :: String -> Int -> ErrorAgendaAppM (Response Json)
validateItem itemId minutes =
  withExceptT toFatalError $ ExceptT $ liftAff
    $ validateItemResponse itemId
    $ ValidateItemPayload { duree_reelle_minutes: minutes }

statusOk :: forall a. Response a -> Boolean
statusOk r = unwrap r.status >= 200 && unwrap r.status < 300

replaceItemById :: String -> CalendarItem -> Array CalendarItem -> Array CalendarItem
replaceItemById targetId updated =
  map
    ( \item -> case item of
        ServerCalendarItem { id } | id == targetId -> updated
        _ -> item
    )

submitIntention :: ErrorAgendaAppM Unit
submitIntention = do
  st <- get
  case validateIntention (st ^. _calendarDraft) of
    Left err -> modify_ (_calendarValidationError .~ Just (validationErrorMessage err))
    Right validDraft ->
      case toNewIntention validDraft of
        Left err ->
          modify_ (_calendarValidationError .~ Just err)
        Right item ->
          if st ^. _syncOfflineModeState then do
            let
              result = applyOfflineMutation true item (st ^. _calendarItems) (st ^. _pendingSync)
            modify_
              ( (_calendarItems .~ result.items)
                  <<< (_calendarDraft .~ emptyDraft)
                  <<< (_calendarValidationError .~ Nothing)
                  <<< (_pendingSync .~ result.pending)
                  <<< ((_view <<< _viewActiveModalS) .~ Nothing)
              )
          else do
            _ <- createItem item
            modify_
              ( (_calendarDraft .~ emptyDraft)
                  <<< (_calendarValidationError .~ Nothing)
                  <<< ((_view <<< _viewActiveModalS) .~ Nothing)
              )
            refreshItems

focusModal :: ErrorAgendaAppM Unit
focusModal = do
  ref <- lift $ H.getRef (wrap "modal-focus")
  case ref of
    Nothing -> pure unit
    Just elem -> liftEffect $ void (focusElement elem)

subscribeToGlobalKeyDown :: ErrorAgendaAppM Unit
subscribeToGlobalKeyDown = do
  win <- liftEffect window
  let target = Window.toEventTarget win
  let
    emitter = HQE.eventListener KeyboardEventTypes.keydown target \ev ->
      case KE.fromEvent ev of
        Nothing -> Nothing
        Just keyEvent -> Just (GlobalKeyDown (KE.key keyEvent))
  _ <- lift $ subscribe emitter
  pure unit

subscribeToGlobalResize :: ErrorAgendaAppM Unit
subscribeToGlobalResize = do
  win <- liftEffect window
  let target = Window.toEventTarget win
  let
    emitter = HQE.eventListener (EventType "resize") target \_ ->
      Just GlobalResize
  _ <- lift $ subscribe emitter
  pure unit

render :: State -> H.ComponentHTML Action Slots Aff
render { calendar, sync, drag, notifications, templates, imports, view } =
  let
    { items, showConflictsOnly, conflictResolution, sortMode } = calendar
    SyncState { offlineMode, syncConflict, updateError } = sync
    DragState { draggingId, dragHoverIndex } = drag
    NotificationState { notificationDefaults, notificationOverrides, notificationPanelOpen, notificationEditor } = notifications
    ImportState { csvInput, csvImportResult, icsInput, icsImportResult } = imports
    ViewState { viewMode, focusDate, validationPanel, isMobile } = view
    agendaModalsInput = buildAgendaModalsInput { calendar, sync, drag, notifications, templates, imports, view }
    { conflictGroups } = agendaModalsInput.filters
    { intentions: unplannedIntentions } = agendaModalsInput.notifications
    conflictIds = detectConflictIds items
    itemsToShow =
      if showConflictsOnly then filter (isConflict conflictIds) items
      else items
    sortedItems = sortItems sortMode conflictIds itemsToShow
  in
    div [ class_ "entity-page calendar-page" ]
      ( [ section [ class_ "calendar-header" ]
            [ h2 [ class_ "calendar-title" ] [ text (viewTitle viewMode) ]
            , div [ class_ "calendar-subtitle" ] [ text "Capture rapide des intentions à planifier." ]
            , div [ class_ "calendar-controls" ]
                [ button
                    [ class_ $ "btn btn-sm calendar-filter"
                        <> guard showConflictsOnly " btn-outline-primary"
                        <> guard (not showConflictsOnly) " btn-outline-secondary"
                    , onClick (const (ControlsAction ControlsToggleConflictFilter))
                    ]
                    [ text "Filtrer: en conflit" ]
                , renderOfflineTogglePage offlineMode
                , renderSortPickerPage sortMode
                , renderConflictActionsPage conflictGroups
                ]
            , toAction <$> renderViewSelector viewMode focusDate
            , toAction <$> renderMobileTools viewMode
            ]
        , div [ class_ $ "calendar-layout" <> guard (viewMode == ViewDay) " calendar-layout--calendar" ]
            [ div [ class_ "calendar-main" ]
                [ maybe (text "") (map toAction <<< renderUpdateError) updateError
                , maybe (text "") (map toAction <<< renderValidationPanel) validationPanel
                , section [ class_ $ "calendar-list-panel" <> guard (viewMode == ViewDay) " calendar-list-panel--calendar" ]
                    [ renderAgendaView viewMode focusDate conflictIds sortedItems isMobile draggingId dragHoverIndex
                    ]
                , maybe (text "") (map toAction <<< renderConflictResolution items) conflictResolution
                , maybe (text "") (map toAction <<< renderSyncConflict) syncConflict
                ]
            , div [ class_ "calendar-side" ]
                [ NotificationAction <$> renderNotificationsPanel notificationPanelOpen notificationDefaults notificationOverrides notificationEditor unplannedIntentions
                , renderAccordion "Import CSV" "calendar-accordion import-csv" $ renderCsvImportPanelPage csvInput csvImportResult
                , renderAccordion "Import ICS" "calendar-accordion import-ics" $ renderIcsImportPanelPage icsInput icsImportResult
                ]
            ]
        ]
          <> [ renderAgendaModals agendaModalsInput ]
          <>
            [ renderCreateFab ]
      )

renderFiltersContent :: forall w. Boolean -> Array (Array String) -> Boolean -> SortMode -> HTML w Action
renderFiltersContent showConflictsOnly conflictGroups offlineMode sortMode =
  div [ class_ "calendar-modal-stack" ]
    [ button
        [ class_ $ "btn btn-sm calendar-filter" <> if showConflictsOnly then " btn-outline-primary" else "btn-outline-secondary"
        , onClick (const (ControlsAction ControlsToggleConflictFilter))
        ]
        [ text "Filtrer: en conflit" ]
    , renderOfflineTogglePage offlineMode
    , renderSortPickerPage sortMode
    , renderConflictActionsPage conflictGroups
    ]

type AgendaModalsInput =
  { activeModal :: Maybe AgendaModal
  , filters ::
      { showConflictsOnly :: Boolean
      , conflictGroups :: Array (Array String)
      , offlineMode :: Boolean
      , sortMode :: SortMode
      }
  , notifications ::
      { defaults :: NotificationDefaults
      , overrides :: Array NotificationOverride
      , editor :: Maybe NotificationEditor
      , intentions :: Array CalendarItem
      }
  , templates :: Templates.TemplateState
  , csvImport ::
      { input :: String
      , result :: Maybe CsvImportResult
      }
  , icsImport ::
      { input :: String
      , result :: Maybe IcsImportResult
      }
  , exportItems :: Array ExportItem
  , draft :: IntentionDraft
  , validationError :: Maybe String
  , editPanel :: Maybe EditPanel
  }

renderAgendaModals :: AgendaModalsInput -> H.ComponentHTML Action Slots Aff
renderAgendaModals { activeModal, filters, notifications, templates, csvImport, icsImport, exportItems, draft, validationError, editPanel } =
  let
    renderModal title content = Modal.renderModal title content (ViewAction ViewCloseModal) (ViewAction ViewCloseModal)
    renderExportModal items =
      slot (Proxy :: _ "export") ExportModal Export.component (ExportInput { items }) absurd
  in
    maybe (div [] [])
      case _ of
        ModalFilters -> renderModal "Filtres" [ renderFiltersContent filters.showConflictsOnly filters.conflictGroups filters.offlineMode filters.sortMode ]
        ModalTools -> renderModal "Outils" [ map toAction renderToolsContent ]
        ModalCreateItem -> Modal.renderModal "Créer un item" [ renderCreateContent draft validationError ]
          (ViewAction ViewCloseCreate)
          (SyncAction SyncSubmitIntention)
        ModalNotifications -> renderModal "Rappels" [ NotificationAction <$> renderNotificationsContent notifications.defaults notifications.overrides notifications.editor notifications.intentions ]
        ModalTemplates -> renderModal "Templates de tâches" [ renderTemplatesPanelPage templates ]
        ModalImportCsv -> renderModal "Import CSV" [ renderCsvImportPanelPage csvImport.input csvImport.result ]
        ModalImportIcs -> renderModal "Import ICS" [ renderIcsImportPanelPage icsImport.input icsImport.result ]
        ModalExport -> renderModal "Export" [ renderExportModal exportItems ]
        ModalEditItem -> case editPanel of
          Nothing -> text ""
          Just panel ->
            Modal.renderModal "Modifier l'item"
              [ renderEditContent panel ]
              (ViewAction ViewEditCancel)
              (ViewAction ViewEditSave)
      activeModal

buildAgendaModalsInput :: State -> AgendaModalsInput
buildAgendaModalsInput { calendar, sync, notifications, templates, imports, view } =
  let
    { items, draft, validationError, showConflictsOnly, sortMode } = calendar
    SyncState { offlineMode } = sync
    NotificationState { notificationDefaults, notificationOverrides, notificationEditor } = notifications
    ImportState { csvInput, csvImportResult, icsInput, icsImportResult } = imports
    ViewState { activeModal, editPanel } = view
    conflictGroups = detectConflictGroups items
  in
    { activeModal
    , filters:
        { showConflictsOnly
        , conflictGroups
        , offlineMode
        , sortMode
        }
    , notifications:
        { defaults: notificationDefaults
        , overrides: notificationOverrides
        , editor: notificationEditor
        , intentions: filter (isUnplannedIntention items) items
        }
    , templates
    , csvImport:
        { input: csvInput
        , result: csvImportResult
        }
    , icsImport:
        { input: icsInput
        , result: icsImportResult
        }
    , exportItems: map toExportItem items
    , draft
    , validationError
    , editPanel
    }

renderAccordion :: forall w i. String -> String -> HTML w i -> HTML w i
renderAccordion title extraClass content =
  div [ class_ ("calendar-accordion-wrap " <> extraClass) ]
    [ details [ class_ "calendar-accordion-details" ]
        [ summary [ class_ "calendar-accordion-summary" ] [ text title ]
        , div [ class_ "calendar-accordion-content" ] [ content ]
        ]
    ]

-- END src/Pages/Calendar.purs

-- BEGIN src/Calendar/Calendar.purs

-- END src/Calendar/Calendar.purs

-- BEGIN src/Calendar/Calendar/Agenda/Day.purs
data DayAction
  = DayListAction ListAction
  | DayDragAction DragAction
  | DayViewAction ViewAction

renderDayCalendar
  :: forall w
   . String
  -> Array String
  -> Array CalendarItem
  -> Boolean
  -> Maybe String
  -> Maybe Int
  -> HTML w DayAction
renderDayCalendar focusDate conflictIds items isMobile draggingId dragHoverIndex =
  let
    itemsForDate = filter (isItemOnDate focusDate) items
    sorted = sortItems SortByTime conflictIds itemsForDate
    layout = buildTimelineLayout sorted
  in
    if null itemsForDate then emptyAgenda
    else
      div [ class_ "calendar-calendar" ]
        [ div [ class_ "calendar-calendar-header" ]
            [ div [ class_ "calendar-calendar-title" ] [ text focusDate ]
            , div [ class_ "calendar-calendar-count" ] [ text $ show (length itemsForDate) <> " items" ]
            ]
        , div [ class_ "calendar-calendar-body" ]
            [ div [ class_ "calendar-calendar-hours" ]
                (map renderHourLabel (enumFromTo 0 23) <> [ renderHourLabelEnd ])
            , div
                [ class_ "calendar-calendar-grid"
                , onDragOver (\ev -> DayDragAction (DragOverCalendar ev))
                , onDrop (\ev -> DayDragAction (DropOnCalendar ev))
                , onTouchMove (\ev -> DayDragAction (DragTouchMoveCalendar ev))
                , onTouchEnd (const (DayDragAction DragTouchEnd))
                , onTouchCancel (const (DayDragAction DragTouchCancel))
                ]
                [ div [ class_ "calendar-calendar-lines" ]
                    (map renderHourLine (enumFromTo 0 23))
                , maybe (text "") renderDropIndicator dragHoverIndex
                , div
                    [ class_ $ "calendar-calendar-items" <> if draggingId == Nothing then "" else " calendar-calendar-items--dragging" ]
                    (map (renderTimelineItem conflictIds isMobile draggingId) layout)
                ]
            ]
        ]

renderHourLabel :: forall w action. Int -> HTML w action
renderHourLabel h =
  div [ class_ "calendar-calendar-hour" ] [ text $ DateTime.formatLocalTimeParts h 0 ]

renderHourLabelEnd :: forall w action. HTML w action
renderHourLabelEnd =
  div [ class_ "calendar-calendar-hour calendar-calendar-hour--end" ] [ text "24:00" ]

renderHourLine :: forall w action. Int -> HTML w action
renderHourLine _ =
  div [ class_ "calendar-calendar-line" ] []

renderTimelineItem
  :: forall w
   . Array String
  -> Boolean
  -> Maybe String
  -> TimelineLayout
  -> HTML w DayAction
renderTimelineItem conflictIds isMobile draggingId layout =
  let
    content = calendarItemContent layout.item
    typeClass =
      case content.itemType of
        ScheduledBlock -> " calendar-calendar-item--scheduled"
        Intention -> " calendar-calendar-item--intention"
    conflictClass = guard (isConflict conflictIds layout.item) " calendar-calendar-item--conflict"
    draggingClass =
      case { draggingId, item: layout.item } of
        { draggingId: Just activeId, item: ServerCalendarItem { id } } | activeId == id ->
          " calendar-calendar-item--dragging"
        _ -> ""
    inlineStyle =
      fold
        [ " --start:"
        , show layout.startMin
        , ";"
        , " --duration:"
        , show layout.duration
        , ";"
        , " --column:"
        , show layout.columnIndex
        , ";"
        , " --columns:"
        , show layout.columnCount
        , ";"
        ]
    dragProps = dragCalendarHandlers DayDragAction layout.item
    editProps = editHandlers DayListAction isMobile draggingId layout.item
  in
    div
      ( [ class_ "calendar-calendar-item"
        , style inlineStyle
        ] <> editProps
      )
      [ renderTimelineEditButton isMobile layout.item
      , div
          ( [ class_ $ "calendar-calendar-card" <> typeClass <> conflictClass <> draggingClass
            ] <> dragProps
          )
          [ div [ class_ "calendar-calendar-meta" ]
              [ div [ class_ "calendar-calendar-item-time" ]
                  [ text $ timeLabel content.windowStart <> " → " <> timeLabel content.windowEnd ]
              , div [ class_ "calendar-calendar-item-title" ] [ text content.title ]
              , div [ class_ "calendar-calendar-footer" ]
                  [ renderCategory content.category
                  , div [ class_ "calendar-calendar-actions" ]
                      [ map DayListAction (renderPrimaryAction layout.item content) ]
                  ]
              ]
          ]
      ]

renderTimelineEditButton :: forall w. Boolean -> CalendarItem -> HTML w DayAction
renderTimelineEditButton isMobile item =
  if isMobile then text ""
  else
    case item of
      ServerCalendarItem _ ->
        button
          [ class_ "btn btn-sm btn-outline-secondary calendar-edit calendar-edit--timeline"
          , attr (AttrName "aria-label") "Editer"
          , onMouseDown (const (DayViewAction (ViewOpenEdit item)))
          , onClick (const (DayViewAction (ViewOpenEdit item)))
          ]
          [ i [ class_ "bi bi-pencil" ] [] ]
      _ -> text ""

-- END src/Calendar/Calendar/Agenda/Day.purs

-- BEGIN src/Calendar/Calendar/Agenda/List.purs
data ListAction
  = ListViewAction ViewAction
  | ListSyncAction SyncAction
  | ListDragAction DragAction

agendaList
  :: forall w
   . Array String
  -> Array CalendarItem
  -> Boolean
  -> HTML w ListAction
agendaList conflictIds items isMobile =
  ul [ class_ "list-group entity-list calendar-list" ] (mapWithIndex (renderItem conflictIds isMobile) items)

renderItem
  :: forall w
   . Array String
  -> Boolean
  -> Int
  -> CalendarItem
  -> HTML w ListAction
renderItem conflictIds isMobile _ item =
  let
    content = calendarItemContent item
    conflictClass = guard (isConflict conflictIds item) " calendar-card--conflict"
    dragProps = dragHandlers ListDragAction item
    editProps = editHandlers identity isMobile Nothing item
  in
    li ([ class_ $ "row list-group-item entity-card calendar-card" <> conflictClass ] <> dragProps <> editProps)
      [ div [ class_ "col entity-card-body" ]
          [ div [ class_ "calendar-card-time" ] [ text (timeLabel content.windowStart) ]
          , div [ class_ "calendar-card-title" ] [ text content.title ]
          , div [ class_ "calendar-card-window" ]
              [ text $ formatDateTimeLocal content.windowStart <> " → " <> formatDateTimeLocal content.windowEnd ]
          , renderCategory content.category
          , renderPrimaryAction item content
          ]
      ]

renderPrimaryAction
  :: forall w
   . CalendarItem
  -> CalendarItemContent
  -> HTML w ListAction
renderPrimaryAction item content =
  case primaryActionFor item of
    PrimaryPlanify ->
      case item of
        ServerCalendarItem { id } ->
          button [ class_ "btn btn-sm btn-outline-primary calendar-primary-action", onClick (const (ListSyncAction (SyncPlanifyFrom id content))) ]
            [ text "Planifier" ]
        _ -> text ""
    PrimaryValidate ->
      case item of
        ServerCalendarItem { id } ->
          button [ class_ "btn btn-sm btn-outline-success calendar-primary-action", onClick (const (ListViewAction (ViewOpenValidation id content))) ]
            [ text "Valider" ]
        _ -> text ""
    PrimaryNone -> text ""

editHandlers
  :: forall r action
   . (ListAction -> action)
  -> Boolean
  -> Maybe String
  -> CalendarItem
  -> Array
       ( IProp
           ( onDoubleClick :: MouseEvent.MouseEvent
           , onTouchEnd :: TouchEvent.TouchEvent
           | r
           )
           action
       )
editHandlers toAction isMobile draggingId item =
  if isMobile then
    if draggingId == Nothing then
      [ onTouchEnd (const (toAction (ListViewAction (ViewMobileTap item)))) ]
    else
      []
  else
    []

renderCategory :: forall w action. Maybe String -> HTML w action
renderCategory category =
  case category of
    Nothing -> text ""
    Just value -> div [ class_ "calendar-card-category" ] [ text value ]

emptyAgenda :: forall w action. HTML w action
emptyAgenda =
  div [ class_ "row entity-empty calendar-empty" ]
    [ div [ class_ "entity-empty-title" ] [ text "Aucune intention aujourd'hui" ]
    , div [ class_ "entity-empty-subtitle" ] [ text "Ajoutez une intention pour demarrer votre journee." ]
    , div [ class_ "calendar-empty-cta" ]
        [ span [ class_ "badge rounded-pill text-bg-primary" ] [ text "Astuce" ]
        , span [ class_ "text-muted" ] [ text "Commencez par un titre et appuyez sur Entrée." ]
        ]
    ]

-- END src/Calendar/Calendar/Agenda/List.purs

-- BEGIN src/Calendar/Calendar/Agenda/Range.purs
data RangeAction = RangeListAction ListAction

renderRangeView
  :: forall w
   . String
  -> Array String
  -> Array String
  -> Array CalendarItem
  -> Boolean
  -> HTML w RangeAction
renderRangeView label dates conflictIds items isMobile =
  if null dates then emptyAgendaRange label
  else
    div [ class_ "calendar-range" ]
      (map (renderDateSection label conflictIds items isMobile) dates)

renderDateSection
  :: forall w
   . String
  -> Array String
  -> Array CalendarItem
  -> Boolean
  -> String
  -> HTML w RangeAction
renderDateSection _ conflictIds items isMobile dateStr =
  let
    itemsForDate = filter (isItemOnDate dateStr) items
    sorted = sortItems SortByTime conflictIds itemsForDate
  in
    section [ class_ "calendar-date-section" ]
      [ div [ class_ "calendar-date-title" ] [ text dateStr ]
      , if null sorted then div [ class_ "calendar-date-empty" ] [ text "Aucun item" ]
        else map RangeListAction (agendaList conflictIds sorted isMobile)
      ]

emptyAgendaRange :: forall w action. String -> HTML w action
emptyAgendaRange label =
  div [ class_ "row entity-empty calendar-empty" ]
    [ div [ class_ "entity-empty-title" ] [ text $ "Aucun item sur la " <> label ]
    , div [ class_ "entity-empty-subtitle" ] [ text "Ajoutez une intention pour demarrer." ]
    ]

-- END src/Calendar/Calendar/Agenda/Range.purs

-- BEGIN src/Calendar/Calendar/Controls.purs
data ControlsAction
  = ControlsSortChanged String
  | ControlsToggleConflictFilter

applyControlsAction :: ControlsAction -> CalendarState -> CalendarState
applyControlsAction action dataState =
  case action of
    ControlsSortChanged raw ->
      (_sortModeS .~ parseSortMode raw) dataState
    ControlsToggleConflictFilter ->
      (_showConflictsOnlyS %~ not) dataState

renderSortPicker :: forall w. SortMode -> HTML w ControlsAction
renderSortPicker sortMode =
  div [ class_ "calendar-sort" ]
    [ text "Trier:"
    , select
        [ class_ "form-select calendar-sort-select"
        , onValueChange ControlsSortChanged
        , value (sortModeValue sortMode)
        ]
        [ option [ value "time" ] [ text "Horaire" ]
        , option [ value "status" ] [ text "Statut" ]
        , option [ value "category" ] [ text "Catégorie" ]
        , option [ value "conflict" ] [ text "Conflit" ]
        ]
    ]

-- END src/Calendar/Calendar/Controls.purs

-- BEGIN src/Calendar/Calendar/CreateForm.purs
data CreateFormAction
  = CreateFormDraftTitleChanged String
  | CreateFormDraftTypeChanged ItemType
  | CreateFormDraftStartChanged String
  | CreateFormDraftEndChanged String
  | CreateFormDraftCategoryChanged String
  | CreateFormDraftStatusChanged String
  | CreateFormDraftDurationChanged String
  | CreateFormSync SyncAction

applyCreateFormAction :: CreateFormAction -> CalendarState -> CalendarState
applyCreateFormAction action dataState =
  case action of
    CreateFormDraftTitleChanged title ->
      ((_draftTitleS .~ title) <<< (_validationError .~ Nothing)) dataState
    CreateFormDraftTypeChanged itemType ->
      ((_draftItemTypeS .~ itemType) <<< (_lastCreateType .~ itemType) <<< (_validationError .~ Nothing)) dataState
    CreateFormDraftStartChanged windowStart ->
      ((_draftWindowStartS .~ windowStart) <<< (_validationError .~ Nothing)) dataState
    CreateFormDraftEndChanged windowEnd ->
      ((_draftWindowEndS .~ windowEnd) <<< (_validationError .~ Nothing)) dataState
    CreateFormDraftCategoryChanged category ->
      ((_draftCategoryS .~ category) <<< (_validationError .~ Nothing)) dataState
    CreateFormDraftStatusChanged raw ->
      ((_draftStatusS .~ parseStatus raw) <<< (_validationError .~ Nothing)) dataState
    CreateFormDraftDurationChanged raw ->
      ((_draftDurationS .~ raw) <<< (_validationError .~ Nothing)) dataState
    CreateFormSync _ ->
      dataState

renderCreateContent
  :: IntentionDraft
  -> Maybe String
  -> H.ComponentHTML Action Slots Aff
renderCreateContent draft validationError =
  div [ class_ "calendar-modal-stack" ]
    [ field "Type" typeInput
    , field "Titre" titleInput
    , dateTimeField "Début" CreateFormDraftStartChanged draft.windowStart
    , dateTimeField "Fin" CreateFormDraftEndChanged draft.windowEnd
    , field "Catégorie" categoryInput
    , field "Statut" statusInput
    , field "Durée réelle (minutes)" durationInput
    , recurrenceInput
    , errorInput
    ]
  where
  field label content =
    div [ class_ "calendar-modal-field" ]
      [ div [ class_ "calendar-notifications-label" ] [ text label ]
      , content
      ]

  dateTimeField label onChange currentValue =
    field label $
      input
        [ class_ "form-control calendar-input"
        , type_ InputDatetimeLocal
        , attr (AttrName "lang") "fr"
        , placeholder label
        , onValueChange (CreateFormAction <<< onChange)
        , value currentValue
        ]

  typeInput =
    div [ class_ "btn-group w-100", attr (AttrName "role") "group" ]
      [ toggleButton Intention "Intention"
      , toggleButton ScheduledBlock "Bloc planifié"
      ]

  titleInput =
    input
      [ class_ "form-control calendar-input"
      , placeholder "Titre"
      , onValueChange (CreateFormAction <<< CreateFormDraftTitleChanged)
      , onKeyDown (\ev -> CreateFormAction (CreateFormSync (SyncDraftTitleKeyDown (KE.key ev))))
      , value draft.title
      ]

  categoryInput =
    input
      [ class_ "form-control calendar-input"
      , placeholder "Catégorie"
      , onValueChange (CreateFormAction <<< CreateFormDraftCategoryChanged)
      , value draft.category
      ]

  statusInput =
    select
      [ class_ "form-select calendar-input"
      , onValueChange (CreateFormAction <<< CreateFormDraftStatusChanged)
      , value (statusValue draft.status)
      ]
      [ option [ value "todo" ] [ text "À faire" ]
      , option [ value "progress" ] [ text "En cours" ]
      , option [ value "done" ] [ text "Fait" ]
      , option [ value "canceled" ] [ text "Annulé" ]
      ]

  durationInput =
    input
      [ class_ "form-control calendar-input"
      , type_ InputNumber
      , placeholder "Ex: 30"
      , onValueChange (CreateFormAction <<< CreateFormDraftDurationChanged)
      , value draft.actualDurationMinutes
      ]

  recurrenceInput =
    renderRecurrenceSlot RecurrenceCreate draft.recurrence CreateRecurrenceCmd

  errorInput =
    maybe (text "") (\msg -> div [ class_ "calendar-error" ] [ text msg ]) validationError

  toggleButton :: ItemType -> String -> H.ComponentHTML Action Slots Aff
  toggleButton itemType label =
    button
      [ class_ $ "btn btn-sm " <> if draft.itemType == itemType then "btn-primary" else "btn-outline-secondary"
      , onClick (const (CreateFormAction (CreateFormDraftTypeChanged itemType)))
      ]
      [ text label ]

-- END src/Calendar/Calendar/CreateForm.purs

-- BEGIN src/Calendar/Calendar/Draft.purs
toNewIntention :: IntentionDraft -> Either String CalendarItem
toNewIntention draft = do
  recurrence <- case draftToRecurrence draft.recurrence of
    Left err -> Left err
    Right ok -> Right ok
  actualDuration <- parseDuration draft.actualDurationMinutes
  windowStart <- maybe (Left "Début invalide (format YYYY-MM-DDTHH:MM).") Right (parseDateTimeLocal draft.windowStart)
  windowEnd <- maybe (Left "Fin invalide (format YYYY-MM-DDTHH:MM).") Right (parseDateTimeLocal draft.windowEnd)
  if windowEnd <= windowStart then Left "La fin doit être après le début."
  else
    Right
      ( NewCalendarItem
          { content:
              { itemType: draft.itemType
              , title: draft.title
              , windowStart
              , windowEnd
              , status: draft.status
              , sourceItemId: Nothing
              , actualDurationMinutes: actualDuration
              , category: toOptionalString draft.category
              , recurrenceRule: recurrence.rule
              , recurrenceExceptionDates: parseExceptionDatesOrEmpty recurrence.exceptions
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

-- END src/Calendar/Calendar/Draft.purs

-- BEGIN src/Calendar/Calendar/Primary.purs
primaryActionFor :: CalendarItem -> PrimaryAction
primaryActionFor (ServerCalendarItem { content }) =
  case content.itemType of
    Intention -> PrimaryPlanify
    ScheduledBlock ->
      if content.status /= Fait then PrimaryValidate else PrimaryNone
primaryActionFor _ = PrimaryNone

-- END src/Calendar/Calendar/Primary.purs

-- BEGIN src/Calendar/Calendar/State.purs
type CalendarState =
  { items :: Array CalendarItem
  , draft :: IntentionDraft
  , validationError :: Maybe String
  , lastCreateType :: ItemType
  , showConflictsOnly :: Boolean
  , conflictResolution :: Maybe ConflictResolution
  , sortMode :: SortMode
  }

calendarInitialState :: CalendarState
calendarInitialState =
  { items: []
  , draft: emptyDraft
  , validationError: Nothing
  , lastCreateType: Intention
  , showConflictsOnly: false
  , conflictResolution: Nothing
  , sortMode: SortByTime
  }

emptyDraft :: IntentionDraft
emptyDraft =
  { itemType: Intention
  , title: ""
  , windowStart: ""
  , windowEnd: ""
  , category: ""
  , status: Todo
  , actualDurationMinutes: ""
  , recurrence: defaultRecurrenceDraft
  }

_items :: Lens' CalendarState (Array CalendarItem)
_items = prop (Proxy :: _ "items")

_draft :: Lens' CalendarState IntentionDraft
_draft = prop (Proxy :: _ "draft")

_validationError :: Lens' CalendarState (Maybe String)
_validationError = prop (Proxy :: _ "validationError")

_showConflictsOnlyS :: Lens' CalendarState Boolean
_showConflictsOnlyS = prop (Proxy :: _ "showConflictsOnly")

_conflictResolutionS :: Lens' CalendarState (Maybe ConflictResolution)
_conflictResolutionS = prop (Proxy :: _ "conflictResolution")

_sortModeS :: Lens' CalendarState SortMode
_sortModeS = prop (Proxy :: _ "sortMode")

_draftTitleS :: Lens' CalendarState String
_draftTitleS = _draft <<< prop (Proxy :: _ "title")

_draftItemTypeS :: Lens' CalendarState ItemType
_draftItemTypeS = _draft <<< prop (Proxy :: _ "itemType")

_draftWindowStartS :: Lens' CalendarState String
_draftWindowStartS = _draft <<< prop (Proxy :: _ "windowStart")

_draftWindowEndS :: Lens' CalendarState String
_draftWindowEndS = _draft <<< prop (Proxy :: _ "windowEnd")

_draftCategoryS :: Lens' CalendarState String
_draftCategoryS = _draft <<< prop (Proxy :: _ "category")

_draftStatusS :: Lens' CalendarState ItemStatus
_draftStatusS = _draft <<< prop (Proxy :: _ "status")

_draftDurationS :: Lens' CalendarState String
_draftDurationS = _draft <<< prop (Proxy :: _ "actualDurationMinutes")

_draftRecurrenceS :: Lens' CalendarState RecurrenceDraft
_draftRecurrenceS = _draft <<< prop (Proxy :: _ "recurrence")

_lastCreateType :: Lens' CalendarState ItemType
_lastCreateType = prop (Proxy :: _ "lastCreateType")

-- END src/Calendar/Calendar/State.purs

-- BEGIN src/Calendar/Calendar/Timeline.purs
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
toTimelineBlock item =
  let
    content = calendarItemContent item
    startMin = minuteOfDay content.windowStart
    endMinRaw = minuteOfDay content.windowEnd
    startClamped = clamp 0 1439 startMin
    endAdjusted = if endMinRaw <= startMin then 1440 else endMinRaw
    endClamped = clamp (startClamped + 1) 1440 endAdjusted
  in
    if endClamped <= 0 || startClamped >= 1440 then Nothing
    else Just { item, startMin: startClamped, endMin: endClamped }

-- END src/Calendar/Calendar/Timeline.purs

-- BEGIN src/Calendar/Calendar/Types.purs
type ConflictResolution =
  { groupIds :: Array String
  , pendingStrategy :: Maybe ResolutionStrategy
  }

data ResolutionStrategy
  = StrategyShift30
  | StrategySwap

derive instance resolutionStrategyGeneric :: Generic ResolutionStrategy _
derive instance resolutionStrategyEq :: Eq ResolutionStrategy
instance resolutionStrategyShow :: Show ResolutionStrategy where
  show = genericShow

data PrimaryAction
  = PrimaryPlanify
  | PrimaryValidate
  | PrimaryNone

derive instance eqPrimaryAction :: Eq PrimaryAction
derive instance primaryActionGeneric :: Generic PrimaryAction _
instance showPrimaryAction :: Show PrimaryAction where
  show = genericShow

-- END src/Calendar/Calendar/Types.purs

-- BEGIN src/Calendar/Conflict.purs
type ConflictBlock =
  { id :: String
  , start :: DateTime
  , end :: DateTime
  }

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

data ConflictAction
  = ConflictOpenResolution (Array String)
  | ConflictChooseStrategy ResolutionStrategy
  | ConflictConfirm
  | ConflictCancel

applyConflictAction :: ConflictAction -> CalendarState -> CalendarState
applyConflictAction action dataState =
  case action of
    ConflictOpenResolution groupIds ->
      (_conflictResolutionS .~ Just { groupIds, pendingStrategy: Nothing }) dataState
    ConflictChooseStrategy strategy ->
      (_conflictResolutionS %~ map (_ { pendingStrategy = Just strategy })) dataState
    ConflictConfirm ->
      (_conflictResolutionS .~ Nothing) dataState
    ConflictCancel ->
      (_conflictResolutionS .~ Nothing) dataState

renderConflictActions :: forall w. Array (Array String) -> HTML w ConflictAction
renderConflictActions conflictGroups =
  if null conflictGroups then text ""
  else
    div [ class_ "calendar-conflict-actions" ]
      [ button
          [ class_ "btn btn-sm btn-outline-danger calendar-conflict-button"
          , onClick (const (ConflictOpenResolution (headOrEmpty conflictGroups)))
          ]
          [ text "Résoudre un conflit" ]
      ]
  where
  headOrEmpty groups =
    case uncons groups of
      Just { head } -> head
      Nothing -> []

renderConflictResolution :: forall w. Array CalendarItem -> ConflictResolution -> HTML w ConflictAction
renderConflictResolution items resolution =
  div [ class_ "calendar-conflict-panel" ]
    [ div [ class_ "calendar-conflict-title" ] [ text "Résolution de conflit" ]
    , div [ class_ "calendar-conflict-subtitle" ] [ text "Choisissez une stratégie puis confirmez." ]
    , ul [ class_ "calendar-conflict-list" ] (map (renderConflictItem items) resolution.groupIds)
    , div [ class_ "calendar-conflict-strategies" ]
        [ button
            [ class_ "btn btn-sm btn-outline-primary"
            , onClick (const (ConflictChooseStrategy StrategyShift30))
            ]
            [ text "Décaler de 30 min" ]
        , button
            [ class_ "btn btn-sm btn-outline-primary"
            , onClick (const (ConflictChooseStrategy StrategySwap))
            ]
            [ text "Échanger" ]
        ]
    , renderConfirmation resolution.pendingStrategy
    , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ConflictCancel) ] [ text "Fermer" ]
    ]

renderConflictItem :: forall w action. Array CalendarItem -> String -> HTML w action
renderConflictItem items itemId =
  case find (matchId itemId) items of
    Just item ->
      let
        content = calendarItemContent item
      in
        li [ class_ "calendar-conflict-item" ]
          [ div [ class_ "calendar-conflict-item-title" ] [ text content.title ]
          , div [ class_ "calendar-conflict-item-window" ]
              [ text $ formatDateTimeLocal content.windowStart <> " → " <> formatDateTimeLocal content.windowEnd ]
          ]
    Nothing -> text ""
  where
  matchId id (ServerCalendarItem { id: candidate }) = id == candidate
  matchId _ _ = false

renderConfirmation :: forall w. Maybe ResolutionStrategy -> HTML w ConflictAction
renderConfirmation pending =
  case pending of
    Nothing -> text ""
    Just strategy ->
      div [ class_ "calendar-conflict-confirmation" ]
        [ div [ class_ "calendar-conflict-confirmation-text" ]
            [ text $ "Confirmer la stratégie: " <> show strategy <> " ?" ]
        , div [ class_ "calendar-conflict-confirmation-actions" ]
            [ button [ class_ "btn btn-sm btn-danger", onClick (const ConflictConfirm) ] [ text "Confirmer" ]
            , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ConflictCancel) ] [ text "Annuler" ]
            ]
        ]

-- END src/Calendar/Conflict.purs

-- BEGIN src/Calendar/Display.purs
data AgendaModal
  = ModalNotifications
  | ModalTemplates
  | ModalImportCsv
  | ModalImportIcs
  | ModalExport
  | ModalTools
  | ModalFilters
  | ModalCreateItem
  | ModalEditItem

derive instance eqAgendaModal :: Eq AgendaModal

type ValidationPanel =
  { itemId :: String
  , proposedMinutes :: Maybe Int
  , inputValue :: String
  }

newtype ViewState = ViewState
  { viewMode :: AgendaView
  , focusDate :: String
  , activeModal :: Maybe AgendaModal
  , validationPanel :: Maybe ValidationPanel
  , editPanel :: Maybe EditPanel
  , isMobile :: Boolean
  , lastTapAt :: Maybe Instant
  }

viewInitialState :: ViewState
viewInitialState =
  ViewState
    { viewMode: ViewDay
    , focusDate: ""
    , activeModal: Nothing
    , validationPanel: Nothing
    , editPanel: Nothing
    , isMobile: false
    , lastTapAt: Nothing
    }

_viewModeS :: Lens' ViewState AgendaView
_viewModeS =
  lens
    (\(ViewState state) -> state.viewMode)
    (\(ViewState state) viewMode -> ViewState (state { viewMode = viewMode }))

_viewFocusDateState :: Lens' ViewState String
_viewFocusDateState =
  lens
    (\(ViewState state) -> state.focusDate)
    (\(ViewState state) focusDate -> ViewState (state { focusDate = focusDate }))

_viewActiveModalS :: Lens' ViewState (Maybe AgendaModal)
_viewActiveModalS =
  lens
    (\(ViewState state) -> state.activeModal)
    (\(ViewState state) activeModal -> ViewState (state { activeModal = activeModal }))

_viewValidationPanelS :: Lens' ViewState (Maybe ValidationPanel)
_viewValidationPanelS =
  lens
    (\(ViewState state) -> state.validationPanel)
    (\(ViewState state) validationPanel -> ViewState (state { validationPanel = validationPanel }))

_viewEditPanelS :: Lens' ViewState (Maybe EditPanel)
_viewEditPanelS =
  lens
    (\(ViewState state) -> state.editPanel)
    (\(ViewState state) editPanel -> ViewState (state { editPanel = editPanel }))

_viewIsMobileS :: Lens' ViewState Boolean
_viewIsMobileS =
  lens
    (\(ViewState state) -> state.isMobile)
    (\(ViewState state) isMobile -> ViewState (state { isMobile = isMobile }))

_viewLastTapAtS :: Lens' ViewState (Maybe Instant)
_viewLastTapAtS =
  lens
    (\(ViewState state) -> state.lastTapAt)
    (\(ViewState state) lastTapAt -> ViewState (state { lastTapAt = lastTapAt }))

type EditPanel =
  { item :: CalendarItem
  , draft :: EditDraft
  , validationError :: Maybe String
  }

data ViewAction
  = ViewOpenValidation String CalendarItemContent
  | ViewValidationMinutesChanged String
  | ViewConfirmValidation
  | ViewCancelValidation
  | ViewChangedAction String
  | ViewFocusDateChanged String
  | ViewOpenModal AgendaModal
  | ViewCloseModal
  | ViewOpenCreate
  | ViewCloseCreate
  | ViewOpenEdit CalendarItem
  | ViewOpenEditFromDoubleClick CalendarItem
  | ViewMobileTap CalendarItem
  | ViewEditTitleChanged String
  | ViewEditStartChanged String
  | ViewEditEndChanged String
  | ViewEditCategoryChanged String
  | ViewEditStatusChanged String
  | ViewEditDurationChanged String
  | ViewEditSave
  | ViewEditCancel
  | ViewSetIsMobile Boolean

data ViewCommand
  = ViewValidateItem String Int
  | ViewUpdateItem String CalendarItem

handleViewAction :: ViewAction -> StateT ViewState (WriterT (Array ViewCommand) Aff) Unit
handleViewAction = case _ of
  ViewOpenValidation itemId content -> do
    suggested <- liftEffect $ suggestDurationMinutes content.windowStart
    modify_ (_viewValidationPanelS .~ Just { itemId, proposedMinutes: suggested, inputValue: "" })
  ViewValidationMinutesChanged raw ->
    modify_ (_viewValidationPanelS %~ map (_ { inputValue = raw }))
  ViewConfirmValidation -> do
    viewState <- get
    case viewState ^. _viewValidationPanelS of
      Nothing -> pure unit
      Just panel -> do
        let duration = parsePositiveInt panel.inputValue <|> panel.proposedMinutes
        case duration of
          Nothing -> pure unit
          Just minutes -> do
            modify_ (_viewValidationPanelS .~ Nothing)
            tell [ ViewValidateItem panel.itemId minutes ]
  ViewCancelValidation ->
    modify_ (_viewValidationPanelS .~ Nothing)
  ViewChangedAction raw ->
    modify_ (_viewModeS .~ parseAgendaView raw)
  ViewFocusDateChanged raw ->
    modify_ (_viewFocusDateState .~ raw)
  ViewOpenModal modal ->
    modify_ (_viewActiveModalS .~ Just modal)
  ViewCloseModal ->
    modify_ (_viewActiveModalS .~ Nothing)
  ViewOpenCreate ->
    modify_ (_viewActiveModalS .~ Just ModalCreateItem)
  ViewCloseCreate ->
    modify_ (_viewActiveModalS .~ Nothing)
  ViewOpenEdit item ->
    case buildEditDraft item of
      Nothing -> pure unit
      Just draft ->
        modify_
          ( (_viewEditPanelS .~ Just { item, draft, validationError: Nothing })
              <<< (_viewActiveModalS .~ Just ModalEditItem)
              <<< (_viewLastTapAtS .~ Nothing)
          )
  ViewOpenEditFromDoubleClick item -> do
    viewport <- liftEffect $ window >>= Window.innerWidth
    let isMobileNow = viewport <= 768
    modify_ (_viewIsMobileS .~ isMobileNow)
    if isMobileNow then
      handleViewAction (ViewOpenEdit item)
    else
      pure unit
  ViewMobileTap item -> do
    viewState <- get
    if viewState ^. _viewIsMobileS then do
      now <- liftEffect Now.now
      case viewState ^. _viewLastTapAtS of
        Nothing ->
          modify_ (_viewLastTapAtS .~ Just now)
        Just previous ->
          let
            elapsedMs = case Instant.diff now previous of Milliseconds ms -> ms
          in
            if elapsedMs <= 350.0 then
              modify_ (_viewLastTapAtS .~ Nothing) *> handleViewAction (ViewOpenEdit item)
            else
              modify_ (_viewLastTapAtS .~ Just now)
    else
      pure unit
  ViewEditTitleChanged raw ->
    modify_ (_viewEditPanelS %~ map (\panel -> panel { draft = panel.draft { title = raw }, validationError = Nothing }))
  ViewEditStartChanged raw ->
    modify_ (_viewEditPanelS %~ map (\panel -> panel { draft = panel.draft { windowStart = raw }, validationError = Nothing }))
  ViewEditEndChanged raw ->
    modify_ (_viewEditPanelS %~ map (\panel -> panel { draft = panel.draft { windowEnd = raw }, validationError = Nothing }))
  ViewEditCategoryChanged raw ->
    modify_ (_viewEditPanelS %~ map (\panel -> panel { draft = panel.draft { category = raw }, validationError = Nothing }))
  ViewEditStatusChanged raw ->
    modify_ (_viewEditPanelS %~ map (\panel -> panel { draft = panel.draft { status = parseStatus raw }, validationError = Nothing }))
  ViewEditDurationChanged raw ->
    modify_ (_viewEditPanelS %~ map (\panel -> panel { draft = panel.draft { actualDurationMinutes = raw }, validationError = Nothing }))
  ViewEditSave -> do
    viewState <- get
    case viewState ^. _viewEditPanelS of
      Nothing -> pure unit
      Just panel ->
        case applyEditDraft panel.draft panel.item of
          Left err ->
            modify_ (_viewEditPanelS .~ Just (panel { validationError = Just (editErrorMessage err) }))
          Right updatedItem -> do
            tell [ ViewUpdateItem panel.draft.itemId updatedItem ]
            modify_ ((_viewEditPanelS .~ Nothing) <<< (_viewActiveModalS .~ Nothing))
  ViewEditCancel ->
    modify_ ((_viewEditPanelS .~ Nothing) <<< (_viewActiveModalS .~ Nothing))
  ViewSetIsMobile isMobile ->
    modify_ (_viewIsMobileS .~ isMobile)

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

renderViewSelector :: forall w. AgendaView -> String -> HTML w ViewAction
renderViewSelector viewMode focusDate =
  div [ class_ "calendar-view-selector" ]
    [ div [ class_ "calendar-view-buttons" ]
        [ button
            [ class_ $ "btn btn-sm " <> if viewMode == ViewDay then "btn-primary" else "btn-outline-secondary"
            , onClick (const (ViewChangedAction "day"))
            ]
            [ text "Jour" ]
        , button
            [ class_ $ "btn btn-sm " <> if viewMode == ViewWeek then "btn-primary" else "btn-outline-secondary"
            , onClick (const (ViewChangedAction "week"))
            ]
            [ text "Semaine" ]
        , button
            [ class_ $ "btn btn-sm " <> if viewMode == ViewMonth then "btn-primary" else "btn-outline-secondary"
            , onClick (const (ViewChangedAction "month"))
            ]
            [ text "Mois" ]
        ]
    , div [ class_ "calendar-view-date-field" ]
        [ div [ class_ "calendar-notifications-label" ] [ text "Date de référence" ]
        , input
            [ class_ "form-control calendar-input calendar-view-date"
            , type_ InputDate
            , attr (AttrName "lang") "fr"
            , value focusDate
            , onValueChange ViewFocusDateChanged
            ]
        ]
    ]

renderMobileTools :: forall w. AgendaView -> HTML w ViewAction
renderMobileTools _ =
  div [ class_ "calendar-mobile-tools" ]
    [ button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (ViewOpenModal ModalFilters)) ] [ text "Filtres" ]
    , button [ class_ "btn btn-sm btn-primary", onClick (const (ViewOpenModal ModalTools)) ] [ text "Outils" ]
    ]

renderToolsContent :: forall w. HTML w ViewAction
renderToolsContent =
  div [ class_ "calendar-modal-stack" ]
    [ button [ class_ "btn btn-outline-secondary", onClick (const (ViewOpenModal ModalNotifications)) ] [ text "Rappels" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const (ViewOpenModal ModalTemplates)) ] [ text "Templates" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const (ViewOpenModal ModalImportCsv)) ] [ text "Import CSV" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const (ViewOpenModal ModalImportIcs)) ] [ text "Import ICS" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const (ViewOpenModal ModalExport)) ] [ text "Export" ]
    ]

renderEditContent :: EditPanel -> H.ComponentHTML Action Slots Aff
renderEditContent panel =
  let
    draft = panel.draft
  in
    div [ class_ "calendar-modal-stack" ]
      [ div [ class_ "calendar-modal-field" ]
          [ div [ class_ "calendar-notifications-label" ] [ text "Type" ]
          , div [ class_ "badge rounded-pill text-bg-secondary" ]
              [ text $ case draft.itemType of
                  Intention -> "Intention"
                  ScheduledBlock -> "Bloc planifié"
              ]
          ]
      , div [ class_ "calendar-modal-field" ]
          [ div [ class_ "calendar-notifications-label" ] [ text "Titre" ]
          , input
              [ class_ "form-control calendar-input"
              , placeholder "Titre"
              , onValueChange (ViewAction <<< ViewEditTitleChanged)
              , value draft.title
              ]
          ]
      , div [ class_ "calendar-modal-field" ]
          [ div [ class_ "calendar-notifications-label" ] [ text "Début" ]
          , input
              [ class_ "form-control calendar-input"
              , type_ InputDatetimeLocal
              , attr (AttrName "lang") "fr"
              , placeholder "Début"
              , onValueChange (ViewAction <<< ViewEditStartChanged)
              , value draft.windowStart
              ]
          ]
      , div [ class_ "calendar-modal-field" ]
          [ div [ class_ "calendar-notifications-label" ] [ text "Fin" ]
          , input
              [ class_ "form-control calendar-input"
              , type_ InputDatetimeLocal
              , attr (AttrName "lang") "fr"
              , placeholder "Fin"
              , onValueChange (ViewAction <<< ViewEditEndChanged)
              , value draft.windowEnd
              ]
          ]
      , div [ class_ "calendar-modal-field" ]
          [ div [ class_ "calendar-notifications-label" ] [ text "Catégorie" ]
          , input
              [ class_ "form-control calendar-input"
              , placeholder "Catégorie"
              , onValueChange (ViewAction <<< ViewEditCategoryChanged)
              , value draft.category
              ]
          ]
      , div [ class_ "calendar-modal-field" ]
          [ div [ class_ "calendar-notifications-label" ] [ text "Statut" ]
          , select
              [ class_ "form-select calendar-input"
              , onValueChange (ViewAction <<< ViewEditStatusChanged)
              , value (statusValue draft.status)
              ]
              [ option [ value "todo" ] [ text "À faire" ]
              , option [ value "progress" ] [ text "En cours" ]
              , option [ value "done" ] [ text "Fait" ]
              , option [ value "canceled" ] [ text "Annulé" ]
              ]
          ]
      , div [ class_ "calendar-modal-field" ]
          [ div [ class_ "calendar-notifications-label" ] [ text "Durée réelle (minutes)" ]
          , input
              [ class_ "form-control calendar-input"
              , type_ InputNumber
              , placeholder "Ex: 30"
              , onValueChange (ViewAction <<< ViewEditDurationChanged)
              , value draft.actualDurationMinutes
              ]
          ]
      , renderRecurrenceSlot RecurrenceEdit draft.recurrence EditRecurrenceCmd
      , maybe (text "") (\msg -> div [ class_ "calendar-error" ] [ text msg ]) panel.validationError
      ]

renderValidationPanel :: forall w. ValidationPanel -> HTML w ViewAction
renderValidationPanel panel =
  div [ class_ "calendar-validation-panel" ]
    [ div [ class_ "calendar-conflict-title" ] [ text "Valider la tâche" ]
    , div [ class_ "calendar-conflict-subtitle" ]
        [ text "Saisissez la durée réelle (minutes) ou acceptez la proposition." ]
    , maybe (text "") (\minutes -> div [ class_ "calendar-validation-proposal" ] [ text $ "Proposition: " <> show minutes <> " min" ]) panel.proposedMinutes
    , input
        [ class_ "form-control calendar-input"
        , placeholder "Durée réelle (minutes)"
        , onValueChange ViewValidationMinutesChanged
        , value panel.inputValue
        ]
    , div [ class_ "calendar-conflict-confirmation-actions" ]
        [ button [ class_ "btn btn-sm btn-success", onClick (const ViewConfirmValidation) ] [ text "Confirmer" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ViewCancelValidation) ] [ text "Annuler" ]
        ]
    ]

statusValue :: ItemStatus -> String
statusValue status =
  case status of
    Todo -> "todo"
    EnCours -> "progress"
    Fait -> "done"
    Annule -> "canceled"

parseStatus :: String -> ItemStatus
parseStatus raw =
  case raw of
    "progress" -> EnCours
    "done" -> Fait
    "canceled" -> Annule
    _ -> Todo

editErrorMessage :: EditError -> String
editErrorMessage err =
  case err of
    EditValidation validation -> validationErrorMessage validation
    EditRecurrence msg -> msg
    EditDuration msg -> msg
    EditUnsupported -> "Impossible de modifier cet item."

validationErrorMessage :: ValidationError -> String
validationErrorMessage err =
  case err of
    TitleEmpty -> "Le titre est obligatoire."
    WindowStartInvalid -> "La date de début est invalide."
    WindowEndInvalid -> "La date de fin est invalide."
    WindowOrderInvalid -> "La fin doit être après le début."
    WindowTooShort -> "La fin doit être au minimum 5 minutes après le début."

-- END src/Calendar/Display.purs

-- BEGIN src/Calendar/Drag.purs
newtype DragState = DragState
  { draggingId :: Maybe String
  , dragHoverIndex :: Maybe Int
  , dragOffsetMinutes :: Maybe Int
  , touchStartItemId :: Maybe String
  , touchStartAt :: Maybe Instant
  , touchDragActive :: Boolean
  }

type DragCtx =
  { items :: Array CalendarItem
  , focusDate :: String
  , offlineMode :: Boolean
  }

dragInitialState :: DragState
dragInitialState =
  DragState
    { draggingId: Nothing
    , dragHoverIndex: Nothing
    , dragOffsetMinutes: Nothing
    , touchStartItemId: Nothing
    , touchStartAt: Nothing
    , touchDragActive: false
    }

_draggingIdS :: Lens' DragState (Maybe String)
_draggingIdS =
  lens
    (\(DragState state) -> state.draggingId)
    (\(DragState state) draggingId -> DragState (state { draggingId = draggingId }))

_dragHoverIndexS :: Lens' DragState (Maybe Int)
_dragHoverIndexS =
  lens
    (\(DragState state) -> state.dragHoverIndex)
    (\(DragState state) dragHoverIndex -> DragState (state { dragHoverIndex = dragHoverIndex }))

_dragOffsetMinutesS :: Lens' DragState (Maybe Int)
_dragOffsetMinutesS =
  lens
    (\(DragState state) -> state.dragOffsetMinutes)
    (\(DragState state) dragOffsetMinutes -> DragState (state { dragOffsetMinutes = dragOffsetMinutes }))

_touchStartItemIdS :: Lens' DragState (Maybe String)
_touchStartItemIdS =
  lens
    (\(DragState state) -> state.touchStartItemId)
    (\(DragState state) touchStartItemId -> DragState (state { touchStartItemId = touchStartItemId }))

_touchStartAtS :: Lens' DragState (Maybe Instant)
_touchStartAtS =
  lens
    (\(DragState state) -> state.touchStartAt)
    (\(DragState state) touchStartAt -> DragState (state { touchStartAt = touchStartAt }))

_touchDragActiveS :: Lens' DragState Boolean
_touchDragActiveS =
  lens
    (\(DragState state) -> state.touchDragActive)
    (\(DragState state) touchDragActive -> DragState (state { touchDragActive = touchDragActive }))

data DragAction
  = DragStart String DragEvent
  | DragOver String DragEvent
  | DropOn String
  | DragEnd
  | DragOverCalendar DragEvent
  | DropOnCalendar DragEvent
  | DragTouchStart String TouchEvent.TouchEvent
  | DragTouchMove TouchEvent.TouchEvent
  | DragTouchMoveCalendar TouchEvent.TouchEvent
  | DragTouchEnd
  | DragTouchCancel

data DragCommand
  = DragSetItems (Array CalendarItem)
  | DragUpsertPending CalendarItem
  | DragSetUpdateError (Maybe String)
  | DragUpdateItem String CalendarItem
  | DragRefreshItems

handleDragAction :: DragCtx -> DragAction -> StateT DragState (WriterT (Array DragCommand) Aff) Unit
handleDragAction ctx = case _ of
  DragStart itemId ev -> do
    let
      duration =
        find
          ( \item -> case item of
              ServerCalendarItem { id } -> id == itemId
              _ -> false
          )
          ctx.items <#> \item ->
          durationMinutesBetween (calendarItemContent item).windowStart (calendarItemContent item).windowEnd
    offset <- liftEffect $ dragOffsetFromEvent ev duration
    modify_ ((_draggingIdS .~ Just itemId) <<< (_dragOffsetMinutesS .~ offset))
  DragOver _ ev ->
    liftEffect $ preventDefault (toEvent ev)
  DropOn targetId -> do
    dragState <- get
    case dragState ^. _draggingIdS of
      Nothing -> pure unit
      Just draggingId -> do
        let reordered = moveItemBefore draggingId targetId ctx.items
        tell [ DragSetItems reordered ]
        modify_ (_draggingIdS .~ Nothing)
  DragEnd ->
    modify_ ((_draggingIdS .~ Nothing) <<< (_dragHoverIndexS .~ Nothing) <<< (_dragOffsetMinutesS .~ Nothing))
  DragOverCalendar ev -> do
    liftEffect $ preventDefault (toEvent ev)
    idx <- liftEffect $ dragMinuteIndexFromEvent ev
    dragState <- get
    let
      offset = fromMaybe 0 (dragState ^. _dragOffsetMinutesS)
      adjusted = idx <#> \minuteIndex -> computeDropMinuteIndex minuteIndex offset
    modify_ (_dragHoverIndexS .~ adjusted)
  DropOnCalendar ev -> do
    liftEffect $ preventDefault (toEvent ev)
    idx <- liftEffect $ dragMinuteIndexFromEvent ev
    dragState <- get
    case dragState ^. _draggingIdS of
      Nothing -> pure unit
      Just draggingId -> do
        let
          offset = fromMaybe 0 (dragState ^. _dragOffsetMinutesS)
          minuteIndex = fromMaybe 0 idx
          adjustedIndex = computeDropMinuteIndex minuteIndex offset
          totalMinutes = indexToMinutes adjustedIndex
          hour = Int.quot totalMinutes 60
          minute = Int.rem totalMinutes 60
          newStart =
            parseDateLocal ctx.focusDate >>= \dateValue ->
              DateTime.timeFromParts hour minute <#> \timeValue ->
                combineDateWithTime dateValue timeValue
          updated = do
            start <- newStart
            item <-
              find
                ( \item -> case item of
                    ServerCalendarItem { id } -> id == draggingId
                    _ -> false
                )
                ctx.items
            let mins = durationMinutesBetween (calendarItemContent item).windowStart (calendarItemContent item).windowEnd
            end <- shiftMinutes mins start
            pure { start, end }
          resetDragState =
            (_draggingIdS .~ Nothing)
              <<< (_dragHoverIndexS .~ Nothing)
              <<< (_dragOffsetMinutesS .~ Nothing)
        case updated of
          Nothing ->
            modify_ resetDragState
          Just { start, end } -> do
            let
              result = updateItemWindowById draggingId start end ctx.items
            case result.updated of
              Nothing ->
                modify_ resetDragState
              Just updatedItem ->
                if ctx.offlineMode then do
                  tell [ DragSetItems result.items ]
                  tell [ DragUpsertPending updatedItem ]
                  tell [ DragSetUpdateError Nothing ]
                  modify_ resetDragState
                else do
                  tell [ DragUpdateItem draggingId updatedItem ]
                  modify_ resetDragState
  DragTouchStart itemId ev -> do
    liftEffect $ preventDefault (TouchEvent.toEvent ev)
    now <- liftEffect Now.now
    let
      duration =
        find
          ( \item -> case item of
              ServerCalendarItem { id } -> id == itemId
              _ -> false
          )
          ctx.items <#> \item ->
          durationMinutesBetween (calendarItemContent item).windowStart (calendarItemContent item).windowEnd
    offset <- liftEffect $ dragOffsetFromTouch ev duration
    modify_
      ( (_touchStartItemIdS .~ Just itemId)
          <<< (_touchStartAtS .~ Just now)
          <<< (_dragOffsetMinutesS .~ offset)
          <<< (_touchDragActiveS .~ false)
      )
  DragTouchMove ev -> do
    dragState <- get
    when (dragState ^. _touchDragActiveS)
      $ liftEffect
      $ preventDefault (TouchEvent.toEvent ev)
    case { draggingId: dragState ^. _draggingIdS, touchStartAt: dragState ^. _touchStartAtS, touchStartItemId: dragState ^. _touchStartItemIdS } of
      { draggingId: Nothing, touchStartAt: Just startedAt, touchStartItemId: Just itemId } -> do
        now <- liftEffect Now.now
        let elapsedMs = case Instant.diff now startedAt of Milliseconds ms -> ms
        when (elapsedMs >= 350.0) do
          liftEffect $ preventDefault (TouchEvent.toEvent ev)
          modify_ ((_draggingIdS .~ Just itemId) <<< (_touchDragActiveS .~ true))
          liftEffect $ vibrateIfAvailable 10
      _ -> pure unit
  DragTouchMoveCalendar ev -> do
    dragState <- get
    when (dragState ^. _touchDragActiveS) do
      liftEffect $ preventDefault (TouchEvent.toEvent ev)
      idx <- liftEffect $ dragMinuteIndexFromTouch ev
      let
        offset = fromMaybe 0 (dragState ^. _dragOffsetMinutesS)
        adjusted = idx <#> \minuteIndex -> computeDropMinuteIndex minuteIndex offset
      modify_ (_dragHoverIndexS .~ adjusted)
  DragTouchEnd -> do
    dragState <- get
    let
      resetTouchState =
        (_draggingIdS .~ Nothing)
          <<< (_dragHoverIndexS .~ Nothing)
          <<< (_dragOffsetMinutesS .~ Nothing)
          <<< (_touchStartItemIdS .~ Nothing)
          <<< (_touchStartAtS .~ Nothing)
          <<< (_touchDragActiveS .~ false)
    case dragState ^. _draggingIdS of
      Nothing ->
        modify_ resetTouchState
      Just draggingId ->
        case dragState ^. _dragHoverIndexS of
          Nothing -> modify_ resetTouchState
          Just minuteIndex -> do
            let
              offset = fromMaybe 0 (dragState ^. _dragOffsetMinutesS)
              adjustedIndex = computeDropMinuteIndex minuteIndex offset
              totalMinutes = indexToMinutes adjustedIndex
              hour = Int.quot totalMinutes 60
              minute = Int.rem totalMinutes 60
              newStart =
                parseDateLocal ctx.focusDate >>= \dateValue ->
                  DateTime.timeFromParts hour minute <#> \timeValue ->
                    combineDateWithTime dateValue timeValue
              updated = do
                start <- newStart
                item <-
                  find
                    ( \item -> case item of
                        ServerCalendarItem { id } -> id == draggingId
                        _ -> false
                    )
                    ctx.items
                let mins = durationMinutesBetween (calendarItemContent item).windowStart (calendarItemContent item).windowEnd
                end <- shiftMinutes mins start
                pure { start, end }
            case updated of
              Nothing ->
                modify_ resetTouchState
              Just { start, end } -> do
                let
                  result = updateItemWindowById draggingId start end ctx.items
                case result.updated of
                  Nothing ->
                    modify_ resetTouchState
                  Just updatedItem ->
                    if ctx.offlineMode then do
                      tell [ DragSetItems result.items ]
                      tell [ DragUpsertPending updatedItem ]
                      tell [ DragSetUpdateError Nothing ]
                      modify_ resetTouchState
                    else do
                      tell [ DragUpdateItem draggingId updatedItem ]
                      modify_ resetTouchState
  DragTouchCancel ->
    modify_
      ( (_draggingIdS .~ Nothing)
          <<< (_dragHoverIndexS .~ Nothing)
          <<< (_dragOffsetMinutesS .~ Nothing)
          <<< (_touchStartItemIdS .~ Nothing)
          <<< (_touchStartAtS .~ Nothing)
          <<< (_touchDragActiveS .~ false)
      )

dragHandlers
  :: forall action r
   . (DragAction -> action)
  -> CalendarItem
  -> Array
       ( IProp
           ( draggable :: Boolean
           , onDragStart :: DragEvent
           , onDragOver :: DragEvent
           , onDrop :: DragEvent
           , onDragEnd :: DragEvent
           | r
           )
           action
       )
dragHandlers onAction (ServerCalendarItem { id }) =
  [ draggable true
  , onDragStart (\ev -> onAction $ DragStart id ev)
  , onDragOver (\ev -> onAction $ DragOver id ev)
  , onDrop (const $ onAction $ DropOn id)
  , onDragEnd (const (onAction DragEnd))
  ]
dragHandlers _ _ = []

dragCalendarHandlers
  :: forall action r
   . (DragAction -> action)
  -> CalendarItem
  -> Array
       ( IProp
           ( draggable :: Boolean
           , onDragStart :: DragEvent
           , onDragEnd :: DragEvent
           , onTouchStart :: TouchEvent.TouchEvent
           , onTouchMove :: TouchEvent.TouchEvent
           , onTouchEnd :: TouchEvent.TouchEvent
           , onTouchCancel :: TouchEvent.TouchEvent
           | r
           )
           action
       )
dragCalendarHandlers onAction (ServerCalendarItem { id, content }) =
  if content.itemType == Intention then
    [ draggable true
    , onDragStart (\ev -> onAction (DragStart id ev))
    , onDragEnd (const (onAction DragEnd))
    , onTouchStart (\ev -> onAction (DragTouchStart id ev))
    , onTouchMove (\ev -> onAction (DragTouchMove ev))
    , onTouchEnd (const (onAction DragTouchEnd))
    , onTouchCancel (const (onAction DragTouchCancel))
    ]
  else []
dragCalendarHandlers _ _ = []

renderDropIndicator :: forall w action. Int -> HTML w action
renderDropIndicator idx =
  let
    totalMinutes = indexToMinutes idx
    label = indexToTimeLabel idx
    inlineStyle = "top: calc(" <> show totalMinutes <> " * var(--calendar-minute-height));"
  in
    div
      [ class_ "calendar-calendar-drop-indicator"
      , style inlineStyle
      ]
      [ div [ class_ "calendar-calendar-drop-label" ] [ text label ] ]

computeDropMinuteIndex :: Int -> Int -> Int
computeDropMinuteIndex cursorIndex offsetMinutes =
  let
    totalMinutes = max 0 ((cursorIndex * 5) - offsetMinutes)
    rawIndex = Int.quot totalMinutes 5
  in
    clamp 0 287 rawIndex

indexToMinutes :: Int -> Int
indexToMinutes idx =
  clamp 0 1439 (idx * 5)

indexToTimeLabel :: Int -> String
indexToTimeLabel idx =
  let
    totalMinutes = indexToMinutes idx
    hour = Int.quot totalMinutes 60
    minute = Int.rem totalMinutes 60
  in
    DateTime.formatLocalTimeParts hour minute

-- Internal helpers

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

touchClientY :: TouchEvent.TouchEvent -> Maybe Int
touchClientY ev =
  TouchList.item 0 (TouchEvent.touches ev) <#> Touch.clientY

dragOffsetFromTouch :: TouchEvent.TouchEvent -> Maybe Int -> Effect (Maybe Int)
dragOffsetFromTouch ev duration = do
  let
    dur = fromMaybe 0 duration
    event = TouchEvent.toEvent ev
    clientY = fromMaybe 0 (touchClientY ev)
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

dragMinuteIndexFromTouch :: TouchEvent.TouchEvent -> Effect (Maybe Int)
dragMinuteIndexFromTouch ev = do
  let
    event = TouchEvent.toEvent ev
    clientY = fromMaybe 0 (touchClientY ev)
  win <- window
  doc <- Window.document win
  gridEl <- ParentNode.querySelector (ParentNode.QuerySelector ".calendar-calendar-grid") (HTMLDocument.toParentNode doc)
  let
    targetElFromEvent =
      (Event.currentTarget event <|> Event.target event)
        >>= HTMLElement.fromEventTarget
        <#> HTMLElement.toElement
    targetEl = case gridEl of
      Just el -> Just el
      Nothing -> targetElFromEvent
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

updateItemWindowById
  :: String
  -> DateTime
  -> DateTime
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

-- END src/Calendar/Drag.purs

-- BEGIN src/Calendar/Edit.purs
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
        , windowStart: formatDateTimeLocal content.windowStart
        , windowEnd: formatDateTimeLocal content.windowEnd
        , category: case content.category of
            Nothing -> ""
            Just value -> value
        , status: content.status
        , actualDurationMinutes: case content.actualDurationMinutes of
            Nothing -> ""
            Just minutes -> show minutes
        , recurrence: draftFromRecurrence content.recurrenceRule (map DateTime.formatLocalDate content.recurrenceExceptionDates)
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
      windowStart <- maybe (Left (EditValidation WindowStartInvalid)) Right (parseDateTimeLocal draft.windowStart)
      windowEnd <- maybe (Left (EditValidation WindowEndInvalid)) Right (parseDateTimeLocal draft.windowEnd)
      case item of
        ServerCalendarItem payload ->
          Right
            ( ServerCalendarItem
                payload
                  { content =
                      payload.content
                        { title = draft.title
                        , windowStart = windowStart
                        , windowEnd = windowEnd
                        , category = toOptionalString draft.category
                        , status = draft.status
                        , actualDurationMinutes = actualDuration
                        , recurrenceRule = recurrence.rule
                        , recurrenceExceptionDates = parseExceptionDatesOrEmpty recurrence.exceptions
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

-- END src/Calendar/Edit.purs

-- BEGIN src/Calendar/Helpers.purs
isDateTimeLocal :: String -> Boolean
isDateTimeLocal = DateTime.isLocalDateTime

parseDateTimeLocal :: String -> Maybe DateTime
parseDateTimeLocal = DateTime.parseLocalDateTime

parseDateLocal :: String -> Maybe Date
parseDateLocal = DateTime.parseLocalDate

parseTimeLocal :: String -> Maybe Time
parseTimeLocal = DateTime.parseLocalTime

parsePositiveInt :: String -> Maybe Int
parsePositiveInt raw =
  Int.fromString (StringCommon.trim raw) >>= \val ->
    if val > 0 then Just val else Nothing

isTimeLocal :: String -> Boolean
isTimeLocal = DateTime.isLocalTime

combineDateWithTime :: Date -> Time -> DateTime
combineDateWithTime dateValue timeValue =
  DateTime dateValue timeValue

shiftMinutes :: Int -> DateTime -> Maybe DateTime
shiftMinutes offset start =
  adjust (Minutes (Int.toNumber offset)) start

shiftMinutesRaw :: Int -> String -> Maybe String
shiftMinutesRaw offset start = do
  dt <- parseDateTimeLocal start
  newDt <- shiftMinutes offset dt
  pure $ formatDateTimeLocal newDt

durationMinutesBetween :: DateTime -> DateTime -> Int
durationMinutesBetween start end =
  let
    Minutes n = diff end start
    minutes = Int.floor n
  in
    max 1 minutes

durationMinutesBetweenRaw :: String -> String -> Maybe Int
durationMinutesBetweenRaw start end = do
  startDt <- parseDateTimeLocal start
  endDt <- parseDateTimeLocal end
  pure $ durationMinutesBetween startDt endDt

durationMinutesBetweenDateTime :: DateTime -> DateTime -> Int
durationMinutesBetweenDateTime start now =
  durationMinutesBetween start now

suggestDurationMinutes :: DateTime -> Effect (Maybe Int)
suggestDurationMinutes start = do
  now <- nowDateTime
  pure $ Just (durationMinutesBetweenDateTime start now)

formatDate :: DateTime -> String
formatDate dt =
  DateTime.formatLocalDate (date dt)

formatDateOnly :: Date -> String
formatDateOnly = DateTime.formatLocalDate

formatDateTimeLocal :: DateTime -> String
formatDateTimeLocal = DateTime.formatLocalDateTime

timeLabel :: DateTime -> String
timeLabel raw =
  DateTime.formatLocalTime (time raw)

normalizeHeader :: String -> String
normalizeHeader = StringCommon.trim >>> StringCommon.toLower

toOptionalString :: String -> Maybe String
toOptionalString raw =
  let
    trimmed = StringCommon.trim raw
  in
    if trimmed == "" then Nothing else Just trimmed

validateIntention :: IntentionDraft -> Either ValidationError IntentionDraft
validateIntention draft =
  case unit of
    _ | StringCommon.trim draft.title == "" -> Left TitleEmpty
    _ | not (isDateTimeLocal draft.windowStart) -> Left WindowStartInvalid
    _ | not (isDateTimeLocal draft.windowEnd) -> Left WindowEndInvalid
    _ | draft.windowEnd <= draft.windowStart -> Left WindowOrderInvalid
    _ | maybe true (_ <= 5) (durationMinutesBetweenRaw draft.windowStart draft.windowEnd) -> Left WindowTooShort
    _ -> Right draft

calendarItemContent :: CalendarItem -> CalendarItemContent
calendarItemContent (NewCalendarItem { content }) = content
calendarItemContent (ServerCalendarItem { content }) = content

isItemOnDate :: String -> CalendarItem -> Boolean
isItemOnDate dateStr item =
  case parseDateLocal dateStr of
    Nothing -> false
    Just dateValue -> date (calendarItemContent item).windowStart == dateValue

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

minuteOfDay :: DateTime -> Int
minuteOfDay raw =
  let
    t = time raw
  in
    (fromEnum (hour t) * 60) + fromEnum (minute t)

-- END src/Calendar/Helpers.purs

-- BEGIN src/Calendar/Import.purs
newtype ImportState = ImportState
  { csvInput :: String
  , csvImportResult :: Maybe CsvImportResult
  , icsInput :: String
  , icsImportResult :: Maybe IcsImportResult
  }

type ImportCtx =
  { items :: Array CalendarItem
  , pending :: Array CalendarItem
  , offlineMode :: Boolean
  }

importInitialState :: ImportState
importInitialState =
  ImportState
    { csvInput: ""
    , csvImportResult: Nothing
    , icsInput: ""
    , icsImportResult: Nothing
    }

_csvInputS :: Lens' ImportState String
_csvInputS =
  lens
    (\(ImportState state) -> state.csvInput)
    (\(ImportState state) csvInput -> ImportState (state { csvInput = csvInput }))

_csvImportResultS :: Lens' ImportState (Maybe CsvImportResult)
_csvImportResultS =
  lens
    (\(ImportState state) -> state.csvImportResult)
    (\(ImportState state) csvImportResult -> ImportState (state { csvImportResult = csvImportResult }))

_icsInputS :: Lens' ImportState String
_icsInputS =
  lens
    (\(ImportState state) -> state.icsInput)
    (\(ImportState state) icsInput -> ImportState (state { icsInput = icsInput }))

_icsImportResultS :: Lens' ImportState (Maybe IcsImportResult)
_icsImportResultS =
  lens
    (\(ImportState state) -> state.icsImportResult)
    (\(ImportState state) icsImportResult -> ImportState (state { icsImportResult = icsImportResult }))

data ImportAction
  = ImportCsvInputChanged String
  | ImportParseCsvInput
  | ImportApplyCsv
  | ImportClearCsv
  | ImportIcsInputChanged String
  | ImportParseIcsInput
  | ImportApplyIcs
  | ImportClearIcs

data ImportCommand
  = ImportSetItems (Array CalendarItem)
  | ImportSetPending (Array CalendarItem)

handleImportAction :: ImportCtx -> ImportAction -> StateT ImportState (WriterT (Array ImportCommand) Aff) Unit
handleImportAction ctx = case _ of
  ImportCsvInputChanged raw ->
    modify_ (_csvInputS .~ raw)
  ImportParseCsvInput -> do
    st <- get
    let result = parseCsvImport (st ^. _csvInputS)
    modify_ (_csvImportResultS .~ Just result)
  ImportApplyCsv -> do
    importState <- get
    case importState ^. _csvImportResultS of
      Nothing -> pure unit
      Just result ->
        if null result.items then pure unit
        else do
          if ctx.offlineMode then do
            let
              initial = { items: ctx.items, pending: ctx.pending }
              final = foldl (\acc item -> applyOfflineMutation true item acc.items acc.pending) initial result.items
            tell [ ImportSetItems final.items ]
            tell [ ImportSetPending final.pending ]
          else tell [ ImportSetItems (ctx.items <> result.items) ]
          modify_ ((_csvInputS .~ "") <<< (_csvImportResultS .~ Nothing))
  ImportClearCsv ->
    modify_ ((_csvInputS .~ "") <<< (_csvImportResultS .~ Nothing))
  ImportIcsInputChanged raw ->
    modify_ (_icsInputS .~ raw)
  ImportParseIcsInput -> do
    st <- get
    let result = parseIcsImport (st ^. _icsInputS)
    modify_ (_icsImportResultS .~ Just result)
  ImportApplyIcs -> do
    importState <- get
    case importState ^. _icsImportResultS of
      Nothing -> pure unit
      Just result ->
        if null result.items then pure unit
        else do
          if ctx.offlineMode then do
            let
              initial = { items: ctx.items, pending: ctx.pending }
              final = foldl (\acc item -> applyOfflineMutation true item acc.items acc.pending) initial result.items
            tell [ ImportSetItems final.items ]
            tell [ ImportSetPending final.pending ]
          else tell [ ImportSetItems (ctx.items <> result.items) ]
          modify_ ((_icsInputS .~ "") <<< (_icsImportResultS .~ Nothing))
  ImportClearIcs ->
    modify_ ((_icsInputS .~ "") <<< (_icsImportResultS .~ Nothing))

renderCsvImportPanel :: forall w. String -> Maybe CsvImportResult -> HTML w ImportAction
renderCsvImportPanel csvInput result =
  section [ class_ "calendar-import" ]
    [ renderPanelHeader
        { baseClass: "calendar-import"
        , title: "Import CSV"
        , subtitle: "Colonnes minimales: type, titre, fenetre_debut, fenetre_fin."
        }
        []
    , textarea
        [ class_ "form-control calendar-import-textarea"
        , placeholder "Collez votre CSV ici..."
        , value csvInput
        , onValueChange ImportCsvInputChanged
        ]
    , div [ class_ "calendar-import-actions" ]
        [ button [ class_ "btn btn-sm btn-outline-primary", onClick (const ImportParseCsvInput) ] [ text "Analyser" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ImportClearCsv) ] [ text "Effacer" ]
        , button [ class_ "btn btn-sm btn-success", onClick (const ImportApplyCsv) ] [ text "Ajouter a la liste" ]
        ]
    , maybe (text "") renderCsvImportResult result
    ]

renderCsvImportResult :: forall w action. CsvImportResult -> HTML w action
renderCsvImportResult result =
  let
    okCount = length result.items
    errorCount = length result.errors
  in
    div [ class_ "calendar-import-result" ]
      [ div [ class_ "calendar-import-summary" ]
          [ text $ "Valides: " <> show okCount <> " • Erreurs: " <> show errorCount ]
      , if null result.errors then text "" else renderCsvImportErrors result.errors
      ]

renderCsvImportErrors :: forall w action. Array CsvImportError -> HTML w action
renderCsvImportErrors errors =
  div [ class_ "calendar-import-errors" ]
    (map renderCsvImportError errors)

renderCsvImportError :: forall w action. CsvImportError -> HTML w action
renderCsvImportError err =
  div [ class_ "calendar-import-error" ]
    [ text $ "Ligne " <> show err.rowNumber <> ": " <> err.message ]

renderIcsImportPanel :: forall w. String -> Maybe IcsImportResult -> HTML w ImportAction
renderIcsImportPanel icsInput result =
  section [ class_ "calendar-import" ]
    [ renderPanelHeader
        { baseClass: "calendar-import"
        , title: "Import ICS"
        , subtitle: "Support basique: SUMMARY, DTSTART, DTEND."
        }
        []
    , textarea
        [ class_ "form-control calendar-import-textarea"
        , placeholder "Collez votre fichier ICS ici..."
        , value icsInput
        , onValueChange ImportIcsInputChanged
        ]
    , div [ class_ "calendar-import-actions" ]
        [ button [ class_ "btn btn-sm btn-outline-primary", onClick (const ImportParseIcsInput) ] [ text "Analyser" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ImportClearIcs) ] [ text "Effacer" ]
        , button [ class_ "btn btn-sm btn-success", onClick (const ImportApplyIcs) ] [ text "Ajouter a la liste" ]
        ]
    , maybe (text "") renderIcsImportResult result
    ]

renderIcsImportResult :: forall w action. IcsImportResult -> HTML w action
renderIcsImportResult result =
  let
    okCount = length result.items
    errorCount = length result.errors
  in
    div [ class_ "calendar-import-result" ]
      [ div [ class_ "calendar-import-summary" ]
          [ text $ "Valides: " <> show okCount <> " • Erreurs: " <> show errorCount ]
      , if null result.errors then text "" else renderIcsImportErrors result.errors
      ]

renderIcsImportErrors :: forall w action. Array IcsImportError -> HTML w action
renderIcsImportErrors errors =
  div [ class_ "calendar-import-errors" ]
    (map renderIcsImportError errors)

renderIcsImportError :: forall w action. IcsImportError -> HTML w action
renderIcsImportError err =
  div [ class_ "calendar-import-error" ]
    [ text $ "Evenement " <> show err.eventIndex <> ": " <> err.message ]

-- END src/Calendar/Import.purs

-- BEGIN src/Calendar/Imports.purs
parseCsvImport :: String -> CsvImportResult
parseCsvImport raw =
  let
    lines = map stripCR (StringCommon.split (Pattern "\n") raw)
    indexed = mapWithIndex (\idx -> { row: idx + 1, raw: _ }) lines
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
    sourceItemIdIdx = findFor [ "source_item_id" ]
    actualDurationIdx = findFor [ "actual_duration_minutes" ]
    recurrenceRuleTypeIdx = findFor [ "recurrence_rule_type" ]
    recurrenceRuleIntervalIdx = findFor [ "recurrence_rule_interval_days" ]
    recurrenceExceptionIdx = findFor [ "recurrence_exception_dates" ]
  in
    case
      { typeIdx
      , titleIdx
      , startIdx
      , endIdx
      , categoryIdx
      , statusIdx
      , sourceItemIdIdx
      , actualDurationIdx
      , recurrenceRuleTypeIdx
      , recurrenceRuleIntervalIdx
      , recurrenceExceptionIdx
      }
      of
      { typeIdx: Just t
      , titleIdx: Just ti
      , startIdx: Just s
      , endIdx: Just e
      , categoryIdx: Just c
      , statusIdx: Just st
      , sourceItemIdIdx: Just source
      , actualDurationIdx: Just duration
      , recurrenceRuleTypeIdx: Just ruleType
      , recurrenceRuleIntervalIdx: Just ruleInterval
      , recurrenceExceptionIdx: Just exceptions
      } ->
        Right
          { typeIdx: t
          , titleIdx: ti
          , startIdx: s
          , endIdx: e
          , categoryIdx: c
          , statusIdx: st
          , sourceItemIdIdx: source
          , actualDurationIdx: duration
          , recurrenceRuleTypeIdx: ruleType
          , recurrenceRuleIntervalIdx: ruleInterval
          , recurrenceExceptionIdx: exceptions
          }
      _ ->
        Left
          "Colonnes manquantes: type, titre, fenetre_debut, fenetre_fin, categorie, statut, source_item_id, actual_duration_minutes, recurrence_rule_type, recurrence_rule_interval_days, recurrence_exception_dates."

type CsvHeader =
  { typeIdx :: Int
  , titleIdx :: Int
  , startIdx :: Int
  , endIdx :: Int
  , categoryIdx :: Int
  , statusIdx :: Int
  , sourceItemIdIdx :: Int
  , actualDurationIdx :: Int
  , recurrenceRuleTypeIdx :: Int
  , recurrenceRuleIntervalIdx :: Int
  , recurrenceExceptionIdx :: Int
  }

parseCsvRows :: CsvHeader -> Array { row :: Int, raw :: String } -> CsvImportResult
parseCsvRows header rows =
  foldl parseRow { items: [], errors: [] } rows
  where
  parseRow acc row =
    let
      fields = splitCsvLine row.raw
      fieldAt idx = index fields idx
    in
      case
        { typeVal: fieldAt header.typeIdx
        , titleVal: fieldAt header.titleIdx
        , startVal: fieldAt header.startIdx
        , endVal: fieldAt header.endIdx
        }
        of
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
    windowStartRaw = StringCommon.trim startVal
    windowEndRaw = StringCommon.trim endVal
    category = lookupField header.categoryIdx fields >>= toOptionalString
    sourceItemId = lookupField header.sourceItemIdIdx fields >>= toOptionalString
    actualDurationRaw = lookupField header.actualDurationIdx fields >>= toOptionalString
    ruleTypeRaw = lookupField header.recurrenceRuleTypeIdx fields >>= toOptionalString
    ruleIntervalRaw = lookupField header.recurrenceRuleIntervalIdx fields >>= toOptionalString
    exceptionRaw = lookupField header.recurrenceExceptionIdx fields >>= toOptionalString
    exceptions = splitExceptions exceptionRaw
  if title == "" then Left "Le titre est vide."
  else
    case parseDateTimeLocal windowStartRaw of
      Nothing -> Left "Début invalide (format YYYY-MM-DDTHH:MM)."
      Just windowStart ->
        case parseDateTimeLocal windowEndRaw of
          Nothing -> Left "Fin invalide (format YYYY-MM-DDTHH:MM)."
          Just windowEnd ->
            if windowEnd <= windowStart then Left "La fin doit être après le début."
            else
              case parseActualDuration actualDurationRaw of
                Left err -> Left err
                Right actualDurationMinutes ->
                  case parseRecurrenceRule ruleTypeRaw ruleIntervalRaw of
                    Left err -> Left err
                    Right recurrenceRule ->
                      case validateExceptions exceptions of
                        Left err -> Left err
                        Right validExceptions ->
                          Right $ NewCalendarItem
                            { content:
                                { itemType
                                , title
                                , windowStart
                                , windowEnd
                                , status
                                , sourceItemId
                                , actualDurationMinutes
                                , category
                                , recurrenceRule
                                , recurrenceExceptionDates: validExceptions
                                }
                            }

lookupField :: Int -> Array String -> Maybe String
lookupField idx fields = index fields idx

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

parseActualDuration :: Maybe String -> Either String (Maybe Int)
parseActualDuration raw =
  case raw of
    Nothing -> Right Nothing
    Just value ->
      case Int.fromString value of
        Nothing -> Left "Durée réelle invalide."
        Just minutes | minutes <= 0 -> Left "Durée réelle invalide."
        Just minutes -> Right (Just minutes)

parseRecurrenceRule :: Maybe String -> Maybe String -> Either String (Maybe RecurrenceRule)
parseRecurrenceRule rawType rawInterval =
  case rawType of
    Nothing ->
      case rawInterval of
        Nothing -> Right Nothing
        Just _ -> Left "Intervalle de récurrence sans type."
    Just ruleType ->
      case normalizeHeader ruleType of
        "daily" ->
          if rawInterval == Nothing then Right (Just Daily)
          else Left "Intervalle invalide pour daily."
        "weekly" ->
          if rawInterval == Nothing then Right (Just Weekly)
          else Left "Intervalle invalide pour weekly."
        "monthly" ->
          if rawInterval == Nothing then Right (Just Monthly)
          else Left "Intervalle invalide pour monthly."
        "yearly" ->
          if rawInterval == Nothing then Right (Just Yearly)
          else Left "Intervalle invalide pour yearly."
        "every" ->
          case rawInterval >>= Int.fromString of
            Nothing -> Left "Intervalle requis pour every."
            Just days | days <= 0 -> Left "Intervalle requis pour every."
            Just days -> Right (Just (EveryXDays days))
        _ -> Left "Type de récurrence invalide (daily, weekly, monthly, yearly, every)."

splitExceptions :: Maybe String -> Array String
splitExceptions raw =
  case raw of
    Nothing -> []
    Just value ->
      StringCommon.split (Pattern ";") value
        # map StringCommon.trim
        # filter (\entry -> entry /= "")

validateExceptions :: Array String -> Either String (Array Date)
validateExceptions exceptions = parseExceptionDates exceptions

parseExceptionDates :: Array String -> Either String (Array Date)
parseExceptionDates exceptions =
  let
    parsed = map DateTime.parseLocalDate exceptions
  in
    if any (_ == Nothing) parsed then Left "Exception invalide (format YYYY-MM-DD)."
    else Right (mapMaybe identity parsed)

parseExceptionDatesOrEmpty :: Array String -> Array Date
parseExceptionDatesOrEmpty exceptions =
  case parseExceptionDates exceptions of
    Left _ -> []
    Right dates -> dates

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
  let
    len = String.length line
  in
    if len > 0 && String.charAt (len - 1) line == Just '\r' then String.slice 0 (len - 1) line
    else line

type IcsEventDraft =
  { summary :: Maybe String
  , dtStart :: Maybe String
  , dtEnd :: Maybe String
  , itemType :: Maybe String
  , status :: Maybe String
  , sourceItemId :: Maybe String
  , actualDurationMinutes :: Maybe String
  , category :: Maybe String
  , rrule :: Maybe String
  , exdates :: Array String
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
        state
          { current =
              Just
                { summary: Nothing
                , dtStart: Nothing
                , dtEnd: Nothing
                , itemType: Nothing
                , status: Nothing
                , sourceItemId: Nothing
                , actualDurationMinutes: Nothing
                , category: Nothing
                , rrule: Nothing
                , exdates: []
                }
          }
      "END:VEVENT" ->
        case state.current of
          Nothing -> state
          Just event ->
            let
              nextIndex = state.index + 1
            in
              case buildIcsItem nextIndex event of
                Left err -> state { errors = state.errors <> [ err ], current = Nothing, index = nextIndex }
                Right item -> state { items = state.items <> [ item ], current = Nothing, index = nextIndex }
      _ ->
        case state.current of
          Nothing -> state
          Just event ->
            let
              key = extractIcsKey line
              value = extractIcsValue line
              trimmed = StringCommon.trim value
              updated =
                case key of
                  "SUMMARY" -> event { summary = toOptionalString value }
                  "DTSTART" -> event { dtStart = Just value }
                  "DTEND" -> event { dtEnd = Just value }
                  "X-FAVS-TYPE" -> event { itemType = toOptionalString value }
                  "X-FAVS-STATUS" -> event { status = toOptionalString value }
                  "X-FAVS-SOURCE-ITEM-ID" -> event { sourceItemId = Just trimmed }
                  "X-FAVS-ACTUAL-DURATION-MINUTES" -> event { actualDurationMinutes = Just trimmed }
                  "CATEGORIES" -> event { category = toOptionalString value }
                  "RRULE" -> event { rrule = toOptionalString value }
                  "EXDATE" -> event { exdates = event.exdates <> splitIcsExdates trimmed }
                  _ -> event
            in
              state { current = Just updated }

buildIcsItem :: Int -> IcsEventDraft -> Either IcsImportError CalendarItem
buildIcsItem index event = do
  title <- maybe (Left { eventIndex: index, message: "SUMMARY manquant." }) Right event.summary
  startRaw <- maybe (Left { eventIndex: index, message: "DTSTART manquant." }) Right event.dtStart
  endRaw <- maybe (Left { eventIndex: index, message: "DTEND manquant." }) Right event.dtEnd
  typeRaw <- maybe (Left { eventIndex: index, message: "X-FAVS-TYPE manquant." }) Right event.itemType
  statusRaw <- maybe (Left { eventIndex: index, message: "X-FAVS-STATUS manquant." }) Right event.status
  sourceRaw <- maybe (Left { eventIndex: index, message: "X-FAVS-SOURCE-ITEM-ID manquant." }) Right event.sourceItemId
  durationRaw <- maybe (Left { eventIndex: index, message: "X-FAVS-ACTUAL-DURATION-MINUTES manquant." }) Right event.actualDurationMinutes
  windowStart <- maybe (Left { eventIndex: index, message: "DTSTART invalide." }) Right (parseIcsDateTime startRaw)
  windowEnd <- maybe (Left { eventIndex: index, message: "DTEND invalide." }) Right (parseIcsDateTime endRaw)
  itemType <- lmap ({ eventIndex: index, message: _ }) (parseIcsItemType typeRaw)
  status <- lmap ({ eventIndex: index, message: _ }) (parseIcsStatus statusRaw)
  sourceItemId <- lmap ({ eventIndex: index, message: _ }) (parseIcsSourceItemId sourceRaw)
  actualDurationMinutes <- lmap ({ eventIndex: index, message: _ }) (parseIcsActualDuration durationRaw)
  recurrenceRule <- lmap ({ eventIndex: index, message: _ }) (parseIcsRrule event.rrule)
  recurrenceExceptionDates <- lmap ({ eventIndex: index, message: _ }) (parseIcsExceptions event.exdates)
  if windowEnd <= windowStart then Left { eventIndex: index, message: "La fin doit être après le début." }
  else
    Right $ NewCalendarItem
      { content:
          { itemType
          , title
          , windowStart
          , windowEnd
          , status
          , sourceItemId
          , actualDurationMinutes
          , category: event.category
          , recurrenceRule
          , recurrenceExceptionDates
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

parseIcsDateTime :: String -> Maybe DateTime
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
        formatted = StringCommon.joinWith "" [ y, "-", m, "-", d, "T", hh, ":", mm ]
      in
        DateTime.parseLocalDateTime formatted

endsWithChar :: Char -> String -> Boolean
endsWithChar ch str =
  let
    len = String.length str
  in
    len > 0 && String.charAt (len - 1) str == Just ch

splitIcsExdates :: String -> Array String
splitIcsExdates raw =
  if raw == "" then []
  else StringCommon.split (Pattern ",") raw

parseIcsItemType :: String -> Either String ItemType
parseIcsItemType raw =
  case normalizeHeader raw of
    "intention" -> Right Intention
    "bloc_planifie" -> Right ScheduledBlock
    _ -> Left "X-FAVS-TYPE invalide (INTENTION ou BLOC_PLANIFIE)."

parseIcsStatus :: String -> Either String ItemStatus
parseIcsStatus raw =
  case normalizeHeader raw of
    "todo" -> Right Todo
    "en_cours" -> Right EnCours
    "fait" -> Right Fait
    "annule" -> Right Annule
    _ -> Left "X-FAVS-STATUS invalide (TODO, EN_COURS, FAIT, ANNULE)."

parseIcsSourceItemId :: String -> Either String (Maybe String)
parseIcsSourceItemId raw =
  Right (toOptionalString raw)

parseIcsActualDuration :: String -> Either String (Maybe Int)
parseIcsActualDuration raw =
  case toOptionalString raw of
    Nothing -> Right Nothing
    Just trimmed ->
      case Int.fromString trimmed of
        Nothing -> Left "X-FAVS-ACTUAL-DURATION-MINUTES invalide."
        Just minutes | minutes <= 0 -> Left "X-FAVS-ACTUAL-DURATION-MINUTES invalide."
        Just minutes -> Right (Just minutes)

parseIcsRrule :: Maybe String -> Either String (Maybe RecurrenceRule)
parseIcsRrule Nothing = Right Nothing
parseIcsRrule (Just raw) =
  let
    parts = StringCommon.split (Pattern ";") raw
    freq = findMap (stripPrefix "FREQ=") parts
    interval = findMap (stripPrefix "INTERVAL=") parts
  in
    case map StringCommon.toUpper freq of
      Nothing -> Left "RRULE invalide."
      Just "DAILY" ->
        case interval >>= Int.fromString of
          Nothing -> Right (Just Daily)
          Just value | value <= 0 -> Left "RRULE invalide."
          Just value | value == 1 -> Right (Just Daily)
          Just value -> Right (Just (EveryXDays value))
      Just "WEEKLY" -> if interval == Nothing then Right (Just Weekly) else Left "RRULE invalide."
      Just "MONTHLY" -> if interval == Nothing then Right (Just Monthly) else Left "RRULE invalide."
      Just "YEARLY" -> if interval == Nothing then Right (Just Yearly) else Left "RRULE invalide."
      _ -> Left "RRULE invalide."
  where
  stripPrefix prefix value =
    if String.slice 0 (String.length prefix) value == prefix then
      Just (String.slice (String.length prefix) (String.length value) value)
    else
      Nothing

parseIcsExceptions :: Array String -> Either String (Array Date)
parseIcsExceptions raw =
  let
    parsed = map parseIcsDate raw
  in
    if any (_ == Nothing) parsed then Left "EXDATE invalide."
    else Right (mapMaybe identity parsed)

parseIcsDate :: String -> Maybe Date
parseIcsDate raw =
  let
    trimmed = StringCommon.trim raw
    cleaned = if endsWithChar 'Z' trimmed then String.slice 0 (String.length trimmed - 1) trimmed else trimmed
    base =
      if String.length cleaned >= 8 then
        if String.charAt 8 cleaned == Just 'T' then String.slice 0 8 cleaned else String.slice 0 8 cleaned
      else
        ""
    formatted =
      if String.length base == 8 then
        String.slice 0 4 base <> "-" <> String.slice 4 6 base <> "-" <> String.slice 6 8 base
      else ""
  in
    if formatted == "" then Nothing
    else DateTime.parseLocalDate formatted

-- END src/Calendar/Imports.purs

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

type CalendarItemContent =
  { itemType :: ItemType
  , title :: String
  , windowStart :: DateTime
  , windowEnd :: DateTime
  , status :: ItemStatus
  , sourceItemId :: Maybe String
  , actualDurationMinutes :: Maybe Int
  , category :: Maybe String
  , recurrenceRule :: Maybe RecurrenceRule
  , recurrenceExceptionDates :: Array Date
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

data CalendarItem
  = NewCalendarItem { content :: CalendarItemContent }
  | ServerCalendarItem { content :: CalendarItemContent, id :: String }

derive instance calendarItemGeneric :: Generic CalendarItem _
derive instance calendarItemEq :: Eq CalendarItem
instance calendarItemShow :: Show CalendarItem where
  show = genericShow

type IntentionDraft =
  { itemType :: ItemType
  , title :: String
  , windowStart :: String
  , windowEnd :: String
  , category :: String
  , status :: ItemStatus
  , actualDurationMinutes :: String
  , recurrence :: RecurrenceDraft
  }

data ValidationError
  = TitleEmpty
  | WindowStartInvalid
  | WindowEndInvalid
  | WindowOrderInvalid
  | WindowTooShort

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

instance calendarItemDecodeJson :: DecodeJson CalendarItem where
  decodeJson json = do
    obj <- decodeJson json
    itemType <- obj .: "type"
    title <- obj .: "titre"
    windowStartRaw <- obj .: "fenetre_debut"
    windowEndRaw <- obj .: "fenetre_fin"
    windowStart <- maybe (Left $ UnexpectedValue (encodeJson windowStartRaw)) Right (DateTime.parseLocalDateTime windowStartRaw)
    windowEnd <- maybe (Left $ UnexpectedValue (encodeJson windowEndRaw)) Right (DateTime.parseLocalDateTime windowEndRaw)
    status <- obj .: "statut"
    sourceItemId <- obj .:? "source_item_id"
    actualDurationMinutes <- obj .:? "duree_reelle_minutes"
    category <- obj .:? "categorie"
    recurrenceRule <- obj .:? "recurrence_rule"
    recurrenceExceptionDatesRaw <- obj .:? "recurrence_exception_dates"
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
        , recurrenceExceptionDates: parseExceptionDatesOrEmpty (fromMaybe [] recurrenceExceptionDatesRaw)
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
      ~> "fenetre_debut" := formatDateTimeLocal windowStart
      ~> "fenetre_fin" := formatDateTimeLocal windowEnd
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
          ~> "recurrence_exception_dates" := map DateTime.formatLocalDate recurrenceExceptionDates
          ~> base
      Nothing -> base

-- END src/Calendar/Model.purs

-- BEGIN src/Calendar/Notifications.purs
newtype NotificationState = NotificationState
  { notificationDefaults :: NotificationDefaults
  , notificationOverrides :: Array NotificationOverride
  , notificationPanelOpen :: Boolean
  , notificationEditor :: Maybe NotificationEditor
  }

type NotificationEditor =
  { itemId :: String
  , startTime :: String
  , beforeEndRaw :: String
  }

notificationInitialState :: NotificationState
notificationInitialState =
  NotificationState
    { notificationDefaults: defaultNotificationDefaults
    , notificationOverrides: []
    , notificationPanelOpen: false
    , notificationEditor: Nothing
    }

_notificationDefaultsS :: Lens' NotificationState NotificationDefaults
_notificationDefaultsS =
  lens
    (\(NotificationState state) -> state.notificationDefaults)
    (\(NotificationState state) notificationDefaults -> NotificationState (state { notificationDefaults = notificationDefaults }))

_notificationOverridesS :: Lens' NotificationState (Array NotificationOverride)
_notificationOverridesS =
  lens
    (\(NotificationState state) -> state.notificationOverrides)
    (\(NotificationState state) notificationOverrides -> NotificationState (state { notificationOverrides = notificationOverrides }))

_notificationPanelOpenS :: Lens' NotificationState Boolean
_notificationPanelOpenS =
  lens
    (\(NotificationState state) -> state.notificationPanelOpen)
    (\(NotificationState state) notificationPanelOpen -> NotificationState (state { notificationPanelOpen = notificationPanelOpen }))

_notificationEditorS :: Lens' NotificationState (Maybe NotificationEditor)
_notificationEditorS =
  lens
    (\(NotificationState state) -> state.notificationEditor)
    (\(NotificationState state) notificationEditor -> NotificationState (state { notificationEditor = notificationEditor }))

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

handleNotificationAction :: NotificationAction -> StateT NotificationState (WriterT (Array Void) Aff) Unit
handleNotificationAction = case _ of
  NotificationTogglePanel ->
    modify_ (_notificationPanelOpenS %~ not)
  NotificationDefaultStartTimeChanged raw ->
    if isTimeLocal raw then
      modify_ (_notificationDefaultsS <<< prop (Proxy :: _ "startDayTime") .~ raw)
    else
      pure unit
  NotificationDefaultBeforeEndChanged raw ->
    case parsePositiveInt raw of
      Just hours ->
        modify_ (_notificationDefaultsS <<< prop (Proxy :: _ "beforeEndHours") .~ hours)
      Nothing -> pure unit
  NotificationOpenEditor itemId -> do
    st <- get
    let
      existing = lookupNotificationOverride itemId (st ^. _notificationOverridesS)
      startTime =
        fromMaybe (st ^. _notificationDefaultsS).startDayTime (existing >>= _.startDayTime)
      beforeEnd =
        fromMaybe (st ^. _notificationDefaultsS).beforeEndHours (existing >>= _.beforeEndHours)
    modify_ (_notificationEditorS .~ Just { itemId, startTime, beforeEndRaw: show beforeEnd })
  NotificationStartTimeChanged raw ->
    modify_ (_notificationEditorS %~ map (_ { startTime = raw }))
  NotificationBeforeEndChanged raw ->
    modify_ (_notificationEditorS %~ map (_ { beforeEndRaw = raw }))
  NotificationSaveOverride -> do
    st <- get
    case st ^. _notificationEditorS of
      Nothing -> pure unit
      Just editor -> do
        let
          cleanedTime = if isTimeLocal editor.startTime then Just editor.startTime else Nothing
          cleanedHours = parsePositiveInt editor.beforeEndRaw
        modify_
          ( (_notificationOverridesS %~ upsertNotificationOverride editor.itemId cleanedTime cleanedHours)
              <<< (_notificationEditorS .~ Nothing)
          )
  NotificationResetOverride itemId ->
    modify_
      ( (_notificationOverridesS %~ removeNotificationOverride itemId)
          <<< (_notificationEditorS .~ Nothing)
      )
  NotificationCancelOverride ->
    modify_ (_notificationEditorS .~ Nothing)

renderNotificationsPanel
  :: forall w
   . Boolean
  -> NotificationDefaults
  -> Array NotificationOverride
  -> Maybe NotificationEditor
  -> Array CalendarItem
  -> HTML w NotificationAction
renderNotificationsPanel isOpen defaults overrides editor intentions =
  if null intentions then text ""
  else
    section [ class_ "calendar-notifications" ]
      [ renderPanelHeader
          { baseClass: "calendar-notifications"
          , title: "Rappels des intentions non planifiées"
          , subtitle: "Les rappels par défaut s'appliquent aux intentions non planifiées."
          }
          [ button
              [ class_ $ "btn btn-sm calendar-notifications-toggle" <> if isOpen then " btn-outline-primary" else " btn-outline-secondary"
              , onClick (const NotificationTogglePanel)
              ]
              [ text $ if isOpen then "Masquer" else "Configurer" ]
          ]
      , if isOpen then renderNotificationDefaults defaults else text ""
      , if isOpen then renderNotificationList defaults overrides editor intentions else text ""
      ]

renderNotificationsContent
  :: forall w
   . NotificationDefaults
  -> Array NotificationOverride
  -> Maybe NotificationEditor
  -> Array CalendarItem
  -> HTML w NotificationAction
renderNotificationsContent defaults overrides editor intentions =
  if null intentions then
    div [ class_ "calendar-modal-empty" ]
      [ text "Aucune intention non planifiée." ]
  else
    div [ class_ "calendar-notifications-modal" ]
      [ renderNotificationDefaults defaults
      , renderNotificationList defaults overrides editor intentions
      ]

renderNotificationDefaults :: forall w. NotificationDefaults -> HTML w NotificationAction
renderNotificationDefaults defaults =
  div [ class_ "calendar-notifications-defaults" ]
    [ div [ class_ "calendar-notifications-section-title" ] [ text "Rappels par défaut" ]
    , div [ class_ "calendar-notifications-controls" ]
        [ div [ class_ "calendar-notifications-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Jour de début" ]
            , input
                [ class_ "form-control calendar-input"
                , type_ InputTime
                , value defaults.startDayTime
                , onValueChange NotificationDefaultStartTimeChanged
                ]
            ]
        , div [ class_ "calendar-notifications-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Avant fin (heures)" ]
            , input
                [ class_ "form-control calendar-input"
                , type_ InputNumber
                , value (show defaults.beforeEndHours)
                , onValueChange NotificationDefaultBeforeEndChanged
                ]
            ]
        ]
    ]

renderNotificationList
  :: forall w
   . NotificationDefaults
  -> Array NotificationOverride
  -> Maybe NotificationEditor
  -> Array CalendarItem
  -> HTML w NotificationAction
renderNotificationList defaults overrides editor intentions =
  div [ class_ "calendar-notifications-list" ]
    (map (renderNotificationItem defaults overrides editor) intentions)

renderNotificationItem
  :: forall w
   . NotificationDefaults
  -> Array NotificationOverride
  -> Maybe NotificationEditor
  -> CalendarItem
  -> HTML w NotificationAction
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
        div [ class_ "calendar-notification-item" ]
          [ div [ class_ "calendar-notification-header" ]
              [ div []
                  [ div [ class_ "calendar-notification-title" ] [ text content.title ]
                  , div [ class_ "calendar-notification-window" ] [ text $ formatDateTimeLocal content.windowStart <> " → " <> formatDateTimeLocal content.windowEnd ]
                  ]
              , div [ class_ "calendar-notification-actions" ]
                  [ div [ class_ $ "calendar-notification-badge" <> guard hasOverride " calendar-notification-badge--custom" ]
                      [ text $ if hasOverride then "Personnalisé" else "Par défaut" ]
                  , button
                      [ class_ "btn btn-sm btn-outline-secondary"
                      , onClick (const (NotificationOpenEditor id))
                      ]
                      [ text "Personnaliser" ]
                  ]
              ]
          , renderReminderTimes reminders
          , maybe (text "") (renderNotificationEditor id) editorForItem
          ]
    _ -> text ""

renderReminderTimes :: forall w action. Array ReminderTime -> HTML w action
renderReminderTimes reminders =
  div [ class_ "calendar-notification-times" ]
    (map (\reminder -> div [ class_ "calendar-notification-time" ] [ text $ reminder.label <> ": " <> reminder.at ]) reminders)

renderNotificationEditor :: forall w. String -> NotificationEditor -> HTML w NotificationAction
renderNotificationEditor itemId editor =
  div [ class_ "calendar-notification-editor" ]
    [ div [ class_ "calendar-notifications-section-title" ] [ text "Surcharge de rappel" ]
    , div [ class_ "calendar-notifications-controls" ]
        [ div [ class_ "calendar-notifications-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Jour de début" ]
            , input
                [ class_ "form-control calendar-input"
                , type_ InputTime
                , value editor.startTime
                , onValueChange NotificationStartTimeChanged
                ]
            ]
        , div [ class_ "calendar-notifications-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Avant fin (heures)" ]
            , input
                [ class_ "form-control calendar-input"
                , type_ InputNumber
                , value editor.beforeEndRaw
                , onValueChange NotificationBeforeEndChanged
                ]
            ]
        ]
    , div [ class_ "calendar-notification-editor-actions" ]
        [ button [ class_ "btn btn-sm btn-success", onClick (const NotificationSaveOverride) ] [ text "Enregistrer" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const NotificationCancelOverride) ] [ text "Annuler" ]
        , button [ class_ "btn btn-sm btn-outline-danger", onClick (const (NotificationResetOverride itemId)) ] [ text "Réinitialiser" ]
        ]
    ]

reminderTimesForIntention :: NotificationDefaults -> Maybe NotificationOverride -> CalendarItemContent -> Array ReminderTime
reminderTimesForIntention defaults override content =
  if content.itemType /= Intention then []
  else
    let
      startTime = fromMaybe defaults.startDayTime (override >>= _.startDayTime)
      beforeEndHours = fromMaybe defaults.beforeEndHours (override >>= _.beforeEndHours)
      startReminder =
        parseTimeLocal startTime <#> \timeValue ->
          mkReminder "Jour de début" (formatDateTimeLocal (combineDateWithTime (date content.windowStart) timeValue))
      beforeEndReminder =
        shiftMinutes (negate (beforeEndHours * 60)) content.windowEnd <#> \dateTimeValue ->
          mkReminder (show beforeEndHours <> "h avant fin") (formatDateTimeLocal dateTimeValue)
    in
      catMaybes [ startReminder, beforeEndReminder ]
  where
  mkReminder label at = { label, at }

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

-- END src/Calendar/Notifications.purs

-- BEGIN src/Calendar/Offline.purs
type OfflineMutationResult =
  { items :: Array CalendarItem
  , pending :: Array CalendarItem
  }

applyOfflineMutation :: Boolean -> CalendarItem -> Array CalendarItem -> Array CalendarItem -> OfflineMutationResult
applyOfflineMutation offline item items pending =
  if offline then { items: items <> [ item ], pending: pending <> [ item ] }
  else { items, pending }

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

-- END src/Calendar/Offline.purs

-- BEGIN src/Calendar/purs
newtype SyncState = SyncState
  { offlineMode :: Boolean
  , pendingSync :: Array CalendarItem
  , syncConflict :: Maybe (Array CalendarItem)
  , updateError :: Maybe String
  }

syncInitialState :: SyncState
syncInitialState =
  SyncState
    { offlineMode: false
    , pendingSync: []
    , syncConflict: Nothing
    , updateError: Nothing
    }

_syncOfflineMode :: Lens' SyncState Boolean
_syncOfflineMode =
  lens
    (\(SyncState state) -> state.offlineMode)
    (\(SyncState state) offlineMode -> SyncState (state { offlineMode = offlineMode }))

_syncPendingSync :: Lens' SyncState (Array CalendarItem)
_syncPendingSync =
  lens
    (\(SyncState state) -> state.pendingSync)
    (\(SyncState state) pendingSync -> SyncState (state { pendingSync = pendingSync }))

_syncConflict :: Lens' SyncState (Maybe (Array CalendarItem))
_syncConflict =
  lens
    (\(SyncState state) -> state.syncConflict)
    (\(SyncState state) syncConflict -> SyncState (state { syncConflict = syncConflict }))

_syncUpdateError :: Lens' SyncState (Maybe String)
_syncUpdateError =
  lens
    (\(SyncState state) -> state.updateError)
    (\(SyncState state) updateError -> SyncState (state { updateError = updateError }))

data SyncAction
  = SyncDraftTitleKeyDown String
  | SyncSubmitIntention
  | SyncPlanifyFrom String CalendarItemContent
  | SyncToggleOffline
  | SyncResolveKeepLocal
  | SyncResolveDiscardLocal
  | SyncDismissUpdateError

data SyncCommand
  = SyncSetItems (Array CalendarItem)
  | SyncCreateItem CalendarItem
  | SyncRefreshItems
  | SyncSubmitIntentionCmd
  | SyncRunPending (Array CalendarItem)

handleSyncAction :: Array CalendarItem -> SyncAction -> StateT SyncState (WriterT (Array SyncCommand) Aff) Unit
handleSyncAction items = case _ of
  SyncDraftTitleKeyDown key ->
    when (key == "Enter") (tell [ SyncSubmitIntentionCmd ])
  SyncSubmitIntention ->
    tell [ SyncSubmitIntentionCmd ]
  SyncPlanifyFrom sourceId content -> do
    syncState <- get
    let item = toScheduledBlock sourceId content
    if syncState ^. _syncOfflineMode then do
      let
        result = applyOfflineMutation true item items (syncState ^. _syncPendingSync)
      modify_ (_syncPendingSync .~ result.pending)
      tell [ SyncSetItems result.items ]
    else
      tell [ SyncCreateItem item ]
  SyncToggleOffline -> do
    syncState <- get
    if syncState ^. _syncOfflineMode then do
      modify_ (_syncOfflineMode .~ false)
      syncPending
    else modify_ (_syncOfflineMode .~ true)
  SyncResolveKeepLocal ->
    modify_ ((_syncConflict .~ Nothing) <<< (_syncOfflineMode .~ true))
  SyncResolveDiscardLocal -> do
    modify_ ((_syncConflict .~ Nothing) <<< (_syncPendingSync .~ []))
    tell [ SyncRefreshItems ]
  SyncDismissUpdateError ->
    modify_ (_syncUpdateError .~ Nothing)

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

renderOfflineToggle :: forall w. Boolean -> HTML w SyncAction
renderOfflineToggle offlineMode =
  div [ class_ "calendar-offline-toggle" ]
    [ button
        [ class_ $ "btn btn-sm " <> if offlineMode then "btn-outline-warning" else "btn-outline-secondary"
        , onClick (const SyncToggleOffline)
        ]
        [ text $ if offlineMode then "Mode hors ligne actif" else "Passer hors ligne" ]
    ]

renderUpdateError :: forall w. String -> HTML w SyncAction
renderUpdateError message =
  div [ class_ "calendar-error calendar-error--update" ]
    [ text message
    , button
        [ class_ "btn btn-sm btn-outline-secondary calendar-error-dismiss"
        , onClick (const SyncDismissUpdateError)
        ]
        [ text "OK" ]
    ]

updateErrorMessage :: Int -> String
updateErrorMessage status =
  "Echec de mise a jour de l'item (HTTP " <> show status <> ")."

renderSyncConflict :: forall w. Array CalendarItem -> HTML w SyncAction
renderSyncConflict pending =
  div [ class_ "calendar-sync-conflict" ]
    [ div [ class_ "calendar-conflict-title" ] [ text "Conflit de synchronisation" ]
    , div [ class_ "calendar-conflict-subtitle" ]
        [ text "Choisissez comment resoudre la synchronisation des changements locaux." ]
    , ul [ class_ "calendar-conflict-list" ] (map (renderSyncConflictItem pending) pendingIds)
    , div [ class_ "calendar-conflict-confirmation-actions" ]
        [ button [ class_ "btn btn-sm btn-danger", onClick (const SyncResolveDiscardLocal) ] [ text "Abandonner local" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const SyncResolveKeepLocal) ] [ text "Conserver local" ]
        ]
    ]
  where
  pendingIds = mapMaybe extractId pending
  extractId (ServerCalendarItem { id }) = Just id
  extractId _ = Nothing

renderSyncConflictItem :: forall w action. Array CalendarItem -> String -> HTML w action
renderSyncConflictItem items itemId =
  case find (matchId itemId) items of
    Just item ->
      let
        content = calendarItemContent item
      in
        li [ class_ "calendar-conflict-item" ]
          [ div [ class_ "calendar-conflict-item-title" ] [ text content.title ]
          , div [ class_ "calendar-conflict-item-window" ]
              [ text $ formatDateTimeLocal content.windowStart <> " → " <> formatDateTimeLocal content.windowEnd ]
          ]
    Nothing -> text ""
  where
  matchId id (ServerCalendarItem { id: candidate }) = id == candidate
  matchId _ _ = false

syncPending :: StateT SyncState (WriterT (Array SyncCommand) Aff) Unit
syncPending = do
  syncState <- get
  if null (syncState ^. _syncPendingSync) then
    tell [ SyncRefreshItems ]
  else
    tell [ SyncRunPending (syncState ^. _syncPendingSync) ]

-- END src/Calendar/purs
