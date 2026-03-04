module Pages.Calendar
  ( component
  , decodeCalendarItemsResponse
  ) where

import Prelude hiding (div)

import Affjax.Web (Response)
import Api.Calendar (ValidateItemPayload(..), createItemResponse, getItemsResponse, updateItemResponse, validateItemResponse)
import Calendar.Calendar.Agenda.Day (DayAction(..), renderDayCalendar)
import Calendar.Calendar.Agenda.List (ListAction(..))
import Calendar.Calendar.Agenda.Range (RangeAction(..), renderRangeView)
import Calendar.Calendar.Controls (ControlsAction(..), applyControlsAction)
import Calendar.Calendar.Controls as CCtrl
import Calendar.Calendar.CreateForm (CreateFormAction(..), applyCreateFormAction)
import Calendar.Calendar.CreateForm as CForm
import Calendar.Calendar.Draft (toNewIntention)
import Calendar.Calendar.State (CalendarState, _draft, _items, _lastCreateType, _validationError, calendarInitialState, emptyDraft)
import Calendar.Conflict (ConflictAction, applyConflictAction, detectConflictGroups, detectConflictIds)
import Calendar.Conflict as CConf
import Calendar.Display (AgendaModal(..), EditPanel, ViewAction(..), ViewCommand(..), ViewState(..), _viewActiveModalS, handleViewAction, viewInitialState, viewTitle)
import Calendar.Display as Disp
import Calendar.Drag (DragAction, DragCommand(..), DragState(..), dragInitialState, handleDragAction)
import Calendar.Export (ExportAction, ExportState(..), exportInitialState, handleExportAction)
import Calendar.Export as Exp
import Calendar.Helpers (formatDate, generateDateRange, generateMonthDates, isConflict, isUnplannedIntention, shiftMinutes, sortItems, validateIntention)
import Calendar.Import (ImportAction, ImportCommand(..), ImportState(..), handleImportAction, importInitialState)
import Calendar.Import as Imp
import Calendar.Model (AgendaView(..), CalendarItem(..), CsvImportResult, ExportFormat, IcsImportResult, IntentionDraft, ItemType(..), NotificationDefaults, NotificationOverride, SortMode, TaskTemplate, TemplateDraft, ValidationError(..))
import Calendar.Notifications (NotificationAction, NotificationEditor, NotificationState(..), handleNotificationAction, notificationInitialState)
import Calendar.Notifications as Notif
import Calendar.Offline (applyOfflineMutation, upsertPendingItem)
import Calendar.Sync (SyncAction(..), SyncCommand(..), SyncState(..), handleSyncAction, syncInitialState, updateErrorMessage)
import Calendar.Sync as Sync
import Calendar.Templates (TemplateAction, TemplateCommand(..), TemplateState(..), handleTemplateAction, templateInitialState)
import Calendar.Templates as Tmpl
import Control.Monad.Except (ExceptT(..), withExceptT)
import Control.Monad.RWS (get, modify_)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (filter, foldM, null)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Lens (Lens', (.~), (%~), (^.))
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
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, getRef, mkComponent, mkEval) as H
import Halogen (subscribe)
import Halogen.HTML (HTML, button, details, div, h2, i, section, summary, text)
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (attr)
import Halogen.Query.Event as HQE
import Type.Proxy (Proxy(..))
import Ui.Errors (FatalError, handleError, toFatalError)
import Ui.Focus (focusElement)
import Ui.Modal (renderModal) as Modal
import Ui.Utils (class_)
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KeyboardEventTypes

type NoOutput = Void
type AgendaAppM = H.HalogenM State Action () NoOutput Aff
type ErrorAgendaAppM = ExceptT FatalError AgendaAppM

type State =
  { calendar :: CalendarState
  , sync :: SyncState
  , drag :: DragState
  , notifications :: NotificationState
  , templates :: TemplateState
  , imports :: ImportState
  , exports :: ExportState
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
  | TemplateAction TemplateAction
  | ImportAction ImportAction
  | ExportAction ExportAction

data Command
  = SyncCmd SyncCommand
  | DragCmd DragCommand
  | ViewCmd ViewCommand
  | TemplateCmd TemplateCommand
  | ImportCmd ImportCommand

class ToCommand cmd where
  toCommand :: cmd -> Command

instance toCommandSyncCommand :: ToCommand SyncCommand where
  toCommand = SyncCmd

instance toCommandDragCommand :: ToCommand DragCommand where
  toCommand = DragCmd

instance toCommandViewCommand :: ToCommand ViewCommand where
  toCommand = ViewCmd

instance toCommandTemplateCommand :: ToCommand TemplateCommand where
  toCommand = TemplateCmd

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

instance toActionExportAction :: ToAction ExportAction where
  toAction = ExportAction

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

renderOfflineToggle :: forall w. Boolean -> HTML w Action
renderOfflineToggle = map toAction <<< Sync.renderOfflineToggle

renderSortPicker :: forall w. SortMode -> HTML w Action
renderSortPicker = map toAction <<< CCtrl.renderSortPicker

renderConflictActions :: forall w. Array (Array String) -> HTML w Action
renderConflictActions = map toAction <<< CConf.renderConflictActions

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
        end = if draft.windowEnd == "" then fromMaybe start (shiftMinutes 30 start) else draft.windowEnd
      in
        draft
          { windowStart = start
          , windowEnd = end
          }

renderTemplatesPanel
  :: forall w
   . Array TaskTemplate
  -> TemplateDraft
  -> Maybe String
  -> HTML w Action
renderTemplatesPanel templates draft editingId =
  map TemplateAction (Tmpl.renderTemplatesPanel templates draft editingId)

renderCsvImportPanel :: forall w. String -> Maybe CsvImportResult -> HTML w Action
renderCsvImportPanel csvInput result =
  map ImportAction (Imp.renderCsvImportPanel csvInput result)

renderIcsImportPanel :: forall w. String -> Maybe IcsImportResult -> HTML w Action
renderIcsImportPanel icsInput result =
  map ImportAction (Imp.renderIcsImportPanel icsInput result)

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
  map toAction (Exp.renderExportPanel format typeFilter statusFilter categoryFilter startDate endDate output)

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
  , templates: templateInitialState
  , imports: importInitialState
  , exports: exportInitialState
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

_templates :: Lens' State TemplateState
_templates = prop (Proxy :: _ "templates")

_imports :: Lens' State ImportState
_imports = prop (Proxy :: _ "imports")

_exports :: Lens' State ExportState
_exports = prop (Proxy :: _ "exports")

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

_syncOfflineMode :: Lens' State Boolean
_syncOfflineMode = _sync <<< Sync._syncOfflineMode

_syncPendingSync :: Lens' State (Array CalendarItem)
_syncPendingSync = _sync <<< Sync._syncPendingSync

_syncConflict :: Lens' State (Maybe (Array CalendarItem))
_syncConflict = _sync <<< Sync._syncConflict

_syncUpdateError :: Lens' State (Maybe String)
_syncUpdateError = _sync <<< Sync._syncUpdateError

_viewFocusDate :: Lens' State String
_viewFocusDate = _view <<< Disp._viewFocusDate

class HasStateLens s where
  getLens :: Lens' State s

instance hasStateLensSync :: HasStateLens SyncState where
  getLens = _sync

instance hasStateLensDrag :: HasStateLens DragState where
  getLens = _drag

instance hasStateLensView :: HasStateLens ViewState where
  getLens = _view

instance hasStateLensTemplates :: HasStateLens TemplateState where
  getLens = _templates

instance hasStateLensImports :: HasStateLens ImportState where
  getLens = _imports

instance hasStateLensExports :: HasStateLens ExportState where
  getLens = _exports

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
        modify_ ((_syncPendingSync .~ []) <<< (_syncConflict .~ Nothing))
        refreshItems
      else modify_ (_syncConflict .~ Just pending)

runDragCommand :: DragCommand -> ErrorAgendaAppM Unit
runDragCommand = case _ of
  DragSetItems items -> modify_ (_calendarItems .~ items)
  DragUpsertPending item -> modify_ (_syncPendingSync %~ upsertPendingItem item)
  DragSetUpdateError err -> modify_ (_syncUpdateError .~ err)
  DragRefreshItems -> refreshItems
  DragUpdateItem itemId updatedItem -> do
    resp <- updateItem itemId updatedItem
    if statusOk resp then modify_ (_syncUpdateError .~ Nothing)
    else modify_ (_syncUpdateError .~ Just (updateErrorMessage (unwrap resp.status)))
    refreshItems

runViewCommand :: ViewCommand -> ErrorAgendaAppM Unit
runViewCommand = case _ of
  ViewValidateItem itemId minutes ->
    validateItem itemId minutes >>= (const refreshItems)
  ViewUpdateItem itemId updatedItem -> do
    st <- get
    if st ^. _syncOfflineMode then
      modify_ ((_calendarItems %~ replaceItemById itemId updatedItem) <<< (_syncPendingSync %~ upsertPendingItem updatedItem) <<< (_syncUpdateError .~ Nothing))
    else do
      resp <- updateItem itemId updatedItem
      if statusOk resp then modify_ (_syncUpdateError .~ Nothing)
      else modify_ (_syncUpdateError .~ Just (updateErrorMessage (unwrap resp.status)))
      refreshItems

runTemplateCommand :: TemplateCommand -> ErrorAgendaAppM Unit
runTemplateCommand (TemplateSetDraft draft) = modify_ (_calendarDraft .~ draft)

runImportCommand :: ImportCommand -> ErrorAgendaAppM Unit
runImportCommand (ImportSetItems items) = modify_ (_calendarItems .~ items)
runImportCommand (ImportSetPending pending) = modify_ (_syncPendingSync .~ pending)

runCommand :: Command -> ErrorAgendaAppM Unit
runCommand = case _ of
  SyncCmd cmd -> runSyncCommand cmd
  DragCmd cmd -> runDragCommand cmd
  ViewCmd cmd -> runViewCommand cmd
  TemplateCmd cmd -> runTemplateCommand cmd
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
      , focusDate: st ^. _viewFocusDate
      , offlineMode: st ^. _syncOfflineMode
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
        nextDraft = prefillCreateDraft lastType (st ^. _viewFocusDate) baseDraft
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
          let updatedDraft = prefillCreateDraft itemType (st ^. _viewFocusDate) (st ^. _calendarDraft)
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
    ViewAction viewAction -> handleViewActionFlow viewAction
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
    TemplateAction templateAction -> withSubState $ handleTemplateAction templateAction
    ImportAction importAction -> do
      st <- get
      let
        ctx =
          { items: st ^. _calendarItems
          , pending: st ^. _syncPendingSync
          , offlineMode: st ^. _syncOfflineMode
          }
      withSubState $ handleImportAction ctx importAction
    ExportAction exportAction -> do
      st <- get
      withSubState $ handleExportAction (st ^. _calendarItems) exportAction

initAction :: ErrorAgendaAppM Unit
initAction = do
  now <- liftEffect nowDateTime
  modify_ $ _viewFocusDate .~ formatDate now
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
          if st ^. _syncOfflineMode then do
            let
              result = applyOfflineMutation true item (st ^. _calendarItems) (st ^. _syncPendingSync)
            modify_
              ( (_calendarItems .~ result.items)
                  <<< (_calendarDraft .~ emptyDraft)
                  <<< (_calendarValidationError .~ Nothing)
                  <<< (_syncPendingSync .~ result.pending)
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

render
  :: forall m
   . State
  -> H.ComponentHTML Action () m
render { calendar, sync, drag, notifications, templates, imports, exports, view } =
  let
    { items, showConflictsOnly, conflictResolution, sortMode } = calendar
    SyncState { offlineMode, syncConflict, updateError } = sync
    DragState { draggingId, dragHoverIndex } = drag
    NotificationState { notificationDefaults, notificationOverrides, notificationPanelOpen, notificationEditor } = notifications
    TemplateState { templates: templateItems, templateDraft, editingTemplateId } = templates
    ImportState { csvInput, csvImportResult, icsInput, icsImportResult } = imports
    ExportState { exportFormat, exportTypeFilter, exportStatusFilter, exportCategoryFilter, exportStartDate, exportEndDate, exportOutput } = exports
    ViewState { viewMode, focusDate, validationPanel, isMobile } = view
    agendaModalsInput = buildAgendaModalsInput { calendar, sync, drag, notifications, templates, imports, exports, view }
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
                , renderOfflineToggle offlineMode
                , renderSortPicker sortMode
                , renderConflictActions conflictGroups
                ]
            , toAction <$> Disp.renderViewSelector viewMode focusDate
            , toAction <$> Disp.renderMobileTools viewMode
            ]
        , div [ class_ $ "calendar-layout" <> guard (viewMode == ViewDay) " calendar-layout--calendar" ]
            [ div [ class_ "calendar-main" ]
                [ maybe (text "") (map toAction <<< Sync.renderUpdateError) updateError
                , maybe (text "") (map toAction <<< Disp.renderValidationPanel) validationPanel
                , section [ class_ $ "calendar-list-panel" <> guard (viewMode == ViewDay) " calendar-list-panel--calendar" ]
                    [ renderAgendaView viewMode focusDate conflictIds sortedItems isMobile draggingId dragHoverIndex
                    ]
                , maybe (text "") (map toAction <<< CConf.renderConflictResolution items) conflictResolution
                , maybe (text "") (map toAction <<< Sync.renderSyncConflict) syncConflict
                ]
            , div [ class_ "calendar-side" ]
                [ NotificationAction <$> Notif.renderNotificationsPanel notificationPanelOpen notificationDefaults notificationOverrides notificationEditor unplannedIntentions
                , renderAccordion "Templates de tâches" "calendar-accordion templates" $ renderTemplatesPanel templateItems templateDraft editingTemplateId
                , renderAccordion "Import CSV" "calendar-accordion import-csv" $ renderCsvImportPanel csvInput csvImportResult
                , renderAccordion "Import ICS" "calendar-accordion import-ics" $ renderIcsImportPanel icsInput icsImportResult
                , renderAccordion "Export" "calendar-accordion export" $ renderExportPanel exportFormat exportTypeFilter exportStatusFilter exportCategoryFilter exportStartDate exportEndDate exportOutput
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
    , renderOfflineToggle offlineMode
    , renderSortPicker sortMode
    , renderConflictActions conflictGroups
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
  , templates ::
      { items :: Array TaskTemplate
      , draft :: TemplateDraft
      , editingId :: Maybe String
      }
  , csvImport ::
      { input :: String
      , result :: Maybe CsvImportResult
      }
  , icsImport ::
      { input :: String
      , result :: Maybe IcsImportResult
      }
  , export ::
      { format :: ExportFormat
      , typeFilter :: String
      , statusFilter :: String
      , categoryFilter :: String
      , startDate :: String
      , endDate :: String
      , output :: String
      }
  , draft :: IntentionDraft
  , validationError :: Maybe String
  , editPanel :: Maybe EditPanel
  }

renderAgendaModals :: forall w. AgendaModalsInput -> HTML w Action
renderAgendaModals { activeModal, filters, notifications, templates, csvImport, icsImport, export, draft, validationError, editPanel } =
  let renderModal title content = Modal.renderModal title content (ViewAction ViewCloseModal) (ViewAction ViewCloseModal)
    in  maybe (div [] [])
              case _ of
                ModalFilters    -> renderModal "Filtres" [ renderFiltersContent filters.showConflictsOnly filters.conflictGroups filters.offlineMode filters.sortMode ]
                ModalTools      -> renderModal "Outils" [ map toAction Disp.renderToolsContent ]
                ModalCreateItem -> Modal.renderModal "Créer un item" [ map toAction (CForm.renderCreateContent draft validationError) ]
                                                     (ViewAction ViewCloseCreate)
                                                     (SyncAction SyncSubmitIntention)
                ModalNotifications -> renderModal "Rappels" [ NotificationAction <$> Notif.renderNotificationsContent notifications.defaults notifications.overrides notifications.editor notifications.intentions ]
                ModalTemplates -> renderModal "Templates de tâches" [ renderTemplatesPanel templates.items templates.draft templates.editingId ]
                ModalImportCsv -> renderModal "Import CSV" [ renderCsvImportPanel csvImport.input csvImport.result ]
                ModalImportIcs -> renderModal "Import ICS" [ renderIcsImportPanel icsImport.input icsImport.result ]
                ModalExport -> renderModal "Export" [ renderExportPanel export.format export.typeFilter export.statusFilter export.categoryFilter export.startDate export.endDate export.output ]
                ModalEditItem -> case editPanel of
                             Nothing -> text ""
                             Just panel -> renderModal "Modifier l'item" [ toAction <$> Disp.renderEditContent panel ]
              activeModal

validationErrorMessage :: ValidationError -> String
validationErrorMessage err =
  case err of
    TitleEmpty -> "Le titre est obligatoire."
    WindowStartInvalid -> "La date de début est invalide."
    WindowEndInvalid -> "La date de fin est invalide."
    WindowOrderInvalid -> "La fin doit être après le début."
    WindowTooShort -> "La fin doit être au minimum 5 minutes après le début."

buildAgendaModalsInput :: State -> AgendaModalsInput
buildAgendaModalsInput { calendar, sync, notifications, templates, imports, exports, view } =
  let
    { items, draft, validationError, showConflictsOnly, sortMode } = calendar
    SyncState { offlineMode } = sync
    NotificationState { notificationDefaults, notificationOverrides, notificationEditor } = notifications
    TemplateState { templates: templateItems, templateDraft, editingTemplateId } = templates
    ImportState { csvInput, csvImportResult, icsInput, icsImportResult } = imports
    ExportState { exportFormat, exportTypeFilter, exportStatusFilter, exportCategoryFilter, exportStartDate, exportEndDate, exportOutput } = exports
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
    , templates:
        { items: templateItems
        , draft: templateDraft
        , editingId: editingTemplateId
        }
    , csvImport:
        { input: csvInput
        , result: csvImportResult
        }
    , icsImport:
        { input: icsInput
        , result: icsImportResult
        }
    , export:
        { format: exportFormat
        , typeFilter: exportTypeFilter
        , statusFilter: exportStatusFilter
        , categoryFilter: exportCategoryFilter
        , startDate: exportStartDate
        , endDate: exportEndDate
        , output: exportOutput
        }
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
