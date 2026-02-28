module Pages.Calendar
  ( component
  ) where

import Prelude hiding (div)

import Affjax.Web (Response)
import Agenda.Commands
  ( Command(..)
  , DragCommand(..)
  , ImportCommand(..)
  , SyncCommand(..)
  , TemplateCommand(..)
  , ViewCommand(..)
  )
import Agenda.Conflicts (detectConflictGroups, detectConflictIds)
import Agenda.Calendar
  ( CalendarAction(..)
  , CalendarState
  , CalendarUiAction(..)
  , calendarInitialState
  , emptyDraft
  , handleCalendarAction
  , validateIntention
  , toNewIntention
  )
import Agenda.Calendar as Cal
import Agenda.Display
  ( AgendaModal(..)
  , ValidationPanel
  , ViewAction
  , ViewState
  , handleViewAction
  , viewInitialState
  , viewTitle
  )
import Agenda.Display as Disp
import Agenda.Drag
  ( DragAction
  , DragState
  , dragInitialState
  , handleDragAction
  )
import Agenda.Export
  ( ExportAction
  , ExportState
  , exportInitialState
  , handleExportAction
  )
import Agenda.Export as Exp
import Agenda.Helpers
  ( formatDate
  , isConflict
  , isUnplannedIntention
  , sortItems
  )
import Agenda.Import
  ( ImportAction
  , ImportState
  , handleImportAction
  , importInitialState
  )
import Agenda.Import as Imp
import Agenda.Notifications
  ( NotificationAction
  , NotificationEditor
  , NotificationState
  , handleNotificationAction
  , notificationInitialState
  )
import Agenda.Notifications as Notif
import Agenda.Offline (applyOfflineMutation, upsertPendingItem)
import Agenda.Sync
  ( SyncAction
  , SyncState
  , handleSyncAction
  , syncInitialState
  , updateErrorMessage
  )
import Agenda.Sync as Sync
import Agenda.Templates
  ( TemplateAction
  , TemplateState
  , handleTemplateAction
  , templateInitialState
  )
import Agenda.Templates as Tmpl
import Agenda.Model
  ( AgendaView(..)
  , CalendarItem
  , CsvImportResult
  , ExportFormat
  , IcsImportResult
  , IntentionDraft
  , NotificationDefaults
  , NotificationOverride
  , SortMode
  , TaskTemplate
  , TemplateDraft
  , ValidationError
  )
import Api.Agenda (createItemResponse, getItemsResponse, updateItemResponse, validateItemResponse)
import Control.Monad.Except (ExceptT(..), withExceptT)
import Control.Monad.RWS (get, modify_)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (filter, foldM, null)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Lens (Lens', (.~), (%~), (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval) as H
import Halogen.HTML (HTML, button, details, div, h2, section, summary, text)
import Halogen.HTML.Events (onClick)
import Ui.Utils (class_)
import Type.Proxy (Proxy(..))
import Ui.Errors (FatalError, handleError, toFatalError)
import Ui.Modal (renderModal) as Modal

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
  | CalendarAction CalendarAction
  | SyncAction SyncAction
  | DragAction DragAction
  | ViewAction ViewAction
  | NotificationAction NotificationAction
  | TemplateAction TemplateAction
  | ImportAction ImportAction
  | ExportAction ExportAction

calendarUiToAction :: CalendarUiAction -> Action
calendarUiToAction = case _ of
  CalendarUiCalendar action -> CalendarAction action
  CalendarUiSync action -> SyncAction action
  CalendarUiView action -> ViewAction action
  CalendarUiDrag action -> DragAction action

mapCalendarUi :: forall w. HTML w CalendarUiAction -> HTML w Action
mapCalendarUi = map calendarUiToAction

renderOfflineToggle :: forall w. Boolean -> HTML w Action
renderOfflineToggle = map SyncAction <<< Sync.renderOfflineToggle

renderUpdateError :: forall w. String -> HTML w Action
renderUpdateError = map SyncAction <<< Sync.renderUpdateError

renderSyncConflict :: forall w. Array CalendarItem -> HTML w Action
renderSyncConflict = map SyncAction <<< Sync.renderSyncConflict

renderSortPicker :: forall w. SortMode -> HTML w Action
renderSortPicker = mapCalendarUi <<< Cal.renderSortPicker

renderConflictActions :: forall w. Array (Array String) -> HTML w Action
renderConflictActions = mapCalendarUi <<< Cal.renderConflictActions

renderConflictResolution :: forall w. Array CalendarItem -> Cal.ConflictResolution -> HTML w Action
renderConflictResolution items resolution =
  mapCalendarUi (Cal.renderConflictResolution items resolution)

renderForm :: forall w. IntentionDraft -> Maybe ValidationError -> HTML w Action
renderForm draft validationError =
  mapCalendarUi (Cal.renderForm draft validationError)

renderDateTimeContent :: forall w. IntentionDraft -> HTML w Action
renderDateTimeContent draft =
  mapCalendarUi (Cal.renderDateTimeContent draft)

renderAgendaView
  :: forall w
   . AgendaView
  -> String
  -> Array String
  -> Array CalendarItem
  -> Maybe String
  -> Maybe Int
  -> HTML w Action
renderAgendaView viewMode focusDate conflictIds items draggingId dragHoverIndex =
  mapCalendarUi (Cal.renderAgendaView viewMode focusDate conflictIds items draggingId dragHoverIndex)

renderViewSelector :: forall w. AgendaView -> String -> HTML w Action
renderViewSelector viewMode focusDate =
  map ViewAction (Disp.renderViewSelector viewMode focusDate)

renderMobileTools :: forall w. AgendaView -> HTML w Action
renderMobileTools viewMode =
  map ViewAction (Disp.renderMobileTools viewMode)

renderToolsContent :: forall w. HTML w Action
renderToolsContent =
  map ViewAction Disp.renderToolsContent

renderValidationPanel :: forall w. ValidationPanel -> HTML w Action
renderValidationPanel panel =
  map ViewAction (Disp.renderValidationPanel panel)

renderNotificationsPanel
  :: forall w
   . Boolean
  -> NotificationDefaults
  -> Array NotificationOverride
  -> Maybe NotificationEditor
  -> Array CalendarItem
  -> HTML w Action
renderNotificationsPanel isOpen defaults overrides editor intentions =
  map NotificationAction (Notif.renderNotificationsPanel isOpen defaults overrides editor intentions)

renderNotificationsContent
  :: forall w
   . NotificationDefaults
  -> Array NotificationOverride
  -> Maybe NotificationEditor
  -> Array CalendarItem
  -> HTML w Action
renderNotificationsContent defaults overrides editor intentions =
  map NotificationAction (Notif.renderNotificationsContent defaults overrides editor intentions)

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
  map ExportAction (Exp.renderExportPanel format typeFilter statusFilter categoryFilter startDate endDate output)

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
_calendarItems = _calendar <<< Cal._items

_calendarDraft :: Lens' State IntentionDraft
_calendarDraft = _calendar <<< Cal._draft

_calendarValidationError :: Lens' State (Maybe ValidationError)
_calendarValidationError = _calendar <<< Cal._validationError

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

withSubState :: forall s a. Lens' State s -> StateT s (WriterT (Array Command) Aff) a -> ErrorAgendaAppM a
withSubState lens action = do
  st <- get
  let subState = st ^. lens
  Tuple (Tuple result nextSubState) cmds <- liftAff $ runWriterT (runStateT action subState)
  modify_ (lens .~ nextSubState)
  traverse_ runCommand cmds
  pure result

runSyncCommand :: SyncCommand -> ErrorAgendaAppM Unit
runSyncCommand = case _ of
  SyncSetItems items     -> modify_ (_calendarItems .~ items)
  SyncCreateItem item    -> createItem item >>= (const refreshItems)
  SyncRefreshItems       -> refreshItems
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
      if ok
        then do
          modify_ ((_syncPendingSync .~ []) <<< (_syncConflict .~ Nothing))
          refreshItems
        else modify_ (_syncConflict .~ Just pending)

runDragCommand :: DragCommand -> ErrorAgendaAppM Unit
runDragCommand = case _ of
  DragSetItems items     -> modify_ (_calendarItems .~ items)
  DragUpsertPending item -> modify_ (_syncPendingSync %~ upsertPendingItem item)
  DragSetUpdateError err -> modify_ (_syncUpdateError .~ err)
  DragRefreshItems       -> refreshItems
  DragUpdateItem itemId updatedItem -> do
    resp <- updateItem itemId updatedItem
    if statusOk resp
      then modify_ (_syncUpdateError .~ Nothing)
      else modify_ (_syncUpdateError .~ Just (updateErrorMessage (unwrap resp.status)))
    refreshItems

runViewCommand :: ViewCommand -> ErrorAgendaAppM Unit
runViewCommand (ViewValidateItem itemId minutes) = validateItem itemId minutes >>= (const refreshItems)

runTemplateCommand :: TemplateCommand -> ErrorAgendaAppM Unit
runTemplateCommand (TemplateSetDraft draft) = modify_ (_calendarDraft .~ draft)

runImportCommand :: ImportCommand -> ErrorAgendaAppM Unit
runImportCommand (ImportSetItems items)     = modify_ (_calendarItems .~ items)
runImportCommand (ImportSetPending pending) = modify_ (_syncPendingSync .~ pending)

runCommand :: Command -> ErrorAgendaAppM Unit
runCommand = case _ of
  SyncCmd     cmd -> runSyncCommand cmd
  DragCmd     cmd -> runDragCommand cmd
  ViewCmd     cmd -> runViewCommand cmd
  TemplateCmd cmd -> runTemplateCommand cmd
  ImportCmd   cmd -> runImportCommand cmd


handleAction :: Action -> AgendaAppM Unit
handleAction action = handleError $
  case action of
    Init -> initAction
    CalendarAction calendarAction -> withSubState _calendar $ handleCalendarAction calendarAction
    SyncAction syncAction -> do
      st <- get
      withSubState _sync $ handleSyncAction (st ^. _calendarItems) syncAction
    DragAction dragAction -> do
      st <- get
      let
        ctx =
          { items: st ^. _calendarItems
          , focusDate: st ^. _viewFocusDate
          , offlineMode: st ^. _syncOfflineMode
          }
      withSubState _drag $ handleDragAction ctx dragAction
    ViewAction viewAction -> withSubState _view $ handleViewAction viewAction
    NotificationAction notificationAction -> withSubState _notifications $ handleNotificationAction notificationAction
    TemplateAction templateAction -> withSubState _templates $ handleTemplateAction templateAction
    ImportAction importAction -> do
      st <- get
      let
        ctx =
          { items: st ^. _calendarItems
          , pending: st ^. _syncPendingSync
          , offlineMode: st ^. _syncOfflineMode
          }
      withSubState _imports $ handleImportAction ctx importAction
    ExportAction exportAction -> do
      st <- get
      withSubState _exports $ handleExportAction (st ^. _calendarItems) exportAction

initAction :: ErrorAgendaAppM Unit
initAction = do
  now <- liftEffect nowDateTime
  modify_ $ _viewFocusDate .~ formatDate now
  refreshItems

refreshItems :: ErrorAgendaAppM Unit
refreshItems = do
  jsonResponse <- withExceptT toFatalError $ ExceptT $ liftAff getItemsResponse
  items <- (_.body >>> decodeJson >>> lmap toFatalError >>> pure >>> ExceptT) jsonResponse
  modify_ (_calendarItems .~ items)

createItem :: CalendarItem -> ErrorAgendaAppM (Response Json)
createItem item = withExceptT toFatalError $ ExceptT $ liftAff $ createItemResponse item

updateItem :: String -> CalendarItem -> ErrorAgendaAppM (Response Json)
updateItem itemId item = withExceptT toFatalError $ ExceptT $ liftAff $ updateItemResponse itemId item

validateItem :: String -> Int -> ErrorAgendaAppM (Response Json)
validateItem itemId minutes = withExceptT toFatalError $ ExceptT $ liftAff $ validateItemResponse itemId minutes

statusOk :: forall a. Response a -> Boolean
statusOk r = unwrap r.status >= 200 && unwrap r.status < 300

submitIntention :: ErrorAgendaAppM Unit
submitIntention = do
  st <- get
  case validateIntention (st ^. _calendarDraft) of
    Left err -> modify_ (_calendarValidationError .~ Just err)
    Right validDraft -> do
      let item = toNewIntention validDraft
      if st ^. _syncOfflineMode
        then do
          let
            result = applyOfflineMutation true item (st ^. _calendarItems) (st ^. _syncPendingSync)
          modify_
            ( (_calendarItems .~ result.items)
                <<< (_calendarDraft .~ emptyDraft)
                <<< (_calendarValidationError .~ Nothing)
                <<< (_syncPendingSync .~ result.pending)
            )
        else do
          _ <- createItem item
          modify_ ((_calendarDraft .~ emptyDraft) <<< (_calendarValidationError .~ Nothing))
          refreshItems


render
  :: forall m
   . State
  -> H.ComponentHTML Action () m
render { calendar, sync, drag, notifications, templates, imports, exports, view } =
  let
    { items, draft, validationError, showConflictsOnly, conflictResolution, sortMode } = calendar
    { offlineMode, syncConflict, updateError } = sync
    { draggingId, dragHoverIndex } = drag
    { notificationDefaults, notificationOverrides, notificationPanelOpen, notificationEditor } = notifications
    { templates: templateItems, templateDraft, editingTemplateId } = templates
    { csvInput, csvImportResult, icsInput, icsImportResult } = imports
    { exportFormat, exportTypeFilter, exportStatusFilter, exportCategoryFilter, exportStartDate, exportEndDate, exportOutput } = exports
    { viewMode, focusDate, validationPanel } = view
    agendaModalsInput = buildAgendaModalsInput { calendar, sync, drag, notifications, templates, imports, exports, view }
    { conflictGroups } = agendaModalsInput.filters
    { intentions: unplannedIntentions } = agendaModalsInput.notifications
    conflictIds = detectConflictIds items
    itemsToShow =
      if showConflictsOnly then filter (isConflict conflictIds) items
      else items
    sortedItems = sortItems sortMode conflictIds itemsToShow
  in
  div [ class_ "entity-page agenda-page" ]
      [ section [ class_ "agenda-header" ]
          [ h2 [ class_ "agenda-title" ] [ text (viewTitle viewMode) ]
          , div [ class_ "agenda-subtitle" ] [ text "Capture rapide des intentions a planifier." ]
          , div [ class_ "agenda-controls" ]
              [ button
                  [ class_ $ "btn btn-sm agenda-filter" <> if showConflictsOnly then " btn-outline-primary" else " btn-outline-secondary"
                  , onClick (const (CalendarAction CalendarToggleConflictFilter))
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
      , renderAgendaModals agendaModalsInput
      ]


renderFiltersContent :: forall w. Boolean -> Array (Array String) -> Boolean -> SortMode -> HTML w Action
renderFiltersContent showConflictsOnly conflictGroups offlineMode sortMode =
  div [ class_ "agenda-modal-stack" ]
    [ button
        [ class_ $ "btn btn-sm agenda-filter" <> if showConflictsOnly then " btn-outline-primary" else "btn-outline-secondary"
        , onClick (const (CalendarAction CalendarToggleConflictFilter))
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
  }

renderAgendaModals :: forall w. AgendaModalsInput -> HTML w Action
renderAgendaModals { activeModal, filters, notifications, templates, csvImport, icsImport, export, draft } =
  let
    renderModal modal title content =
      map (either ViewAction identity) (Modal.renderModal activeModal modal title content)
  in
  div []
    [ renderModal ModalFilters "Filtres"
        [ renderFiltersContent filters.showConflictsOnly filters.conflictGroups filters.offlineMode filters.sortMode ]
    , renderModal ModalTools "Outils"
        [ renderToolsContent ]
    , renderModal ModalDateTime "Dates et heures"
        [ renderDateTimeContent draft ]
    , renderModal ModalNotifications "Rappels"
        [ renderNotificationsContent notifications.defaults notifications.overrides notifications.editor notifications.intentions ]
    , renderModal ModalTemplates "Templates de taches"
        [ renderTemplatesPanel templates.items templates.draft templates.editingId ]
    , renderModal ModalImportCsv "Import CSV"
        [ renderCsvImportPanel csvImport.input csvImport.result ]
    , renderModal ModalImportIcs "Import ICS"
        [ renderIcsImportPanel icsImport.input icsImport.result ]
    , renderModal ModalExport "Export"
        [ renderExportPanel export.format export.typeFilter export.statusFilter export.categoryFilter export.startDate export.endDate export.output ]
    ]

buildAgendaModalsInput :: State -> AgendaModalsInput
buildAgendaModalsInput { calendar, sync, notifications, templates, imports, exports, view } =
  let
    { items, draft, showConflictsOnly, sortMode } = calendar
    { offlineMode } = sync
    { notificationDefaults, notificationOverrides, notificationEditor } = notifications
    { templates: templateItems, templateDraft, editingTemplateId } = templates
    { csvInput, csvImportResult, icsInput, icsImportResult } = imports
    { exportFormat, exportTypeFilter, exportStatusFilter, exportCategoryFilter, exportStartDate, exportEndDate, exportOutput } = exports
    { activeModal } = view
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
  }


renderAccordion :: forall w i. String -> String -> HTML w i -> HTML w i
renderAccordion title extraClass content =
  div [ class_ ("agenda-accordion-wrap " <> extraClass) ]
    [ details [ class_ "agenda-accordion-details" ]
        [ summary [ class_ "agenda-accordion-summary" ] [ text title ]
        , div [ class_ "agenda-accordion-content" ] [ content ]
        ]
    ]
