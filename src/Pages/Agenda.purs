module Pages.Agenda
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
import Agenda.Data
  ( DataAction(..)
  , DataState
  , dataInitialState
  , emptyDraft
  , handleDataAction
  , renderAgendaView
  , renderConflictActions
  , renderConflictResolution
  , renderDateTimeContent
  , renderForm
  , renderSortPicker
  , validateIntention
  , toNewIntention
  )
import Agenda.Display
  ( AgendaModal(..)
  , ViewAction(..)
  , ViewState
  , handleViewAction
  , renderMobileTools
  , renderToolsContent
  , renderValidationPanel
  , renderViewSelector
  , viewInitialState
  , viewTitle
  )
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
  , renderExportPanel
  )
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
  , renderCsvImportPanel
  , renderIcsImportPanel
  )
import Agenda.Notifications
  ( NotificationAction
  , NotificationEditor
  , NotificationState
  , handleNotificationAction
  , notificationInitialState
  , renderNotificationsContent
  , renderNotificationsPanel
  )
import Agenda.Offline (applyOfflineMutation, upsertPendingItem)
import Agenda.Sync
  ( SyncAction
  , SyncState
  , handleSyncAction
  , renderOfflineToggle
  , renderSyncConflict
  , renderUpdateError
  , syncInitialState
  , updateErrorMessage
  )
import Agenda.Templates
  ( TemplateAction
  , TemplateState
  , handleTemplateAction
  , renderTemplatesPanel
  , templateInitialState
  )
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
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (filter, foldM, null)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
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
import Ui.Modal (renderModal)


type NoOutput = Void

type AgendaAppM = H.HalogenM State Action () NoOutput Aff

type ErrorAgendaAppM = ExceptT FatalError AgendaAppM


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


data Action
  = InitAction
  | DataAction DataAction
  | SyncAction SyncAction
  | DragAction DragAction
  | ViewAction ViewAction
  | NotificationAction NotificationAction
  | TemplateAction TemplateAction
  | ImportAction ImportAction
  | ExportAction ExportAction


component :: forall q i. H.Component q i NoOutput Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = pure InitAction
        }
    }


initialState :: forall i. i -> State
initialState = const
  { data: dataInitialState
  , sync: syncInitialState
  , drag: dragInitialState
  , notifications: notificationInitialState
  , templates: templateInitialState
  , imports: importInitialState
  , exports: exportInitialState
  , view: viewInitialState
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

_viewFocusDate :: Lens' State String
_viewFocusDate = _view <<< prop (Proxy :: _ "focusDate")


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
  SyncSetItems items ->
    modify_ (_dataItems .~ items)
  SyncCreateItem item -> do
    _ <- createItem item
    refreshItems
  SyncRefreshItems ->
    refreshItems
  SyncSubmitIntentionCmd ->
    submitIntention
  SyncRunPending pending ->
    if null pending then
      refreshItems
    else do
      ok <- foldM (\acc item -> if not acc
                                 then pure false
                                 else do
                                   resp <- createItem item
                                   pure (statusOk resp))
                  true
                  pending
      if ok then do
        modify_ ((_syncPendingSync .~ []) <<< (_syncConflict .~ Nothing))
        refreshItems
      else modify_ (_syncConflict .~ Just pending)

runDragCommand :: DragCommand -> ErrorAgendaAppM Unit
runDragCommand = case _ of
  DragSetItems items ->
    modify_ (_dataItems .~ items)
  DragUpsertPending item ->
    modify_ (_syncPendingSync %~ upsertPendingItem item)
  DragSetUpdateError err ->
    modify_ (_syncUpdateError .~ err)
  DragUpdateItem itemId updatedItem -> do
    resp <- updateItem itemId updatedItem
    if statusOk resp then
      modify_ (_syncUpdateError .~ Nothing)
    else
      modify_ (_syncUpdateError .~ Just (updateErrorMessage (unwrap resp.status)))
    refreshItems
  DragRefreshItems ->
    refreshItems

runViewCommand :: ViewCommand -> ErrorAgendaAppM Unit
runViewCommand = case _ of
  ViewValidateItem itemId minutes -> do
    _ <- validateItem itemId minutes
    refreshItems

runTemplateCommand :: TemplateCommand -> ErrorAgendaAppM Unit
runTemplateCommand = case _ of
  TemplateSetDraft draft ->
    modify_ (_dataDraft .~ draft)

runImportCommand :: ImportCommand -> ErrorAgendaAppM Unit
runImportCommand = case _ of
  ImportSetItems items ->
    modify_ (_dataItems .~ items)
  ImportSetPending pending ->
    modify_ (_syncPendingSync .~ pending)

runCommand :: Command -> ErrorAgendaAppM Unit
runCommand = case _ of
  SyncCmd cmd -> runSyncCommand cmd
  DragCmd cmd -> runDragCommand cmd
  ViewCmd cmd -> runViewCommand cmd
  TemplateCmd cmd -> runTemplateCommand cmd
  ImportCmd cmd -> runImportCommand cmd


handleAction :: Action -> AgendaAppM Unit
handleAction action = handleError $
  case action of
    InitAction -> initAction
    DataAction dataAction -> withSubState _data $ handleDataAction dataAction
    SyncAction syncAction -> do
      st <- get
      withSubState _sync $ handleSyncAction (st ^. _dataItems) syncAction
    DragAction dragAction -> do
      st <- get
      let
        ctx =
          { items: st ^. _dataItems
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
          { items: st ^. _dataItems
          , pending: st ^. _syncPendingSync
          , offlineMode: st ^. _syncOfflineMode
          }
      withSubState _imports $ handleImportAction ctx importAction
    ExportAction exportAction -> do
      st <- get
      withSubState _exports $ handleExportAction (st ^. _dataItems) exportAction


initAction :: ErrorAgendaAppM Unit
initAction = do
  now <- liftEffect nowDateTime
  lift $ modify_ $ _viewFocusDate .~ formatDate now
  refreshItems


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
              , renderOfflineToggle SyncAction offlineMode
              , renderSortPicker DataAction sortMode
              , renderConflictActions DataAction conflictGroups
              ]
          , renderViewSelector ViewAction viewMode focusDate
          , renderMobileTools ViewAction viewMode
          ]
      , div [ class_ $ "agenda-layout" <> if viewMode == ViewDay then " agenda-layout--calendar" else "" ]
          [ div [ class_ "agenda-main" ]
              [ maybe (text "") (renderUpdateError SyncAction) updateError
              , maybe (text "") (renderValidationPanel ViewAction) validationPanel
              , section [ class_ $ "agenda-list-panel" <> if viewMode == ViewDay then " agenda-list-panel--calendar" else "" ]
                  [ if viewMode == ViewDay then div [ class_ "agenda-calendar-form-header" ] [ renderForm DataAction SyncAction ViewAction draft validationError ]
                    else text ""
                  , renderAgendaView DragAction SyncAction ViewAction viewMode focusDate conflictIds sortedItems draggingId dragHoverIndex
                  ]
              , maybe (text "") (renderConflictResolution DataAction items) conflictResolution
              , maybe (text "") (renderSyncConflict SyncAction) syncConflict
              ]
          , div [ class_ "agenda-side" ]
              [ renderNotificationsPanel NotificationAction notificationPanelOpen notificationDefaults notificationOverrides notificationEditor unplannedIntentions
              , renderAccordion "Templates de taches" "agenda-accordion templates" $ renderTemplatesPanel TemplateAction templateItems templateDraft editingTemplateId
              , renderAccordion "Import CSV" "agenda-accordion import-csv" $ renderCsvImportPanel ImportAction csvInput csvImportResult
              , renderAccordion "Import ICS" "agenda-accordion import-ics" $ renderIcsImportPanel ImportAction icsInput icsImportResult
              , renderAccordion "Export" "agenda-accordion export" $ renderExportPanel ExportAction exportFormat exportTypeFilter exportStatusFilter exportCategoryFilter exportStartDate exportEndDate exportOutput
              ]
          ]
      , renderAgendaModals
          activeModal
          showConflictsOnly
          conflictGroups
          offlineMode
          sortMode
          notificationDefaults
          notificationOverrides
          notificationEditor
          draft
          unplannedIntentions
          templateItems
          templateDraft
          editingTemplateId
          csvInput
          csvImportResult
          icsInput
          icsImportResult
          exportFormat
          exportTypeFilter
          exportStatusFilter
          exportCategoryFilter
          exportStartDate
          exportEndDate
          exportOutput
      ]


renderFiltersContent :: forall w. Boolean -> Array (Array String) -> Boolean -> SortMode -> HTML w Action
renderFiltersContent showConflictsOnly conflictGroups offlineMode sortMode =
  div [ class_ "agenda-modal-stack" ]
    [ button
        [ class_ $ "btn btn-sm agenda-filter" <> if showConflictsOnly then " btn-outline-primary" else "btn-outline-secondary"
        , onClick (const (DataAction DataToggleConflictFilter))
        ]
        [ text "Filtrer: en conflit" ]
    , renderOfflineToggle SyncAction offlineMode
    , renderSortPicker DataAction sortMode
    , renderConflictActions DataAction conflictGroups
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
        [ renderToolsContent ViewAction ]
    , renderModal "Dates et heures" (activeModal == Just ModalDateTime) (ViewAction ViewCloseModal)
        [ renderDateTimeContent DataAction draft ]
    , renderModal "Rappels" (activeModal == Just ModalNotifications) (ViewAction ViewCloseModal)
        [ renderNotificationsContent NotificationAction defaults overrides editor intentions ]
    , renderModal "Templates de taches" (activeModal == Just ModalTemplates) (ViewAction ViewCloseModal)
        [ renderTemplatesPanel TemplateAction templates templateDraft editingTemplateId ]
    , renderModal "Import CSV" (activeModal == Just ModalImportCsv) (ViewAction ViewCloseModal)
        [ renderCsvImportPanel ImportAction csvInput csvImportResult ]
    , renderModal "Import ICS" (activeModal == Just ModalImportIcs) (ViewAction ViewCloseModal)
        [ renderIcsImportPanel ImportAction icsInput icsImportResult ]
    , renderModal "Export" (activeModal == Just ModalExport) (ViewAction ViewCloseModal)
        [ renderExportPanel ExportAction exportFormat exportTypeFilter exportStatusFilter exportCategoryFilter exportStartDate exportEndDate exportOutput ]
    ]


renderAccordion :: forall w i. String -> String -> HTML w i -> HTML w i
renderAccordion title extraClass content =
  div [ class_ ("agenda-accordion-wrap " <> extraClass) ]
    [ details [ class_ "agenda-accordion-details" ]
        [ summary [ class_ "agenda-accordion-summary" ] [ text title ]
        , div [ class_ "agenda-accordion-content" ] [ content ]
        ]
    ]
