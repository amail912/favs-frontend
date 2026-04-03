module Pages.Calendar
  ( component
  , decodeCalendarItemsResponse
  , CalendarItem(..)
  , CalendarItemContent(..)
  , TaskCalendarItemFields
  , TripCalendarItemFields
  , TaskDraft
  , TripDraft
  , ItemStatus(..)
  , ItemType(..)
  , SortMode(..)
  , ValidationError(..)
  , TripValidationError(..)
  , toNewTask
  , toNewTrip
  , buildTimelineLayout
  , MobileOverlapStack
  , MobileHiddenCard
  , buildMobileOverlapStacks
  , applyMobileOverlapPromotions
  , toTimelineBlock
  , TaskEditDraft
  , TripEditDraft
  , EditDraft(..)
  , EditError(..)
  , applyEditDraft
  , buildEditDraft
  , durationMinutesBetween
  , sortItems
  , validateTask
  , validateTrip
  , decodeTripPlacesResponse
  , decodeSharedUsersResponse
  , decodePeriodTripsResponse
  , normalizePeriodTripGroups
  , deriveSharedPresence
  , SharedPeriodTrip
  , SharedPeriodTripGroup
  , SharedPresence
  , SharedPresenceLoadState(..)
  , SharedPresenceGroup
  , SharedPresenceSegment
  , SharedPresenceState(..)
  , PresenceCueColorToken(..)
  , SharedPresenceCuePreference(..)
  , encodePresenceCuePreferencesJson
  , decodePresenceCuePreferencesJson
  , resolveSharedPresenceToneClass
  , SharedPresenceSegmentLayout
  , shouldRenderSharedPresenceRail
  , shouldRenderDayCalendarShell
  , maxVisibleSharedPresenceUsers
  , buildSharedPresenceSegmentLayouts
  , buildSharedPresenceRailView
  , sharedPresenceSegmentRailClass
  , sharedPresenceLaneToneClass
  , presenceInspectionStateText
  , presenceInspectionTimeText
  , presenceInspectionAriaLabel
  , tripWriteErrorMessage
  , validateShareUsername
  , shareWriteErrorMessage
  , subscriptionWriteErrorMessage
  , calendarItemPrimaryText
  , calendarItemSecondaryText
  , calendarItemCardClass
  , calendarItemTimelineCardClass
  , calendarItemSupportsEdit
  , computeDropMinuteIndex
  , computeMinuteIndexFromClientY
  , resolveDroppedWindow
  , resolveDroppedWindowFromIndex
  , DayFocusTarget(..)
  , computeDayFocusTarget
  , indexToTimeLabel
  ) where

import Prelude hiding (div)
import Affjax.Web (Response)
import Api.Calendar (PeriodTrip(..), PeriodTripGroup(..), TripPlace(..), TripSharingUser(..), addSharedUserResponse, addSubscribedUserResponse, createItemResponse, deleteSharedUserResponse, deleteSubscribedUserResponse, getItemsResponse, getPeriodTripsResponse, getSharedUsersResponse, getSubscribedUsersResponse, getTripPlacesResponse, updateItemResponse)
import Calendar.Recurrence (RecurrenceDraft, RecurrenceRule, defaultRecurrenceDraft, draftFromRecurrence, draftToRecurrence)
import Calendar.Recurrence as Recurrence
import Calendar.ExImport.Export as Export
import Calendar.ExImport.Import as Import
import Control.Monad.Except (ExceptT(..), withExceptT)
import Control.Monad.State.Trans (get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json, jsonEmptyObject, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (any, filter, find, findIndex, foldl, length, mapMaybe, mapWithIndex, null, sortBy, uncons, updateAt)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (fold, foldMap)
import Data.Lens (Iso', Lens', iso, view, (.~), (%~), (^.), lens)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap, wrap)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Now (nowDateTime)
import Halogen (Component, ComponentHTML, HalogenM, Slot, defaultEval, fork, getRef, mkComponent, mkEval) as H
import Halogen (subscribe)
import Halogen.HTML (HTML, button, div, h2, i, input, li, option, section, select, slot, span, text, ul)
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events (handler', onClick, onDragEnter, onDragOver, onDrop, onMouseDown, onValueChange, onValueInput, onKeyDown, onDragEnd, onDragStart, onScroll, onTouchStart, onTouchMove, onTouchEnd, onTouchCancel)
import Halogen.HTML.Properties (attr, style, IProp, value, placeholder, type_, draggable, ref, disabled)
import Halogen.Query.Event as HQE
import Type.Proxy (Proxy(..))
import Ui.Errors (FatalError, handleError, toFatalError)
import Ui.Focus (focusElement, openDateInputPicker)
import Ui.AuthSession as AuthSession
import Ui.LocalStorage as LocalStorage
import Ui.Modal (renderBottomSheet, renderModal, renderModalWithValidateState) as Modal
import Ui.Utils (class_)
import Web.Event.Event (EventType(..), preventDefault)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KeyboardEventTypes
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.FocusEvent as FocusEvent
import Data.Enum (enumFromTo, fromEnum, toEnum)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Control.Alt ((<|>))
import Data.Time.Duration (Days(..), Minutes(..), Milliseconds(..))
import Effect (Effect)
import Web.Event.Event (currentTarget, target) as Event
import Web.HTML.Event.DragEvent (DragEvent, dataTransfer, toEvent)
import Data.MediaType (MediaType(..))
import Web.HTML.Event.DataTransfer (setData)
import Web.HTML.HTMLElement as HTMLElement
import Web.DOM.Element (Element, getBoundingClientRect)
import Web.TouchEvent.TouchEvent as TouchEvent
import Web.TouchEvent.TouchList as TouchList
import Web.TouchEvent.Touch as Touch
import Helpers.DateTime as DateTime
import Data.Date (Date, canonicalDate, month, year)
import Data.DateTime (DateTime(..), adjust, date, diff, time)
import Data.Time (Time, hour, minute)
import Data.String.Common as StringCommon
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Int as Int
import Data.Traversable (traverse)

-- foldl comes from Data.Array in this module

foreign import scrollElementIntoView :: Element -> Effect Unit
foreign import viewportVisibleHeight :: Effect Number

-- BEGIN src/Pages/Calendar.purs
type NoOutput = Void
type AgendaAppM = H.HalogenM State Action Slots NoOutput Aff
type ErrorAgendaAppM = ExceptT FatalError AgendaAppM

data CreateItemMode
  = CreateTask
  | CreateTrip

derive instance eqCreateItemMode :: Eq CreateItemMode
derive instance genericCreateItemMode :: Generic CreateItemMode _

instance showCreateItemMode :: Show CreateItemMode where
  show = genericShow

data DayFocusTarget
  = FocusCurrentTime
  | FocusFirstTask
  | FocusTop

derive instance eqDayFocusTarget :: Eq DayFocusTarget
derive instance genericDayFocusTarget :: Generic DayFocusTarget _

instance showDayFocusTarget :: Show DayFocusTarget where
  show = genericShow

type State =
  { calendar :: CalendarState
  , sync :: SyncState
  , mouseDrag :: DragState
  , view :: ViewState
  }

data Action
  = Init
  | CreateFormAction CreateFormAction
  | DragAction DragAction
  | ShareAction ShareAction
  | SubscriptionAction SubscriptionAction
  | ViewAction ViewAction
  | SyncDraftTitleKeyDown String
  | SyncSubmitTask
  | SyncDismissUpdateError
  | GlobalKeyDown String
  | GlobalResize
  | ImportApplyItems (Array CalendarItemContent)
  | CreateRecurrenceCmd Recurrence.RecurrenceCommand
  | EditRecurrenceCmd Recurrence.RecurrenceCommand

type DragAction =
  { log :: String
  , dragAction :: DragActionImpl
  }

data DragActionImpl
  = DragStart String DragEvent
  | DragEnd
  | DragOverCalendar DragEvent
  | DropOnCalendar DragEvent
  | TouchDragStart String TouchEvent.TouchEvent
  | TouchLongPressReady Int
  | TouchDragMove TouchEvent.TouchEvent
  | TouchDrop TouchEvent.TouchEvent
  | TouchDragCancel

data RecurrenceSlot
  = RecurrenceCreate
  | RecurrenceEdit

derive instance eqRecurrenceSlot :: Eq RecurrenceSlot
derive instance ordRecurrenceSlot :: Ord RecurrenceSlot
type RecurrenceSlotDef slot = forall query. H.Slot query Recurrence.RecurrenceCommand slot

data ExportSlot = ExportModal

derive instance eqExportSlot :: Eq ExportSlot
derive instance ordExportSlot :: Ord ExportSlot
type ExportSlotDef slot = forall query. H.Slot query Void slot

data ImportSlot
  = ImportCsv
  | ImportIcs

derive instance eqImportSlot :: Eq ImportSlot
derive instance ordImportSlot :: Ord ImportSlot
type ImportSlotDef slot = forall query. H.Slot query Import.Output slot

type Slots =
  ( recurrence :: RecurrenceSlotDef RecurrenceSlot
  , export :: ExportSlotDef ExportSlot
  , importCsv :: ImportSlotDef ImportSlot
  , importIcs :: ImportSlotDef ImportSlot
  )

class ToAction a where
  toAction :: a -> Action

instance toActionCreateFormAction :: ToAction CreateFormAction where
  toAction = CreateFormAction

instance toActionShareAction :: ToAction ShareAction where
  toAction = ShareAction

instance toActionSubscriptionAction :: ToAction SubscriptionAction where
  toAction = SubscriptionAction

instance toActionViewAction :: ToAction ViewAction where
  toAction = ViewAction

data ShareAction
  = ShareUsernameChanged String
  | ShareSubmitRequested
  | ShareDeleteRequested String
  | ShareReloadRequested

data SubscriptionAction
  = SubscriptionUsernameChanged String
  | SubscriptionSubmitRequested
  | SubscriptionDeleteRequested String
  | SubscriptionReloadRequested

renderAgendaView
  :: forall w
   . CalendarView
  -> String
  -> String
  -> Array CalendarItem
  -> SharedPresenceLoadState
  -> PresenceCuePreferencesState
  -> Maybe SharedPresenceInspection
  -> Maybe Number
  -> Boolean
  -> Boolean
  -> Array MobileOverlapPromotion
  -> Maybe String
  -> Maybe Int
  -> HTML w Action
renderAgendaView viewMode focusDate todayDate items sharedPresence presenceCuePreferences presenceInspection presenceInspectionInitialTopPx presenceInspectionPinned isMobile promotedOverlaps draggingId dragHoverIndex =
  case viewMode of
    ViewDay ->
      renderDayCalendar focusDate todayDate items sharedPresence presenceCuePreferences presenceInspection presenceInspectionInitialTopPx presenceInspectionPinned isMobile promotedOverlaps draggingId dragHoverIndex
    ViewWeek ->
      renderRangeView "Semaine" (generateDateRange focusDate 7) items isMobile
    ViewMonth ->
      renderRangeView "Mois" (generateMonthDates focusDate) items isMobile

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

prefillCreateDraft :: CreateItemMode -> String -> CreateDraft -> CreateDraft
prefillCreateDraft _ _ draft = draft

itemTypeIso :: Iso' ItemType Export.ItemType
itemTypeIso = iso toExport fromExport
  where
  toExport = case _ of
    Task -> Export.Task
  fromExport = case _ of
    Export.Task -> Task

itemStatusIso :: Iso' ItemStatus Export.ItemStatus
itemStatusIso = iso toExport fromExport
  where
  toExport = case _ of
    Todo -> Export.Todo
    InProgress -> Export.InProgress
    Done -> Export.Done
    Canceled -> Export.Canceled
  fromExport = case _ of
    Export.Todo -> Todo
    Export.InProgress -> InProgress
    Export.Done -> Done
    Export.Canceled -> Canceled

fromExportItemType :: Export.ItemType -> ItemType
fromExportItemType = case _ of
  Export.Task -> Task

fromExportItemStatus :: Export.ItemStatus -> ItemStatus
fromExportItemStatus = case _ of
  Export.Todo -> Todo
  Export.InProgress -> InProgress
  Export.Done -> Done
  Export.Canceled -> Canceled

toCalendarItemContent :: Export.Item -> CalendarItemContent
toCalendarItemContent (Export.Item item) =
  TaskCalendarItemContent
    { itemType: fromExportItemType item.itemType
    , title: item.title
    , windowStart: item.windowStart
    , windowEnd: item.windowEnd
    , status: fromExportItemStatus item.status
    , sourceItemId: item.sourceItemId
    , actualDurationMinutes: item.actualDurationMinutes
    , category: item.category
    , recurrenceRule: item.recurrenceRule
    , recurrenceExceptionDates: item.recurrenceExceptionDates
    }

toCalendarItem :: CalendarItemContent -> CalendarItem
toCalendarItem content =
  NewCalendarItem { content }

toExportItem :: CalendarItem -> Maybe Export.Item
toExportItem item =
  case calendarItemContent item of
    TaskCalendarItemContent content ->
      Just
        ( Export.Item
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
        )
    TripCalendarItemContent _ ->
      Nothing

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
  , mouseDrag: dragInitialState
  , view: viewInitialState
  }

_calendar :: Lens' State CalendarState
_calendar = prop (Proxy :: _ "calendar")

_sync :: Lens' State SyncState
_sync = prop (Proxy :: _ "sync")

_mouseDrag :: Lens' State DragState
_mouseDrag = prop (Proxy :: _ "mouseDrag")

_view :: Lens' State ViewState
_view = prop (Proxy :: _ "view")

_calendarItems :: Lens' State (Array CalendarItem)
_calendarItems = _calendar <<< _items

_calendarDraft :: Lens' State CreateDraft
_calendarDraft = _calendar <<< _draft

_calendarValidationError :: Lens' State (Maybe String)
_calendarValidationError = _calendar <<< _validationError

_calendarLastCreateMode :: Lens' State CreateItemMode
_calendarLastCreateMode = _calendar <<< _lastCreateMode

_calendarTripPlaces :: Lens' State TripPlacesState
_calendarTripPlaces = _calendar <<< _tripPlaces

_calendarShareList :: Lens' State ShareListState
_calendarShareList = _calendar <<< _shareList

_calendarSubscriptionList :: Lens' State ShareListState
_calendarSubscriptionList = _calendar <<< _subscriptionList

_calendarSharedPresence :: Lens' State SharedPresenceLoadState
_calendarSharedPresence = _calendar <<< _sharedPresence

_calendarPresenceCuePreferences :: Lens' State PresenceCuePreferencesState
_calendarPresenceCuePreferences = _calendar <<< _presenceCuePreferences

_syncUpdateErrorState :: Lens' State (Maybe String)
_syncUpdateErrorState = _sync <<< _syncUpdateError

_viewFocusDatePage :: Lens' State String
_viewFocusDatePage = _view <<< _viewFocusDateState

handleAction :: Action -> AgendaAppM Unit
handleAction action = handleError $
  case action of
    Init -> initAction
    CreateFormAction formAction ->
      lift (applyCreateFormAction formAction)
    ShareAction shareAction ->
      lift (handleShareAction shareAction)
    SubscriptionAction subscriptionAction ->
      lift (handleSubscriptionAction subscriptionAction)
    SyncDraftTitleKeyDown key ->
      when (key == "Enter") submitTask
    SyncSubmitTask ->
      submitTask
    SyncDismissUpdateError ->
      modify_ (_syncUpdateErrorState .~ Nothing)
    DragAction dragAction ->
      handleDragAction dragAction
    ViewAction viewAction -> do
      handleViewAction viewAction
      case viewAction of
        ViewOpenModal _ -> focusModal
        ViewOpenCreate -> do
          focusModal
          st <- get
          let
            lastMode = st ^. _calendarLastCreateMode
            baseDraft = emptyCreateDraft lastMode
            nextDraft = prefillCreateDraft lastMode (st ^. _viewFocusDatePage) baseDraft
          modify_ ((_calendarDraft .~ nextDraft) <<< (_calendarValidationError .~ Nothing))
        ViewOpenEdit _ -> focusModal
        ViewOpenEditFromDoubleClick _ -> focusModal
        ViewChangedAction _ -> do
          scheduleDayTimelineFocus
          st' <- get
          when (st' ^. (_view <<< _viewMode) == ViewDay) $
            lift loadSharedPresenceForFocusDate
        ViewFocusDateChanged _ -> do
          scheduleDayTimelineFocus
          st' <- get
          when (st' ^. (_view <<< _viewMode) == ViewDay) $
            lift loadSharedPresenceForFocusDate
        ViewCloseCreate -> do
          st <- get
          let
            lastMode = st ^. _calendarLastCreateMode
          modify_ ((_calendarDraft .~ emptyCreateDraft lastMode) <<< (_calendarValidationError .~ Nothing))
        _ -> pure unit
    GlobalKeyDown key ->
      if key == "Escape" then do
        st <- get
        case st ^. (_view <<< _viewActiveModal) of
          Just ModalEditItem -> handleViewAction ViewEditCancel
          Just ModalCreateItem -> do
            handleViewAction ViewCloseCreate
            st' <- get
            let
              lastMode = st' ^. _calendarLastCreateMode
            modify_ ((_calendarDraft .~ emptyCreateDraft lastMode) <<< (_calendarValidationError .~ Nothing))
          Just _ -> handleViewAction ViewCloseModal
          Nothing -> pure unit
      else pure unit
    GlobalResize -> do
      viewport <- liftEffect $ window >>= Window.innerWidth
      handleViewAction (ViewSetIsMobile (viewport <= 768))
    CreateRecurrenceCmd (Recurrence.RecurrenceApplied draft) ->
      modify_
        ( _calendar
            %~
              ( \calendarState ->
                  case calendarState.draft of
                    CreateTaskDraft taskDraft ->
                      calendarState
                        { draft = CreateTaskDraft (taskDraft { recurrence = draft })
                        , validationError = Nothing
                        }
                    CreateTripDraft _ ->
                      calendarState
              )
        )
    EditRecurrenceCmd (Recurrence.RecurrenceApplied draft) ->
      modify_
        ((_view <<< _viewEditPanel) %~ map (updateEditPanelRecurrence draft))
    ImportApplyItems contents -> do
      let
        imported = map toCalendarItem contents
      modify_ (_calendarItems %~ (_ <> imported))

handleDragAction :: DragAction -> ErrorAgendaAppM Unit
handleDragAction { log, dragAction } = do
  liftEffect $ Console.log log
  case dragAction of
    DragStart itemId ev -> do
      liftEffect $ setData (MediaType "text/plain") itemId (dataTransfer ev)
      st <- get
      let
        duration =
          find
            ( \item -> case item of
                ServerCalendarItem { id } -> id == itemId
                _ -> false
            )
            (st ^. _calendarItems) <#> \item ->
            durationMinutesBetween (calendarItemWindowStart item) (calendarItemWindowEnd item)
      offset <- liftEffect $ dragOffsetFromEvent ev duration
      modify_ (_mouseDrag %~ ((_draggingId .~ Just itemId) <<< (_dragOffsetMinutes .~ offset)))
    DragEnd ->
      modify_ (_mouseDrag %~ clearActiveDragState)
    TouchDragStart itemId ev -> do
      st <- get
      let
        draggedItem =
          find
            ( \item -> case item of
                ServerCalendarItem { id } -> id == itemId
                _ -> false
            )
            (st ^. _calendarItems)
        duration =
          draggedItem <#> \item ->
            durationMinutesBetween (calendarItemWindowStart item) (calendarItemWindowEnd item)
        startIndex =
          draggedItem <#> \item ->
            Int.quot (minuteOfDay (calendarItemWindowStart item)) 5
      case startIndex of
        Nothing ->
          modify_ (_mouseDrag %~ clearTouchPendingState)
        Just initialIndex -> do
          offset <- liftEffect $ dragOffsetFromTouchEvent ev duration
          let nextToken = (st ^. (_mouseDrag <<< _touchLongPressToken)) + 1
          modify_
            ( _mouseDrag
                %~
                  ( (_touchLongPressToken .~ nextToken)
                      <<<
                        ( _touchPending .~ Just
                            { token: nextToken
                            , itemId
                            , startIndex: initialIndex
                            , offsetMinutes: offset
                            }
                        )
                      <<< clearActiveDragState
                  )
            )
          liftAff $ delay (Milliseconds 450.0)
          handleDragAction { log: "TouchLongPressReady@calendar-card", dragAction: TouchLongPressReady nextToken }
    TouchLongPressReady token -> do
      st <- get
      case st ^. (_mouseDrag <<< _touchPending) of
        Just pending | pending.token == token ->
          modify_
            ( _mouseDrag
                %~
                  ( (_draggingId .~ Just pending.itemId)
                      <<< (_dragHoverIndex .~ Just pending.startIndex)
                      <<< (_dragOffsetMinutes .~ pending.offsetMinutes)
                      <<< (_touchPending .~ Nothing)
                  )
            )
        _ ->
          pure unit
    DragOverCalendar ev -> do
      liftEffect $ preventDefault (toEvent ev)
      gridRef <- lift $ H.getRef (wrap "day-calendar-grid")
      idx <-
        case gridRef of
          Just grid -> liftEffect $ dragMinuteIndexFromElement ev grid
          Nothing -> liftEffect $ dragMinuteIndexFromEvent ev
      st <- get
      let
        offset = fromMaybe 0 (st ^. (_mouseDrag <<< _dragOffsetMinutes))
        adjusted = idx <#> \minuteIndex -> computeDropMinuteIndex minuteIndex offset
      modify_ (_mouseDrag <<< _dragHoverIndex .~ adjusted)
    TouchDragMove ev -> do
      gridRef <- lift $ H.getRef (wrap "day-calendar-grid")
      st <- get
      case st ^. (_mouseDrag <<< _draggingId) of
        Nothing ->
          modify_ (_mouseDrag %~ clearTouchPendingState)
        Just _ -> do
          liftEffect $ preventDefault (TouchEvent.toEvent ev)
          idx <-
            case gridRef of
              Just grid -> liftEffect $ touchMinuteIndexFromElement ev grid
              Nothing -> pure Nothing
          let
            offset = fromMaybe 0 (st ^. (_mouseDrag <<< _dragOffsetMinutes))
            adjusted = idx <#> \minuteIndex -> computeDropMinuteIndex minuteIndex offset
          modify_ (_mouseDrag <<< _dragHoverIndex .~ adjusted)
    DropOnCalendar ev -> do
      liftEffect $ preventDefault (toEvent ev)
      st <- get
      let items = st ^. _calendarItems
      case st ^. (_mouseDrag <<< _draggingId) of
        Nothing -> pure unit
        Just draggingId -> do
          gridRef <- lift $ H.getRef (wrap "day-calendar-grid")
          let
            hoverIndex = st ^. (_mouseDrag <<< _dragHoverIndex)
          fallbackIndex <-
            case gridRef of
              Just grid -> liftEffect $ dragMinuteIndexFromElement ev grid
              Nothing -> liftEffect $ dragMinuteIndexFromEvent ev
          let
            updated = do
              item <-
                find
                  ( \item -> case item of
                      ServerCalendarItem { id } -> id == draggingId
                      _ -> false
                  )
                  items
              let mins = durationMinutesBetween (calendarItemWindowStart item) (calendarItemWindowEnd item)
              case hoverIndex of
                Just adjustedIndex ->
                  resolveDroppedWindowFromIndex
                    (st ^. _viewFocusDatePage)
                    mins
                    adjustedIndex
                Nothing ->
                  resolveDroppedWindow
                    (st ^. _viewFocusDatePage)
                    (fromMaybe 0 (st ^. (_mouseDrag <<< _dragOffsetMinutes)))
                    mins
                    =<< fallbackIndex
            resetDragState =
              clearActiveDragState
          case updated of
            Nothing ->
              modify_ (_mouseDrag %~ resetDragState)
            Just { start, end } -> do
              let
                result = updateItemWindowById draggingId start end items
              case result.updated of
                Nothing ->
                  modify_ (_mouseDrag %~ resetDragState)
                Just updatedItem ->
                  do
                    resp <- updateItem draggingId updatedItem
                    if statusOk resp then modify_ (_syncUpdateErrorState .~ Nothing)
                    else modify_ (_syncUpdateErrorState .~ Just (updateErrorMessage (unwrap resp.status)))
                    refreshItems
                    modify_ (_mouseDrag %~ resetDragState)
    TouchDrop ev -> do
      st <- get
      let items = st ^. _calendarItems
      case st ^. (_mouseDrag <<< _draggingId) of
        Nothing ->
          modify_ (_mouseDrag %~ clearTouchPendingState)
        Just draggingId -> do
          liftEffect $ preventDefault (TouchEvent.toEvent ev)
          gridRef <- lift $ H.getRef (wrap "day-calendar-grid")
          fallbackIndex <-
            case gridRef of
              Just grid -> liftEffect $ touchMinuteIndexFromElement ev grid
              Nothing -> pure Nothing
          let
            updated = do
              item <-
                find
                  ( \item -> case item of
                      ServerCalendarItem { id } -> id == draggingId
                      _ -> false
                  )
                  items
              let mins = durationMinutesBetween (calendarItemWindowStart item) (calendarItemWindowEnd item)
              case st ^. (_mouseDrag <<< _dragHoverIndex) of
                Just adjustedIndex ->
                  resolveDroppedWindowFromIndex
                    (st ^. _viewFocusDatePage)
                    mins
                    adjustedIndex
                Nothing ->
                  resolveDroppedWindow
                    (st ^. _viewFocusDatePage)
                    (fromMaybe 0 (st ^. (_mouseDrag <<< _dragOffsetMinutes)))
                    mins
                    =<< fallbackIndex
            resetDragState =
              clearActiveDragState
          case updated of
            Nothing ->
              modify_ (_mouseDrag %~ resetDragState)
            Just { start, end } -> do
              let result = updateItemWindowById draggingId start end items
              case result.updated of
                Nothing ->
                  modify_ (_mouseDrag %~ resetDragState)
                Just updatedItem -> do
                  resp <- updateItem draggingId updatedItem
                  if statusOk resp then modify_ (_syncUpdateErrorState .~ Nothing)
                  else modify_ (_syncUpdateErrorState .~ Just (updateErrorMessage (unwrap resp.status)))
                  refreshItems
                  modify_ (_mouseDrag %~ resetDragState)
    TouchDragCancel ->
      modify_ (_mouseDrag %~ (clearTouchPendingState <<< clearActiveDragState))

initAction :: ErrorAgendaAppM Unit
initAction = do
  now <- liftEffect nowDateTime
  modify_ $ (_viewFocusDatePage .~ formatDate now) <<< (_view <<< _viewTodayDateState .~ formatDate now)
  viewport <- liftEffect $ window >>= Window.innerWidth
  handleViewAction (ViewSetIsMobile (viewport <= 768))
  subscribeToGlobalKeyDown
  subscribeToGlobalResize
  lift loadPresenceCuePreferences
  lift loadTripPlaces
  lift loadSharedUsers
  lift loadSubscribedUsers
  lift loadSharedPresenceForFocusDate
  refreshItems
  scheduleDayTimelineFocus

refreshItems :: ErrorAgendaAppM Unit
refreshItems = do
  jsonResponse <- withExceptT toFatalError $ ExceptT $ liftAff getItemsResponse
  items <- decodeCalendarItemsResponse jsonResponse # pure >>> ExceptT
  modify_ (_calendarItems .~ items)

loadPresenceCuePreferences :: AgendaAppM Unit
loadPresenceCuePreferences = do
  ownerUsername <- liftEffect AuthSession.loadAuthenticatedUsername
  preferences <-
    case ownerUsername of
      Nothing ->
        pure []
      Just username -> do
        raw <- liftEffect $ LocalStorage.getItem (presenceCuePreferencesStorageKey username)
        pure $ either (const []) identity (decodePresenceCuePreferencesJson raw)
  modify_
    ( _calendarPresenceCuePreferences .~ { ownerUsername, preferences }
    )

loadTripPlaces :: AgendaAppM Unit
loadTripPlaces = do
  modify_ (_calendarTripPlaces .~ TripPlacesLoading)
  result <- liftAff getTripPlacesResponse
  case result of
    Left _ ->
      modify_ (_calendarTripPlaces .~ TripPlacesError "Impossible de charger les lieux de trajet.")
    Right response ->
      if statusOk response then
        case decodeTripPlacesResponse response of
          Right places ->
            modify_ (_calendarTripPlaces .~ TripPlacesLoaded places)
          Left _ ->
            modify_ (_calendarTripPlaces .~ TripPlacesError "Impossible de charger les lieux de trajet.")
      else
        modify_ (_calendarTripPlaces .~ TripPlacesError "Impossible de charger les lieux de trajet.")

loadSharedUsers :: AgendaAppM Unit
loadSharedUsers = do
  modify_ (_calendarShareList %~ _ { isLoading = true, loadError = Nothing })
  result <- liftAff getSharedUsersResponse
  case result of
    Left _ ->
      modify_
        ( _calendarShareList
            %~
              _
                { isLoading = false
                , loadError = Just "Impossible de charger les partages de trajets."
                }
        )
    Right response ->
      if statusOk response then
        case decodeSharedUsersResponse response of
          Left _ ->
            modify_
              ( _calendarShareList
                  %~
                    _
                      { isLoading = false
                      , loadError = Just "Impossible de charger les partages de trajets."
                      }
              )
          Right usernames ->
            modify_
              ( _calendarShareList
                  %~
                    _
                      { usernames = usernames
                      , hasLoaded = true
                      , isLoading = false
                      , loadError = Nothing
                      }
              )
      else
        modify_
          ( _calendarShareList
              %~
                _
                  { isLoading = false
                  , loadError = Just "Impossible de charger les partages de trajets."
                  }
          )

loadSubscribedUsers :: AgendaAppM Unit
loadSubscribedUsers = do
  modify_ (_calendarSubscriptionList %~ _ { isLoading = true, loadError = Nothing })
  result <- liftAff getSubscribedUsersResponse
  case result of
    Left _ ->
      modify_
        ( _calendarSubscriptionList
            %~
              _
                { isLoading = false
                , loadError = Just "Impossible de charger les abonnements de trajets."
                }
        )
    Right response ->
      if statusOk response then
        case decodeSharedUsersResponse response of
          Left _ ->
            modify_
              ( _calendarSubscriptionList
                  %~
                    _
                      { isLoading = false
                      , loadError = Just "Impossible de charger les abonnements de trajets."
                      }
              )
          Right usernames ->
            modify_
              ( _calendarSubscriptionList
                  %~
                    _
                      { usernames = usernames
                      , hasLoaded = true
                      , isLoading = false
                      , loadError = Nothing
                      }
              )
      else
        modify_
          ( _calendarSubscriptionList
              %~
                _
                  { isLoading = false
                  , loadError = Just "Impossible de charger les abonnements de trajets."
                  }
          )

loadSharedPresenceForFocusDate :: AgendaAppM Unit
loadSharedPresenceForFocusDate = do
  st <- get
  let focusDate = st ^. _viewFocusDatePage
  modify_ (_view <<< _viewPresenceInspection .~ Nothing)
  modify_ (_view <<< _viewPresenceInspectionPinned .~ false)
  modify_ (_view <<< _viewActiveModal %~ clearPresenceInspectionModal)
  case buildDayPeriodRange focusDate of
    Nothing ->
      modify_ (_calendarSharedPresence .~ SharedPresenceError "Impossible de charger les trajets partagés pour cette date.")
    Just { start, end } -> do
      modify_ (_calendarSharedPresence .~ SharedPresenceLoading)
      result <- liftAff $ getPeriodTripsResponse start end
      case result of
        Left _ ->
          modify_ (_calendarSharedPresence .~ SharedPresenceError "Impossible de charger les trajets partagés pour cette date.")
        Right response ->
          if statusOk response then
            case decodePeriodTripsResponse response of
              Left _ ->
                modify_ (_calendarSharedPresence .~ SharedPresenceError "Impossible de charger les trajets partagés pour cette date.")
              Right groups ->
                case buildDayPeriodDateTimeRange focusDate of
                  Nothing ->
                    modify_ (_calendarSharedPresence .~ SharedPresenceError "Impossible de charger les trajets partagés pour cette date.")
                  Just period ->
                    modify_ (_calendarSharedPresence .~ SharedPresenceLoaded (deriveSharedPresence period.start period.end (normalizePeriodTripGroups groups)))
          else
            modify_ (_calendarSharedPresence .~ SharedPresenceError "Impossible de charger les trajets partagés pour cette date.")
  where
  clearPresenceInspectionModal = case _ of
    Just ModalPresenceInspection -> Nothing
    value -> value

handleShareAction :: ShareAction -> AgendaAppM Unit
handleShareAction = case _ of
  ShareUsernameChanged raw ->
    modify_
      ( _calendarShareList
          %~
            _
              { usernameDraft = raw
              , submitError = Nothing
              }
      )
  ShareReloadRequested ->
    loadSharedUsers
  ShareSubmitRequested -> do
    st <- get
    let shareList = st ^. _calendarShareList
    case validateShareUsername shareList.usernameDraft of
      Left err ->
        modify_ (_calendarShareList %~ _ { submitError = Just err })
      Right username -> do
        modify_
          ( _calendarShareList
              %~
                _
                  { isAdding = true
                  , submitError = Nothing
                  }
          )
        result <- liftAff $ addSharedUserResponse (TripSharingUser { username })
        case result of
          Left _ ->
            modify_ (_calendarShareList %~ _ { isAdding = false, submitError = Just genericShareWriteErrorMessage })
          Right response ->
            if statusOk response then do
              modify_
                ( _calendarShareList
                    %~
                      _
                        { isAdding = false
                        , usernameDraft = ""
                        , submitError = Nothing
                        }
                )
              loadSharedUsers
            else
              modify_
                ( _calendarShareList
                    %~
                      _
                        { isAdding = false
                        , submitError = Just (shareWriteErrorMessage response)
                        }
                )
  ShareDeleteRequested username -> do
    modify_
      ( _calendarShareList
          %~
            \shareList -> shareList
              { deletingUsernames = markUsernameDeleting username shareList.deletingUsernames
              , submitError = Nothing
              }
      )
    result <- liftAff $ deleteSharedUserResponse username
    case result of
      Left _ ->
        modify_
          ( _calendarShareList
              %~
                \shareList -> shareList
                  { deletingUsernames = unmarkUsernameDeleting username shareList.deletingUsernames
                  , submitError = Just genericShareWriteErrorMessage
                  }
          )
      Right response ->
        if statusOk response then do
          modify_
            ( _calendarShareList
                %~
                  \shareList -> shareList
                    { deletingUsernames = unmarkUsernameDeleting username shareList.deletingUsernames
                    , submitError = Nothing
                    }
            )
          loadSharedUsers
        else
          modify_
            ( _calendarShareList
                %~
                  \shareList -> shareList
                    { deletingUsernames = unmarkUsernameDeleting username shareList.deletingUsernames
                    , submitError = Just genericShareWriteErrorMessage
                    }
            )

handleSubscriptionAction :: SubscriptionAction -> AgendaAppM Unit
handleSubscriptionAction = case _ of
  SubscriptionUsernameChanged raw ->
    modify_
      ( _calendarSubscriptionList
          %~
            _
              { usernameDraft = raw
              , submitError = Nothing
              }
      )
  SubscriptionReloadRequested ->
    loadSubscribedUsers
  SubscriptionSubmitRequested -> do
    st <- get
    let subscriptionList = st ^. _calendarSubscriptionList
    case validateShareUsername subscriptionList.usernameDraft of
      Left err ->
        modify_ (_calendarSubscriptionList %~ _ { submitError = Just err })
      Right username -> do
        modify_
          ( _calendarSubscriptionList
              %~
                _
                  { isAdding = true
                  , submitError = Nothing
                  }
          )
        result <- liftAff $ addSubscribedUserResponse (TripSharingUser { username })
        case result of
          Left _ ->
            modify_ (_calendarSubscriptionList %~ _ { isAdding = false, submitError = Just genericSubscriptionWriteErrorMessage })
          Right response ->
            if statusOk response then do
              modify_
                ( _calendarSubscriptionList
                    %~
                      _
                        { isAdding = false
                        , usernameDraft = ""
                        , submitError = Nothing
                        }
                )
              loadSubscribedUsers
            else
              modify_
                ( _calendarSubscriptionList
                    %~
                      _
                        { isAdding = false
                        , submitError = Just (subscriptionWriteErrorMessage response)
                        }
                )
  SubscriptionDeleteRequested username -> do
    modify_
      ( _calendarSubscriptionList
          %~
            \subscriptionList -> subscriptionList
              { deletingUsernames = markUsernameDeleting username subscriptionList.deletingUsernames
              , submitError = Nothing
              }
      )
    result <- liftAff $ deleteSubscribedUserResponse username
    case result of
      Left _ ->
        modify_
          ( _calendarSubscriptionList
              %~
                \subscriptionList -> subscriptionList
                  { deletingUsernames = unmarkUsernameDeleting username subscriptionList.deletingUsernames
                  , submitError = Just genericSubscriptionWriteErrorMessage
                  }
          )
      Right response ->
        if statusOk response then do
          modify_
            ( _calendarSubscriptionList
                %~
                  \subscriptionList -> subscriptionList
                    { deletingUsernames = unmarkUsernameDeleting username subscriptionList.deletingUsernames
                    , submitError = Nothing
                    }
            )
          loadSubscribedUsers
        else
          modify_
            ( _calendarSubscriptionList
                %~
                  \subscriptionList -> subscriptionList
                    { deletingUsernames = unmarkUsernameDeleting username subscriptionList.deletingUsernames
                    , submitError = Just genericSubscriptionWriteErrorMessage
                    }
            )

scheduleDayTimelineFocus :: ErrorAgendaAppM Unit
scheduleDayTimelineFocus = do
  st <- get
  let
    viewMode = st ^. (_view <<< _viewMode)
    focusDate = st ^. _viewFocusDatePage
    contextKey = "day:" <> focusDate
    currentContext = st ^. (_view <<< _viewDayFocusContext)
  case viewMode of
    ViewDay -> do
      when (currentContext /= Just contextKey) $
        modify_
          ( _view
              %~
                ( (_viewDayFocusContext .~ Just contextKey)
                    <<< (_viewDayFocusApplied .~ false)
                    <<< (_viewDayFocusUserScrolled .~ false)
                    <<< (_viewDayFocusIgnoreScroll .~ false)
                )
          )
      stReady <- get
      let
        alreadyApplied = stReady ^. (_view <<< _viewDayFocusApplied)
        userScrolled = stReady ^. (_view <<< _viewDayFocusUserScrolled)
      when (not alreadyApplied && not userScrolled) do
        liftAff $ delay (Milliseconds 0.0)
        stCurrent <- get
        let sameContext = stCurrent ^. (_view <<< _viewDayFocusContext) == Just contextKey
        when sameContext do
          focusItemRef <- lift $ H.getRef (wrap "day-focus-item")
          case focusItemRef of
            Nothing -> pure unit
            Just focusItem -> do
              modify_
                ( _view
                    %~
                      ( (_viewDayFocusIgnoreScroll .~ true)
                          <<< (_viewDayFocusApplied .~ true)
                      )
                )
              liftEffect $ scrollElementIntoView focusItem
              liftAff $ delay (Milliseconds 50.0)
              stAfter <- get
              when (stAfter ^. (_view <<< _viewDayFocusContext) == Just contextKey) $
                modify_ (_view <<< _viewDayFocusIgnoreScroll .~ false)
    _ -> pure unit

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

statusOk :: forall a. Response a -> Boolean
statusOk r = unwrap r.status >= 200 && unwrap r.status < 300

submitTask :: ErrorAgendaAppM Unit
submitTask = do
  st <- get
  case st ^. _calendarDraft of
    CreateTaskDraft draft ->
      case validateTask draft of
        Left err -> modify_ (_calendarValidationError .~ Just (validationErrorMessage err))
        Right validDraft ->
          case toNewTask validDraft of
            Left err ->
              modify_ (_calendarValidationError .~ Just err)
            Right item -> do
              response <- createItem item
              if statusOk response then do
                modify_
                  ( (_calendarDraft .~ emptyCreateDraft CreateTask)
                      <<< (_calendarValidationError .~ Nothing)
                      <<< (_calendarLastCreateMode .~ CreateTask)
                      <<< ((_view <<< _viewActiveModal) .~ Nothing)
                  )
                refreshItems
              else
                modify_ (_calendarValidationError .~ Just (createErrorMessage (unwrap response.status)))
    CreateTripDraft draft ->
      case validateTrip draft of
        Left err ->
          modify_ (_calendarValidationError .~ Just (tripValidationErrorMessage err))
        Right validDraft ->
          case toNewTrip validDraft of
            Left err ->
              modify_ (_calendarValidationError .~ Just err)
            Right item -> do
              response <- createItem item
              if statusOk response then do
                modify_
                  ( (_calendarDraft .~ emptyCreateDraft CreateTrip)
                      <<< (_calendarValidationError .~ Nothing)
                      <<< (_calendarLastCreateMode .~ CreateTrip)
                      <<< ((_view <<< _viewActiveModal) .~ Nothing)
                  )
                refreshItems
              else
                modify_ (_calendarValidationError .~ Just (tripWriteErrorMessage response))

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
render { calendar, sync, mouseDrag, view } =
  let
    { items, shareList, subscriptionList, sharedPresence, presenceCuePreferences } = calendar
    { updateError } = sync
    draggingId = mouseDrag.draggingId
    dragHoverIndex = mouseDrag.dragHoverIndex
    { viewMode, focusDate, todayDate, isMobile, promotedOverlaps, presenceInspection, presenceInspectionInitialTopPx, presenceInspectionPinned } = view
    agendaModalsInput = buildAgendaModalsInput { calendar, sync, mouseDrag, view }
    sortedItems = sortItems SortByTime items
  in
    div [ class_ "entity-page calendar-page" ]
      ( [ section [ class_ "calendar-header" ]
            [ h2 [ class_ "calendar-title" ] [ text (viewTitle viewMode) ]
            , div [ class_ "calendar-subtitle" ] [ text "Capture rapide des tâches à organiser." ]
            , toAction <$> renderViewSelector viewMode focusDate todayDate
            , toAction <$> renderMobileTools viewMode
            ]
        , div [ class_ $ "calendar-layout" <> guard (viewMode == ViewDay) " calendar-layout--calendar" ]
            [ div [ class_ "calendar-main" ]
                [ maybe (text "") renderUpdateError updateError
                , section [ class_ $ "calendar-list-panel" <> guard (viewMode == ViewDay) " calendar-list-panel--calendar" ]
                    [ renderAgendaView viewMode focusDate todayDate sortedItems sharedPresence presenceCuePreferences presenceInspection presenceInspectionInitialTopPx presenceInspectionPinned isMobile promotedOverlaps draggingId dragHoverIndex ]
                ]
            , div [ class_ "calendar-side" ]
                [ map toAction (renderShareManager sharePanelConfig shareList)
                , map toAction (renderSubscriptionManager subscriptionPanelConfig subscriptionList)
                ]
            ]
        ]
          <> [ renderAgendaModals agendaModalsInput ]
          <>
            [ renderCreateFab ]
      )

type AgendaModalsInput =
  { activeModal :: Maybe AgendaModal
  , overlapSheet :: Maybe OverlapSheet
  , sharedPresenceOverflowSheet :: Maybe SharedPresenceOverflowSheet
  , exportItems :: Array Export.Item
  , draft :: CreateDraft
  , tripPlaces :: TripPlacesState
  , shareList :: ShareListState
  , subscriptionList :: ShareListState
  , validationError :: Maybe String
  , editPanel :: Maybe EditPanel
  , presenceCuePreferences :: PresenceCuePreferencesState
  , presenceInspection :: Maybe SharedPresenceInspection
  , presenceInspectionPinned :: Boolean
  }

renderAgendaModals :: AgendaModalsInput -> H.ComponentHTML Action Slots Aff
renderAgendaModals { activeModal, overlapSheet, sharedPresenceOverflowSheet, exportItems, draft, tripPlaces, shareList, subscriptionList, validationError, editPanel, presenceCuePreferences, presenceInspection, presenceInspectionPinned } =
  let
    renderModal title content = Modal.renderModal title content (ViewAction ViewCloseModal) (ViewAction ViewCloseModal)
    renderExportModal items =
      slot (Proxy :: _ "export") ExportModal Export.component { items } absurd
    createValidateState =
      { action: SyncSubmitTask
      , disabled: isCreateSubmitDisabled draft tripPlaces
      }
    editValidateState panel =
      { action: ViewAction ViewEditSave
      , disabled: isEditSubmitDisabled panel tripPlaces
      }
  in
    maybe (div [] [])
      case _ of
        ModalTools -> renderModal "Actions" [ map toAction renderToolsContent ]
        ModalTripShares -> renderModal "Partage des trajets" [ map toAction (renderShareManager sharePanelConfig shareList) ]
        ModalTripSubscriptions -> renderModal "Abonnements trajets" [ map toAction (renderSubscriptionManager subscriptionPanelConfig subscriptionList) ]
        ModalCreateItem -> Modal.renderModalWithValidateState "Créer un item" [ renderCreateContent draft tripPlaces validationError ]
          (ViewAction ViewCloseCreate)
          createValidateState
        ModalImportCsv ->
          renderModal "Import CSV"
            [ slot (Proxy :: _ "importCsv") ImportCsv Import.component { mode: Import.Csv }
                (\(Import.ApplyItems items) -> ImportApplyItems (map toCalendarItemContent items))
            ]
        ModalImportIcs ->
          renderModal "Import ICS"
            [ slot (Proxy :: _ "importIcs") ImportIcs Import.component { mode: Import.Ics }
                (\(Import.ApplyItems items) -> ImportApplyItems (map toCalendarItemContent items))
            ]
        ModalExport -> renderModal "Export" [ renderExportModal exportItems ]
        ModalOverlapGroup -> case overlapSheet of
          Nothing -> text ""
          Just overlap ->
            Modal.renderBottomSheet "Elements superposes"
              [ renderOverlapSheetContent overlap ]
              (ViewAction ViewCloseModal)
        ModalPresenceInspection -> case presenceInspection of
          Nothing -> text ""
          Just inspection ->
            Modal.renderBottomSheet inspection.username
              [ renderPresenceInspectionContent presenceCuePreferences inspection presenceInspectionPinned ]
              (ViewAction ViewCloseModal)
        ModalSharedPresenceOverflow -> case sharedPresenceOverflowSheet of
          Nothing -> text ""
          Just overflowSheet ->
            Modal.renderBottomSheet "Autres partages"
              [ renderSharedPresenceOverflowContent overflowSheet ]
              (ViewAction ViewCloseModal)
        ModalEditItem -> case editPanel of
          Nothing -> text ""
          Just panel ->
            Modal.renderModalWithValidateState "Modifier l'item"
              [ renderEditContent panel tripPlaces ]
              (ViewAction ViewEditCancel)
              (editValidateState panel)
      activeModal

buildAgendaModalsInput :: State -> AgendaModalsInput
buildAgendaModalsInput { calendar, view } =
  let
    { items, draft, tripPlaces, shareList, subscriptionList, validationError, presenceCuePreferences } = calendar
    { activeModal, overlapSheet, sharedPresenceOverflowSheet, editPanel, presenceInspection, presenceInspectionPinned } = view
  in
    { activeModal
    , overlapSheet
    , sharedPresenceOverflowSheet
    , exportItems: mapMaybe toExportItem items
    , draft
    , tripPlaces
    , shareList
    , subscriptionList
    , validationError
    , editPanel
    , presenceCuePreferences
    , presenceInspection
    , presenceInspectionPinned
    }

-- END src/Pages/Calendar.purs

-- BEGIN src/Calendar/Calendar.purs

-- END src/Calendar/Calendar.purs

-- BEGIN src/Calendar/Calendar/Agenda/Day.purs
renderDayCalendar
  :: forall w
   . String
  -> String
  -> Array CalendarItem
  -> SharedPresenceLoadState
  -> PresenceCuePreferencesState
  -> Maybe SharedPresenceInspection
  -> Maybe Number
  -> Boolean
  -> Boolean
  -> Array MobileOverlapPromotion
  -> Maybe String
  -> Maybe Int
  -> HTML w Action
renderDayCalendar focusDate todayDate items sharedPresence presenceCuePreferences presenceInspection presenceInspectionInitialTopPx presenceInspectionPinned isMobile promotedOverlaps draggingId dragHoverIndex =
  let
    itemsForDate = filter (isItemOnDate focusDate) items
    sorted = sortItems SortByTime itemsForDate
    focusTargetIdentity = map calendarItemIdentity (uncons sorted <#> _.head)
    dragPreview = renderDayDragPreview isMobile draggingId dragHoverIndex itemsForDate
    showPresenceRail = shouldRenderSharedPresenceRail sharedPresence
    railView = case sharedPresence of
      SharedPresenceLoaded groups -> buildSharedPresenceRailView groups
      _ -> emptySharedPresenceRailView
    desktopInspection =
      if isMobile then
        Nothing
      else
        presenceInspection >>= resolvePresenceInspection railView.visibleLayouts
    timelineItems =
      if isMobile then
        map (renderMobileOverlapStack draggingId focusTargetIdentity) (applyMobileOverlapPromotions promotedOverlaps (buildMobileOverlapStacks sorted))
      else
        map (renderTimelineItem isMobile draggingId focusTargetIdentity) (buildTimelineLayout sorted)
    dayLabel = DateTime.formatCalendarDayDateLabelWithReference focusDate todayDate
  in
    if not (shouldRenderDayCalendarShell itemsForDate sharedPresence) then emptyAgenda
    else
      div [ class_ "calendar-calendar" ]
        [ div [ class_ "calendar-calendar-header" ]
            [ div [ class_ "calendar-calendar-title" ] [ text dayLabel ]
            , div [ class_ "calendar-calendar-count" ] [ text $ show (length itemsForDate) <> " items" ]
            ]
        , div
            [ class_ "calendar-calendar-body"
            , ref (wrap "day-timeline-scroll")
            , onScroll (const (ViewAction ViewTimelineScrolled))
            ]
            [ div [ class_ "calendar-calendar-hours" ]
                (map renderHourLabel (enumFromTo 0 23) <> [ renderHourLabelEnd ])
            , div
                [ class_ $ "calendar-calendar-grid" <> guard showPresenceRail " calendar-calendar-grid--with-presence"
                , onDragEnter (\ev -> DragAction { log: "DragEnterCalendar@calendar-grid", dragAction: DragOverCalendar ev })
                , onDragOver (\ev -> DragAction { log: "DragOverCalendar@calendar-grid", dragAction: DragOverCalendar ev })
                , onDrop (\ev -> DragAction { log: "DropOnCalendar@calendar-grid", dragAction: DropOnCalendar ev })
                ]
                [ div [ class_ "calendar-calendar-lines" ]
                    (map renderHourLine (enumFromTo 0 23))
                , maybe (text "") renderDropIndicator dragHoverIndex
                , maybe (text "") identity dragPreview
                , if showPresenceRail then renderSharedPresenceRail isMobile presenceCuePreferences presenceInspection railView else text ""
                , maybe (text "") (renderPresenceInspectionCard presenceCuePreferences presenceInspectionInitialTopPx presenceInspectionPinned) desktopInspection
                , div
                    [ class_ $
                        "calendar-calendar-items"
                          <> guard showPresenceRail " calendar-calendar-items--with-presence"
                          <> if draggingId == Nothing || isMobile then "" else " calendar-calendar-items--dragging"
                    , ref (wrap "day-calendar-grid")
                    , onTouchMove (\ev -> DragAction { log: "TouchMove@calendar-grid", dragAction: TouchDragMove ev })
                    , onTouchEnd (\ev -> DragAction { log: "TouchEnd@calendar-grid", dragAction: TouchDrop ev })
                    , onTouchCancel (const (DragAction { log: "TouchCancel@calendar-grid", dragAction: TouchDragCancel }))
                    ]
                    timelineItems
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

onMouseEnterAction
  :: forall r i
   . i
  -> IProp (onMouseEnter :: MouseEvent.MouseEvent | r) i
onMouseEnterAction action =
  handler' (EventType "mouseenter") (const (Just action))

onMouseLeaveAction
  :: forall r i
   . i
  -> IProp (onMouseLeave :: MouseEvent.MouseEvent | r) i
onMouseLeaveAction action =
  handler' (EventType "mouseleave") (const (Just action))

onFocusAction
  :: forall r i
   . i
  -> IProp (onFocus :: FocusEvent.FocusEvent | r) i
onFocusAction action =
  handler' (EventType "focus") (const (Just action))

onBlurAction
  :: forall r i
   . i
  -> IProp (onBlur :: FocusEvent.FocusEvent | r) i
onBlurAction action =
  handler' (EventType "blur") (const (Just action))

renderSharedPresenceRail :: forall w. Boolean -> PresenceCuePreferencesState -> Maybe SharedPresenceInspection -> SharedPresenceRailView -> HTML w Action
renderSharedPresenceRail isMobile presenceCuePreferences presenceInspection railView =
  let
    railClass =
      "calendar-presence-rail" <>
        guard isMobile " calendar-presence-rail--mobile"
  in
    div [ class_ railClass ]
      ( map (renderSharedPresenceSegment isMobile presenceCuePreferences presenceInspection) railView.visibleLayouts
          <> foldMap (\overflow -> [ renderSharedPresenceOverflowButton overflow ]) railView.overflow
      )

renderSharedPresenceSegment :: forall w. Boolean -> PresenceCuePreferencesState -> Maybe SharedPresenceInspection -> SharedPresenceSegmentLayout -> HTML w Action
renderSharedPresenceSegment _ presenceCuePreferences presenceInspection layout =
  let
    isActive = maybe false (\activeInspection -> isSamePresenceInspection activeInspection layout) presenceInspection
    inspection = toPresenceInspection layout
    toneClass = resolveSharedPresenceToneClass presenceCuePreferences layout
  in
    button
      [ class_ $
          "calendar-presence-rail__segment"
            <> " "
            <> sharedPresenceSegmentRailClass layout.state
            <> " "
            <> toneClass
            <> guard isActive " calendar-presence-rail__segment--active"
      , style $
          " --start:" <> show layout.startMin <> ";"
            <> " --duration:"
            <> show layout.duration
            <> ";"
            <> " --lane:"
            <> show layout.laneIndex
            <> ";"
            <> " --lanes:"
            <> show layout.laneCount
            <> ";"
      , attr (AttrName "type") "button"
      , attr (AttrName "aria-label") (presenceInspectionAriaLabel layout.username layout)
      , attr (AttrName "data-username") layout.username
      , attr (AttrName "data-segment-index") (show layout.segmentIndex)
      , onClick (const (ViewAction (ViewInspectPresence PresenceInspectTap inspection)))
      , onMouseEnterAction (ViewAction (ViewPresenceSegmentMouseEnter inspection))
      , onMouseLeaveAction (ViewAction (ViewPresenceSegmentMouseLeave inspection))
      , onFocusAction (ViewAction (ViewInspectPresence PresenceInspectFocus inspection))
      , onBlurAction (ViewAction ViewClearPresenceInspection)
      ]
      []

renderPresenceInspectionCard :: forall w. PresenceCuePreferencesState -> Maybe Number -> Boolean -> SharedPresenceInspection -> HTML w Action
renderPresenceInspectionCard presenceCuePreferences presenceInspectionInitialTopPx isPinned inspection =
  let
    computedTopStyle = case presenceInspectionInitialTopPx of
      Nothing -> ""
      Just topPx ->
        " --panel-top:"
          <> show topPx
          <> "px;"
          <> " --panel-translate-y: 0;"
  in
    div
      [ class_ "calendar-presence-inspection"
      , style $
          " --start:"
            <> show inspection.startMin
            <> ";"
            <> " --duration:"
            <> show inspection.duration
            <> ";"
            <> computedTopStyle
      , onMouseEnterAction (ViewAction ViewPresenceInspectionPanelMouseEnter)
      , onMouseLeaveAction (ViewAction ViewPresenceInspectionPanelMouseLeave)
      ]
      [ renderPresenceInspectionContent presenceCuePreferences inspection isPinned ]

renderPresenceInspectionContent :: forall w. PresenceCuePreferencesState -> SharedPresenceInspection -> Boolean -> HTML w Action
renderPresenceInspectionContent presenceCuePreferences inspection isPinned =
  div [ class_ "calendar-presence-inspection__content" ]
    [ div [ class_ "calendar-presence-inspection__header" ]
        [ div [ class_ "calendar-presence-inspection__title" ] [ text inspection.username ]
        , if isPinned then
            button
              [ class_ "calendar-presence-inspection__close"
              , attr (AttrName "type") "button"
              , attr (AttrName "aria-label") "Fermer l'inspection"
              , onClick (const (ViewAction ViewDismissPresenceInspection))
              ]
              [ text "Fermer" ]
          else
            text ""
        ]
    , div [ class_ "calendar-presence-inspection__state" ] [ text (presenceInspectionStateText inspection.state) ]
    , div [ class_ "calendar-presence-inspection__time" ] [ text (presenceInspectionTimeText inspection) ]
    , maybe (text "") (renderPresenceCueEditor presenceCuePreferences) (presenceCueTargetFromInspection inspection)
    ]

renderPresenceCueEditor :: forall w. PresenceCuePreferencesState -> SharedPresenceCueTarget -> HTML w Action
renderPresenceCueEditor presenceCuePreferences target =
  let
    currentColorToken =
      lookupPresenceCuePreference presenceCuePreferences.preferences target
    renderTokenButton token =
      let
        isActive = currentColorToken == Just token
      in
        button
          [ class_ $
              "calendar-presence-cue-editor__option "
                <> presenceCueEditorTokenClass token
                <> guard isActive " calendar-presence-cue-editor__option--active"
          , attr (AttrName "type") "button"
          , attr (AttrName "aria-label") ("Utiliser la couleur " <> presenceCueColorTokenLabel token)
          , attr (AttrName "data-cue-color-token") (presenceCueColorTokenValue token)
          , onClick (const (ViewAction (ViewSetPresenceCueColor target (Just token))))
          ]
          [ text (presenceCueColorTokenLabel token) ]
    defaultIsActive = currentColorToken == Nothing
  in
    div [ class_ "calendar-presence-cue-editor" ]
      [ div [ class_ "calendar-presence-cue-editor__label" ] [ text "Couleur du lieu" ]
      , div [ class_ "calendar-presence-cue-editor__options" ]
          ( [ button
                [ class_ $
                    "calendar-presence-cue-editor__option calendar-presence-cue-editor__option--default"
                      <> guard defaultIsActive " calendar-presence-cue-editor__option--active"
                , attr (AttrName "type") "button"
                , attr (AttrName "aria-label") "Revenir à la couleur par défaut"
                , attr (AttrName "data-cue-color-token") "default"
                , onClick (const (ViewAction (ViewSetPresenceCueColor target Nothing)))
                ]
                [ text "Par défaut" ]
            ]
              <> map renderTokenButton allPresenceCueColorTokens
          )
      ]

renderDayDragPreview
  :: forall w
   . Boolean
  -> Maybe String
  -> Maybe Int
  -> Array CalendarItem
  -> Maybe (HTML w Action)
renderDayDragPreview isMobile draggingId dragHoverIndex items =
  if not isMobile then
    Nothing
  else do
    itemId <- draggingId
    hoverIndex <- dragHoverIndex
    item <-
      find
        ( \entry -> case entry of
            ServerCalendarItem { id } -> id == itemId
            _ -> false
        )
        items
    let
      duration = durationMinutesBetween (calendarItemWindowStart item) (calendarItemWindowEnd item)
      inlineStyle =
        fold
          [ " --start:"
          , show (indexToMinutes hoverIndex)
          , ";"
          , " --duration:"
          , show duration
          , ";"
          ]
    pure
      ( div
          [ class_ "calendar-calendar-drag-preview"
          , style inlineStyle
          , attr (AttrName "aria-hidden") "true"
          ]
          [ div [ class_ "calendar-calendar-card calendar-calendar-card--drag-preview" ]
              [ renderTimelineCardContent item ]
          ]
      )

renderTimelineItem
  :: forall w
   . Boolean
  -> Maybe String
  -> Maybe String
  -> TimelineLayout
  -> HTML w Action
renderTimelineItem isMobile draggingId focusTargetIdentity layout =
  let
    draggingClass =
      case { draggingId, item: layout.item } of
        { draggingId: Just activeId, item: ServerCalendarItem { id } } | activeId == id ->
          " calendar-calendar-item--dragging"
        _ -> ""
    focusTargetProps =
      if focusTargetIdentity == Just (calendarItemIdentity layout.item) then
        [ ref (wrap "day-focus-item")
        , attr (AttrName "data-day-focus-item") "true"
        ]
      else
        []
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
    dragProps = dragCalendarHandlers isMobile layout.item
    touchProps = touchCalendarHandlers isMobile layout.item
    editProps = editHandlers isMobile draggingId layout.item
  in
    div
      ( [ class_ "calendar-calendar-item"
        , style inlineStyle
        ] <> focusTargetProps <> editProps
      )
      [ renderTimelineEditButton isMobile layout.item
      , div
          ([ class_ $ calendarItemTimelineCardClass layout.item <> draggingClass ] <> dragProps)
          [ div touchProps [ renderTimelineCardContent layout.item ] ]
      ]

renderMobileOverlapStack
  :: forall w
   . Maybe String
  -> Maybe String
  -> MobileOverlapStack
  -> HTML w Action
renderMobileOverlapStack draggingId focusTargetIdentity stack =
  let
    draggingClass =
      case { draggingId, item: stack.topItem } of
        { draggingId: Just activeId, item: ServerCalendarItem { id } } | activeId == id ->
          " calendar-calendar-item--dragging"
        _ -> ""
    focusTargetProps =
      if any (\item -> focusTargetIdentity == Just (calendarItemIdentity item)) stack.items then
        [ ref (wrap "day-focus-item")
        , attr (AttrName "data-day-focus-item") "true"
        ]
      else
        []
    inlineStyle =
      fold
        [ " --start:"
        , show stack.startMin
        , ";"
        , " --duration:"
        , show stack.duration
        , ";"
        , " --card-start:"
        , show (stack.topStartMin - stack.startMin)
        , ";"
        , " --card-duration:"
        , show stack.topDuration
        , ";"
        , " --stack-depth:"
        , show stack.hiddenCount
        , ";"
        ]
    dragProps = dragCalendarHandlers true stack.topItem
    touchProps = touchCalendarHandlers true stack.topItem
    overlapSheet =
      { groupKey: stack.groupKey
      , items: stack.items
      , topItem: stack.topItem
      , hiddenCount: stack.hiddenCount
      }
  in
    div
      ( [ class_ "calendar-calendar-stack"
        , style inlineStyle
        ] <> focusTargetProps
      )
      ( map (renderMobileOverlapShadow stack.hiddenCount stack.startMin) stack.hiddenCards
          <>
            [ div
                ([ class_ $ calendarItemTimelineCardClass stack.topItem <> " calendar-calendar-stack-top" <> draggingClass ] <> dragProps)
                [ div touchProps [ renderTimelineCardContent stack.topItem ]
                , renderOverlapSummaryButton overlapSheet
                ]
            ]
      )

renderMobileOverlapShadow :: forall w. Int -> Int -> MobileHiddenCard -> HTML w Action
renderMobileOverlapShadow shadowCount groupStartMin hiddenCard =
  let
    zIndex = shadowCount - hiddenCard.stackIndex + 1
  in
    div
      [ class_ "calendar-calendar-card calendar-calendar-card--shadow"
      , attr (AttrName "aria-hidden") "true"
      , style $
          " --stack-index:" <> show hiddenCard.stackIndex <> ";"
            <> " --card-start:"
            <> show (hiddenCard.startMin - groupStartMin)
            <> ";"
            <> " --card-duration:"
            <> show hiddenCard.duration
            <> ";"
            <> " z-index:"
            <> show zIndex
            <> ";"
      ]
      []

renderTimelineCardContent
  :: forall w
   . CalendarItem
  -> HTML w Action
renderTimelineCardContent item =
  div [ class_ "calendar-calendar-content" ]
    [ div [ class_ "calendar-calendar-meta" ]
        [ div [ class_ "calendar-calendar-item-time" ]
            [ text $ calendarItemTimelineTimeText item ]
        , div [ class_ "calendar-calendar-item-title" ] [ text (calendarItemPrimaryText item) ]
        , renderCategory (calendarItemCategory item)
        ]
    ]

renderOverlapSummaryButton :: forall w. OverlapSheet -> HTML w Action
renderOverlapSummaryButton overlapSheet =
  if overlapSheet.hiddenCount <= 0 then
    text ""
  else
    button
      [ class_ "calendar-calendar-stack-summary"
      , attr (AttrName "aria-label") $ "Afficher " <> show overlapSheet.hiddenCount <> " elements superposes"
      , onClick (const (ViewAction (ViewOpenOverlapSheet overlapSheet)))
      ]
      [ text $ "+" <> show overlapSheet.hiddenCount ]

renderOverlapSheetContent :: forall w. OverlapSheet -> HTML w Action
renderOverlapSheetContent overlapSheet =
  div [ class_ "calendar-overlap-sheet" ]
    [ div [ class_ "calendar-overlap-sheet__hint" ]
        [ text $ show (length overlapSheet.items) <> " elements sur ce creneau" ]
    , ul [ class_ "calendar-overlap-sheet__list" ]
        (map (renderOverlapSheetItem overlapSheet) overlapSheet.items)
    ]

renderOverlapSheetItem :: forall w. OverlapSheet -> CalendarItem -> HTML w Action
renderOverlapSheetItem overlapSheet item =
  let
    isCurrentTop = calendarItemIdentity item == calendarItemIdentity overlapSheet.topItem
    itemClasses =
      "calendar-overlap-sheet__item"
        <> guard isCurrentTop " calendar-overlap-sheet__item--active"
    itemProps =
      [ class_ itemClasses
      , disabled isCurrentTop
      ]
        <>
          if isCurrentTop then
            []
          else
            [ onClick (const (ViewAction (ViewPromoteOverlapItem overlapSheet item))) ]
  in
    li []
      [ button itemProps
          [ div [ class_ "calendar-overlap-sheet__time" ]
              [ text $ calendarItemTimelineTimeText item ]
          , div [ class_ "calendar-overlap-sheet__title" ] [ text (calendarItemPrimaryText item) ]
          , renderCategory (calendarItemCategory item)
          , if isCurrentTop then
              div [ class_ "calendar-overlap-sheet__state" ] [ text "Visible" ]
            else
              text ""
          ]
      ]

renderSharedPresenceOverflowButton :: forall w. SharedPresenceRailOverflow -> HTML w Action
renderSharedPresenceOverflowButton overflow =
  button
    [ class_ "calendar-presence-overflow"
    , attr (AttrName "type") "button"
    , attr (AttrName "aria-label") ("Afficher " <> show overflow.hiddenCount <> " autres utilisateurs partages")
    , attr (AttrName "data-hidden-count") (show overflow.hiddenCount)
    , onClick (const (ViewAction (ViewOpenSharedPresenceOverflow { hiddenGroups: overflow.hiddenGroups })))
    ]
    [ text ("+" <> show overflow.hiddenCount) ]

renderSharedPresenceOverflowContent :: forall w. SharedPresenceOverflowSheet -> HTML w Action
renderSharedPresenceOverflowContent overflowSheet =
  div [ class_ "calendar-presence-overflow-sheet" ]
    [ div [ class_ "calendar-presence-overflow-sheet__hint" ]
        [ text $ show (length overflowSheet.hiddenGroups) <> " utilisateurs masques dans le rail" ]
    , ul [ class_ "calendar-presence-overflow-sheet__list" ]
        (map renderSharedPresenceOverflowGroup overflowSheet.hiddenGroups)
    ]

renderSharedPresenceOverflowGroup :: forall w. SharedPresenceGroup -> HTML w Action
renderSharedPresenceOverflowGroup group =
  li [ class_ "calendar-presence-overflow-sheet__item" ]
    [ div [ class_ "calendar-presence-overflow-sheet__username" ] [ text group.username ]
    , div [ class_ "calendar-presence-overflow-sheet__segments" ]
        (map (renderSharedPresenceOverflowSegment group.username) group.segments)
    ]

renderSharedPresenceOverflowSegment :: forall w. String -> SharedPresenceSegment -> HTML w Action
renderSharedPresenceOverflowSegment username segment =
  let
    inspection =
      { username
      , segmentIndex: 0
      , laneIndex: 0
      , laneCount: 1
      , startMin: minuteOfDay segment.start
      , duration: durationMinutesBetween segment.start segment.end
      , state: segment.state
      }
  in
    div [ class_ "calendar-presence-overflow-sheet__segment" ]
      [ div [ class_ "calendar-presence-overflow-sheet__state" ] [ text (presenceInspectionStateText segment.state) ]
      , div [ class_ "calendar-presence-overflow-sheet__time" ] [ text (presenceInspectionTimeText inspection) ]
      ]

renderTimelineEditButton :: forall w. Boolean -> CalendarItem -> HTML w Action
renderTimelineEditButton isMobile item =
  if isMobile || not (calendarItemSupportsEdit item) then text ""
  else
    button
      [ class_ "btn btn-sm btn-outline-secondary calendar-edit calendar-edit--timeline"
      , attr (AttrName "aria-label") "Editer"
      , onMouseDown (const (ViewAction (ViewOpenEdit item)))
      , onClick (const (ViewAction (ViewOpenEdit item)))
      ]
      [ i [ class_ "bi bi-pencil" ] [] ]

-- END src/Calendar/Calendar/Agenda/Day.purs

agendaList
  :: forall w
   . Array CalendarItem
  -> Boolean
  -> HTML w Action
agendaList items isMobile =
  ul [ class_ "list-group entity-list calendar-list" ] (mapWithIndex (renderItem isMobile) items)

renderItem
  :: forall w
   . Boolean
  -> Int
  -> CalendarItem
  -> HTML w Action
renderItem isMobile _ item =
  let
    editProps = editHandlers isMobile Nothing item
  in
    li ([ class_ (calendarItemCardClass item) ] <> editProps)
      [ div [ class_ "col entity-card-body" ]
          [ div [ class_ "calendar-card-time" ] [ text (calendarItemListTimeText item) ]
          , div [ class_ "calendar-card-title" ] [ text (calendarItemPrimaryText item) ]
          , div [ class_ "calendar-card-window" ] [ text (calendarItemSecondaryText item) ]
          , renderCategory (calendarItemCategory item)
          ]
      ]

editHandlers
  :: forall r
   . Boolean
  -> Maybe String
  -> CalendarItem
  -> Array
       ( IProp
           ( onDoubleClick :: MouseEvent.MouseEvent
           | r
           )
           Action
       )
editHandlers _ _ _ = []

renderCategory :: forall w action. Maybe String -> HTML w action
renderCategory category =
  case category of
    Nothing -> text ""
    Just value -> div [ class_ "calendar-card-category" ] [ text value ]

emptyAgenda :: forall w action. HTML w action
emptyAgenda =
  div [ class_ "row entity-empty calendar-empty" ]
    [ div [ class_ "entity-empty-title" ] [ text "Aucune tâche aujourd'hui" ]
    , div [ class_ "entity-empty-subtitle" ] [ text "Ajoutez une tâche pour commencer à organiser votre journée." ]
    , div [ class_ "calendar-empty-cta" ]
        [ span [ class_ "badge rounded-pill text-bg-primary" ] [ text "Astuce" ]
        , span [ class_ "text-muted" ] [ text "Commencez par un titre puis appuyez sur Entrée." ]
        ]
    ]

-- END src/Calendar/Calendar/Agenda/List.purs

renderRangeView
  :: forall w
   . String
  -> Array String
  -> Array CalendarItem
  -> Boolean
  -> HTML w Action
renderRangeView label dates items isMobile =
  if null dates then emptyAgendaRange label
  else
    div [ class_ "calendar-range" ]
      (map (renderDateSection label items isMobile) dates)

renderDateSection
  :: forall w
   . String
  -> Array CalendarItem
  -> Boolean
  -> String
  -> HTML w Action
renderDateSection _ items isMobile dateStr =
  let
    itemsForDate = filter (isItemOnDate dateStr) items
    sorted = sortItems SortByTime itemsForDate
  in
    section [ class_ "calendar-date-section" ]
      [ div [ class_ "calendar-date-title" ] [ text dateStr ]
      , if null sorted then div [ class_ "calendar-date-empty" ] [ text "Aucun item" ]
        else agendaList sorted isMobile
      ]

emptyAgendaRange :: forall w action. String -> HTML w action
emptyAgendaRange label =
  div [ class_ "row entity-empty calendar-empty" ]
    [ div [ class_ "entity-empty-title" ] [ text $ "Aucun item sur la " <> label ]
    , div [ class_ "entity-empty-subtitle" ] [ text "Ajoutez une tâche pour commencer." ]
    ]

-- END src/Calendar/Calendar/Agenda/Range.purs

-- BEGIN src/Calendar/Calendar/CreateForm.purs
data CreateFormAction
  = CreateFormDraftTitleChanged String
  | CreateFormDraftModeChanged String
  | CreateFormDraftStartChanged String
  | CreateFormDraftEndChanged String
  | CreateFormDraftCategoryChanged String
  | CreateFormDraftStatusChanged String
  | CreateFormDraftDurationChanged String
  | CreateFormDraftDeparturePlaceChanged String
  | CreateFormDraftArrivalPlaceChanged String

applyCreateFormAction :: CreateFormAction -> AgendaAppM Unit
applyCreateFormAction action =
  case action of
    CreateFormDraftTitleChanged title ->
      modify_
        ( _calendar
            %~
              ( \calendarState ->
                  case calendarState.draft of
                    CreateTaskDraft draft ->
                      calendarState { draft = CreateTaskDraft (draft { title = title }), validationError = Nothing }
                    CreateTripDraft _ ->
                      calendarState
              )
        )
    CreateFormDraftModeChanged rawMode -> do
      let mode = parseCreateItemMode rawMode
      modify_ (_calendar %~ ((_draft .~ emptyCreateDraft mode) <<< (_lastCreateMode .~ mode) <<< (_validationError .~ Nothing)))
      st <- get
      let updatedDraft = prefillCreateDraft mode (st ^. _viewFocusDatePage) (st ^. _calendarDraft)
      modify_ (_calendarDraft .~ updatedDraft)
    CreateFormDraftStartChanged windowStart ->
      modify_
        ( _calendar
            %~
              ( \calendarState ->
                  calendarState
                    { draft = mapCreateDraftWindowStart windowStart calendarState.draft
                    , validationError = Nothing
                    }
              )
        )
    CreateFormDraftEndChanged windowEnd ->
      modify_
        ( _calendar
            %~
              ( \calendarState ->
                  calendarState
                    { draft = mapCreateDraftWindowEnd windowEnd calendarState.draft
                    , validationError = Nothing
                    }
              )
        )
    CreateFormDraftCategoryChanged category ->
      modify_
        ( _calendar
            %~
              ( \calendarState ->
                  case calendarState.draft of
                    CreateTaskDraft draft ->
                      calendarState { draft = CreateTaskDraft (draft { category = category }), validationError = Nothing }
                    CreateTripDraft _ ->
                      calendarState
              )
        )
    CreateFormDraftStatusChanged raw ->
      modify_
        ( _calendar
            %~
              ( \calendarState ->
                  case calendarState.draft of
                    CreateTaskDraft draft ->
                      calendarState { draft = CreateTaskDraft (draft { status = parseStatus raw }), validationError = Nothing }
                    CreateTripDraft _ ->
                      calendarState
              )
        )
    CreateFormDraftDurationChanged raw ->
      modify_
        ( _calendar
            %~
              ( \calendarState ->
                  case calendarState.draft of
                    CreateTaskDraft draft ->
                      calendarState { draft = CreateTaskDraft (draft { actualDurationMinutes = raw }), validationError = Nothing }
                    CreateTripDraft _ ->
                      calendarState
              )
        )
    CreateFormDraftDeparturePlaceChanged departurePlaceId ->
      modify_
        ( _calendar
            %~
              ( \calendarState ->
                  case calendarState.draft of
                    CreateTripDraft draft ->
                      calendarState { draft = CreateTripDraft (draft { departurePlaceId = departurePlaceId }), validationError = Nothing }
                    CreateTaskDraft _ ->
                      calendarState
              )
        )
    CreateFormDraftArrivalPlaceChanged arrivalPlaceId ->
      modify_
        ( _calendar
            %~
              ( \calendarState ->
                  case calendarState.draft of
                    CreateTripDraft draft ->
                      calendarState { draft = CreateTripDraft (draft { arrivalPlaceId = arrivalPlaceId }), validationError = Nothing }
                    CreateTaskDraft _ ->
                      calendarState
              )
        )

renderCreateContent
  :: CreateDraft
  -> TripPlacesState
  -> Maybe String
  -> H.ComponentHTML Action Slots Aff
renderCreateContent draft tripPlaces validationError =
  case draft of
    CreateTaskDraft taskDraft ->
      div [ class_ "calendar-modal-stack" ]
        [ modeField CreateTask
        , field "Titre" (taskTitleInput taskDraft)
        , dateTimeField "Début" CreateFormDraftStartChanged taskDraft.windowStart
        , dateTimeField "Fin" CreateFormDraftEndChanged taskDraft.windowEnd
        , field "Catégorie" (taskCategoryInput taskDraft)
        , field "Statut" (taskStatusInput taskDraft)
        , field "Durée réelle (minutes)" (taskDurationInput taskDraft)
        , renderRecurrenceSlot RecurrenceCreate taskDraft.recurrence CreateRecurrenceCmd
        , errorInput validationError
        ]
    CreateTripDraft tripDraft ->
      div [ class_ "calendar-modal-stack" ]
        [ modeField CreateTrip
        , renderTripPlacesField "Départ" CreateFormDraftDeparturePlaceChanged tripDraft.departurePlaceId tripPlaces
        , renderTripPlacesField "Arrivée" CreateFormDraftArrivalPlaceChanged tripDraft.arrivalPlaceId tripPlaces
        , dateTimeField "Départ" CreateFormDraftStartChanged tripDraft.windowStart
        , dateTimeField "Arrivée" CreateFormDraftEndChanged tripDraft.windowEnd
        , renderTripPlacesFeedback tripPlaces
        , errorInput validationError
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

  modeField currentMode =
    field "Type" $
      select
        [ class_ "form-select calendar-input"
        , onValueChange (CreateFormAction <<< CreateFormDraftModeChanged)
        , value (createItemModeValue currentMode)
        ]
        [ option [ value "task" ] [ text "Tâche" ]
        , option [ value "trip" ] [ text "Trajet" ]
        ]

  taskTitleInput taskDraft =
    input
      [ class_ "form-control calendar-input"
      , placeholder "Titre"
      , onValueChange (CreateFormAction <<< CreateFormDraftTitleChanged)
      , onKeyDown (\ev -> SyncDraftTitleKeyDown (KE.key ev))
      , value taskDraft.title
      ]

  taskCategoryInput taskDraft =
    input
      [ class_ "form-control calendar-input"
      , placeholder "Catégorie"
      , onValueChange (CreateFormAction <<< CreateFormDraftCategoryChanged)
      , value taskDraft.category
      ]

  taskStatusInput taskDraft =
    select
      [ class_ "form-select calendar-input"
      , onValueChange (CreateFormAction <<< CreateFormDraftStatusChanged)
      , value (statusValue taskDraft.status)
      ]
      [ option [ value "todo" ] [ text "À faire" ]
      , option [ value "in_progress" ] [ text "En cours" ]
      , option [ value "done" ] [ text "Terminé" ]
      , option [ value "canceled" ] [ text "Annulé" ]
      ]

  taskDurationInput taskDraft =
    input
      [ class_ "form-control calendar-input"
      , type_ InputNumber
      , placeholder "Ex: 30"
      , onValueChange (CreateFormAction <<< CreateFormDraftDurationChanged)
      , value taskDraft.actualDurationMinutes
      ]

  errorInput message =
    maybe (text "") (\msg -> div [ class_ "calendar-error" ] [ text msg ]) message

-- END src/Calendar/Calendar/CreateForm.purs

-- BEGIN src/Calendar/Calendar/Draft.purs
toNewTask :: TaskDraft -> Either String CalendarItem
toNewTask draft = do
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
              TaskCalendarItemContent
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

toNewTrip :: TripDraft -> Either String CalendarItem
toNewTrip draft = do
  validDraft <- lmap tripValidationErrorMessage (validateTrip draft)
  windowStart <- maybe (Left "La date de départ est invalide.") Right (parseDateTimeLocal validDraft.windowStart)
  windowEnd <- maybe (Left "La date d'arrivée est invalide.") Right (parseDateTimeLocal validDraft.windowEnd)
  pure
    ( NewCalendarItem
        { content:
            TripCalendarItemContent
              { windowStart
              , windowEnd
              , departurePlaceId: validDraft.departurePlaceId
              , arrivalPlaceId: validDraft.arrivalPlaceId
              }
        }
    )

-- END src/Calendar/Calendar/Draft.purs

-- END src/Calendar/Calendar/Primary.purs

-- BEGIN src/Calendar/Calendar/State.purs
type CalendarState =
  { items :: Array CalendarItem
  , draft :: CreateDraft
  , validationError :: Maybe String
  , lastCreateMode :: CreateItemMode
  , tripPlaces :: TripPlacesState
  , shareList :: ShareListState
  , subscriptionList :: ShareListState
  , sharedPresence :: SharedPresenceLoadState
  , presenceCuePreferences :: PresenceCuePreferencesState
  }

calendarInitialState :: CalendarState
calendarInitialState =
  { items: []
  , draft: emptyCreateDraft CreateTask
  , validationError: Nothing
  , lastCreateMode: CreateTask
  , tripPlaces: TripPlacesLoading
  , shareList: shareListInitialState
  , subscriptionList: shareListInitialState
  , sharedPresence: SharedPresenceLoading
  , presenceCuePreferences: presenceCuePreferencesInitialState
  }

shareListInitialState :: ShareListState
shareListInitialState =
  { usernames: []
  , hasLoaded: false
  , isLoading: true
  , loadError: Nothing
  , usernameDraft: ""
  , submitError: Nothing
  , isAdding: false
  , deletingUsernames: []
  }

emptyTaskDraft :: TaskDraft
emptyTaskDraft =
  { itemType: Task
  , title: ""
  , windowStart: ""
  , windowEnd: ""
  , category: ""
  , status: Todo
  , actualDurationMinutes: ""
  , recurrence: defaultRecurrenceDraft
  }

emptyTripDraft :: TripDraft
emptyTripDraft =
  { departurePlaceId: ""
  , arrivalPlaceId: ""
  , windowStart: ""
  , windowEnd: ""
  }

emptyCreateDraft :: CreateItemMode -> CreateDraft
emptyCreateDraft = case _ of
  CreateTask -> CreateTaskDraft emptyTaskDraft
  CreateTrip -> CreateTripDraft emptyTripDraft

_items :: Lens' CalendarState (Array CalendarItem)
_items = prop (Proxy :: _ "items")

_draft :: Lens' CalendarState CreateDraft
_draft = prop (Proxy :: _ "draft")

_validationError :: Lens' CalendarState (Maybe String)
_validationError = prop (Proxy :: _ "validationError")

_lastCreateMode :: Lens' CalendarState CreateItemMode
_lastCreateMode = prop (Proxy :: _ "lastCreateMode")

_tripPlaces :: Lens' CalendarState TripPlacesState
_tripPlaces = prop (Proxy :: _ "tripPlaces")

_shareList :: Lens' CalendarState ShareListState
_shareList = prop (Proxy :: _ "shareList")

_subscriptionList :: Lens' CalendarState ShareListState
_subscriptionList = prop (Proxy :: _ "subscriptionList")

_sharedPresence :: Lens' CalendarState SharedPresenceLoadState
_sharedPresence = prop (Proxy :: _ "sharedPresence")

_presenceCuePreferences :: Lens' CalendarState PresenceCuePreferencesState
_presenceCuePreferences = prop (Proxy :: _ "presenceCuePreferences")

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

type MobileOverlapStack =
  { groupKey :: String
  , items :: Array CalendarItem
  , topItem :: CalendarItem
  , topStartMin :: Int
  , topDuration :: Int
  , hiddenCards :: Array MobileHiddenCard
  , startMin :: Int
  , duration :: Int
  , hiddenCount :: Int
  }

type MobileHiddenCard =
  { item :: CalendarItem
  , startMin :: Int
  , duration :: Int
  , stackIndex :: Int
  }

type MobileOverlapPromotion =
  { groupKey :: String
  , itemIdentity :: String
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

buildMobileOverlapStacks :: Array CalendarItem -> Array MobileOverlapStack
buildMobileOverlapStacks items =
  let
    blocks = sortBy compareStart (mapMaybe toTimelineBlock items)
    groups = groupTimelineBlocks blocks
  in
    mapMaybe toMobileStack groups
  where
  compareStart a b = compare a.startMin b.startMin

applyMobileOverlapPromotions :: Array MobileOverlapPromotion -> Array MobileOverlapStack -> Array MobileOverlapStack
applyMobileOverlapPromotions promotions =
  map (applyMobileOverlapPromotion promotions)

applyMobileOverlapPromotion :: Array MobileOverlapPromotion -> MobileOverlapStack -> MobileOverlapStack
applyMobileOverlapPromotion promotions stack =
  case find (\promotion -> promotion.groupKey == stack.groupKey) promotions of
    Nothing -> stack
    Just promotion -> fromMaybe stack (promoteMobileOverlapStack promotion.itemIdentity stack)

promoteMobileOverlapStack :: String -> MobileOverlapStack -> Maybe MobileOverlapStack
promoteMobileOverlapStack promotedIdentity stack =
  map (buildMobileOverlapStackFromBlocks sorted) (find (\block -> calendarItemIdentity block.item == promotedIdentity) sorted)
  where
  sorted = sortBy compareMobilePriority (mapMaybe toTimelineBlock stack.items)

toMobileStack :: Array TimelineBlock -> Maybe MobileOverlapStack
toMobileStack group =
  let
    sorted = sortBy compareMobilePriority group
  in
    case uncons sorted of
      Nothing -> Nothing
      Just { head } ->
        Just (buildMobileOverlapStackFromBlocks sorted head)

buildMobileOverlapStackFromBlocks :: Array TimelineBlock -> TimelineBlock -> MobileOverlapStack
buildMobileOverlapStackFromBlocks sorted topBlock =
  let
    items = map _.item sorted
    hiddenBlocks = filter (\block -> calendarItemIdentity block.item /= calendarItemIdentity topBlock.item) sorted
    groupStartMin = foldl (\acc block -> min acc block.startMin) topBlock.startMin hiddenBlocks
    groupEndMin = foldl (\acc block -> max acc block.endMin) topBlock.endMin hiddenBlocks
    hiddenCards =
      mapWithIndex
        ( \index block ->
            { item: block.item
            , startMin: block.startMin
            , duration: max 1 (block.endMin - block.startMin)
            , stackIndex: index + 1
            }
        )
        hiddenBlocks
  in
    { groupKey: overlapGroupKey items
    , items
    , topItem: topBlock.item
    , topStartMin: topBlock.startMin
    , topDuration: max 1 (topBlock.endMin - topBlock.startMin)
    , hiddenCards
    , startMin: groupStartMin
    , duration: max 1 (groupEndMin - groupStartMin)
    , hiddenCount: length hiddenBlocks
    }

compareMobilePriority :: TimelineBlock -> TimelineBlock -> Ordering
compareMobilePriority a b =
  compare a.startMin b.startMin
    <> compare a.endMin b.endMin
    <> compare (calendarItemIdentity a.item) (calendarItemIdentity b.item)

overlapGroupKey :: Array CalendarItem -> String
overlapGroupKey items =
  StringCommon.joinWith "|"
    (map calendarItemIdentity items)

upsertMobileOverlapPromotion :: MobileOverlapPromotion -> Array MobileOverlapPromotion -> Array MobileOverlapPromotion
upsertMobileOverlapPromotion promotion promotions =
  [ promotion ] <> filter (\entry -> entry.groupKey /= promotion.groupKey) promotions

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
    startMin = minuteOfDay (calendarItemWindowStart item)
    endMinRaw = minuteOfDay (calendarItemWindowEnd item)
    startClamped = clamp 0 1439 startMin
    endAdjusted = if endMinRaw <= startMin then 1440 else endMinRaw
    endClamped = clamp (startClamped + 1) 1440 endAdjusted
  in
    if endClamped <= 0 || startClamped >= 1440 then Nothing
    else Just { item, startMin: startClamped, endMin: endClamped }

calendarItemIdentity :: CalendarItem -> String
calendarItemIdentity = case _ of
  ServerCalendarItem { id } -> "server:" <> id
  NewCalendarItem { content } ->
    let
      identityLabel =
        case content of
          TaskCalendarItemContent taskContent -> taskContent.title
          TripCalendarItemContent tripContent -> tripRouteLabel tripContent
      identityStart =
        case content of
          TaskCalendarItemContent taskContent -> taskContent.windowStart
          TripCalendarItemContent tripContent -> tripContent.windowStart
      identityEnd =
        case content of
          TaskCalendarItemContent taskContent -> taskContent.windowEnd
          TripCalendarItemContent tripContent -> tripContent.windowEnd
    in
      fold
        [ "new:"
        , identityLabel
        , ":"
        , show identityStart
        , ":"
        , show identityEnd
        ]

-- END src/Calendar/Calendar/Timeline.purs

-- END src/Calendar/Calendar/Types.purs

-- BEGIN src/Calendar/Conflict.purs
type ConflictBlock =
  { id :: String
  , start :: DateTime
  , end :: DateTime
  }

-- END src/Calendar/Conflict.purs

-- BEGIN src/Calendar/Display.purs
data AgendaModal
  = ModalImportCsv
  | ModalImportIcs
  | ModalExport
  | ModalTools
  | ModalTripShares
  | ModalTripSubscriptions
  | ModalCreateItem
  | ModalEditItem
  | ModalOverlapGroup
  | ModalPresenceInspection
  | ModalSharedPresenceOverflow

derive instance eqAgendaModal :: Eq AgendaModal

type OverlapSheet =
  { groupKey :: String
  , items :: Array CalendarItem
  , topItem :: CalendarItem
  , hiddenCount :: Int
  }

type SharedPresenceOverflowSheet =
  { hiddenGroups :: SharedPresence
  }

type SharedPresenceInspection =
  { username :: String
  , segmentIndex :: Int
  , laneIndex :: Int
  , laneCount :: Int
  , startMin :: Int
  , duration :: Int
  , state :: SharedPresenceState
  }

type SharedPresenceInspectionHoverSegment =
  { username :: String
  , segmentIndex :: Int
  }

type ViewState =
  { viewMode :: CalendarView
  , focusDate :: String
  , todayDate :: String
  , activeModal :: Maybe AgendaModal
  , overlapSheet :: Maybe OverlapSheet
  , sharedPresenceOverflowSheet :: Maybe SharedPresenceOverflowSheet
  , presenceInspection :: Maybe SharedPresenceInspection
  , presenceInspectionPinned :: Boolean
  , presenceInspectionInitialTopPx :: Maybe Number
  , presenceInspectionHoverSegment :: Maybe SharedPresenceInspectionHoverSegment
  , presenceInspectionHoverPanel :: Boolean
  , presenceInspectionCloseToken :: Int
  , promotedOverlaps :: Array MobileOverlapPromotion
  , editPanel :: Maybe EditPanel
  , isMobile :: Boolean
  , dayFocusContext :: Maybe String
  , dayFocusApplied :: Boolean
  , dayFocusUserScrolled :: Boolean
  , dayFocusIgnoreScroll :: Boolean
  }

viewInitialState :: ViewState
viewInitialState =
  { viewMode: ViewDay
  , focusDate: ""
  , todayDate: ""
  , activeModal: Nothing
  , overlapSheet: Nothing
  , sharedPresenceOverflowSheet: Nothing
  , presenceInspection: Nothing
  , presenceInspectionPinned: false
  , presenceInspectionInitialTopPx: Nothing
  , presenceInspectionHoverSegment: Nothing
  , presenceInspectionHoverPanel: false
  , presenceInspectionCloseToken: 0
  , promotedOverlaps: []
  , editPanel: Nothing
  , isMobile: false
  , dayFocusContext: Nothing
  , dayFocusApplied: false
  , dayFocusUserScrolled: false
  , dayFocusIgnoreScroll: false
  }

_viewMode :: Lens' ViewState CalendarView
_viewMode = lens
  _.viewMode
  (_ { viewMode = _ })

_viewFocusDateState :: Lens' ViewState String
_viewFocusDateState =
  lens
    _.focusDate
    (_ { focusDate = _ })

_viewTodayDateState :: Lens' ViewState String
_viewTodayDateState =
  lens
    _.todayDate
    (_ { todayDate = _ })

_viewActiveModal :: Lens' ViewState (Maybe AgendaModal)
_viewActiveModal =
  lens
    _.activeModal
    (_ { activeModal = _ })

_viewOverlapSheet :: Lens' ViewState (Maybe OverlapSheet)
_viewOverlapSheet =
  lens
    _.overlapSheet
    (_ { overlapSheet = _ })

_viewSharedPresenceOverflowSheet :: Lens' ViewState (Maybe SharedPresenceOverflowSheet)
_viewSharedPresenceOverflowSheet =
  lens
    _.sharedPresenceOverflowSheet
    (_ { sharedPresenceOverflowSheet = _ })

_viewPresenceInspection :: Lens' ViewState (Maybe SharedPresenceInspection)
_viewPresenceInspection =
  lens
    _.presenceInspection
    (_ { presenceInspection = _ })

_viewPresenceInspectionPinned :: Lens' ViewState Boolean
_viewPresenceInspectionPinned =
  lens
    _.presenceInspectionPinned
    (_ { presenceInspectionPinned = _ })

_viewPresenceInspectionInitialTopPx :: Lens' ViewState (Maybe Number)
_viewPresenceInspectionInitialTopPx =
  lens
    _.presenceInspectionInitialTopPx
    (_ { presenceInspectionInitialTopPx = _ })

_viewPresenceInspectionHoverSegment :: Lens' ViewState (Maybe SharedPresenceInspectionHoverSegment)
_viewPresenceInspectionHoverSegment =
  lens
    _.presenceInspectionHoverSegment
    (_ { presenceInspectionHoverSegment = _ })

_viewPresenceInspectionHoverPanel :: Lens' ViewState Boolean
_viewPresenceInspectionHoverPanel =
  lens
    _.presenceInspectionHoverPanel
    (_ { presenceInspectionHoverPanel = _ })

_viewPresenceInspectionCloseToken :: Lens' ViewState Int
_viewPresenceInspectionCloseToken =
  lens
    _.presenceInspectionCloseToken
    (_ { presenceInspectionCloseToken = _ })

_viewPromotedOverlaps :: Lens' ViewState (Array MobileOverlapPromotion)
_viewPromotedOverlaps =
  lens
    _.promotedOverlaps
    (_ { promotedOverlaps = _ })

_viewEditPanel :: Lens' ViewState (Maybe EditPanel)
_viewEditPanel =
  lens
    _.editPanel
    (_ { editPanel = _ })

_viewIsMobile :: Lens' ViewState Boolean
_viewIsMobile =
  lens
    _.isMobile
    (_ { isMobile = _ })

_viewDayFocusContext :: Lens' ViewState (Maybe String)
_viewDayFocusContext =
  lens
    _.dayFocusContext
    (_ { dayFocusContext = _ })

_viewDayFocusApplied :: Lens' ViewState Boolean
_viewDayFocusApplied =
  lens
    _.dayFocusApplied
    (_ { dayFocusApplied = _ })

_viewDayFocusUserScrolled :: Lens' ViewState Boolean
_viewDayFocusUserScrolled =
  lens
    _.dayFocusUserScrolled
    (_ { dayFocusUserScrolled = _ })

_viewDayFocusIgnoreScroll :: Lens' ViewState Boolean
_viewDayFocusIgnoreScroll =
  lens
    _.dayFocusIgnoreScroll
    (_ { dayFocusIgnoreScroll = _ })

type EditPanel =
  { item :: CalendarItem
  , draft :: EditDraft
  , validationError :: Maybe String
  }

data ViewAction
  = ViewOpenDayDatePicker
  | ViewChangedAction String
  | ViewFocusDateChanged String
  | ViewTimelineScrolled
  | ViewOpenModal AgendaModal
  | ViewOpenOverlapSheet OverlapSheet
  | ViewPromoteOverlapItem OverlapSheet CalendarItem
  | ViewOpenSharedPresenceOverflow SharedPresenceOverflowSheet
  | ViewPresenceSegmentMouseEnter SharedPresenceInspection
  | ViewPresenceSegmentMouseLeave SharedPresenceInspection
  | ViewPresenceInspectionPanelMouseEnter
  | ViewPresenceInspectionPanelMouseLeave
  | ViewInspectPresence SharedPresenceInspectionTrigger SharedPresenceInspection
  | ViewClearPresenceInspection
  | ViewDismissPresenceInspection
  | ViewSetPresenceCueColor SharedPresenceCueTarget (Maybe PresenceCueColorToken)
  | ViewCloseModal
  | ViewOpenCreate
  | ViewCloseCreate
  | ViewOpenEdit CalendarItem
  | ViewOpenEditFromDoubleClick CalendarItem
  | ViewEditTitleChanged String
  | ViewEditStartChanged String
  | ViewEditEndChanged String
  | ViewEditCategoryChanged String
  | ViewEditStatusChanged String
  | ViewEditDurationChanged String
  | ViewEditDeparturePlaceChanged String
  | ViewEditArrivalPlaceChanged String
  | ViewEditSave
  | ViewEditCancel
  | ViewSetIsMobile Boolean

data SharedPresenceInspectionTrigger
  = PresenceInspectHover
  | PresenceInspectFocus
  | PresenceInspectTap

handleViewAction :: ViewAction -> ErrorAgendaAppM Unit
handleViewAction = case _ of
  ViewOpenDayDatePicker -> do
    ref <- lift $ H.getRef (wrap "day-view-date-input")
    case ref of
      Nothing -> pure unit
      Just elem -> liftEffect $ void (openDateInputPicker elem)
  ViewChangedAction raw ->
    let
      nextView = parseAgendaView raw
      resetPromotions =
        if nextView == ViewDay then
          identity
        else
          (_viewPromotedOverlaps .~ [])
    in
      modify_
        ( _view
            %~
              ( (_viewMode .~ nextView)
                  <<< resetPromotions
                  <<< (_viewPresenceInspection .~ Nothing)
                  <<< (_viewPresenceInspectionPinned .~ false)
                  <<< clearPresenceInspectionHoverState
                  <<< (_viewSharedPresenceOverflowSheet .~ Nothing)
                  <<< (_viewDayFocusContext .~ Nothing)
                  <<< (_viewDayFocusApplied .~ false)
                  <<< (_viewDayFocusUserScrolled .~ false)
                  <<< (_viewDayFocusIgnoreScroll .~ false)
              )
        )
  ViewFocusDateChanged raw ->
    modify_
      ( _view
          %~
            ( (_viewFocusDateState .~ raw)
                <<< (_viewPresenceInspection .~ Nothing)
                <<< (_viewPresenceInspectionPinned .~ false)
                <<< clearPresenceInspectionHoverState
                <<< (_viewSharedPresenceOverflowSheet .~ Nothing)
                <<< (_viewDayFocusContext .~ Nothing)
                <<< (_viewDayFocusApplied .~ false)
                <<< (_viewDayFocusUserScrolled .~ false)
                <<< (_viewDayFocusIgnoreScroll .~ false)
            )
      )
  ViewTimelineScrolled -> do
    st <- get
    if st ^. (_view <<< _viewDayFocusIgnoreScroll) then
      modify_ (_view <<< _viewDayFocusIgnoreScroll .~ false)
    else
      modify_ (_view <<< _viewDayFocusUserScrolled .~ true)
  ViewOpenModal modal ->
    modify_
      ( _view %~
          ( (_viewActiveModal .~ Just modal)
              <<< clearPresenceInspectionHoverState
              <<< (_viewSharedPresenceOverflowSheet .~ Nothing)
          )
      )
  ViewOpenOverlapSheet overlapSheet ->
    modify_
      ( _view
          %~
            ( (_viewOverlapSheet .~ Just overlapSheet)
                <<< (_viewSharedPresenceOverflowSheet .~ Nothing)
                <<< (_viewPresenceInspection .~ Nothing)
                <<< (_viewPresenceInspectionPinned .~ false)
                <<< clearPresenceInspectionHoverState
                <<< (_viewActiveModal .~ Just ModalOverlapGroup)
            )
      )
  ViewPromoteOverlapItem overlapSheet item ->
    modify_
      ( _view
          %~
            ( (_viewPromotedOverlaps %~ upsertMobileOverlapPromotion { groupKey: overlapSheet.groupKey, itemIdentity: calendarItemIdentity item })
                <<< (_viewActiveModal .~ Nothing)
                <<< (_viewOverlapSheet .~ Nothing)
            )
      )
  ViewOpenSharedPresenceOverflow overflowSheet ->
    modify_
      ( _view
          %~
            ( (_viewSharedPresenceOverflowSheet .~ Just overflowSheet)
                <<< (_viewOverlapSheet .~ Nothing)
                <<< (_viewPresenceInspection .~ Nothing)
                <<< (_viewPresenceInspectionPinned .~ false)
                <<< clearPresenceInspectionHoverState
                <<< (_viewActiveModal .~ Just ModalSharedPresenceOverflow)
            )
      )
  ViewPresenceSegmentMouseEnter inspection -> do
    st <- get
    let isMobileNow = st ^. (_view <<< _viewIsMobile)
    let isPinned = st ^. (_view <<< _viewPresenceInspectionPinned)
    when (not isMobileNow && not isPinned) do
      panelTopPx <- computePresenceInspectionInitialTopPx inspection
      modify_
        ( _view %~
            ( (_viewPresenceInspection .~ Just inspection)
                <<< (_viewPresenceInspectionPinned .~ false)
                <<< (_viewPresenceInspectionInitialTopPx .~ panelTopPx)
                <<< (_viewPresenceInspectionHoverSegment .~ Just (toPresenceInspectionHoverSegment inspection))
                <<< (_viewPresenceInspectionCloseToken %~ (_ + 1))
            )
        )
  ViewPresenceSegmentMouseLeave inspection -> do
    st <- get
    let isMobileNow = st ^. (_view <<< _viewIsMobile)
    let isPinned = st ^. (_view <<< _viewPresenceInspectionPinned)
    let hoverSegment = st ^. (_view <<< _viewPresenceInspectionHoverSegment)
    let leavingSegment = toPresenceInspectionHoverSegment inspection
    when (not isMobileNow && not isPinned && hoverSegment == Just leavingSegment) do
      modify_ (_view <<< _viewPresenceInspectionHoverSegment .~ Nothing)
      schedulePresenceInspectionClose
  ViewPresenceInspectionPanelMouseEnter -> do
    st <- get
    let isMobileNow = st ^. (_view <<< _viewIsMobile)
    let isPinned = st ^. (_view <<< _viewPresenceInspectionPinned)
    when (not isMobileNow && not isPinned) $
      modify_
        ( _view %~
            ( (_viewPresenceInspectionHoverPanel .~ true)
                <<< (_viewPresenceInspectionCloseToken %~ (_ + 1))
            )
        )
  ViewPresenceInspectionPanelMouseLeave -> do
    st <- get
    let isMobileNow = st ^. (_view <<< _viewIsMobile)
    let isPinned = st ^. (_view <<< _viewPresenceInspectionPinned)
    when (not isMobileNow && not isPinned) do
      modify_ (_view <<< _viewPresenceInspectionHoverPanel .~ false)
      schedulePresenceInspectionClose
  ViewInspectPresence trigger inspection -> do
    st <- get
    let isMobileNow = st ^. (_view <<< _viewIsMobile)
    let isPinned = st ^. (_view <<< _viewPresenceInspectionPinned)
    panelTopPx <-
      if isMobileNow then
        pure Nothing
      else
        computePresenceInspectionInitialTopPx inspection
    modify_
      ( _view %~
          case trigger, isMobileNow, isPinned of
            PresenceInspectTap, true, _ ->
              (_viewPresenceInspection .~ Just inspection)
                <<< (_viewPresenceInspectionPinned .~ false)
                <<< (_viewPresenceInspectionInitialTopPx .~ Nothing)
                <<< clearPresenceInspectionHoverState
                <<< (_viewActiveModal .~ Just ModalPresenceInspection)
                <<< (_viewOverlapSheet .~ Nothing)
                <<< (_viewSharedPresenceOverflowSheet .~ Nothing)
            PresenceInspectTap, false, _ ->
              (_viewPresenceInspection .~ Just inspection)
                <<< (_viewPresenceInspectionPinned .~ true)
                <<< (_viewPresenceInspectionInitialTopPx .~ panelTopPx)
                <<< clearPresenceInspectionHoverState
                <<< (_viewSharedPresenceOverflowSheet .~ Nothing)
                <<< (_viewActiveModal %~ closePresenceModal)
            PresenceInspectHover, false, false ->
              (_viewPresenceInspection .~ Just inspection)
                <<< (_viewPresenceInspectionInitialTopPx .~ panelTopPx)
                <<< (_viewPresenceInspectionPinned .~ false)
            PresenceInspectFocus, false, false ->
              (_viewPresenceInspection .~ Just inspection)
                <<< (_viewPresenceInspectionInitialTopPx .~ panelTopPx)
                <<< (_viewPresenceInspectionPinned .~ false)
            _, _, _ ->
              identity
      )
  ViewClearPresenceInspection -> do
    st <- get
    let isMobileNow = st ^. (_view <<< _viewIsMobile)
    let activeModal = st ^. (_view <<< _viewActiveModal)
    let isPinned = st ^. (_view <<< _viewPresenceInspectionPinned)
    if (isMobileNow && activeModal == Just ModalPresenceInspection) || isPinned then
      pure unit
    else
      modify_
        ( _view %~
            ( (_viewPresenceInspection .~ Nothing)
                <<< (_viewPresenceInspectionPinned .~ false)
                <<< (_viewPresenceInspectionInitialTopPx .~ Nothing)
                <<< clearPresenceInspectionHoverState
                <<< (_viewActiveModal %~ closePresenceModal)
            )
        )
  ViewDismissPresenceInspection ->
    modify_
      ( _view %~
          ( (_viewPresenceInspection .~ Nothing)
              <<< (_viewPresenceInspectionPinned .~ false)
              <<< (_viewPresenceInspectionInitialTopPx .~ Nothing)
              <<< clearPresenceInspectionHoverState
              <<< (_viewActiveModal %~ closePresenceModal)
          )
      )
  ViewSetPresenceCueColor target nextColorToken -> do
    st <- get
    let cuePreferences = st ^. _calendarPresenceCuePreferences
    let
      nextPreferences =
        setPresenceCuePreference target nextColorToken cuePreferences.preferences
    let nextState = cuePreferences { preferences = nextPreferences }
    modify_ (_calendarPresenceCuePreferences .~ nextState)
    liftEffect $ persistPresenceCuePreferences nextState
  ViewCloseModal ->
    modify_
      ( _view %~
          ( (_viewActiveModal .~ Nothing)
              <<< (_viewOverlapSheet .~ Nothing)
              <<< (_viewSharedPresenceOverflowSheet .~ Nothing)
              <<< (_viewPresenceInspection .~ Nothing)
              <<< (_viewPresenceInspectionPinned .~ false)
              <<< (_viewPresenceInspectionInitialTopPx .~ Nothing)
              <<< clearPresenceInspectionHoverState
          )
      )
  ViewOpenCreate ->
    modify_ (_view <<< _viewActiveModal .~ Just ModalCreateItem)
  ViewCloseCreate ->
    modify_ (_view <<< _viewActiveModal .~ Nothing)
  ViewOpenEdit item ->
    case buildEditDraft item of
      Nothing -> pure unit
      Just draft ->
        modify_
          ( _view
              %~
                ( (_viewEditPanel .~ Just { item, draft, validationError: Nothing })
                    <<< (_viewActiveModal .~ Just ModalEditItem)
                )
          )
  ViewOpenEditFromDoubleClick item -> do
    viewport <- liftEffect $ window >>= Window.innerWidth
    let isMobileNow = viewport <= 768
    modify_ (_view <<< _viewIsMobile .~ isMobileNow)
    if isMobileNow then
      handleViewAction (ViewOpenEdit item)
    else
      pure unit
  ViewEditTitleChanged raw ->
    modify_ (_view <<< _viewEditPanel %~ map (updateEditPanelTitle raw))
  ViewEditStartChanged raw ->
    modify_ (_view <<< _viewEditPanel %~ map (updateEditPanelWindowStart raw))
  ViewEditEndChanged raw ->
    modify_ (_view <<< _viewEditPanel %~ map (updateEditPanelWindowEnd raw))
  ViewEditCategoryChanged raw ->
    modify_ (_view <<< _viewEditPanel %~ map (updateEditPanelCategory raw))
  ViewEditStatusChanged raw ->
    modify_ (_view <<< _viewEditPanel %~ map (updateEditPanelStatus (parseStatus raw)))
  ViewEditDurationChanged raw ->
    modify_ (_view <<< _viewEditPanel %~ map (updateEditPanelDuration raw))
  ViewEditDeparturePlaceChanged raw ->
    modify_ (_view <<< _viewEditPanel %~ map (updateEditPanelDeparturePlace raw))
  ViewEditArrivalPlaceChanged raw ->
    modify_ (_view <<< _viewEditPanel %~ map (updateEditPanelArrivalPlace raw))
  ViewEditSave -> do
    st <- get
    case st ^. (_view <<< _viewEditPanel) of
      Nothing -> pure unit
      Just panel ->
        case applyEditDraft panel.draft panel.item of
          Left err ->
            modify_ (_view <<< _viewEditPanel .~ Just (panel { validationError = Just (editErrorMessage err) }))
          Right updatedItem -> do
            let itemId = editDraftItemId panel.draft
            resp <- updateItem itemId updatedItem
            if statusOk resp then do
              modify_ (_syncUpdateErrorState .~ Nothing)
              refreshItems
              modify_
                ((_view <<< _viewEditPanel .~ Nothing) <<< (_view <<< _viewActiveModal .~ Nothing))
            else
              case panel.draft of
                EditTripDraft _ ->
                  modify_ (_view <<< _viewEditPanel .~ Just (panel { validationError = Just (tripWriteErrorMessage resp) }))
                EditTaskDraft _ ->
                  modify_ (_syncUpdateErrorState .~ Just (updateErrorMessage (unwrap resp.status)))
  ViewEditCancel ->
    modify_ ((_view <<< _viewEditPanel .~ Nothing) <<< (_view <<< _viewActiveModal .~ Nothing))
  ViewSetIsMobile isMobile ->
    modify_ (_view <<< _viewIsMobile .~ isMobile)
  where
  clearPresenceInspectionHoverState =
    (_viewPresenceInspectionHoverSegment .~ Nothing)
      <<< (_viewPresenceInspectionHoverPanel .~ false)
      <<< (_viewPresenceInspectionInitialTopPx .~ Nothing)
      <<< (_viewPresenceInspectionCloseToken %~ (_ + 1))

  computePresenceInspectionInitialTopPx inspection = do
    timelineRef <- lift $ H.getRef (wrap "day-timeline-scroll")
    gridRef <- lift $ H.getRef (wrap "day-calendar-grid")
    case timelineRef, gridRef of
      Just _, Just gridEl -> do
        gridRect <- liftEffect $ getBoundingClientRect gridEl
        viewportHeightPx <- liftEffect viewportVisibleHeight
        let minuteHeightPx = if gridRect.height <= 0.0 then 0.0 else gridRect.height / 1440.0
        if minuteHeightPx <= 0.0 then
          pure Nothing
        else do
          let segmentCenterMin = Int.toNumber inspection.startMin + (Int.toNumber inspection.duration / 2.0)
          let segmentCenterViewportY = gridRect.top + segmentCenterMin * minuteHeightPx
          let desiredTopViewportY = segmentCenterViewportY - (presenceInspectionPanelInitialEstimatePx / 2.0)
          let minTopViewportY = presenceInspectionPanelViewportPaddingPx
          let maxTopViewportYRaw = viewportHeightPx - presenceInspectionPanelViewportPaddingPx - presenceInspectionPanelInitialEstimatePx
          let maxTopViewportY = max minTopViewportY maxTopViewportYRaw
          let clampedTopViewportY = max minTopViewportY (min maxTopViewportY desiredTopViewportY)
          pure (Just (clampedTopViewportY - gridRect.top))
      _, _ ->
        pure Nothing

  schedulePresenceInspectionClose = do
    st <- get
    let nextToken = (st ^. (_view <<< _viewPresenceInspectionCloseToken)) + 1
    modify_ (_view <<< _viewPresenceInspectionCloseToken .~ nextToken)
    _ <- lift $ H.fork do
      liftAff $ delay (Milliseconds presenceInspectionHoverCloseDelayMs)
      stAfter <- get
      let isMobileNow = stAfter ^. (_view <<< _viewIsMobile)
      let isPinned = stAfter ^. (_view <<< _viewPresenceInspectionPinned)
      let isHoveringPanel = stAfter ^. (_view <<< _viewPresenceInspectionHoverPanel)
      let isHoveringSegment = stAfter ^. (_view <<< _viewPresenceInspectionHoverSegment) /= Nothing
      let currentToken = stAfter ^. (_view <<< _viewPresenceInspectionCloseToken)
      when
        ( not isMobileNow
            && not isPinned
            && not isHoveringPanel
            && not isHoveringSegment
            &&
              nextToken == currentToken
        ) $
        modify_
          ( _view %~
              ( (_viewPresenceInspection .~ Nothing)
                  <<< (_viewPresenceInspectionPinned .~ false)
                  <<< clearPresenceInspectionHoverState
                  <<< (_viewActiveModal %~ closePresenceModal)
              )
          )
      pure unit
    pure unit

  closePresenceModal = case _ of
    Just ModalPresenceInspection -> Nothing
    value -> value

viewTitle :: CalendarView -> String
viewTitle viewMode =
  case viewMode of
    ViewDay -> "Vue Jour"
    ViewWeek -> "Vue Semaine"
    ViewMonth -> "Vue Mois"

parseAgendaView :: String -> CalendarView
parseAgendaView raw =
  case raw of
    "week" -> ViewWeek
    "month" -> ViewMonth
    _ -> ViewDay

renderViewSelector :: forall w. CalendarView -> String -> String -> HTML w ViewAction
renderViewSelector viewMode focusDate todayDate =
  let
    dayLabel = DateTime.formatCalendarDayDateLabelWithReference focusDate todayDate
    dateInput =
      input
        [ class_ "form-control calendar-input calendar-view-date"
        , type_ InputDate
        , attr (AttrName "lang") "fr"
        , value focusDate
        , onValueChange ViewFocusDateChanged
        ]
  in
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
          [ div [ class_ "calendar-notifications-label" ] [ text "Date" ]
          , case viewMode of
              ViewDay ->
                div [ class_ "calendar-view-date-trigger-wrap" ]
                  [ button
                      [ class_ "calendar-view-date-trigger"
                      , attr (AttrName "type") "button"
                      , attr (AttrName "aria-label") "Choisir la date du jour"
                      , onClick (const ViewOpenDayDatePicker)
                      ]
                      [ text dayLabel ]
                  , input
                      [ class_ "calendar-view-date calendar-view-date--overlay"
                      , type_ InputDate
                      , attr (AttrName "lang") "fr"
                      , attr (AttrName "aria-label") "Date du jour"
                      , ref (wrap "day-view-date-input")
                      , value focusDate
                      , onValueChange ViewFocusDateChanged
                      ]
                  ]
              _ ->
                dateInput
          ]
      ]

renderMobileTools :: forall w. CalendarView -> HTML w ViewAction
renderMobileTools _ =
  div [ class_ "calendar-mobile-tools" ]
    [ button [ class_ "btn btn-sm btn-primary", onClick (const (ViewOpenModal ModalTools)) ] [ text "Actions" ] ]

renderToolsContent :: forall w. HTML w ViewAction
renderToolsContent =
  div [ class_ "calendar-modal-stack" ]
    [ button [ class_ "btn btn-outline-secondary", onClick (const (ViewOpenModal ModalTripShares)) ] [ text "Partage trajets" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const (ViewOpenModal ModalTripSubscriptions)) ] [ text "Abonnements trajets" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const (ViewOpenModal ModalImportCsv)) ] [ text "Import CSV" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const (ViewOpenModal ModalImportIcs)) ] [ text "Import ICS" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const (ViewOpenModal ModalExport)) ] [ text "Export" ]
    ]

type UserListPanelConfig action =
  { title :: String
  , primaryHint :: String
  , secondaryHint :: String
  , loadingText :: String
  , emptyText :: String
  , usernameChanged :: String -> action
  , submitRequested :: action
  , deleteRequested :: String -> action
  , reloadRequested :: action
  }

sharePanelConfig :: UserListPanelConfig ShareAction
sharePanelConfig =
  { title: "Qui peut voir mes trajets"
  , primaryHint: "Cette liste controle uniquement qui peut voir vos trajets."
  , secondaryHint: "Voir les trajets d'une autre personne se regle separement dans les abonnements."
  , loadingText: "Chargement des partages..."
  , emptyText: "Personne n'a encore acces a vos trajets."
  , usernameChanged: ShareUsernameChanged
  , submitRequested: ShareSubmitRequested
  , deleteRequested: ShareDeleteRequested
  , reloadRequested: ShareReloadRequested
  }

subscriptionPanelConfig :: UserListPanelConfig SubscriptionAction
subscriptionPanelConfig =
  { title: "Utilisateurs suivis"
  , primaryHint: "Cette liste controle uniquement les trajets que vous souhaitez suivre."
  , secondaryHint: "Vous ne verrez les trajets d'une personne que si elle vous les partage aussi."
  , loadingText: "Chargement des abonnements..."
  , emptyText: "Vous ne suivez encore aucun utilisateur pour les trajets."
  , usernameChanged: SubscriptionUsernameChanged
  , submitRequested: SubscriptionSubmitRequested
  , deleteRequested: SubscriptionDeleteRequested
  , reloadRequested: SubscriptionReloadRequested
  }

renderShareManager :: forall w. UserListPanelConfig ShareAction -> ShareListState -> HTML w ShareAction
renderShareManager config shareList =
  renderUserListManager config shareList

renderSubscriptionManager :: forall w. UserListPanelConfig SubscriptionAction -> ShareListState -> HTML w SubscriptionAction
renderSubscriptionManager config subscriptionList =
  renderUserListManager config subscriptionList

renderUserListManager :: forall w action. UserListPanelConfig action -> ShareListState -> HTML w action
renderUserListManager config userList =
  section [ class_ "calendar-list-panel calendar-share-panel" ]
    [ div [ class_ "calendar-share-panel__header" ]
        [ div [ class_ "calendar-share-panel__title" ] [ text config.title ]
        , div [ class_ "calendar-share-panel__hint" ]
            [ text config.primaryHint ]
        , div [ class_ "calendar-share-panel__hint text-muted" ]
            [ text config.secondaryHint ]
        ]
    , div [ class_ "calendar-share-panel__form" ]
        [ div [ class_ "calendar-share-panel__row" ]
            [ input
                [ class_ "form-control calendar-input"
                , placeholder "Nom d'utilisateur"
                , value userList.usernameDraft
                , onValueInput config.usernameChanged
                , disabled userList.isAdding
                ]
            , button
                [ class_ "btn btn-primary"
                , onClick (const config.submitRequested)
                , disabled (isShareSubmitDisabled userList)
                ]
                [ text "Ajouter" ]
            ]
        , maybe (text "") (\msg -> div [ class_ "calendar-error" ] [ text msg ]) userList.submitError
        ]
    , renderUserListManagerBody config userList
    ]

renderUserListManagerBody :: forall w action. UserListPanelConfig action -> ShareListState -> HTML w action
renderUserListManagerBody config userList
  | userList.isLoading && not userList.hasLoaded =
      div [ class_ "calendar-share-panel__empty text-muted" ] [ text config.loadingText ]
  | otherwise =
      div [ class_ "calendar-share-panel__body" ]
        [ maybe (text "") renderLoadError userList.loadError
        , if null userList.usernames then
            div [ class_ "calendar-share-panel__empty text-muted" ] [ text config.emptyText ]
          else
            ul [ class_ "calendar-share-panel__list" ] (map renderUsername userList.usernames)
        ]
      where
      renderLoadError message =
        div [ class_ "calendar-error" ]
          [ div [] [ text message ]
          , button
              [ class_ "btn btn-sm btn-outline-secondary"
              , onClick (const config.reloadRequested)
              ]
              [ text "Reessayer" ]
          ]

      renderUsername username =
        li [ class_ "calendar-share-panel__item" ]
          [ div [ class_ "calendar-share-panel__username" ] [ text username ]
          , button
              [ class_ "btn btn-sm btn-outline-secondary"
              , onClick (const (config.deleteRequested username))
              , disabled (isUsernameDeleting username userList)
              ]
              [ text "Retirer" ]
          ]

renderEditContent :: EditPanel -> TripPlacesState -> H.ComponentHTML Action Slots Aff
renderEditContent panel tripPlaces =
  case panel.draft of
    EditTaskDraft draft ->
      div [ class_ "calendar-modal-stack" ]
        [ staticTypeBadge "Tâche"
        , editTextField "Titre" ViewEditTitleChanged draft.title
        , editDateTimeField "Début" ViewEditStartChanged draft.windowStart
        , editDateTimeField "Fin" ViewEditEndChanged draft.windowEnd
        , editTextField "Catégorie" ViewEditCategoryChanged draft.category
        , div [ class_ "calendar-modal-field" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Statut" ]
            , select
                [ class_ "form-select calendar-input"
                , onValueChange (ViewAction <<< ViewEditStatusChanged)
                , value (statusValue draft.status)
                ]
                [ option [ value "todo" ] [ text "À faire" ]
                , option [ value "in_progress" ] [ text "En cours" ]
                , option [ value "done" ] [ text "Terminé" ]
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
        , renderEditError panel.validationError
        ]
    EditTripDraft draft ->
      div [ class_ "calendar-modal-stack" ]
        [ staticTypeBadge "Trajet"
        , renderTripPlacesEditField "Départ" ViewEditDeparturePlaceChanged draft.departurePlaceId tripPlaces
        , renderTripPlacesEditField "Arrivée" ViewEditArrivalPlaceChanged draft.arrivalPlaceId tripPlaces
        , editDateTimeField "Départ" ViewEditStartChanged draft.windowStart
        , editDateTimeField "Arrivée" ViewEditEndChanged draft.windowEnd
        , renderTripPlacesFeedback tripPlaces
        , renderEditError panel.validationError
        ]
  where
  staticTypeBadge label =
    div [ class_ "calendar-modal-field" ]
      [ div [ class_ "calendar-notifications-label" ] [ text "Type" ]
      , div [ class_ "badge rounded-pill text-bg-secondary" ] [ text label ]
      ]

  editTextField label onChange currentValue =
    div [ class_ "calendar-modal-field" ]
      [ div [ class_ "calendar-notifications-label" ] [ text label ]
      , input
          [ class_ "form-control calendar-input"
          , placeholder label
          , onValueChange (ViewAction <<< onChange)
          , value currentValue
          ]
      ]

  editDateTimeField label onChange currentValue =
    div [ class_ "calendar-modal-field" ]
      [ div [ class_ "calendar-notifications-label" ] [ text label ]
      , input
          [ class_ "form-control calendar-input"
          , type_ InputDatetimeLocal
          , attr (AttrName "lang") "fr"
          , placeholder label
          , onValueChange (ViewAction <<< onChange)
          , value currentValue
          ]
      ]

  renderEditError =
    maybe (text "") (\msg -> div [ class_ "calendar-error" ] [ text msg ])

statusValue :: ItemStatus -> String
statusValue status =
  case status of
    Todo -> "todo"
    InProgress -> "in_progress"
    Done -> "done"
    Canceled -> "canceled"

parseStatus :: String -> ItemStatus
parseStatus raw =
  case raw of
    "in_progress" -> InProgress
    "done" -> Done
    "canceled" -> Canceled
    _ -> Todo

editErrorMessage :: EditError -> String
editErrorMessage err =
  case err of
    EditValidation validation -> validationErrorMessage validation
    EditRecurrence msg -> msg
    EditDuration msg -> msg
    EditTripValidation validation -> tripValidationErrorMessage validation
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
type DragState =
  { draggingId :: Maybe String
  , dragHoverIndex :: Maybe Int
  , dragOffsetMinutes :: Maybe Int
  , touchLongPressToken :: Int
  , touchPending :: Maybe PendingTouchDrag
  }

type PendingTouchDrag =
  { token :: Int
  , itemId :: String
  , startIndex :: Int
  , offsetMinutes :: Maybe Int
  }

type DragCtx =
  { items :: Array CalendarItem
  , focusDate :: String
  , offlineMode :: Boolean
  }

dragInitialState :: DragState
dragInitialState =
  { draggingId: Nothing
  , dragHoverIndex: Nothing
  , dragOffsetMinutes: Nothing
  , touchLongPressToken: 0
  , touchPending: Nothing
  }

_draggingId :: Lens' DragState (Maybe String)
_draggingId =
  lens
    _.draggingId
    (_ { draggingId = _ })

_dragHoverIndex :: Lens' DragState (Maybe Int)
_dragHoverIndex =
  lens
    _.dragHoverIndex
    (_ { dragHoverIndex = _ })

_dragOffsetMinutes :: Lens' DragState (Maybe Int)
_dragOffsetMinutes =
  lens
    _.dragOffsetMinutes
    (_ { dragOffsetMinutes = _ })

_touchLongPressToken :: Lens' DragState Int
_touchLongPressToken =
  lens
    _.touchLongPressToken
    (_ { touchLongPressToken = _ })

_touchPending :: Lens' DragState (Maybe PendingTouchDrag)
_touchPending =
  lens
    _.touchPending
    (_ { touchPending = _ })

clearActiveDragState :: DragState -> DragState
clearActiveDragState =
  (_draggingId .~ Nothing)
    <<< (_dragHoverIndex .~ Nothing)
    <<< (_dragOffsetMinutes .~ Nothing)

clearTouchPendingState :: DragState -> DragState
clearTouchPendingState =
  (_touchPending .~ Nothing)

dragCalendarHandlers
  :: forall r
   . Boolean
  -> CalendarItem
  -> Array
       ( IProp
           ( draggable :: Boolean
           , onDragStart :: DragEvent
           , onDragEnd :: DragEvent
           | r
           )
           Action
       )
dragCalendarHandlers true _ = []
dragCalendarHandlers false item | calendarItemSupportsDragEdit item =
  case item of
    ServerCalendarItem { id } ->
      [ draggable true
      , onDragStart (\ev -> DragAction { log: "DragStart@calendar-card", dragAction: DragStart id ev })
      , onDragEnd (const (DragAction { log: "DragEnd@calendar-card", dragAction: DragEnd }))
      ]
    _ -> []
dragCalendarHandlers false _ =
  []

touchCalendarHandlers
  :: forall r
   . Boolean
  -> CalendarItem
  -> Array
       ( IProp
           ( onTouchStart :: TouchEvent.TouchEvent
           , onTouchCancel :: TouchEvent.TouchEvent
           | r
           )
           Action
       )
touchCalendarHandlers false _ = []
touchCalendarHandlers true item | calendarItemSupportsDragEdit item =
  case item of
    ServerCalendarItem { id } ->
      [ onTouchStart (\ev -> DragAction { log: "TouchStart@calendar-card", dragAction: TouchDragStart id ev })
      , onTouchCancel (const (DragAction { log: "TouchCancel@calendar-card", dragAction: TouchDragCancel }))
      ]
    _ -> []
touchCalendarHandlers true _ = []

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

computeMinuteIndexFromClientY :: Int -> Number -> Number -> Maybe Int
computeMinuteIndexFromClientY clientY rectTop rectHeight
  | rectHeight <= 0.0 = Nothing
  | otherwise =
      let
        minuteHeight = rectHeight / 1440.0
        offsetPx = Int.toNumber clientY - rectTop
        rawMinutes = if minuteHeight <= 0.0 then 0.0 else offsetPx / minuteHeight
        minutes = max 0 (min 1439 (Int.floor rawMinutes))
      in
        Just (Int.quot minutes 5)

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

touchClientYFromEvent :: TouchEvent.TouchEvent -> Maybe Int
touchClientYFromEvent ev =
  let
    activeTouches = TouchEvent.touches ev
    changed = TouchEvent.changedTouches ev
  in
    (Touch.clientY <$> TouchList.item 0 activeTouches)
      <|> (Touch.clientY <$> TouchList.item 0 changed)

dragOffsetFromTouchEvent :: TouchEvent.TouchEvent -> Maybe Int -> Effect (Maybe Int)
dragOffsetFromTouchEvent ev duration = do
  let
    dur = fromMaybe 0 duration
    event = TouchEvent.toEvent ev
    clientY = fromMaybe 0 (touchClientYFromEvent ev)
    targetEl =
      Event.currentTarget event
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

dragMinuteIndexFromElement :: DragEvent -> Element -> Effect (Maybe Int)
dragMinuteIndexFromElement ev el = do
  let
    event = toEvent ev
    mouse = MouseEvent.fromEvent event
    clientY = maybe 0 MouseEvent.clientY mouse
  rect <- getBoundingClientRect el
  pure (computeMinuteIndexFromClientY clientY rect.top rect.height)

touchMinuteIndexFromElement :: TouchEvent.TouchEvent -> Element -> Effect (Maybe Int)
touchMinuteIndexFromElement ev el =
  map
    (\rect -> touchClientYFromEvent ev >>= \clientY -> computeMinuteIndexFromClientY clientY rect.top rect.height)
    (getBoundingClientRect el)

dragMinuteIndexFromEvent :: DragEvent -> Effect (Maybe Int)
dragMinuteIndexFromEvent ev = do
  let
    event = toEvent ev
    targetEl =
      (Event.currentTarget event <|> Event.target event)
        >>= HTMLElement.fromEventTarget
        <#> HTMLElement.toElement
  case targetEl of
    Just el -> dragMinuteIndexFromElement ev el
    Nothing -> pure Nothing

resolveDroppedWindow :: String -> Int -> Int -> Int -> Maybe { start :: DateTime, end :: DateTime }
resolveDroppedWindow focusDate offsetMinutes durationMinutes minuteIndex = do
  let adjustedIndex = computeDropMinuteIndex minuteIndex offsetMinutes
  resolveDroppedWindowFromIndex focusDate durationMinutes adjustedIndex

resolveDroppedWindowFromIndex :: String -> Int -> Int -> Maybe { start :: DateTime, end :: DateTime }
resolveDroppedWindowFromIndex focusDate durationMinutes adjustedIndex = do
  dateValue <- parseDateLocal focusDate
  let
    totalMinutes = indexToMinutes adjustedIndex
    hour = Int.quot totalMinutes 60
    minute = Int.rem totalMinutes 60
  timeValue <- DateTime.timeFromParts hour minute
  start <- pure (combineDateWithTime dateValue timeValue)
  end <- shiftMinutes durationMinutes start
  pure { start, end }

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
          nextContent =
            case payload.content of
              TaskCalendarItemContent content ->
                TaskCalendarItemContent
                  ( content
                      { windowStart = newStart
                      , windowEnd = newEnd
                      }
                  )
              TripCalendarItemContent content ->
                TripCalendarItemContent
                  ( content
                      { windowStart = newStart
                      , windowEnd = newEnd
                      }
                  )
          updatedItem =
            ServerCalendarItem
              payload
                { content = nextContent }
        in
          { items: acc.items <> [ updatedItem ], updated: Just updatedItem }
      _ ->
        { items: acc.items <> [ item ], updated: acc.updated }

-- END src/Calendar/Drag.purs

-- BEGIN src/Calendar/Edit.purs
type TaskEditDraft =
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

type TripEditDraft =
  { itemId :: String
  , departurePlaceId :: String
  , arrivalPlaceId :: String
  , windowStart :: String
  , windowEnd :: String
  }

data EditDraft
  = EditTaskDraft TaskEditDraft
  | EditTripDraft TripEditDraft

derive instance editDraftEq :: Eq EditDraft
derive instance editDraftGeneric :: Generic EditDraft _

instance editDraftShow :: Show EditDraft where
  show = genericShow

data EditError
  = EditValidation ValidationError
  | EditRecurrence String
  | EditDuration String
  | EditTripValidation TripValidationError
  | EditUnsupported

derive instance editErrorEq :: Eq EditError
derive instance editErrorGeneric :: Generic EditError _
instance editErrorShow :: Show EditError where
  show = genericShow

buildEditDraft :: CalendarItem -> Maybe EditDraft
buildEditDraft item =
  case item of
    ServerCalendarItem { id, content: TaskCalendarItemContent content } ->
      Just
        ( EditTaskDraft
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
        )
    ServerCalendarItem { id, content: TripCalendarItemContent content } ->
      Just
        ( EditTripDraft
            { itemId: id
            , departurePlaceId: content.departurePlaceId
            , arrivalPlaceId: content.arrivalPlaceId
            , windowStart: formatDateTimeLocal content.windowStart
            , windowEnd: formatDateTimeLocal content.windowEnd
            }
        )
    _ -> Nothing

applyEditDraft :: EditDraft -> CalendarItem -> Either EditError CalendarItem
applyEditDraft draft item =
  case draft of
    EditTaskDraft taskDraft ->
      let
        normalizedTaskDraft :: TaskDraft
        normalizedTaskDraft =
          { itemType: taskDraft.itemType
          , title: taskDraft.title
          , windowStart: taskDraft.windowStart
          , windowEnd: taskDraft.windowEnd
          , category: taskDraft.category
          , status: taskDraft.status
          , actualDurationMinutes: taskDraft.actualDurationMinutes
          , recurrence: taskDraft.recurrence
          }
      in
        case validateTask normalizedTaskDraft of
          Left err -> Left (EditValidation err)
          Right _ -> do
            recurrence <- case draftToRecurrence taskDraft.recurrence of
              Left err -> Left (EditRecurrence err)
              Right ok -> Right ok
            actualDuration <- parseTaskDuration taskDraft.actualDurationMinutes
            windowStart <- maybe (Left (EditValidation WindowStartInvalid)) Right (parseDateTimeLocal taskDraft.windowStart)
            windowEnd <- maybe (Left (EditValidation WindowEndInvalid)) Right (parseDateTimeLocal taskDraft.windowEnd)
            case item of
              ServerCalendarItem payload@{ content: TaskCalendarItemContent content } ->
                Right
                  ( ServerCalendarItem
                      payload
                        { content =
                            TaskCalendarItemContent
                              ( content
                                  { title = taskDraft.title
                                  , windowStart = windowStart
                                  , windowEnd = windowEnd
                                  , category = toOptionalString taskDraft.category
                                  , status = taskDraft.status
                                  , actualDurationMinutes = actualDuration
                                  , recurrenceRule = recurrence.rule
                                  , recurrenceExceptionDates = parseExceptionDatesOrEmpty recurrence.exceptions
                                  }
                              )
                        }
                  )
              _ -> Left EditUnsupported
    EditTripDraft tripDraft ->
      case
        validateTrip
          { departurePlaceId: tripDraft.departurePlaceId
          , arrivalPlaceId: tripDraft.arrivalPlaceId
          , windowStart: tripDraft.windowStart
          , windowEnd: tripDraft.windowEnd
          }
        of
        Left err -> Left (EditTripValidation err)
        Right _ -> do
          windowStart <- maybe (Left (EditTripValidation TripWindowStartInvalid)) Right (parseDateTimeLocal tripDraft.windowStart)
          windowEnd <- maybe (Left (EditTripValidation TripWindowEndInvalid)) Right (parseDateTimeLocal tripDraft.windowEnd)
          case item of
            ServerCalendarItem payload@{ content: TripCalendarItemContent content } ->
              Right
                ( ServerCalendarItem
                    payload
                      { content =
                          TripCalendarItemContent
                            ( content
                                { departurePlaceId = tripDraft.departurePlaceId
                                , arrivalPlaceId = tripDraft.arrivalPlaceId
                                , windowStart = windowStart
                                , windowEnd = windowEnd
                                }
                            )
                      }
                )
            _ -> Left EditUnsupported
  where
  parseTaskDuration raw =
    if StringCommon.trim raw == "" then
      Right Nothing
    else
      case parsePositiveInt raw of
        Just minutes -> Right (Just minutes)
        Nothing -> Left (EditDuration "Durée réelle invalide.")

editDraftItemId :: EditDraft -> String
editDraftItemId = case _ of
  EditTaskDraft draft -> draft.itemId
  EditTripDraft draft -> draft.itemId

-- END src/Calendar/Edit.purs

-- BEGIN src/Calendar/Helpers.purs
isDateTimeLocal :: String -> Boolean
isDateTimeLocal = DateTime.isLocalDateTime

parseDateTimeLocal :: String -> Maybe DateTime
parseDateTimeLocal = DateTime.parseLocalDateTime

parseDateLocal :: String -> Maybe Date
parseDateLocal = DateTime.parseLocalDate

buildDayPeriodRange :: String -> Maybe { start :: String, end :: String }
buildDayPeriodRange selectedDate = do
  startDate <- parseDateLocal selectedDate
  startTime <- parseTimeLocal "00:00"
  endDate <- addDaysToDate 1 startDate
  pure
    { start: formatDateTimeLocal (DateTime startDate startTime)
    , end: formatDateTimeLocal (DateTime endDate startTime)
    }

buildDayPeriodDateTimeRange :: String -> Maybe { start :: DateTime, end :: DateTime }
buildDayPeriodDateTimeRange selectedDate = do
  startDate <- parseDateLocal selectedDate
  startTime <- parseTimeLocal "00:00"
  endDate <- addDaysToDate 1 startDate
  pure
    { start: DateTime startDate startTime
    , end: DateTime endDate startTime
    }

parseExceptionDatesOrEmpty :: Array String -> Array Date
parseExceptionDatesOrEmpty exceptions =
  let
    parsed = map DateTime.parseLocalDate exceptions
  in
    if any (_ == Nothing) parsed then []
    else mapMaybe identity parsed

parseTimeLocal :: String -> Maybe Time
parseTimeLocal = DateTime.parseLocalTime

parsePositiveInt :: String -> Maybe Int
parsePositiveInt raw =
  Int.fromString (StringCommon.trim raw) >>= \val ->
    if val > 0 then Just val else Nothing

combineDateWithTime :: Date -> Time -> DateTime
combineDateWithTime dateValue timeValue =
  DateTime dateValue timeValue

shiftMinutes :: Int -> DateTime -> Maybe DateTime
shiftMinutes offset start =
  adjust (Minutes (Int.toNumber offset)) start

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

formatDate :: DateTime -> String
formatDate dt =
  DateTime.formatLocalDate (date dt)

formatDateOnly :: Date -> String
formatDateOnly = DateTime.formatLocalDate

formatDateTimeLocal :: DateTime -> String
formatDateTimeLocal = DateTime.formatLocalDateTime

computeDayFocusTarget :: String -> DateTime -> Array CalendarItem -> DayFocusTarget
computeDayFocusTarget selectedDate now itemsForDate
  | selectedDate == formatDate now = FocusCurrentTime
  | null itemsForDate = FocusTop
  | otherwise = FocusFirstTask

timeLabel :: DateTime -> String
timeLabel raw =
  DateTime.formatLocalTime (time raw)

toOptionalString :: String -> Maybe String
toOptionalString raw =
  let
    trimmed = StringCommon.trim raw
  in
    if trimmed == "" then Nothing else Just trimmed

createItemModeValue :: CreateItemMode -> String
createItemModeValue = case _ of
  CreateTask -> "task"
  CreateTrip -> "trip"

parseCreateItemMode :: String -> CreateItemMode
parseCreateItemMode raw =
  case raw of
    "trip" -> CreateTrip
    _ -> CreateTask

mapCreateDraftWindowStart :: String -> CreateDraft -> CreateDraft
mapCreateDraftWindowStart raw = case _ of
  CreateTaskDraft draft -> CreateTaskDraft (draft { windowStart = raw })
  CreateTripDraft draft -> CreateTripDraft (draft { windowStart = raw })

mapCreateDraftWindowEnd :: String -> CreateDraft -> CreateDraft
mapCreateDraftWindowEnd raw = case _ of
  CreateTaskDraft draft -> CreateTaskDraft (draft { windowEnd = raw })
  CreateTripDraft draft -> CreateTripDraft (draft { windowEnd = raw })

validateTask :: TaskDraft -> Either ValidationError TaskDraft
validateTask draft =
  case unit of
    _ | StringCommon.trim draft.title == "" -> Left TitleEmpty
    _ | not (isDateTimeLocal draft.windowStart) -> Left WindowStartInvalid
    _ | not (isDateTimeLocal draft.windowEnd) -> Left WindowEndInvalid
    _ | draft.windowEnd <= draft.windowStart -> Left WindowOrderInvalid
    _ | maybe true (_ <= 5) (durationMinutesBetweenRaw draft.windowStart draft.windowEnd) -> Left WindowTooShort
    _ -> Right draft

validateTrip :: TripDraft -> Either TripValidationError TripDraft
validateTrip draft =
  case unit of
    _ | StringCommon.trim draft.departurePlaceId == "" -> Left TripDeparturePlaceMissing
    _ | StringCommon.trim draft.arrivalPlaceId == "" -> Left TripArrivalPlaceMissing
    _ | draft.departurePlaceId == draft.arrivalPlaceId -> Left TripPlacesMustDiffer
    _ | not (isDateTimeLocal draft.windowStart) -> Left TripWindowStartInvalid
    _ | not (isDateTimeLocal draft.windowEnd) -> Left TripWindowEndInvalid
    _ | draft.windowEnd <= draft.windowStart -> Left TripWindowOrderInvalid
    _ -> Right draft

tripValidationErrorMessage :: TripValidationError -> String
tripValidationErrorMessage err =
  case err of
    TripDeparturePlaceMissing -> "Le lieu de départ est obligatoire."
    TripArrivalPlaceMissing -> "Le lieu d'arrivée est obligatoire."
    TripWindowStartInvalid -> "La date de départ est invalide."
    TripWindowEndInvalid -> "La date d'arrivée est invalide."
    TripWindowOrderInvalid -> "L'arrivée doit être après le départ."
    TripPlacesMustDiffer -> "Le lieu de départ et d'arrivée doivent être différents."

decodeTripPlacesResponse :: Response Json -> Either FatalError (Array String)
decodeTripPlacesResponse response = do
  tripPlaces <- lmap toFatalError (decodeJson response.body :: Either JsonDecodeError (Array TripPlace))
  pure $ map (\(TripPlace place) -> place.name) tripPlaces

decodeSharedUsersResponse :: Response Json -> Either FatalError (Array String)
decodeSharedUsersResponse response = do
  sharedUsers <- lmap toFatalError (decodeJson response.body :: Either JsonDecodeError (Array TripSharingUser))
  pure $ map (\(TripSharingUser user) -> user.username) sharedUsers

decodePeriodTripsResponse :: Response Json -> Either FatalError (Array PeriodTripGroup)
decodePeriodTripsResponse response =
  lmap toFatalError (decodeJson response.body :: Either JsonDecodeError (Array PeriodTripGroup))

normalizePeriodTripGroups :: Array PeriodTripGroup -> Array SharedPeriodTripGroup
normalizePeriodTripGroups groups =
  mapMaybe normalizeGroup groups
  where
  normalizeGroup (PeriodTripGroup group) =
    { username: group.username, trips: _ } <$> traverse normalizeTrip group.trips

  normalizeTrip (PeriodTrip trip) = do
    windowStart <- parseDateTimeLocal trip.windowStart
    windowEnd <- parseDateTimeLocal trip.windowEnd
    pure
      { windowStart
      , windowEnd
      , departurePlaceId: trip.departurePlaceId
      , arrivalPlaceId: trip.arrivalPlaceId
      }

deriveSharedPresence :: DateTime -> DateTime -> Array SharedPeriodTripGroup -> Array SharedPresenceGroup
deriveSharedPresence periodStart periodEnd =
  mapMaybe (deriveSharedPresenceGroup periodStart periodEnd)

deriveSharedPresenceGroup :: DateTime -> DateTime -> SharedPeriodTripGroup -> Maybe SharedPresenceGroup
deriveSharedPresenceGroup periodStart periodEnd { username, trips } =
  if periodStart >= periodEnd then Nothing
  else Just
    { username
    , segments: coalescePresenceSegments (derivePresenceSegments periodStart periodEnd trips)
    }

derivePresenceSegments :: DateTime -> DateTime -> Array SharedPeriodTrip -> Array SharedPresenceSegment
derivePresenceSegments periodStart periodEnd trips =
  go periodStart initialPresenceState trips []
  where
  initialPresenceState =
    case uncons trips of
      Just { head: trip }
        | trip.windowStart < periodStart && trip.windowEnd <= periodStart ->
            PresenceAtPlace trip.arrivalPlaceId
        | trip.windowStart < periodStart && trip.windowEnd > periodStart ->
            PresenceInTransit
              { departurePlaceId: trip.departurePlaceId
              , arrivalPlaceId: trip.arrivalPlaceId
              }
      _ ->
        PresenceUnknown

  go cursor currentState remaining acc =
    case uncons remaining of
      Nothing ->
        appendPresenceSegment acc
          { start: cursor
          , end: periodEnd
          , state: currentState
          }
      Just { head: trip, tail: rest }
        | trip.windowEnd <= periodStart ->
            go cursor (PresenceAtPlace trip.arrivalPlaceId) rest acc
        | trip.windowStart < periodStart && trip.windowEnd > periodStart ->
            let
              transitState =
                PresenceInTransit
                  { departurePlaceId: trip.departurePlaceId
                  , arrivalPlaceId: trip.arrivalPlaceId
                  }
              transitEnd = min trip.windowEnd periodEnd
              nextAcc =
                appendPresenceSegment acc
                  { start: cursor
                  , end: transitEnd
                  , state: transitState
                  }
            in
              go transitEnd (PresenceAtPlace trip.arrivalPlaceId) rest nextAcc
        | trip.windowStart >= periodEnd ->
            appendPresenceSegment acc
              { start: cursor
              , end: periodEnd
              , state: currentState
              }
        | otherwise ->
            let
              nextAcc =
                appendPresenceSegment acc
                  { start: cursor
                  , end: min trip.windowStart periodEnd
                  , state: currentState
                  }
              transitState =
                PresenceInTransit
                  { departurePlaceId: trip.departurePlaceId
                  , arrivalPlaceId: trip.arrivalPlaceId
                  }
              transitStart = max trip.windowStart periodStart
              transitEnd = min trip.windowEnd periodEnd
              afterTransit =
                appendPresenceSegment nextAcc
                  { start: transitStart
                  , end: transitEnd
                  , state: transitState
                  }
            in
              go transitEnd (PresenceAtPlace trip.arrivalPlaceId) rest afterTransit

appendPresenceSegment :: Array SharedPresenceSegment -> SharedPresenceSegment -> Array SharedPresenceSegment
appendPresenceSegment acc segment =
  if segment.start >= segment.end then acc else acc <> [ segment ]

coalescePresenceSegments :: Array SharedPresenceSegment -> Array SharedPresenceSegment
coalescePresenceSegments =
  foldl coalesce []
  where
  coalesce acc next =
    case unsnocSegments acc of
      Just { init: rest, last: previous }
        | previous.end == next.start && previous.state == next.state ->
            rest <>
              [ { start: previous.start
                , end: next.end
                , state: previous.state
                }
              ]
      _ ->
        acc <> [ next ]

unsnocSegments :: Array SharedPresenceSegment -> Maybe { init :: Array SharedPresenceSegment, last :: SharedPresenceSegment }
unsnocSegments items =
  case length items of
    0 -> Nothing
    count ->
      case { init: takeSegments (count - 1) items, last: dropSegments (count - 1) items } of
        { last: [ last ] } -> Just { init: takeSegments (count - 1) items, last }
        _ -> Nothing

takeSegments :: Int -> Array SharedPresenceSegment -> Array SharedPresenceSegment
takeSegments count =
  mapMaybeWithIndex (\index item -> if index < count then Just item else Nothing)

dropSegments :: Int -> Array SharedPresenceSegment -> Array SharedPresenceSegment
dropSegments count =
  mapMaybeWithIndex (\index item -> if index >= count then Just item else Nothing)

mapMaybeWithIndex :: forall a b. (Int -> a -> Maybe b) -> Array a -> Array b
mapMaybeWithIndex fn items =
  mapMaybe identity (mapWithIndex fn items)

takeArray :: forall a. Int -> Array a -> Array a
takeArray count =
  mapMaybeWithIndex (\index item -> if index < count then Just item else Nothing)

dropArray :: forall a. Int -> Array a -> Array a
dropArray count =
  mapMaybeWithIndex (\index item -> if index >= count then Just item else Nothing)

shouldRenderSharedPresenceRail :: SharedPresenceLoadState -> Boolean
shouldRenderSharedPresenceRail = case _ of
  SharedPresenceLoaded groups -> not (null groups)
  _ -> false

shouldRenderDayCalendarShell :: Array CalendarItem -> SharedPresenceLoadState -> Boolean
shouldRenderDayCalendarShell items sharedPresence =
  not (null items) || shouldRenderSharedPresenceRail sharedPresence

maxVisibleSharedPresenceUsers :: Int
maxVisibleSharedPresenceUsers = 3

type SharedPresenceSegmentLayout =
  { username :: String
  , segmentIndex :: Int
  , laneIndex :: Int
  , laneCount :: Int
  , startMin :: Int
  , duration :: Int
  , state :: SharedPresenceState
  }

type SharedPresenceRailOverflow =
  { hiddenCount :: Int
  , hiddenGroups :: SharedPresence
  }

type SharedPresenceRailView =
  { visibleGroups :: SharedPresence
  , hiddenGroups :: SharedPresence
  , visibleLayouts :: Array SharedPresenceSegmentLayout
  , overflow :: Maybe SharedPresenceRailOverflow
  }

allPresenceCueColorTokens :: Array PresenceCueColorToken
allPresenceCueColorTokens =
  [ CueBlue
  , CueGreen
  , CueAmber
  , CueRose
  , CueViolet
  , CueSlate
  ]

presenceCueColorTokenValue :: PresenceCueColorToken -> String
presenceCueColorTokenValue = case _ of
  CueBlue -> "blue"
  CueGreen -> "green"
  CueAmber -> "amber"
  CueRose -> "rose"
  CueViolet -> "violet"
  CueSlate -> "slate"

parsePresenceCueColorToken :: String -> Maybe PresenceCueColorToken
parsePresenceCueColorToken = case _ of
  "blue" -> Just CueBlue
  "green" -> Just CueGreen
  "amber" -> Just CueAmber
  "rose" -> Just CueRose
  "violet" -> Just CueViolet
  "slate" -> Just CueSlate
  _ -> Nothing

presenceCueColorTokenLabel :: PresenceCueColorToken -> String
presenceCueColorTokenLabel = case _ of
  CueBlue -> "Bleu"
  CueGreen -> "Vert"
  CueAmber -> "Ambre"
  CueRose -> "Rose"
  CueViolet -> "Violet"
  CueSlate -> "Gris"

presenceCueColorTokenClass :: PresenceCueColorToken -> String
presenceCueColorTokenClass token =
  "calendar-presence-rail__segment--color-" <> presenceCueColorTokenValue token

presenceCueEditorTokenClass :: PresenceCueColorToken -> String
presenceCueEditorTokenClass token =
  "calendar-presence-cue-editor__option--color-" <> presenceCueColorTokenValue token

encodePresenceCuePreferencesJson :: Array SharedPresenceCuePreference -> String
encodePresenceCuePreferencesJson =
  stringify <<< encodeJson

decodePresenceCuePreferencesJson :: String -> Either String (Array SharedPresenceCuePreference)
decodePresenceCuePreferencesJson raw =
  if StringCommon.trim raw == "" then
    Right []
  else
    case jsonParser raw of
      Left err -> Left err
      Right json ->
        lmap show (decodeJson json :: Either JsonDecodeError (Array SharedPresenceCuePreference))

presenceCuePreferencesStorageKey :: String -> String
presenceCuePreferencesStorageKey ownerUsername =
  "favs.calendar.presence-cues." <> ownerUsername

lookupPresenceCuePreference :: Array SharedPresenceCuePreference -> SharedPresenceCueTarget -> Maybe PresenceCueColorToken
lookupPresenceCuePreference preferences target =
  map unwrapColorToken
    ( find
        ( \(SharedPresenceCuePreference preference) ->
            preference.sharedUsername == target.sharedUsername && preference.placeId == target.placeId
        )
        preferences
    )
  where
  unwrapColorToken (SharedPresenceCuePreference preference) = preference.colorToken

setPresenceCuePreference :: SharedPresenceCueTarget -> Maybe PresenceCueColorToken -> Array SharedPresenceCuePreference -> Array SharedPresenceCuePreference
setPresenceCuePreference target nextColorToken preferences =
  let
    retained =
      filter
        ( \(SharedPresenceCuePreference preference) ->
            preference.sharedUsername /= target.sharedUsername || preference.placeId /= target.placeId
        )
        preferences
  in
    case nextColorToken of
      Nothing -> retained
      Just colorToken ->
        retained <>
          [ SharedPresenceCuePreference
              { sharedUsername: target.sharedUsername
              , placeId: target.placeId
              , colorToken
              }
          ]

persistPresenceCuePreferences :: PresenceCuePreferencesState -> Effect Unit
persistPresenceCuePreferences { ownerUsername, preferences } =
  case ownerUsername of
    Nothing ->
      pure unit
    Just username ->
      if null preferences then
        LocalStorage.removeItem (presenceCuePreferencesStorageKey username)
      else
        LocalStorage.setItem
          (presenceCuePreferencesStorageKey username)
          (encodePresenceCuePreferencesJson preferences)

resolveSharedPresenceToneClass :: PresenceCuePreferencesState -> SharedPresenceSegmentLayout -> String
resolveSharedPresenceToneClass presenceCuePreferences layout =
  case layout.state of
    PresenceAtPlace placeId ->
      fromMaybe
        (sharedPresenceLaneToneClass layout.laneIndex)
        ( presenceCueColorTokenClass
            <$> lookupPresenceCuePreference
              presenceCuePreferences.preferences
              { sharedUsername: layout.username
              , placeId
              }
        )
    PresenceUnknown ->
      sharedPresenceLaneToneClass layout.laneIndex
    PresenceInTransit _ ->
      sharedPresenceLaneToneClass layout.laneIndex

presenceCueTargetFromInspection :: SharedPresenceInspection -> Maybe SharedPresenceCueTarget
presenceCueTargetFromInspection inspection =
  case inspection.state of
    PresenceAtPlace placeId ->
      Just
        { sharedUsername: inspection.username
        , placeId
        }
    _ ->
      Nothing

buildSharedPresenceSegmentLayouts :: SharedPresence -> Array SharedPresenceSegmentLayout
buildSharedPresenceSegmentLayouts groups =
  let
    laneCount = max 1 (length groups)
  in
    foldl (<>) []
      ( mapWithIndex
          ( \laneIndex group ->
              mapWithIndex
                ( \segmentIndex segment ->
                    { username: group.username
                    , segmentIndex
                    , laneIndex
                    , laneCount
                    , startMin: minuteOfDay segment.start
                    , duration: durationMinutesBetween segment.start segment.end
                    , state: segment.state
                    }
                )
                group.segments
          )
          groups
      )

emptySharedPresenceRailView :: SharedPresenceRailView
emptySharedPresenceRailView =
  { visibleGroups: []
  , hiddenGroups: []
  , visibleLayouts: []
  , overflow: Nothing
  }

buildSharedPresenceRailView :: SharedPresence -> SharedPresenceRailView
buildSharedPresenceRailView groups =
  let
    visibleGroups = takeArray maxVisibleSharedPresenceUsers groups
    hiddenGroups = dropArray maxVisibleSharedPresenceUsers groups
    hiddenCount = length hiddenGroups
  in
    { visibleGroups
    , hiddenGroups
    , visibleLayouts: buildSharedPresenceSegmentLayouts visibleGroups
    , overflow:
        if hiddenCount <= 0 then
          Nothing
        else
          Just
            { hiddenCount
            , hiddenGroups
            }
    }

toPresenceInspection :: SharedPresenceSegmentLayout -> SharedPresenceInspection
toPresenceInspection layout =
  { username: layout.username
  , segmentIndex: layout.segmentIndex
  , laneIndex: layout.laneIndex
  , laneCount: layout.laneCount
  , startMin: layout.startMin
  , duration: layout.duration
  , state: layout.state
  }

toPresenceInspectionHoverSegment :: SharedPresenceInspection -> SharedPresenceInspectionHoverSegment
toPresenceInspectionHoverSegment inspection =
  { username: inspection.username
  , segmentIndex: inspection.segmentIndex
  }

isSamePresenceInspection :: SharedPresenceInspection -> SharedPresenceSegmentLayout -> Boolean
isSamePresenceInspection inspection layout =
  inspection.username == layout.username && inspection.segmentIndex == layout.segmentIndex

resolvePresenceInspection :: Array SharedPresenceSegmentLayout -> SharedPresenceInspection -> Maybe SharedPresenceInspection
resolvePresenceInspection layouts inspection =
  toPresenceInspection <$> find (isSamePresenceInspection inspection) layouts

presenceInspectionStateText :: SharedPresenceState -> String
presenceInspectionStateText = case _ of
  PresenceUnknown -> "Lieu inconnu"
  PresenceAtPlace placeId -> "Lieu: " <> placeId
  PresenceInTransit { departurePlaceId, arrivalPlaceId } -> "Trajet: " <> departurePlaceId <> " → " <> arrivalPlaceId

minuteToTimeLabel :: Int -> String
minuteToTimeLabel totalMinutes =
  if totalMinutes >= 1440 then
    "24:00"
  else
    DateTime.formatLocalTimeParts (Int.quot totalMinutes 60) (Int.rem totalMinutes 60)

presenceInspectionTimeText :: SharedPresenceInspection -> String
presenceInspectionTimeText inspection =
  "De " <> minuteToTimeLabel inspection.startMin <> " à " <> minuteToTimeLabel (inspection.startMin + inspection.duration)

presenceInspectionAriaLabel :: String -> SharedPresenceSegmentLayout -> String
presenceInspectionAriaLabel username layout =
  username <> " · " <> presenceInspectionStateText layout.state <> " · " <> presenceInspectionTimeText (toPresenceInspection layout)

sharedPresenceSegmentRailClass :: SharedPresenceState -> String
sharedPresenceSegmentRailClass = case _ of
  PresenceUnknown -> "calendar-presence-rail__segment--unknown"
  PresenceAtPlace _ -> "calendar-presence-rail__segment--place"
  PresenceInTransit _ -> "calendar-presence-rail__segment--transit"

sharedPresenceLaneToneClass :: Int -> String
sharedPresenceLaneToneClass laneIndex =
  "calendar-presence-rail__segment--tone-" <> show (Int.rem laneIndex 4)

presenceInspectionHoverCloseDelayMs :: Number
presenceInspectionHoverCloseDelayMs = 140.0

presenceInspectionPanelInitialEstimatePx :: Number
presenceInspectionPanelInitialEstimatePx = 248.0

presenceInspectionPanelViewportPaddingPx :: Number
presenceInspectionPanelViewportPaddingPx = 12.0

validateShareUsername :: String -> Either String String
validateShareUsername raw =
  let
    trimmed = StringCommon.trim raw
  in
    if trimmed == "" then Left "Le nom d'utilisateur est obligatoire." else Right trimmed

responseMessage :: Response Json -> Maybe String
responseMessage response =
  case decodeJson response.body :: Either JsonDecodeError { message :: String } of
    Right payload -> Just payload.message
    Left _ -> Nothing

textResponseMessage :: Response String -> Maybe String
textResponseMessage response =
  case jsonParser response.body of
    Left _ -> Nothing
    Right json ->
      case decodeJson json :: Either JsonDecodeError { message :: String } of
        Right payload -> Just payload.message
        Left _ -> Nothing

tripWriteErrorMessage :: Response Json -> String
tripWriteErrorMessage response =
  case responseMessage response of
    Just "departurePlaceId must reference an existing trip place" -> "Le lieu de départ est invalide."
    Just "arrivalPlaceId must reference an existing trip place" -> "Le lieu d'arrivée est invalide."
    Just "departurePlaceId and arrivalPlaceId must be different" -> "Le lieu de départ et d'arrivée doivent être différents."
    Just "windowEnd must be strictly after windowStart" -> "L'arrivée doit être après le départ."
    Just "trip time window overlaps another trip" -> "Ce trajet chevauche un autre trajet."
    _ -> "Echec de sauvegarde du trajet (HTTP " <> show (unwrap response.status) <> ")."

shareWriteErrorMessage :: Response String -> String
shareWriteErrorMessage response =
  case textResponseMessage response of
    Just "username is required" -> "Le nom d'utilisateur est obligatoire."
    Just "username must not be the authenticated user" -> "Vous ne pouvez pas partager vos trajets avec vous-même."
    Just "username must reference an existing user" -> "Ce nom d'utilisateur est introuvable."
    Just "Unable to decode the body as a TripSharingUser" -> "Le format du partage est invalide."
    _ -> "Echec de mise a jour du partage de trajets (HTTP " <> show (unwrap response.status) <> ")."

subscriptionWriteErrorMessage :: Response String -> String
subscriptionWriteErrorMessage response =
  case textResponseMessage response of
    Just "username is required" -> "Le nom d'utilisateur est obligatoire."
    Just "username must not be the authenticated user" -> "Vous ne pouvez pas vous abonner à vos propres trajets."
    Just "username must reference an existing user" -> "Ce nom d'utilisateur est introuvable."
    Just "Unable to decode the body as a TripSharingUser" -> "Le format de l'abonnement est invalide."
    _ -> "Echec de mise a jour de l'abonnement de trajets (HTTP " <> show (unwrap response.status) <> ")."

createErrorMessage :: Int -> String
createErrorMessage status =
  "Echec de creation de l'item (HTTP " <> show status <> ")."

genericShareWriteErrorMessage :: String
genericShareWriteErrorMessage = "Echec de mise a jour du partage de trajets."

genericSubscriptionWriteErrorMessage :: String
genericSubscriptionWriteErrorMessage = "Echec de mise a jour de l'abonnement de trajets."

isShareSubmitDisabled :: ShareListState -> Boolean
isShareSubmitDisabled shareList =
  shareList.isAdding || shareList.isLoading ||
    case validateShareUsername shareList.usernameDraft of
      Left _ -> true
      Right _ -> false

isUsernameDeleting :: String -> ShareListState -> Boolean
isUsernameDeleting username shareList =
  any (_ == username) shareList.deletingUsernames

markUsernameDeleting :: String -> Array String -> Array String
markUsernameDeleting username usernames =
  if any (_ == username) usernames then usernames else usernames <> [ username ]

unmarkUsernameDeleting :: String -> Array String -> Array String
unmarkUsernameDeleting username usernames =
  filter (_ /= username) usernames

tripPlacesOptions :: TripPlacesState -> Array String
tripPlacesOptions = case _ of
  TripPlacesLoaded places -> places
  _ -> []

tripPlacesReady :: TripPlacesState -> Boolean
tripPlacesReady = case _ of
  TripPlacesLoaded _ -> true
  _ -> false

renderTripPlacesFeedback :: forall w action. TripPlacesState -> HTML w action
renderTripPlacesFeedback tripPlaces =
  case tripPlaces of
    TripPlacesLoading ->
      div [ class_ "calendar-notifications-label" ] [ text "Chargement des lieux de trajet..." ]
    TripPlacesLoaded _ ->
      text ""
    TripPlacesError message ->
      div [ class_ "calendar-error" ] [ text message ]

renderTripPlacesField
  :: String
  -> (String -> CreateFormAction)
  -> String
  -> TripPlacesState
  -> H.ComponentHTML Action Slots Aff
renderTripPlacesField label onChange currentValue tripPlaces =
  let
    emptyLabel = case label of
      "Départ" -> "Choisir le départ"
      _ -> "Choisir l'arrivée"
  in
    div [ class_ "calendar-modal-field" ]
      [ div [ class_ "calendar-notifications-label" ] [ text label ]
      , select
          [ class_ "form-select calendar-input"
          , onValueChange (CreateFormAction <<< onChange)
          , value currentValue
          , disabled (not (tripPlacesReady tripPlaces))
          ]
          ( [ option [ value "" ] [ text emptyLabel ] ]
              <> map (\place -> option [ value place ] [ text place ]) (tripPlacesOptions tripPlaces)
          )
      ]

renderTripPlacesEditField
  :: String
  -> (String -> ViewAction)
  -> String
  -> TripPlacesState
  -> H.ComponentHTML Action Slots Aff
renderTripPlacesEditField label onChange currentValue tripPlaces =
  let
    emptyLabel = case label of
      "Départ" -> "Choisir le départ"
      _ -> "Choisir l'arrivée"
  in
    div [ class_ "calendar-modal-field" ]
      [ div [ class_ "calendar-notifications-label" ] [ text label ]
      , select
          [ class_ "form-select calendar-input"
          , onValueChange (ViewAction <<< onChange)
          , value currentValue
          , disabled (not (tripPlacesReady tripPlaces))
          ]
          ( [ option [ value "" ] [ text emptyLabel ] ]
              <> map (\place -> option [ value place ] [ text place ]) (tripPlacesOptions tripPlaces)
          )
      ]

isCreateSubmitDisabled :: CreateDraft -> TripPlacesState -> Boolean
isCreateSubmitDisabled draft tripPlaces =
  case draft of
    CreateTaskDraft _ -> false
    CreateTripDraft tripDraft ->
      not (tripPlacesReady tripPlaces) || case validateTrip tripDraft of
        Left _ -> true
        Right _ -> false

isEditSubmitDisabled :: EditPanel -> TripPlacesState -> Boolean
isEditSubmitDisabled panel tripPlaces =
  case panel.draft of
    EditTaskDraft _ -> false
    EditTripDraft draft ->
      not (tripPlacesReady tripPlaces)
        ||
          case
            validateTrip
              { departurePlaceId: draft.departurePlaceId
              , arrivalPlaceId: draft.arrivalPlaceId
              , windowStart: draft.windowStart
              , windowEnd: draft.windowEnd
              }
            of
            Left _ -> true
            Right _ -> false

updateEditPanelRecurrence :: RecurrenceDraft -> EditPanel -> EditPanel
updateEditPanelRecurrence recurrence panel =
  case panel.draft of
    EditTaskDraft draft ->
      panel { draft = EditTaskDraft (draft { recurrence = recurrence }), validationError = Nothing }
    EditTripDraft _ ->
      panel

updateEditPanelTitle :: String -> EditPanel -> EditPanel
updateEditPanelTitle raw panel =
  case panel.draft of
    EditTaskDraft draft ->
      panel { draft = EditTaskDraft (draft { title = raw }), validationError = Nothing }
    EditTripDraft _ ->
      panel

updateEditPanelWindowStart :: String -> EditPanel -> EditPanel
updateEditPanelWindowStart raw panel =
  case panel.draft of
    EditTaskDraft draft ->
      panel { draft = EditTaskDraft (draft { windowStart = raw }), validationError = Nothing }
    EditTripDraft draft ->
      panel { draft = EditTripDraft (draft { windowStart = raw }), validationError = Nothing }

updateEditPanelWindowEnd :: String -> EditPanel -> EditPanel
updateEditPanelWindowEnd raw panel =
  case panel.draft of
    EditTaskDraft draft ->
      panel { draft = EditTaskDraft (draft { windowEnd = raw }), validationError = Nothing }
    EditTripDraft draft ->
      panel { draft = EditTripDraft (draft { windowEnd = raw }), validationError = Nothing }

updateEditPanelCategory :: String -> EditPanel -> EditPanel
updateEditPanelCategory raw panel =
  case panel.draft of
    EditTaskDraft draft ->
      panel { draft = EditTaskDraft (draft { category = raw }), validationError = Nothing }
    EditTripDraft _ ->
      panel

updateEditPanelStatus :: ItemStatus -> EditPanel -> EditPanel
updateEditPanelStatus status panel =
  case panel.draft of
    EditTaskDraft draft ->
      panel { draft = EditTaskDraft (draft { status = status }), validationError = Nothing }
    EditTripDraft _ ->
      panel

updateEditPanelDuration :: String -> EditPanel -> EditPanel
updateEditPanelDuration raw panel =
  case panel.draft of
    EditTaskDraft draft ->
      panel { draft = EditTaskDraft (draft { actualDurationMinutes = raw }), validationError = Nothing }
    EditTripDraft _ ->
      panel

updateEditPanelDeparturePlace :: String -> EditPanel -> EditPanel
updateEditPanelDeparturePlace raw panel =
  case panel.draft of
    EditTripDraft draft ->
      panel { draft = EditTripDraft (draft { departurePlaceId = raw }), validationError = Nothing }
    EditTaskDraft _ ->
      panel

updateEditPanelArrivalPlace :: String -> EditPanel -> EditPanel
updateEditPanelArrivalPlace raw panel =
  case panel.draft of
    EditTripDraft draft ->
      panel { draft = EditTripDraft (draft { arrivalPlaceId = raw }), validationError = Nothing }
    EditTaskDraft _ ->
      panel

decodeTaskItemType :: String -> Either JsonDecodeError ItemType
decodeTaskItemType raw =
  case raw of
    "INTENTION" -> Right Task
    "BLOC_PLANIFIE" -> Right Task
    _ -> Left $ UnexpectedValue (encodeJson raw)

calendarItemContent :: CalendarItem -> CalendarItemContent
calendarItemContent (NewCalendarItem { content }) = content
calendarItemContent (ServerCalendarItem { content }) = content

calendarItemWindowStart :: CalendarItem -> DateTime
calendarItemWindowStart item =
  case calendarItemContent item of
    TaskCalendarItemContent content -> content.windowStart
    TripCalendarItemContent content -> content.windowStart

calendarItemWindowEnd :: CalendarItem -> DateTime
calendarItemWindowEnd item =
  case calendarItemContent item of
    TaskCalendarItemContent content -> content.windowEnd
    TripCalendarItemContent content -> content.windowEnd

calendarItemCategory :: CalendarItem -> Maybe String
calendarItemCategory item =
  case calendarItemContent item of
    TaskCalendarItemContent content -> content.category
    TripCalendarItemContent _ -> Nothing

tripRouteLabel :: TripCalendarItemFields -> String
tripRouteLabel { departurePlaceId, arrivalPlaceId } =
  departurePlaceId <> " → " <> arrivalPlaceId

calendarItemPrimaryText :: CalendarItem -> String
calendarItemPrimaryText item =
  case calendarItemContent item of
    TaskCalendarItemContent content -> content.title
    TripCalendarItemContent content -> tripRouteLabel content

calendarItemSecondaryText :: CalendarItem -> String
calendarItemSecondaryText item =
  case calendarItemContent item of
    TaskCalendarItemContent content ->
      formatDateTimeLocal content.windowStart <> " → " <> formatDateTimeLocal content.windowEnd
    TripCalendarItemContent content ->
      "Départ " <> timeLabel content.windowStart <> " · Arrivée " <> timeLabel content.windowEnd

calendarItemTimelineTimeText :: CalendarItem -> String
calendarItemTimelineTimeText item =
  case calendarItemContent item of
    TaskCalendarItemContent content ->
      timeLabel content.windowStart <> " → " <> timeLabel content.windowEnd
    TripCalendarItemContent content ->
      "Départ " <> timeLabel content.windowStart <> " · Arrivée " <> timeLabel content.windowEnd

calendarItemListTimeText :: CalendarItem -> String
calendarItemListTimeText item =
  case calendarItemContent item of
    TaskCalendarItemContent content -> timeLabel content.windowStart
    TripCalendarItemContent _ -> "Trajet"

isTripCalendarItem :: CalendarItem -> Boolean
isTripCalendarItem item =
  case calendarItemContent item of
    TripCalendarItemContent _ -> true
    TaskCalendarItemContent _ -> false

calendarItemCardClass :: CalendarItem -> String
calendarItemCardClass item =
  "row list-group-item entity-card calendar-card"
    <> guard (isTripCalendarItem item) " calendar-card--trip"

calendarItemTimelineCardClass :: CalendarItem -> String
calendarItemTimelineCardClass item =
  "calendar-calendar-card"
    <> guard (not (isTripCalendarItem item)) " calendar-calendar-item--task"
    <> guard (isTripCalendarItem item) " calendar-calendar-item--trip"

calendarItemSupportsEdit :: CalendarItem -> Boolean
calendarItemSupportsEdit = case _ of
  ServerCalendarItem _ -> true
  _ -> false

calendarItemSupportsDragEdit :: CalendarItem -> Boolean
calendarItemSupportsDragEdit = case _ of
  ServerCalendarItem { content: TaskCalendarItemContent _ } -> true
  _ -> false

isItemOnDate :: String -> CalendarItem -> Boolean
isItemOnDate dateStr item =
  case parseDateLocal dateStr of
    Nothing -> false
    Just dateValue -> date (calendarItemWindowStart item) == dateValue

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

sortItems :: SortMode -> Array CalendarItem -> Array CalendarItem
sortItems mode items =
  case mode of
    SortByStatus -> sortBy compareStatus items
    SortByCategory -> sortBy compareCategory items
    SortByTime -> sortBy compareTime items
  where
  compareTime a b = compare (calendarItemWindowStart a) (calendarItemWindowStart b)

  compareStatus a b = compare (statusRank (calendarItemStatus a)) (statusRank (calendarItemStatus b))

  compareCategory a b = compare (categoryKey (calendarItemCategory a)) (categoryKey (calendarItemCategory b))

  calendarItemStatus item =
    case calendarItemContent item of
      TaskCalendarItemContent content -> Just content.status
      TripCalendarItemContent _ -> Nothing

  statusRank (Just Todo) = 0
  statusRank (Just InProgress) = 1
  statusRank (Just Done) = 2
  statusRank (Just Canceled) = 3
  statusRank Nothing = 4

  categoryKey Nothing = "~~~"
  categoryKey (Just value) = value

minuteOfDay :: DateTime -> Int
minuteOfDay raw =
  let
    t = time raw
  in
    (fromEnum (hour t) * 60) + fromEnum (minute t)

data ItemType = Task

derive instance itemTypeGeneric :: Generic ItemType _
derive instance itemTypeEq :: Eq ItemType
instance itemTypeShow :: Show ItemType where
  show = genericShow

data ItemStatus = Todo | InProgress | Done | Canceled

derive instance itemStatusGeneric :: Generic ItemStatus _
derive instance itemStatusEq :: Eq ItemStatus
instance itemStatusShow :: Show ItemStatus where
  show = genericShow

type TaskCalendarItemFields =
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

type TripCalendarItemFields =
  { windowStart :: DateTime
  , windowEnd :: DateTime
  , departurePlaceId :: String
  , arrivalPlaceId :: String
  }

type TripDraft =
  { departurePlaceId :: String
  , arrivalPlaceId :: String
  , windowStart :: String
  , windowEnd :: String
  }

data CalendarItemContent
  = TaskCalendarItemContent TaskCalendarItemFields
  | TripCalendarItemContent TripCalendarItemFields

derive instance calendarItemContentGeneric :: Generic CalendarItemContent _
derive instance calendarItemContentEq :: Eq CalendarItemContent
instance calendarItemContentShow :: Show CalendarItemContent where
  show = genericShow

data CalendarItem
  = NewCalendarItem { content :: CalendarItemContent }
  | ServerCalendarItem { content :: CalendarItemContent, id :: String }

derive instance calendarItemGeneric :: Generic CalendarItem _
derive instance calendarItemEq :: Eq CalendarItem
instance calendarItemShow :: Show CalendarItem where
  show = genericShow

type TaskDraft =
  { itemType :: ItemType
  , title :: String
  , windowStart :: String
  , windowEnd :: String
  , category :: String
  , status :: ItemStatus
  , actualDurationMinutes :: String
  , recurrence :: RecurrenceDraft
  }

data CreateDraft
  = CreateTaskDraft TaskDraft
  | CreateTripDraft TripDraft

derive instance createDraftEq :: Eq CreateDraft
derive instance createDraftGeneric :: Generic CreateDraft _

instance showCreateDraft :: Show CreateDraft where
  show = genericShow

data TripPlacesState
  = TripPlacesLoading
  | TripPlacesLoaded (Array String)
  | TripPlacesError String

derive instance tripPlacesStateEq :: Eq TripPlacesState
derive instance tripPlacesStateGeneric :: Generic TripPlacesState _

instance showTripPlacesState :: Show TripPlacesState where
  show = genericShow

type ShareListState =
  { usernames :: Array String
  , hasLoaded :: Boolean
  , isLoading :: Boolean
  , loadError :: Maybe String
  , usernameDraft :: String
  , submitError :: Maybe String
  , isAdding :: Boolean
  , deletingUsernames :: Array String
  }

data PresenceCueColorToken
  = CueBlue
  | CueGreen
  | CueAmber
  | CueRose
  | CueViolet
  | CueSlate

derive instance presenceCueColorTokenEq :: Eq PresenceCueColorToken
derive instance presenceCueColorTokenGeneric :: Generic PresenceCueColorToken _

instance showPresenceCueColorToken :: Show PresenceCueColorToken where
  show = genericShow

newtype SharedPresenceCuePreference = SharedPresenceCuePreference
  { sharedUsername :: String
  , placeId :: String
  , colorToken :: PresenceCueColorToken
  }

derive instance sharedPresenceCuePreferenceEq :: Eq SharedPresenceCuePreference
derive instance sharedPresenceCuePreferenceGeneric :: Generic SharedPresenceCuePreference _

instance showSharedPresenceCuePreference :: Show SharedPresenceCuePreference where
  show = genericShow

type SharedPresenceCueTarget =
  { sharedUsername :: String
  , placeId :: String
  }

type PresenceCuePreferencesState =
  { ownerUsername :: Maybe String
  , preferences :: Array SharedPresenceCuePreference
  }

presenceCuePreferencesInitialState :: PresenceCuePreferencesState
presenceCuePreferencesInitialState =
  { ownerUsername: Nothing
  , preferences: []
  }

type SharedPeriodTrip =
  { windowStart :: DateTime
  , windowEnd :: DateTime
  , departurePlaceId :: String
  , arrivalPlaceId :: String
  }

type SharedPeriodTripGroup =
  { username :: String
  , trips :: Array SharedPeriodTrip
  }

data SharedPresenceLoadState
  = SharedPresenceLoading
  | SharedPresenceLoaded (Array SharedPresenceGroup)
  | SharedPresenceError String

derive instance sharedPresenceLoadStateEq :: Eq SharedPresenceLoadState
derive instance sharedPresenceLoadStateGeneric :: Generic SharedPresenceLoadState _

instance showSharedPresenceLoadState :: Show SharedPresenceLoadState where
  show = genericShow

data SharedPresenceState
  = PresenceUnknown
  | PresenceAtPlace String
  | PresenceInTransit { departurePlaceId :: String, arrivalPlaceId :: String }

derive instance sharedPresenceStateEq :: Eq SharedPresenceState
derive instance sharedPresenceStateGeneric :: Generic SharedPresenceState _

instance showSharedPresenceState :: Show SharedPresenceState where
  show = genericShow

type SharedPresenceSegment =
  { start :: DateTime
  , end :: DateTime
  , state :: SharedPresenceState
  }

type SharedPresenceGroup =
  { username :: String
  , segments :: Array SharedPresenceSegment
  }

type SharedPresence = Array SharedPresenceGroup

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

data TripValidationError
  = TripDeparturePlaceMissing
  | TripArrivalPlaceMissing
  | TripWindowStartInvalid
  | TripWindowEndInvalid
  | TripWindowOrderInvalid
  | TripPlacesMustDiffer

derive instance tripValidationErrorGeneric :: Generic TripValidationError _
derive instance tripValidationErrorEq :: Eq TripValidationError

instance tripValidationErrorShow :: Show TripValidationError where
  show = genericShow

data SortMode
  = SortByTime
  | SortByStatus
  | SortByCategory

derive instance sortModeGeneric :: Generic SortMode _
derive instance sortModeEq :: Eq SortMode
instance sortModeShow :: Show SortMode where
  show = genericShow

data CalendarView
  = ViewDay
  | ViewWeek
  | ViewMonth

derive instance agendaViewGeneric :: Generic CalendarView _
derive instance agendaViewEq :: Eq CalendarView
instance agendaViewShow :: Show CalendarView where
  show = genericShow

instance presenceCueColorTokenEncodeJson :: EncodeJson PresenceCueColorToken where
  encodeJson = encodeJson <<< presenceCueColorTokenValue

instance presenceCueColorTokenDecodeJson :: DecodeJson PresenceCueColorToken where
  decodeJson json = do
    raw <- decodeJson json
    case parsePresenceCueColorToken raw of
      Just token -> pure token
      Nothing -> Left $ UnexpectedValue json

instance sharedPresenceCuePreferenceEncodeJson :: EncodeJson SharedPresenceCuePreference where
  encodeJson (SharedPresenceCuePreference preference) =
    "sharedUsername" := preference.sharedUsername
      ~> "placeId" := preference.placeId
      ~> "colorToken" := preference.colorToken
      ~> jsonEmptyObject

instance sharedPresenceCuePreferenceDecodeJson :: DecodeJson SharedPresenceCuePreference where
  decodeJson json = do
    obj <- decodeJson json
    sharedUsername <- obj .: "sharedUsername"
    placeId <- obj .: "placeId"
    colorToken <- obj .: "colorToken"
    pure
      ( SharedPresenceCuePreference
          { sharedUsername
          , placeId
          , colorToken
          }
      )

instance itemTypeEncodeJson :: EncodeJson ItemType where
  encodeJson Task = encodeJson "BLOC_PLANIFIE"

instance itemTypeDecodeJson :: DecodeJson ItemType where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "INTENTION" -> pure Task
      "BLOC_PLANIFIE" -> pure Task
      _ -> Left $ UnexpectedValue json

instance itemStatusEncodeJson :: EncodeJson ItemStatus where
  encodeJson Todo = encodeJson "TODO"
  encodeJson InProgress = encodeJson "IN_PROGRESS"
  encodeJson Done = encodeJson "DONE"
  encodeJson Canceled = encodeJson "CANCELED"

instance itemStatusDecodeJson :: DecodeJson ItemStatus where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "TODO" -> pure Todo
      "IN_PROGRESS" -> pure InProgress
      "DONE" -> pure Done
      "CANCELED" -> pure Canceled
      _ -> Left $ UnexpectedValue json

instance calendarItemDecodeJson :: DecodeJson CalendarItem where
  decodeJson json = do
    obj <- decodeJson json
    rawType <- obj .: "type"
    content <-
      case rawType of
        "trip" -> do
          windowStartRaw <- obj .: "windowStart"
          windowEndRaw <- obj .: "windowEnd"
          windowStart <- maybe (Left $ UnexpectedValue (encodeJson windowStartRaw)) Right (DateTime.parseLocalDateTime windowStartRaw)
          windowEnd <- maybe (Left $ UnexpectedValue (encodeJson windowEndRaw)) Right (DateTime.parseLocalDateTime windowEndRaw)
          departurePlaceId <- obj .: "departurePlaceId"
          arrivalPlaceId <- obj .: "arrivalPlaceId"
          pure
            ( TripCalendarItemContent
                { windowStart
                , windowEnd
                , departurePlaceId
                , arrivalPlaceId
                }
            )
        _ -> do
          itemType <- decodeTaskItemType rawType
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
          pure
            ( TaskCalendarItemContent
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
            )
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
encodeCalendarContent = case _ of
  TaskCalendarItemContent { itemType, title, windowStart, windowEnd, status, sourceItemId, actualDurationMinutes, category, recurrenceRule, recurrenceExceptionDates } ->
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
  TripCalendarItemContent { windowStart, windowEnd, departurePlaceId, arrivalPlaceId } ->
    "type" := "trip"
      ~> "windowStart" := formatDateTimeLocal windowStart
      ~> "windowEnd" := formatDateTimeLocal windowEnd
      ~> "departurePlaceId" := departurePlaceId
      ~> "arrivalPlaceId" := arrivalPlaceId
      ~> jsonEmptyObject

-- BEGIN src/Calendar/purs
type SyncState =
  { updateError :: Maybe String }

syncInitialState :: SyncState
syncInitialState =
  { updateError: Nothing }

_syncUpdateError :: Lens' SyncState (Maybe String)
_syncUpdateError = lens _.updateError (_ { updateError = _ })

renderUpdateError :: forall w. String -> HTML w Action
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

-- END src/Calendar/purs
