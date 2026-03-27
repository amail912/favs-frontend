module Pages.Calendar
  ( component
  , decodeCalendarItemsResponse
  , CalendarItem(..)
  , CalendarItemContent
  , TaskDraft
  , ItemStatus(..)
  , ItemType(..)
  , SortMode(..)
  , ValidationError(..)
  , toNewTask
  , buildTimelineLayout
  , MobileOverlapStack
  , MobileHiddenCard
  , buildMobileOverlapStacks
  , applyMobileOverlapPromotions
  , toTimelineBlock
  , EditError(..)
  , applyEditDraft
  , buildEditDraft
  , durationMinutesBetween
  , sortItems
  , validateTask
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
import Api.Calendar (createItemResponse, getItemsResponse, updateItemResponse)
import Calendar.Recurrence (RecurrenceDraft, RecurrenceRule, defaultRecurrenceDraft, draftFromRecurrence, draftToRecurrence)
import Calendar.Recurrence as Recurrence
import Calendar.ExImport.Export as Export
import Calendar.ExImport.Import as Import
import Control.Monad.Except (ExceptT(..), withExceptT)
import Control.Monad.State.Trans (get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Array (any, filter, find, findIndex, foldl, length, mapMaybe, mapWithIndex, null, sortBy, uncons, updateAt)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (fold)
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
import Halogen (Component, ComponentHTML, HalogenM, Slot, defaultEval, getRef, mkComponent, mkEval) as H
import Halogen (subscribe)
import Halogen.HTML (HTML, button, div, h2, i, input, li, option, section, select, slot, span, text, ul)
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events (onClick, onDragEnter, onDragOver, onDrop, onMouseDown, onValueChange, onKeyDown, onDragEnd, onDragStart, onScroll, onTouchStart, onTouchMove, onTouchEnd, onTouchCancel)
import Halogen.HTML.Properties (attr, style, IProp, value, placeholder, type_, draggable, ref, disabled)
import Halogen.Query.Event as HQE
import Type.Proxy (Proxy(..))
import Ui.Errors (FatalError, handleError, toFatalError)
import Ui.Focus (focusElement, openDateInputPicker)
import Ui.Modal (renderBottomSheet, renderModal) as Modal
import Ui.Utils (class_)
import Web.Event.Event (EventType(..), preventDefault)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KeyboardEventTypes
import Web.UIEvent.MouseEvent as MouseEvent
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
import Web.DOM.Element (Element, getBoundingClientRect, setScrollTop)
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

-- foldl comes from Data.Array in this module

-- BEGIN src/Pages/Calendar.purs
type NoOutput = Void
type AgendaAppM = H.HalogenM State Action Slots NoOutput Aff
type ErrorAgendaAppM = ExceptT FatalError AgendaAppM

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

instance toActionViewAction :: ToAction ViewAction where
  toAction = ViewAction

renderAgendaView
  :: forall w
   . CalendarView
  -> String
  -> String
  -> Array CalendarItem
  -> Boolean
  -> Array MobileOverlapPromotion
  -> Maybe String
  -> Maybe Int
  -> HTML w Action
renderAgendaView viewMode focusDate todayDate items isMobile promotedOverlaps draggingId dragHoverIndex =
  case viewMode of
    ViewDay ->
      renderDayCalendar focusDate todayDate items isMobile promotedOverlaps draggingId dragHoverIndex
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

prefillCreateDraft :: ItemType -> String -> TaskDraft -> TaskDraft
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

toExportItem :: CalendarItem -> Export.Item
toExportItem item =
  let
    content = calendarItemContent item
  in
    Export.Item
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

_calendarDraft :: Lens' State TaskDraft
_calendarDraft = _calendar <<< _draft

_calendarValidationError :: Lens' State (Maybe String)
_calendarValidationError = _calendar <<< _validationError

_calendarLastCreateType :: Lens' State ItemType
_calendarLastCreateType = _calendar <<< _lastCreateType

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
            lastType = st ^. _calendarLastCreateType
            baseDraft = emptyDraft { itemType = lastType }
            nextDraft = prefillCreateDraft lastType (st ^. _viewFocusDatePage) baseDraft
          modify_ ((_calendarDraft .~ nextDraft) <<< (_calendarValidationError .~ Nothing))
        ViewOpenEdit _ -> focusModal
        ViewOpenEditFromDoubleClick _ -> focusModal
        ViewChangedAction _ -> scheduleDayTimelineFocus
        ViewFocusDateChanged _ -> scheduleDayTimelineFocus
        ViewCloseCreate -> do
          st <- get
          let
            lastType = st ^. _calendarLastCreateType
          modify_ ((_calendarDraft .~ (emptyDraft { itemType = lastType })) <<< (_calendarValidationError .~ Nothing))
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
              lastType = st' ^. _calendarLastCreateType
            modify_ ((_calendarDraft .~ (emptyDraft { itemType = lastType })) <<< (_calendarValidationError .~ Nothing))
          Just _ -> handleViewAction ViewCloseModal
          Nothing -> pure unit
      else pure unit
    GlobalResize -> do
      viewport <- liftEffect $ window >>= Window.innerWidth
      handleViewAction (ViewSetIsMobile (viewport <= 768))
    CreateRecurrenceCmd (Recurrence.RecurrenceApplied draft) ->
      modify_
        ( (_calendar <<< _draftRecurrenceS .~ draft)
            <<< (_calendarValidationError .~ Nothing)
        )
    EditRecurrenceCmd (Recurrence.RecurrenceApplied draft) ->
      modify_
        ((_view <<< _viewEditPanel) %~ map (\panel -> panel { draft = panel.draft { recurrence = draft }, validationError = Nothing }))
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
            durationMinutesBetween (calendarItemContent item).windowStart (calendarItemContent item).windowEnd
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
            durationMinutesBetween (calendarItemContent item).windowStart (calendarItemContent item).windowEnd
        startIndex =
          draggedItem <#> \item ->
            Int.quot (minuteOfDay (calendarItemContent item).windowStart) 5
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
              let mins = durationMinutesBetween (calendarItemContent item).windowStart (calendarItemContent item).windowEnd
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
      liftEffect $ preventDefault (TouchEvent.toEvent ev)
      st <- get
      let items = st ^. _calendarItems
      case st ^. (_mouseDrag <<< _draggingId) of
        Nothing ->
          modify_ (_mouseDrag %~ clearTouchPendingState)
        Just draggingId -> do
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
              let mins = durationMinutesBetween (calendarItemContent item).windowStart (calendarItemContent item).windowEnd
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
  refreshItems
  scheduleDayTimelineFocus

refreshItems :: ErrorAgendaAppM Unit
refreshItems = do
  jsonResponse <- withExceptT toFatalError $ ExceptT $ liftAff getItemsResponse
  items <- decodeCalendarItemsResponse jsonResponse # pure >>> ExceptT
  modify_ (_calendarItems .~ items)

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
          now <- liftEffect nowDateTime
          let
            itemsForDate = sortItems SortByTime (filter (isItemOnDate focusDate) (stCurrent ^. _calendarItems))
            target = computeDayFocusTarget focusDate now itemsForDate
            minuteOffset = focusMinuteForTarget now itemsForDate target
            scrollTop = focusScrollTopForMinute (stCurrent ^. (_view <<< _viewIsMobile)) minuteOffset
          timelineRef <- lift $ H.getRef (wrap "day-timeline-scroll")
          case timelineRef of
            Nothing -> pure unit
            Just timeline -> do
              modify_
                ( _view
                    %~
                      ( (_viewDayFocusIgnoreScroll .~ true)
                          <<< (_viewDayFocusApplied .~ true)
                      )
                )
              liftEffect $ setScrollTop scrollTop timeline
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
  case validateTask (st ^. _calendarDraft) of
    Left err -> modify_ (_calendarValidationError .~ Just (validationErrorMessage err))
    Right validDraft ->
      case toNewTask validDraft of
        Left err ->
          modify_ (_calendarValidationError .~ Just err)
        Right item ->
          do
            _ <- createItem item
            modify_
              ( (_calendarDraft .~ emptyDraft)
                  <<< (_calendarValidationError .~ Nothing)
                  <<< ((_view <<< _viewActiveModal) .~ Nothing)
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
render { calendar, sync, mouseDrag, view } =
  let
    { items } = calendar
    { updateError } = sync
    draggingId = mouseDrag.draggingId
    dragHoverIndex = mouseDrag.dragHoverIndex
    { viewMode, focusDate, todayDate, isMobile, promotedOverlaps } = view
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
                    [ renderAgendaView viewMode focusDate todayDate sortedItems isMobile promotedOverlaps draggingId dragHoverIndex ]
                ]
            , div [ class_ "calendar-side" ]
                []
            ]
        ]
          <> [ renderAgendaModals agendaModalsInput ]
          <>
            [ renderCreateFab ]
      )

type AgendaModalsInput =
  { activeModal :: Maybe AgendaModal
  , overlapSheet :: Maybe OverlapSheet
  , exportItems :: Array Export.Item
  , draft :: TaskDraft
  , validationError :: Maybe String
  , editPanel :: Maybe EditPanel
  }

renderAgendaModals :: AgendaModalsInput -> H.ComponentHTML Action Slots Aff
renderAgendaModals { activeModal, overlapSheet, exportItems, draft, validationError, editPanel } =
  let
    renderModal title content = Modal.renderModal title content (ViewAction ViewCloseModal) (ViewAction ViewCloseModal)
    renderExportModal items =
      slot (Proxy :: _ "export") ExportModal Export.component { items } absurd
  in
    maybe (div [] [])
      case _ of
        ModalTools -> renderModal "Actions" [ map toAction renderToolsContent ]
        ModalCreateItem -> Modal.renderModal "Créer un item" [ renderCreateContent draft validationError ]
          (ViewAction ViewCloseCreate)
          SyncSubmitTask
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
        ModalEditItem -> case editPanel of
          Nothing -> text ""
          Just panel ->
            Modal.renderModal "Modifier l'item"
              [ renderEditContent panel ]
              (ViewAction ViewEditCancel)
              (ViewAction ViewEditSave)
      activeModal

buildAgendaModalsInput :: State -> AgendaModalsInput
buildAgendaModalsInput { calendar, view } =
  let
    { items, draft, validationError } = calendar
    { activeModal, overlapSheet, editPanel } = view
  in
    { activeModal
    , overlapSheet
    , exportItems: map toExportItem items
    , draft
    , validationError
    , editPanel
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
  -> Boolean
  -> Array MobileOverlapPromotion
  -> Maybe String
  -> Maybe Int
  -> HTML w Action
renderDayCalendar focusDate todayDate items isMobile promotedOverlaps draggingId dragHoverIndex =
  let
    itemsForDate = filter (isItemOnDate focusDate) items
    sorted = sortItems SortByTime itemsForDate
    dragPreview = renderDayDragPreview isMobile draggingId dragHoverIndex itemsForDate
    timelineItems =
      if isMobile then
        map (renderMobileOverlapStack draggingId) (applyMobileOverlapPromotions promotedOverlaps (buildMobileOverlapStacks sorted))
      else
        map (renderTimelineItem isMobile draggingId) (buildTimelineLayout sorted)
    dayLabel = DateTime.formatCalendarDayDateLabelWithReference focusDate todayDate
  in
    if null itemsForDate then emptyAgenda
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
                [ class_ "calendar-calendar-grid"
                , onDragEnter (\ev -> DragAction { log: "DragEnterCalendar@calendar-grid", dragAction: DragOverCalendar ev })
                , onDragOver (\ev -> DragAction { log: "DragOverCalendar@calendar-grid", dragAction: DragOverCalendar ev })
                , onDrop (\ev -> DragAction { log: "DropOnCalendar@calendar-grid", dragAction: DropOnCalendar ev })
                ]
                [ div [ class_ "calendar-calendar-lines" ]
                    (map renderHourLine (enumFromTo 0 23))
                , maybe (text "") renderDropIndicator dragHoverIndex
                , maybe (text "") identity dragPreview
                , div
                    [ class_ $
                        "calendar-calendar-items"
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
      duration = durationMinutesBetween (calendarItemContent item).windowStart (calendarItemContent item).windowEnd
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
  -> TimelineLayout
  -> HTML w Action
renderTimelineItem isMobile draggingId layout =
  let
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
    dragProps = dragCalendarHandlers isMobile layout.item
    touchProps = touchCalendarHandlers isMobile layout.item
    editProps = editHandlers isMobile draggingId layout.item
  in
    div
      ( [ class_ "calendar-calendar-item"
        , style inlineStyle
        ] <> editProps
      )
      [ renderTimelineEditButton isMobile layout.item
      , div
          ([ class_ $ "calendar-calendar-card calendar-calendar-item--task" <> draggingClass ] <> dragProps)
          [ div touchProps [ renderTimelineCardContent layout.item ] ]
      ]

renderMobileOverlapStack
  :: forall w
   . Maybe String
  -> MobileOverlapStack
  -> HTML w Action
renderMobileOverlapStack draggingId stack =
  let
    draggingClass =
      case { draggingId, item: stack.topItem } of
        { draggingId: Just activeId, item: ServerCalendarItem { id } } | activeId == id ->
          " calendar-calendar-item--dragging"
        _ -> ""
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
      [ class_ "calendar-calendar-stack"
      , style inlineStyle
      ]
      ( map (renderMobileOverlapShadow stack.hiddenCount stack.startMin) stack.hiddenCards
          <>
            [ div
                ([ class_ $ "calendar-calendar-card calendar-calendar-item--task calendar-calendar-stack-top" <> draggingClass ] <> dragProps)
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
  let
    content = calendarItemContent item
  in
    div [ class_ "calendar-calendar-content" ]
      [ div [ class_ "calendar-calendar-meta" ]
          [ div [ class_ "calendar-calendar-item-time" ]
              [ text $ timeLabel content.windowStart <> " → " <> timeLabel content.windowEnd ]
          , div [ class_ "calendar-calendar-item-title" ] [ text content.title ]
          , renderCategory content.category
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
    content = calendarItemContent item
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
              [ text $ timeLabel content.windowStart <> " → " <> timeLabel content.windowEnd ]
          , div [ class_ "calendar-overlap-sheet__title" ] [ text content.title ]
          , renderCategory content.category
          , if isCurrentTop then
              div [ class_ "calendar-overlap-sheet__state" ] [ text "Visible" ]
            else
              text ""
          ]
      ]

renderTimelineEditButton :: forall w. Boolean -> CalendarItem -> HTML w Action
renderTimelineEditButton isMobile item =
  if isMobile then text ""
  else
    case item of
      ServerCalendarItem _ ->
        button
          [ class_ "btn btn-sm btn-outline-secondary calendar-edit calendar-edit--timeline"
          , attr (AttrName "aria-label") "Editer"
          , onMouseDown (const (ViewAction (ViewOpenEdit item)))
          , onClick (const (ViewAction (ViewOpenEdit item)))
          ]
          [ i [ class_ "bi bi-pencil" ] [] ]
      _ -> text ""

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
    content = calendarItemContent item
    editProps = editHandlers isMobile Nothing item
  in
    li ([ class_ "row list-group-item entity-card calendar-card" ] <> editProps)
      [ div [ class_ "col entity-card-body" ]
          [ div [ class_ "calendar-card-time" ] [ text (timeLabel content.windowStart) ]
          , div [ class_ "calendar-card-title" ] [ text content.title ]
          , div [ class_ "calendar-card-window" ]
              [ text $ formatDateTimeLocal content.windowStart <> " → " <> formatDateTimeLocal content.windowEnd ]
          , renderCategory content.category
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
  | CreateFormDraftTypeChanged ItemType
  | CreateFormDraftStartChanged String
  | CreateFormDraftEndChanged String
  | CreateFormDraftCategoryChanged String
  | CreateFormDraftStatusChanged String
  | CreateFormDraftDurationChanged String

applyCreateFormAction :: CreateFormAction -> AgendaAppM Unit
applyCreateFormAction action =
  case action of
    CreateFormDraftTitleChanged title ->
      modify_ (_calendar %~ ((_draftTitleS .~ title) <<< (_validationError .~ Nothing)))
    CreateFormDraftTypeChanged itemType -> do
      modify_ (_calendar %~ ((_draftItemTypeS .~ itemType) <<< (_lastCreateType .~ itemType) <<< (_validationError .~ Nothing)))
      st <- get
      let updatedDraft = prefillCreateDraft itemType (st ^. _viewFocusDatePage) (st ^. _calendarDraft)
      modify_ (_calendarDraft .~ updatedDraft)
    CreateFormDraftStartChanged windowStart ->
      modify_ (_calendar %~ ((_draftWindowStartS .~ windowStart) <<< (_validationError .~ Nothing)))
    CreateFormDraftEndChanged windowEnd ->
      modify_ (_calendar %~ ((_draftWindowEndS .~ windowEnd) <<< (_validationError .~ Nothing)))
    CreateFormDraftCategoryChanged category ->
      modify_ (_calendar %~ ((_draftCategoryS .~ category) <<< (_validationError .~ Nothing)))
    CreateFormDraftStatusChanged raw ->
      modify_ (_calendar %~ ((_draftStatusS .~ parseStatus raw) <<< (_validationError .~ Nothing)))
    CreateFormDraftDurationChanged raw ->
      modify_ (_calendar %~ ((_draftDurationS .~ raw) <<< (_validationError .~ Nothing)))

renderCreateContent
  :: TaskDraft
  -> Maybe String
  -> H.ComponentHTML Action Slots Aff
renderCreateContent draft validationError =
  div [ class_ "calendar-modal-stack" ]
    [ field "Tâche" taskTypeDisplay
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

  taskTypeDisplay =
    div [ class_ "badge rounded-pill text-bg-secondary" ] [ text "Tâche" ]

  titleInput =
    input
      [ class_ "form-control calendar-input"
      , placeholder "Titre"
      , onValueChange (CreateFormAction <<< CreateFormDraftTitleChanged)
      , onKeyDown (\ev -> SyncDraftTitleKeyDown (KE.key ev))
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
      , option [ value "in_progress" ] [ text "En cours" ]
      , option [ value "done" ] [ text "Terminé" ]
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

-- END src/Calendar/Calendar/Primary.purs

-- BEGIN src/Calendar/Calendar/State.purs
type CalendarState =
  { items :: Array CalendarItem
  , draft :: TaskDraft
  , validationError :: Maybe String
  , lastCreateType :: ItemType
  }

calendarInitialState :: CalendarState
calendarInitialState =
  { items: []
  , draft: emptyDraft
  , validationError: Nothing
  , lastCreateType: Task
  }

emptyDraft :: TaskDraft
emptyDraft =
  { itemType: Task
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

_draft :: Lens' CalendarState TaskDraft
_draft = prop (Proxy :: _ "draft")

_validationError :: Lens' CalendarState (Maybe String)
_validationError = prop (Proxy :: _ "validationError")

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
    content = calendarItemContent item
    startMin = minuteOfDay content.windowStart
    endMinRaw = minuteOfDay content.windowEnd
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
    fold
      [ "new:"
      , content.title
      , ":"
      , show content.windowStart
      , ":"
      , show content.windowEnd
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
  | ModalCreateItem
  | ModalEditItem
  | ModalOverlapGroup

derive instance eqAgendaModal :: Eq AgendaModal

type OverlapSheet =
  { groupKey :: String
  , items :: Array CalendarItem
  , topItem :: CalendarItem
  , hiddenCount :: Int
  }

type ViewState =
  { viewMode :: CalendarView
  , focusDate :: String
  , todayDate :: String
  , activeModal :: Maybe AgendaModal
  , overlapSheet :: Maybe OverlapSheet
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
  | ViewEditSave
  | ViewEditCancel
  | ViewSetIsMobile Boolean

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
    modify_ (_view <<< _viewActiveModal .~ Just modal)
  ViewOpenOverlapSheet overlapSheet ->
    modify_
      ( _view
          %~
            ( (_viewOverlapSheet .~ Just overlapSheet)
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
  ViewCloseModal ->
    modify_ ((_view <<< _viewActiveModal .~ Nothing) <<< (_view <<< _viewOverlapSheet .~ Nothing))
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
    modify_ (_view <<< _viewEditPanel %~ map (\panel -> panel { draft = panel.draft { title = raw }, validationError = Nothing }))
  ViewEditStartChanged raw ->
    modify_ (_view <<< _viewEditPanel %~ map (\panel -> panel { draft = panel.draft { windowStart = raw }, validationError = Nothing }))
  ViewEditEndChanged raw ->
    modify_ (_view <<< _viewEditPanel %~ map (\panel -> panel { draft = panel.draft { windowEnd = raw }, validationError = Nothing }))
  ViewEditCategoryChanged raw ->
    modify_ (_view <<< _viewEditPanel %~ map (\panel -> panel { draft = panel.draft { category = raw }, validationError = Nothing }))
  ViewEditStatusChanged raw ->
    modify_ (_view <<< _viewEditPanel %~ map (\panel -> panel { draft = panel.draft { status = parseStatus raw }, validationError = Nothing }))
  ViewEditDurationChanged raw ->
    modify_ (_view <<< _viewEditPanel %~ map (\panel -> panel { draft = panel.draft { actualDurationMinutes = raw }, validationError = Nothing }))
  ViewEditSave -> do
    st <- get
    case st ^. (_view <<< _viewEditPanel) of
      Nothing -> pure unit
      Just panel ->
        case applyEditDraft panel.draft panel.item of
          Left err ->
            modify_ (_view <<< _viewEditPanel .~ Just (panel { validationError = Just (editErrorMessage err) }))
          Right updatedItem -> do
            resp <- updateItem panel.draft.itemId updatedItem
            if statusOk resp then modify_ (_syncUpdateErrorState .~ Nothing)
            else modify_ (_syncUpdateErrorState .~ Just (updateErrorMessage (unwrap resp.status)))
            refreshItems
            modify_
              ((_view <<< _viewEditPanel .~ Nothing) <<< (_view <<< _viewActiveModal .~ Nothing))
  ViewEditCancel ->
    modify_ ((_view <<< _viewEditPanel .~ Nothing) <<< (_view <<< _viewActiveModal .~ Nothing))
  ViewSetIsMobile isMobile ->
    modify_ (_view <<< _viewIsMobile .~ isMobile)

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
    [ button [ class_ "btn btn-outline-secondary", onClick (const (ViewOpenModal ModalImportCsv)) ] [ text "Import CSV" ]
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
          , div [ class_ "badge rounded-pill text-bg-secondary" ] [ text "Tâche" ]
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
      , maybe (text "") (\msg -> div [ class_ "calendar-error" ] [ text msg ]) panel.validationError
      ]

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
dragCalendarHandlers false (ServerCalendarItem { id }) =
  [ draggable true
  , onDragStart (\ev -> DragAction { log: "DragStart@calendar-card", dragAction: DragStart id ev })
  , onDragEnd (const (DragAction { log: "DragEnd@calendar-card", dragAction: DragEnd }))
  ]
dragCalendarHandlers false _ = []

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
touchCalendarHandlers true (ServerCalendarItem { id }) =
  [ onTouchStart (\ev -> DragAction { log: "TouchStart@calendar-card", dragAction: TouchDragStart id ev })
  , onTouchCancel (const (DragAction { log: "TouchCancel@calendar-card", dragAction: TouchDragCancel }))
  ]
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
    taskDraft :: TaskDraft
    taskDraft =
      { itemType: draft.itemType
      , title: draft.title
      , windowStart: draft.windowStart
      , windowEnd: draft.windowEnd
      , category: draft.category
      , status: draft.status
      , actualDurationMinutes: draft.actualDurationMinutes
      , recurrence: draft.recurrence
      }
  case validateTask taskDraft of
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

focusMinuteForTarget :: DateTime -> Array CalendarItem -> DayFocusTarget -> Int
focusMinuteForTarget now itemsForDate target =
  case target of
    FocusCurrentTime ->
      max 0 (minuteOfDay now - 120)
    FocusFirstTask ->
      case uncons (sortItems SortByTime itemsForDate) of
        Nothing -> 0
        Just { head } ->
          max 0 (minuteOfDay (calendarItemContent head).windowStart - 30)
    FocusTop ->
      0

focusScrollTopForMinute :: Boolean -> Int -> Number
focusScrollTopForMinute isMobile minuteOffset =
  let
    minuteHeight = if isMobile then 52.0 / 60.0 else 1.0
  in
    Int.toNumber minuteOffset * minuteHeight

timeLabel :: DateTime -> String
timeLabel raw =
  DateTime.formatLocalTime (time raw)

toOptionalString :: String -> Maybe String
toOptionalString raw =
  let
    trimmed = StringCommon.trim raw
  in
    if trimmed == "" then Nothing else Just trimmed

validateTask :: TaskDraft -> Either ValidationError TaskDraft
validateTask draft =
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

sortItems :: SortMode -> Array CalendarItem -> Array CalendarItem
sortItems mode items =
  case mode of
    SortByStatus -> sortBy compareStatus items
    SortByCategory -> sortBy compareCategory items
    SortByTime -> sortBy compareTime items
  where
  compareTime a b = compare (calendarItemContent a).windowStart (calendarItemContent b).windowStart

  compareStatus a b = compare (statusRank (calendarItemContent a).status) (statusRank (calendarItemContent b).status)

  compareCategory a b = compare (categoryKey (calendarItemContent a).category) (categoryKey (calendarItemContent b).category)

  statusRank Todo = 0
  statusRank InProgress = 1
  statusRank Done = 2
  statusRank Canceled = 3

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
