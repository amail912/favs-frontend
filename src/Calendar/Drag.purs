module Calendar.Drag
  ( DragState(..)
  , DragAction(..)
  , DragCommand(..)
  , DragCtx
  , dragInitialState
  , handleDragAction
  , dragHandlers
  , dragCalendarHandlers
  , renderDropIndicator
  , computeDropMinuteIndex
  , indexToMinutes
  , indexToTimeLabel
  ) where

import Prelude hiding (div)

import Calendar.Helpers (calendarItemContent, combineDateWithTime, durationMinutesBetween, pad2, shiftMinutes)
import Calendar.Model (CalendarItem(..), ItemType(..))
import Control.Alt ((<|>))
import Control.Monad.State.Trans (StateT, get, modify_)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Writer.Trans (WriterT)
import Data.Array (find, index, uncons)
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Lens (Lens', lens, (.~), (^.))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.DateTime.Instant (Instant, diff)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now as Now
import Halogen.HTML (HTML, div, text)
import Halogen.HTML.Events (onDragEnd, onDragOver, onDragStart, onDrop, onTouchCancel, onTouchEnd, onTouchMove, onTouchStart)
import Halogen.HTML.Properties (IProp, draggable, style)
import Web.Event.Event (preventDefault)
import Web.Event.Event (currentTarget, target) as Event
import Web.HTML.Event.DragEvent (DragEvent, toEvent)
import Web.HTML.HTMLElement as HTMLElement
import Web.DOM.Element (getBoundingClientRect)
import Web.DOM.ParentNode as ParentNode
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent as MouseEvent
import Web.TouchEvent.Touch (clientY) as Touch
import Web.TouchEvent.TouchEvent as TouchEvent
import Web.TouchEvent.TouchList as TouchList
import Ui.Utils (class_)
import Ui.Vibration (vibrateIfAvailable)

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
          ctx.items >>= \item ->
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
          baseDateTime = ctx.focusDate <> "T00:00"
          offset = fromMaybe 0 (dragState ^. _dragOffsetMinutesS)
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
                ctx.items >>= \item ->
                durationMinutesBetween (calendarItemContent item).windowStart (calendarItemContent item).windowEnd >>= \mins ->
                  shiftMinutes mins start >>= \end ->
                    Just { start, end }
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
          ctx.items >>= \item ->
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
        let elapsedMs = case diff now startedAt of Milliseconds ms -> ms
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
              baseDateTime = ctx.focusDate <> "T00:00"
              offset = fromMaybe 0 (dragState ^. _dragOffsetMinutesS)
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
                    ctx.items >>= \item ->
                    durationMinutesBetween (calendarItemContent item).windowStart (calendarItemContent item).windowEnd >>= \mins ->
                      shiftMinutes mins start >>= \end ->
                        Just { start, end }
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
    pad2 hour <> ":" <> pad2 minute

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
