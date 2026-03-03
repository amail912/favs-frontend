module Calendar.Calendar
  ( CalendarState
  , CalendarAction(..)
  , CalendarUiAction(..)
  , ResolutionStrategy(..)
  , ConflictResolution
  , emptyDraft
  , calendarInitialState
  , _items
  , _draft
  , _validationError
  , _lastCreateType
  , handleCalendarAction
  , applyCalendarAction
  , toNewIntention
  , renderCreateContent
  , renderSortPicker
  , renderConflictActions
  , renderConflictResolution
  , PrimaryAction(..)
  , primaryActionFor
  , renderAgendaView
  ) where

import Prelude hiding (div)

import Calendar.Drag (DragAction(..), dragCalendarHandlers, dragHandlers, renderDropIndicator)
import Calendar.Helpers
  ( calendarItemContent
  , generateDateRange
  , generateMonthDates
  , isConflict
  , isItemOnDate
  , minuteOfDay
  , pad2
  , parsePositiveInt
  , parseSortMode
  , sortItems
  , sortModeValue
  , timeLabel
  , toOptionalString
  )
import Calendar.Model
  ( AgendaView(..)
  , CalendarItem(..)
  , CalendarItemContent
  , IntentionDraft
  , ItemStatus(..)
  , ItemType(..)
  , RecurrenceDraft
  , SortMode(..)
  , defaultRecurrenceDraft
  )
import Calendar.Sync (SyncAction(..))
import Calendar.Display (ViewAction(..))
import Calendar.Commands (Command)
import Calendar.RecurrenceEditor (RecurrenceAction, applyRecurrenceAction, draftToRecurrence, renderRecurrenceEditor)
import Control.Monad.State.Trans (StateT, modify_)
import Control.Monad.Writer.Trans (WriterT)
import Data.Array (filter, find, findIndex, foldl, length, mapMaybe, mapWithIndex, null, sortBy, uncons, updateAt)
import Data.Either (Either(..))
import Data.Enum (enumFromTo)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', (.~), (%~))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.String.Common as StringCommon
import Effect.Aff (Aff)
import Halogen.HTML (HTML, button, div, i, input, li, option, section, select, span, text, ul)
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events (onClick, onDoubleClick, onDragOver, onDrop, onKeyDown, onMouseDown, onTouchCancel, onTouchEnd, onTouchMove, onValueChange)
import Halogen.HTML.Properties (IProp, attr, placeholder, style, type_, value)
import Data.Show.Generic (genericShow)
import DOM.HTML.Indexed.InputType (InputType(..))
import Type.Proxy (Proxy(..))
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as MouseEvent
import Web.TouchEvent.TouchEvent as TouchEvent
import Ui.Utils (class_)

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

data CalendarAction
  = CalendarDraftTitleChanged String
  | CalendarDraftTypeChanged ItemType
  | CalendarDraftStartChanged String
  | CalendarDraftEndChanged String
  | CalendarDraftCategoryChanged String
  | CalendarDraftStatusChanged String
  | CalendarDraftDurationChanged String
  | CalendarDraftRecurrenceChanged RecurrenceAction
  | CalendarToggleConflictFilter
  | CalendarSortChanged String
  | CalendarOpenConflictResolution (Array String)
  | CalendarChooseResolutionStrategy ResolutionStrategy
  | CalendarConfirmResolution
  | CalendarCancelResolution

data CalendarUiAction
  = CalendarUiCalendar CalendarAction
  | CalendarUiSync SyncAction
  | CalendarUiView ViewAction
  | CalendarUiDrag DragAction

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

handleCalendarAction :: CalendarAction -> StateT CalendarState (WriterT (Array Command) Aff) Unit
handleCalendarAction action =
  modify_ $ applyCalendarAction action

applyCalendarAction :: CalendarAction -> CalendarState -> CalendarState
applyCalendarAction action dataState =
  case action of
    CalendarDraftTitleChanged title ->
      ((_draftTitleS .~ title) <<< (_validationError .~ Nothing)) dataState
    CalendarDraftTypeChanged itemType ->
      ((_draftItemTypeS .~ itemType) <<< (_lastCreateType .~ itemType) <<< (_validationError .~ Nothing)) dataState
    CalendarDraftStartChanged windowStart ->
      ((_draftWindowStartS .~ windowStart) <<< (_validationError .~ Nothing)) dataState
    CalendarDraftEndChanged windowEnd ->
      ((_draftWindowEndS .~ windowEnd) <<< (_validationError .~ Nothing)) dataState
    CalendarDraftCategoryChanged category ->
      ((_draftCategoryS .~ category) <<< (_validationError .~ Nothing)) dataState
    CalendarDraftStatusChanged raw ->
      ((_draftStatusS .~ parseStatus raw) <<< (_validationError .~ Nothing)) dataState
    CalendarDraftDurationChanged raw ->
      ((_draftDurationS .~ raw) <<< (_validationError .~ Nothing)) dataState
    CalendarDraftRecurrenceChanged recurrenceAction ->
      ((_draftRecurrenceS %~ applyRecurrenceAction recurrenceAction) <<< (_validationError .~ Nothing)) dataState
    CalendarToggleConflictFilter ->
      (_showConflictsOnlyS %~ not) dataState
    CalendarSortChanged raw ->
      (_sortModeS .~ parseSortMode raw) dataState
    CalendarOpenConflictResolution groupIds ->
      (_conflictResolutionS .~ Just { groupIds, pendingStrategy: Nothing }) dataState
    CalendarChooseResolutionStrategy strategy ->
      (_conflictResolutionS %~ map (_ { pendingStrategy = Just strategy })) dataState
    CalendarConfirmResolution ->
      (_conflictResolutionS .~ Nothing) dataState
    CalendarCancelResolution ->
      (_conflictResolutionS .~ Nothing) dataState

toNewIntention :: IntentionDraft -> Either String CalendarItem
toNewIntention draft = do
  recurrence <- case draftToRecurrence draft.recurrence of
    Left err -> Left err
    Right ok -> Right ok
  actualDuration <- parseDuration draft.actualDurationMinutes
  Right
    ( NewCalendarItem
        { content:
            { itemType: draft.itemType
            , title: draft.title
            , windowStart: draft.windowStart
            , windowEnd: draft.windowEnd
            , status: draft.status
            , sourceItemId: Nothing
            , actualDurationMinutes: actualDuration
            , category: toOptionalString draft.category
            , recurrenceRule: recurrence.rule
            , recurrenceExceptionDates: recurrence.exceptions
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

renderCreateContent
  :: forall w
   . IntentionDraft
  -> Maybe String
  -> HTML w CalendarUiAction
renderCreateContent draft validationError =
  div [ class_ "calendar-modal-stack" ]
    [ field "Type" typeInput
    , field "Titre" titleInput
    , dateTimeField "Début" (CalendarUiCalendar <<< CalendarDraftStartChanged) draft.windowStart
    , dateTimeField "Fin" (CalendarUiCalendar <<< CalendarDraftEndChanged) draft.windowEnd
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
        , onValueChange onChange
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
      , onValueChange (CalendarUiCalendar <<< CalendarDraftTitleChanged)
      , onKeyDown (\ev -> CalendarUiSync (SyncDraftTitleKeyDown (KE.key ev)))
      , value draft.title
      ]

  categoryInput =
    input
      [ class_ "form-control calendar-input"
      , placeholder "Catégorie"
      , onValueChange (CalendarUiCalendar <<< CalendarDraftCategoryChanged)
      , value draft.category
      ]

  statusInput =
    select
      [ class_ "form-select calendar-input"
      , onValueChange (CalendarUiCalendar <<< CalendarDraftStatusChanged)
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
      , onValueChange (CalendarUiCalendar <<< CalendarDraftDurationChanged)
      , value draft.actualDurationMinutes
      ]

  recurrenceInput =
    map (CalendarUiCalendar <<< CalendarDraftRecurrenceChanged) (renderRecurrenceEditor draft.recurrence)

  errorInput =
    maybe (text "") (\msg -> div [ class_ "calendar-error" ] [ text msg ]) validationError

  toggleButton :: ItemType -> String -> HTML w CalendarUiAction
  toggleButton itemType label =
    button
      [ class_ $ "btn btn-sm " <> if draft.itemType == itemType then "btn-primary" else "btn-outline-secondary"
      , onClick (const (CalendarUiCalendar (CalendarDraftTypeChanged itemType)))
      ]
      [ text label ]

renderSortPicker :: forall w. SortMode -> HTML w CalendarUiAction
renderSortPicker sortMode =
  div [ class_ "calendar-sort" ]
    [ text "Trier:"
    , select
        [ class_ "form-select calendar-sort-select"
        , onValueChange (CalendarUiCalendar <<< CalendarSortChanged)
        , value (sortModeValue sortMode)
        ]
        [ option [ value "time" ] [ text "Horaire" ]
        , option [ value "status" ] [ text "Statut" ]
        , option [ value "category" ] [ text "Catégorie" ]
        , option [ value "conflict" ] [ text "Conflit" ]
        ]
    ]

renderConflictActions :: forall w. Array (Array String) -> HTML w CalendarUiAction
renderConflictActions conflictGroups =
  if null conflictGroups then text ""
  else
    div [ class_ "calendar-conflict-actions" ]
      [ button
          [ class_ "btn btn-sm btn-outline-danger calendar-conflict-button"
          , onClick (const (CalendarUiCalendar (CalendarOpenConflictResolution (headOrEmpty conflictGroups))))
          ]
          [ text "Résoudre un conflit" ]
      ]
  where
  headOrEmpty groups =
    case uncons groups of
      Just { head } -> head
      Nothing -> []

renderConflictResolution :: forall w. Array CalendarItem -> ConflictResolution -> HTML w CalendarUiAction
renderConflictResolution items resolution =
  div [ class_ "calendar-conflict-panel" ]
    [ div [ class_ "calendar-conflict-title" ] [ text "Résolution de conflit" ]
    , div [ class_ "calendar-conflict-subtitle" ] [ text "Choisissez une stratégie puis confirmez." ]
    , ul [ class_ "calendar-conflict-list" ] (map (renderConflictItem items) resolution.groupIds)
    , div [ class_ "calendar-conflict-strategies" ]
        [ button
            [ class_ "btn btn-sm btn-outline-primary"
            , onClick (const (CalendarUiCalendar (CalendarChooseResolutionStrategy StrategyShift30)))
            ]
            [ text "Décaler de 30 min" ]
        , button
            [ class_ "btn btn-sm btn-outline-primary"
            , onClick (const (CalendarUiCalendar (CalendarChooseResolutionStrategy StrategySwap)))
            ]
            [ text "Échanger" ]
        ]
    , renderConfirmation resolution.pendingStrategy
    , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (CalendarUiCalendar CalendarCancelResolution)) ] [ text "Fermer" ]
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
              [ text $ content.windowStart <> " → " <> content.windowEnd ]
          ]
    Nothing -> text ""
  where
  matchId id (ServerCalendarItem { id: candidate }) = id == candidate
  matchId _ _ = false

renderConfirmation :: forall w. Maybe ResolutionStrategy -> HTML w CalendarUiAction
renderConfirmation pending =
  case pending of
    Nothing -> text ""
    Just strategy ->
      div [ class_ "calendar-conflict-confirmation" ]
        [ div [ class_ "calendar-conflict-confirmation-text" ]
            [ text $ "Confirmer la stratégie: " <> show strategy <> " ?" ]
        , div [ class_ "calendar-conflict-confirmation-actions" ]
            [ button [ class_ "btn btn-sm btn-danger", onClick (const (CalendarUiCalendar CalendarConfirmResolution)) ] [ text "Confirmer" ]
            , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (CalendarUiCalendar CalendarCancelResolution)) ] [ text "Annuler" ]
            ]
        ]

renderAgendaView
  :: forall w
   . AgendaView
  -> String
  -> Array String
  -> Array CalendarItem
  -> Boolean
  -> Maybe String
  -> Maybe Int
  -> HTML w CalendarUiAction
renderAgendaView viewMode focusDate conflictIds items isMobile draggingId dragHoverIndex =
  case viewMode of
    ViewDay ->
      renderDayCalendar focusDate conflictIds items isMobile draggingId dragHoverIndex
    ViewWeek ->
      renderRangeView "Semaine" (generateDateRange focusDate 7) conflictIds items isMobile
    ViewMonth ->
      renderRangeView "Mois" (generateMonthDates focusDate) conflictIds items isMobile

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

renderDayCalendar
  :: forall w
   . String
  -> Array String
  -> Array CalendarItem
  -> Boolean
  -> Maybe String
  -> Maybe Int
  -> HTML w CalendarUiAction
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
                , onDragOver (\ev -> CalendarUiDrag (DragOverCalendar ev))
                , onDrop (\ev -> CalendarUiDrag (DropOnCalendar ev))
                , onTouchMove (\ev -> CalendarUiDrag (DragTouchMoveCalendar ev))
                , onTouchEnd (const (CalendarUiDrag DragTouchEnd))
                , onTouchCancel (const (CalendarUiDrag DragTouchCancel))
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
  div [ class_ "calendar-calendar-hour" ] [ text $ pad2 h <> ":00" ]

renderHourLabelEnd :: forall w action. HTML w action
renderHourLabelEnd =
  div [ class_ "calendar-calendar-hour calendar-calendar-hour--end" ] [ text "24:00" ]

renderHourLine :: forall w action. Int -> HTML w action
renderHourLine _ =
  div
    [ class_ "calendar-calendar-line" ]
    []

renderTimelineItem
  :: forall w
   . Array String
  -> Boolean
  -> Maybe String
  -> TimelineLayout
  -> HTML w CalendarUiAction
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
    dragProps = dragCalendarHandlers CalendarUiDrag layout.item
    editProps = editHandlers isMobile draggingId layout.item
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
                      [ renderPrimaryAction layout.item content ]
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

renderRangeView
  :: forall w
   . String
  -> Array String
  -> Array String
  -> Array CalendarItem
  -> Boolean
  -> HTML w CalendarUiAction
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
  -> HTML w CalendarUiAction
renderDateSection _ conflictIds items isMobile dateStr =
  let
    itemsForDate = filter (isItemOnDate dateStr) items
    sorted = sortItems SortByTime conflictIds itemsForDate
  in
    section [ class_ "calendar-date-section" ]
      [ div [ class_ "calendar-date-title" ] [ text dateStr ]
      , if null sorted then div [ class_ "calendar-date-empty" ] [ text "Aucun item" ]
        else agendaList conflictIds sorted isMobile
      ]

emptyAgendaRange :: forall w action. String -> HTML w action
emptyAgendaRange label =
  div [ class_ "row entity-empty calendar-empty" ]
    [ div [ class_ "entity-empty-title" ] [ text $ "Aucun item sur la " <> label ]
    , div [ class_ "entity-empty-subtitle" ] [ text "Ajoutez une intention pour demarrer." ]
    ]

agendaList
  :: forall w
   . Array String
  -> Array CalendarItem
  -> Boolean
  -> HTML w CalendarUiAction
agendaList conflictIds items isMobile =
  ul [ class_ "list-group entity-list calendar-list" ] (mapWithIndex (renderItem conflictIds isMobile) items)

renderItem
  :: forall w
   . Array String
  -> Boolean
  -> Int
  -> CalendarItem
  -> HTML w CalendarUiAction
renderItem conflictIds isMobile _ item =
  let
    content = calendarItemContent item
    conflictClass = guard (isConflict conflictIds item) " calendar-card--conflict"
    dragProps = dragHandlers CalendarUiDrag item
    editProps = editHandlers isMobile Nothing item
  in
    li ([ class_ $ "row list-group-item entity-card calendar-card" <> conflictClass ] <> dragProps <> editProps)
      [ div [ class_ "col entity-card-body" ]
          [ div [ class_ "calendar-card-time" ] [ text (timeLabel content.windowStart) ]
          , div [ class_ "calendar-card-title" ] [ text content.title ]
          , div [ class_ "calendar-card-window" ]
              [ text $ content.windowStart <> " → " <> content.windowEnd ]
          , renderCategory content.category
          , renderPrimaryAction item content
          ]
      ]

primaryActionFor :: CalendarItem -> PrimaryAction
primaryActionFor (ServerCalendarItem { content }) =
  case content.itemType of
    Intention -> PrimaryPlanify
    ScheduledBlock ->
      if content.status /= Fait then PrimaryValidate else PrimaryNone
primaryActionFor _ = PrimaryNone

renderPrimaryAction
  :: forall w
   . CalendarItem
  -> CalendarItemContent
  -> HTML w CalendarUiAction
renderPrimaryAction item content =
  case primaryActionFor item of
    PrimaryPlanify ->
      case item of
        ServerCalendarItem { id } ->
          button [ class_ "btn btn-sm btn-outline-primary calendar-primary-action", onClick (const (CalendarUiSync (SyncPlanifyFrom id content))) ]
            [ text "Planifier" ]
        _ -> text ""
    PrimaryValidate ->
      case item of
        ServerCalendarItem { id } ->
          button [ class_ "btn btn-sm btn-outline-success calendar-primary-action", onClick (const (CalendarUiView (ViewOpenValidation id content))) ]
            [ text "Valider" ]
        _ -> text ""
    PrimaryNone -> text ""

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

editHandlers
  :: forall r
   . Boolean
  -> Maybe String
  -> CalendarItem
  -> Array
       ( IProp
           ( onDoubleClick :: MouseEvent.MouseEvent
           , onTouchEnd :: TouchEvent.TouchEvent
           | r
           )
           CalendarUiAction
       )
editHandlers isMobile draggingId item =
  if isMobile then
    [ onDoubleClick (const (CalendarUiView (ViewOpenEditFromDoubleClick item))) ]
      <>
        if draggingId == Nothing then
          [ onTouchEnd (const (CalendarUiView (ViewMobileTap item))) ]
        else
          []
  else
    []

renderTimelineEditButton :: forall w. Boolean -> CalendarItem -> HTML w CalendarUiAction
renderTimelineEditButton isMobile item =
  if isMobile then text ""
  else
    case item of
      ServerCalendarItem _ ->
        button
          [ class_ "btn btn-sm btn-outline-secondary calendar-edit calendar-edit--timeline"
          , attr (AttrName "aria-label") "Editer"
          , onMouseDown (const (CalendarUiView (ViewOpenEdit item)))
          , onClick (const (CalendarUiView (ViewOpenEdit item)))
          ]
          [ i [ class_ "bi bi-pencil" ] [] ]
      _ -> text ""

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
