module Agenda.Calendar
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
  , handleCalendarAction
  , applyCalendarAction
  , validateIntention
  , toNewIntention
  , renderForm
  , renderDateTimeContent
  , renderValidationError
  , renderSortPicker
  , renderConflictActions
  , renderConflictResolution
  , renderAgendaView
  ) where

import Prelude hiding (div)

import Agenda.Drag (DragAction(..), dragCalendarHandlers, dragHandlers, renderDropIndicator)
import Agenda.Helpers
  ( calendarItemContent
  , generateDateRange
  , generateMonthDates
  , isConflict
  , isDateTimeLocal
  , isItemOnDate
  , minuteOfDay
  , pad2
  , parseSortMode
  , sortItems
  , sortModeValue
  , timeLabel
  , toOptionalString
  )
import Agenda.Model
  ( AgendaView(..)
  , CalendarItem(..)
  , CalendarItemContent
  , IntentionDraft
  , ItemStatus(..)
  , ItemType(..)
  , SortMode(..)
  , ValidationError(..)
  )
import Agenda.Sync (SyncAction(..))
import Agenda.Display (ViewAction(..), AgendaModal(..))
import Agenda.Commands (Command)
import Control.Monad.State.Trans (StateT, modify_)
import Control.Monad.Writer.Trans (WriterT)
import Data.Array (filter, find, findIndex, foldl, index, length, mapMaybe, mapWithIndex, null, sortBy, uncons, updateAt)
import Data.Enum (enumFromTo)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', (.~), (%~))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.CodeUnits as String
import Data.String.Common as StringCommon
import Data.String.Pattern (Pattern(..))
import Effect.Aff (Aff)
import Halogen.HTML (HTML, button, div, input, li, option, section, select, span, text, ul)
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events (onClick, onDragOver, onDrop, onKeyDown, onValueChange)
import Halogen.HTML.Properties (attr, placeholder, style, type_, value)
import Data.Show.Generic (genericShow)
import DOM.HTML.Indexed.InputType (InputType(..))
import Type.Proxy (Proxy(..))
import Web.UIEvent.KeyboardEvent as KE
import Ui.Utils (class_)


type CalendarState =
  { items :: Array CalendarItem
  , draft :: IntentionDraft
  , validationError :: Maybe ValidationError
  , showConflictsOnly :: Boolean
  , conflictResolution :: Maybe ConflictResolution
  , sortMode :: SortMode
  }


calendarInitialState :: CalendarState
calendarInitialState =
  { items: []
  , draft: emptyDraft
  , validationError: Nothing
  , showConflictsOnly: false
  , conflictResolution: Nothing
  , sortMode: SortByTime
  }


emptyDraft :: IntentionDraft
emptyDraft =
  { title: ""
  , windowStart: ""
  , windowEnd: ""
  , category: ""
  }


data CalendarAction
  = CalendarDraftTitleChanged String
  | CalendarDraftStartChanged String
  | CalendarDraftEndChanged String
  | CalendarDraftCategoryChanged String
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


_items :: Lens' CalendarState (Array CalendarItem)
_items = prop (Proxy :: _ "items")

_draft :: Lens' CalendarState IntentionDraft
_draft = prop (Proxy :: _ "draft")

_validationError :: Lens' CalendarState (Maybe ValidationError)
_validationError = prop (Proxy :: _ "validationError")

_showConflictsOnlyS :: Lens' CalendarState Boolean
_showConflictsOnlyS = prop (Proxy :: _ "showConflictsOnly")

_conflictResolutionS :: Lens' CalendarState (Maybe ConflictResolution)
_conflictResolutionS = prop (Proxy :: _ "conflictResolution")

_sortModeS :: Lens' CalendarState SortMode
_sortModeS = prop (Proxy :: _ "sortMode")

_draftTitleS :: Lens' CalendarState String
_draftTitleS = _draft <<< prop (Proxy :: _ "title")

_draftWindowStartS :: Lens' CalendarState String
_draftWindowStartS = _draft <<< prop (Proxy :: _ "windowStart")

_draftWindowEndS :: Lens' CalendarState String
_draftWindowEndS = _draft <<< prop (Proxy :: _ "windowEnd")

_draftCategoryS :: Lens' CalendarState String
_draftCategoryS = _draft <<< prop (Proxy :: _ "category")


handleCalendarAction :: CalendarAction -> StateT CalendarState (WriterT (Array Command) Aff) Unit
handleCalendarAction action =
  modify_ $ applyCalendarAction action

applyCalendarAction :: CalendarAction -> CalendarState -> CalendarState
applyCalendarAction action dataState =
  case action of
    CalendarDraftTitleChanged title ->
      ((_draftTitleS .~ title) <<< (_validationError .~ Nothing)) dataState
    CalendarDraftStartChanged windowStart ->
      ((_draftWindowStartS .~ windowStart) <<< (_validationError .~ Nothing)) dataState
    CalendarDraftEndChanged windowEnd ->
      ((_draftWindowEndS .~ windowEnd) <<< (_validationError .~ Nothing)) dataState
    CalendarDraftCategoryChanged category ->
      ((_draftCategoryS .~ category) <<< (_validationError .~ Nothing)) dataState
    CalendarToggleConflictFilter ->
      (_showConflictsOnlyS %~ not) dataState
    CalendarSortChanged raw ->
      (_sortModeS .~ parseSortMode raw) dataState
    CalendarOpenConflictResolution groupIds ->
      (_conflictResolutionS .~ Just { groupIds, pendingStrategy: Nothing }) dataState
    CalendarChooseResolutionStrategy strategy ->
      (_conflictResolutionS %~ map (\res -> res { pendingStrategy = Just strategy })) dataState
    CalendarConfirmResolution ->
      (_conflictResolutionS .~ Nothing) dataState
    CalendarCancelResolution ->
      (_conflictResolutionS .~ Nothing) dataState


validateIntention :: IntentionDraft -> Either ValidationError IntentionDraft
validateIntention draft =
  case unit of
    _ | StringCommon.trim draft.title == "" -> Left TitleEmpty
    _ | not (isDateTimeLocal draft.windowStart) -> Left WindowStartInvalid
    _ | not (isDateTimeLocal draft.windowEnd) -> Left WindowEndInvalid
    _ | draft.windowEnd <= draft.windowStart -> Left WindowOrderInvalid
    _ -> Right draft


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
        , category: toOptionalString category
        , recurrenceRule: Nothing
        , recurrenceExceptionDates: []
        }
    }


renderForm
  :: forall w
   . IntentionDraft
  -> Maybe ValidationError
  -> HTML w CalendarUiAction
renderForm draft validationError =
  section [ class_ "agenda-form" ]
    [ input
        [ class_ "form-control agenda-input"
        , placeholder "Titre de l'intention"
        , onValueChange (CalendarUiCalendar <<< CalendarDraftTitleChanged)
        , onKeyDown (\ev -> CalendarUiSync (SyncDraftTitleKeyDown (KE.key ev)))
        , value draft.title
        ]
    , div [ class_ "agenda-time-row" ]
        [ input
            [ class_ "form-control agenda-input"
            , type_ InputDatetimeLocal
            , attr (AttrName "lang") "fr"
            , placeholder "Debut"
            , onValueChange (CalendarUiCalendar <<< CalendarDraftStartChanged)
            , value draft.windowStart
            ]
        , input
            [ class_ "form-control agenda-input"
            , type_ InputDatetimeLocal
            , attr (AttrName "lang") "fr"
            , placeholder "Fin"
            , onValueChange (CalendarUiCalendar <<< CalendarDraftEndChanged)
            , value draft.windowEnd
            ]
        ]
    , input
        [ class_ "form-control agenda-input"
        , placeholder "Categorie (optionnelle)"
        , onValueChange (CalendarUiCalendar <<< CalendarDraftCategoryChanged)
        , value draft.category
        ]
    , div [ class_ "agenda-datetime-row" ]
        [ button
            [ class_ "btn btn-outline-secondary agenda-datetime-button"
            , onClick (const (CalendarUiView (ViewOpenModal ModalDateTime)))
            ]
            [ text "Dates & heures" ]
        , div [ class_ "agenda-datetime-summary" ]
            [ text $ summarizeDateRange draft.windowStart draft.windowEnd ]
        ]
    , maybe (text "") renderValidationError validationError
    , button [ class_ "btn btn-primary agenda-submit", onClick (const (CalendarUiSync SyncSubmitIntention)) ] [ text "Creer l'intention" ]
    ]

renderDateTimeContent :: forall w. IntentionDraft -> HTML w CalendarUiAction
renderDateTimeContent draft =
  div [ class_ "agenda-modal-stack" ]
    [ div [ class_ "agenda-modal-field" ]
        [ div [ class_ "agenda-notifications-label" ] [ text "Debut" ]
        , input
            [ class_ "form-control agenda-input"
            , type_ InputDatetimeLocal
            , attr (AttrName "lang") "fr"
            , placeholder "Debut"
            , onValueChange (CalendarUiCalendar <<< CalendarDraftStartChanged)
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
            , onValueChange (CalendarUiCalendar <<< CalendarDraftEndChanged)
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

renderValidationError :: forall w action. ValidationError -> HTML w action
renderValidationError err =
  div [ class_ "agenda-error" ]
    [ text $ case err of
        TitleEmpty -> "Le titre est obligatoire."
        WindowStartInvalid -> "La date de debut est invalide."
        WindowEndInvalid -> "La date de fin est invalide."
        WindowOrderInvalid -> "La fin doit etre apres le debut."
    ]

renderSortPicker :: forall w. SortMode -> HTML w CalendarUiAction
renderSortPicker sortMode =
  div [ class_ "agenda-sort" ]
    [ text "Trier:"
    , select
        [ class_ "form-select agenda-sort-select"
        , onValueChange (CalendarUiCalendar <<< CalendarSortChanged)
        , value (sortModeValue sortMode)
        ]
        [ option [ value "time" ] [ text "Horaire" ]
        , option [ value "status" ] [ text "Statut" ]
        , option [ value "category" ] [ text "Categorie" ]
        , option [ value "conflict" ] [ text "Conflit" ]
        ]
    ]

renderConflictActions :: forall w. Array (Array String) -> HTML w CalendarUiAction
renderConflictActions conflictGroups =
  if null conflictGroups then text ""
  else
    div [ class_ "agenda-conflict-actions" ]
      [ button
          [ class_ "btn btn-sm btn-outline-danger agenda-conflict-button"
          , onClick (const (CalendarUiCalendar (CalendarOpenConflictResolution (headOrEmpty conflictGroups))))
          ]
          [ text "Resoudre un conflit" ]
      ]
  where
  headOrEmpty groups =
    case uncons groups of
      Just { head } -> head
      Nothing -> []

renderConflictResolution :: forall w. Array CalendarItem -> ConflictResolution -> HTML w CalendarUiAction
renderConflictResolution items resolution =
  div [ class_ "agenda-conflict-panel" ]
    [ div [ class_ "agenda-conflict-title" ] [ text "Resolution de conflit" ]
    , div [ class_ "agenda-conflict-subtitle" ] [ text "Choisissez une strategie puis confirmez." ]
    , ul [ class_ "agenda-conflict-list" ] (map (renderConflictItem items) resolution.groupIds)
    , div [ class_ "agenda-conflict-strategies" ]
        [ button
            [ class_ "btn btn-sm btn-outline-primary"
            , onClick (const (CalendarUiCalendar (CalendarChooseResolutionStrategy StrategyShift30)))
            ]
            [ text "Decaler de 30 min" ]
        , button
            [ class_ "btn btn-sm btn-outline-primary"
            , onClick (const (CalendarUiCalendar (CalendarChooseResolutionStrategy StrategySwap)))
            ]
            [ text "Echanger" ]
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
        li [ class_ "agenda-conflict-item" ]
          [ div [ class_ "agenda-conflict-item-title" ] [ text content.title ]
          , div [ class_ "agenda-conflict-item-window" ]
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
      div [ class_ "agenda-conflict-confirmation" ]
        [ div [ class_ "agenda-conflict-confirmation-text" ]
            [ text $ "Confirmer la strategie: " <> show strategy <> " ?" ]
        , div [ class_ "agenda-conflict-confirmation-actions" ]
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
  -> Maybe String
  -> Maybe Int
  -> HTML w CalendarUiAction
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

renderDayCalendar
  :: forall w
   . String
  -> Array String
  -> Array CalendarItem
  -> Maybe String
  -> Maybe Int
  -> HTML w CalendarUiAction
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
                , onDragOver (\ev -> CalendarUiDrag (DragOverCalendar ev))
                , onDrop (\ev -> CalendarUiDrag (DropOnCalendar ev))
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

renderHourLabel :: forall w action. Int -> HTML w action
renderHourLabel h =
  div [ class_ "agenda-calendar-hour" ] [ text $ pad2 h <> ":00" ]

renderHourLabelEnd :: forall w action. HTML w action
renderHourLabelEnd =
  div [ class_ "agenda-calendar-hour agenda-calendar-hour--end" ] [ text "24:00" ]

renderHourLine :: forall w action. Int -> HTML w action
renderHourLine _ =
  div
    [ class_ "agenda-calendar-line" ]
    []

renderTimelineItem
  :: forall w
   . Array String
  -> TimelineLayout
  -> HTML w CalendarUiAction
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
    dragProps = dragCalendarHandlers CalendarUiDrag layout.item
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

renderRangeView
  :: forall w
   . String
  -> Array String
  -> Array String
  -> Array CalendarItem
  -> HTML w CalendarUiAction
renderRangeView label dates conflictIds items =
  if null dates then emptyAgendaRange label
  else
    div [ class_ "agenda-range" ]
      (map (renderDateSection label conflictIds items) dates)

renderDateSection
  :: forall w
   . String
  -> Array String
  -> Array CalendarItem
  -> String
  -> HTML w CalendarUiAction
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

emptyAgendaRange :: forall w action. String -> HTML w action
emptyAgendaRange label =
  div [ class_ "row entity-empty agenda-empty" ]
    [ div [ class_ "entity-empty-title" ] [ text $ "Aucun item sur la " <> label ]
    , div [ class_ "entity-empty-subtitle" ] [ text "Ajoutez une intention pour demarrer." ]
    ]

agendaList
  :: forall w
   . Array String
  -> Array CalendarItem
  -> HTML w CalendarUiAction
agendaList conflictIds items =
  ul [ class_ "list-group entity-list agenda-list" ] (mapWithIndex (renderItem conflictIds) items)

renderItem
  :: forall w
   . Array String
  -> Int
  -> CalendarItem
  -> HTML w CalendarUiAction
renderItem conflictIds _ item =
  let
    content = calendarItemContent item
    conflictClass = if isConflict conflictIds item then " agenda-card--conflict" else ""
    dragProps = dragHandlers CalendarUiDrag item
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

renderPlanifyAction
  :: forall w
   . CalendarItem
  -> CalendarItemContent
  -> HTML w CalendarUiAction
renderPlanifyAction (ServerCalendarItem { id, content }) _ | content.itemType == Intention =
  button [ class_ "btn btn-sm btn-outline-primary agenda-planify", onClick (const (CalendarUiSync (SyncPlanifyFrom id content))) ]
    [ text "Planifier" ]
renderPlanifyAction _ _ = text ""

renderValidationAction
  :: forall w
   . CalendarItem
  -> CalendarItemContent
  -> HTML w CalendarUiAction
renderValidationAction (ServerCalendarItem { id, content }) _ | content.status /= Fait =
  button [ class_ "btn btn-sm btn-outline-success agenda-validate", onClick (const (CalendarUiView (ViewOpenValidation id content))) ]
    [ text "Valider" ]
renderValidationAction _ _ = text ""

renderCategory :: forall w action. Maybe String -> HTML w action
renderCategory category =
  case category of
    Nothing -> text ""
    Just value -> div [ class_ "agenda-card-category" ] [ text value ]

emptyAgenda :: forall w action. HTML w action
emptyAgenda =
  div [ class_ "row entity-empty agenda-empty" ]
    [ div [ class_ "entity-empty-title" ] [ text "Aucune intention aujourd'hui" ]
    , div [ class_ "entity-empty-subtitle" ] [ text "Ajoutez une intention pour demarrer votre journee." ]
    , div [ class_ "agenda-empty-cta" ]
        [ span [ class_ "badge rounded-pill text-bg-primary" ] [ text "Astuce" ]
        , span [ class_ "text-muted" ] [ text "Commencez par un titre et appuyez sur Entrée." ]
        ]
    ]
