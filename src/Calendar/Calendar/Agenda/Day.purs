module Calendar.Calendar.Agenda.Day
  ( DayAction(..)
  , renderDayCalendar
  ) where

import Prelude hiding (div)

import Calendar.Calendar.Agenda.List (ListAction, editHandlers, emptyAgenda, renderCategory, renderPrimaryAction)
import Calendar.Calendar.Timeline (TimelineLayout, buildTimelineLayout)
import Calendar.Display (ViewAction(..))
import Calendar.Drag (DragAction(..), dragCalendarHandlers, renderDropIndicator)
import Calendar.Helpers (calendarItemContent, isConflict, isItemOnDate, pad2, sortItems, timeLabel)
import Calendar.Model (CalendarItem(..), ItemType(..), SortMode(..))
import Data.Array (filter, length, null)
import Data.Enum (enumFromTo)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Halogen.HTML (HTML, button, div, i, text)
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events (onClick, onDragOver, onDrop, onMouseDown, onTouchCancel, onTouchEnd, onTouchMove)
import Halogen.HTML.Properties (attr, style)
import Ui.Utils (class_)

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
  div [ class_ "calendar-calendar-hour" ] [ text $ pad2 h <> ":00" ]

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
