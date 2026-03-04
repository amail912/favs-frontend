module Calendar.Calendar.Agenda.List
  ( ListAction(..)
  , agendaList
  , renderCategory
  , renderPrimaryAction
  , emptyAgenda
  , editHandlers
  ) where

import Prelude hiding (div)

import Calendar.Calendar.Primary (primaryActionFor)
import Calendar.Calendar.Types (PrimaryAction(..))
import Calendar.Display (ViewAction(..))
import Calendar.Drag (DragAction, dragHandlers)
import Calendar.Helpers (calendarItemContent, isConflict, timeLabel)
import Calendar.Model (CalendarItem(..), CalendarItemContent)
import Calendar.Sync (SyncAction(..))
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Halogen.HTML (HTML, button, div, li, span, text, ul)
import Halogen.HTML.Events (onClick, onDoubleClick, onTouchEnd)
import Halogen.HTML.Properties (IProp)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.TouchEvent.TouchEvent as TouchEvent
import Ui.Utils (class_)

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
              [ text $ content.windowStart <> " → " <> content.windowEnd ]
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
    [ onDoubleClick (const (toAction (ListViewAction (ViewOpenEditFromDoubleClick item)))) ]
      <>
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
