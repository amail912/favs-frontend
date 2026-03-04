module Calendar.Conflict
  ( ConflictAction(..)
  , detectConflictIds
  , detectConflictGroups
  , applyConflictAction
  , renderConflictActions
  , renderConflictResolution
  ) where

import Prelude hiding (div)

import Calendar.Calendar.Types (ConflictResolution, ResolutionStrategy(..))
import Calendar.Calendar.State (CalendarState, _conflictResolutionS)
import Calendar.Helpers (calendarItemContent)
import Calendar.Model (CalendarItem(..), ItemType(..))
import Data.Array (elem, filter, find, length, mapMaybe, nub, uncons, null)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Lens ((.~), (%~))
import Halogen.HTML (HTML, button, div, li, text, ul)
import Halogen.HTML.Events (onClick)
import Ui.Utils (class_)

type ConflictBlock =
  { id :: String
  , start :: String
  , end :: String
  }

detectConflictIds :: Array CalendarItem -> Array String
detectConflictIds items =
  nub $ go (mapMaybe toConflictBlock items) []
  where
  toConflictBlock :: CalendarItem -> Maybe ConflictBlock
  toConflictBlock (ServerCalendarItem { id, content }) | content.itemType == ScheduledBlock =
    Just { id, start: content.windowStart, end: content.windowEnd }
  toConflictBlock _ = Nothing

  overlaps a b = a.start < b.end && b.start < a.end

  go blocks acc =
    case uncons blocks of
      Nothing -> acc
      Just { head: current, tail: rest } ->
        let
          acc' =
            foldl
              ( \currentAcc other ->
                  if overlaps current other then currentAcc <> [ current.id, other.id ]
                  else currentAcc
              )
              acc
              rest
        in
          go rest acc'

detectConflictGroups :: Array CalendarItem -> Array (Array String)
detectConflictGroups items =
  filter (\group -> length group > 1) $ components allIds []
  where
  blocks = mapMaybe toConflictBlock items
  allIds = map _.id blocks

  toConflictBlock :: CalendarItem -> Maybe ConflictBlock
  toConflictBlock (ServerCalendarItem { id, content }) | content.itemType == ScheduledBlock =
    Just { id, start: content.windowStart, end: content.windowEnd }
  toConflictBlock _ = Nothing

  components ids visited =
    case uncons ids of
      Nothing -> []
      Just { head: current, tail } ->
        if elem current visited then components tail visited
        else
          let
            group = bfs [ current ] []
            newVisited = visited <> group
          in
            [ group ] <> components tail newVisited

  bfs queue visited =
    case uncons queue of
      Nothing -> visited
      Just { head: current, tail } ->
        if elem current visited then bfs tail visited
        else
          let
            next = neighbors current
          in
            bfs (tail <> next) (visited <> [ current ])

  neighbors id =
    case find (\block -> block.id == id) blocks of
      Nothing -> []
      Just current ->
        map _.id $ filter (\block -> block.id /= id && overlaps current block) blocks

  overlaps a b = a.start < b.end && b.start < a.end

data ConflictAction
  = ConflictOpenResolution (Array String)
  | ConflictChooseStrategy ResolutionStrategy
  | ConflictConfirm
  | ConflictCancel

applyConflictAction :: ConflictAction -> CalendarState -> CalendarState
applyConflictAction action dataState =
  case action of
    ConflictOpenResolution groupIds ->
      (_conflictResolutionS .~ Just { groupIds, pendingStrategy: Nothing }) dataState
    ConflictChooseStrategy strategy ->
      (_conflictResolutionS %~ map (_ { pendingStrategy = Just strategy })) dataState
    ConflictConfirm ->
      (_conflictResolutionS .~ Nothing) dataState
    ConflictCancel ->
      (_conflictResolutionS .~ Nothing) dataState

renderConflictActions :: forall w. Array (Array String) -> HTML w ConflictAction
renderConflictActions conflictGroups =
  if null conflictGroups then text ""
  else
    div [ class_ "calendar-conflict-actions" ]
      [ button
          [ class_ "btn btn-sm btn-outline-danger calendar-conflict-button"
          , onClick (const (ConflictOpenResolution (headOrEmpty conflictGroups)))
          ]
          [ text "Résoudre un conflit" ]
      ]
  where
  headOrEmpty groups =
    case uncons groups of
      Just { head } -> head
      Nothing -> []

renderConflictResolution :: forall w. Array CalendarItem -> ConflictResolution -> HTML w ConflictAction
renderConflictResolution items resolution =
  div [ class_ "calendar-conflict-panel" ]
    [ div [ class_ "calendar-conflict-title" ] [ text "Résolution de conflit" ]
    , div [ class_ "calendar-conflict-subtitle" ] [ text "Choisissez une stratégie puis confirmez." ]
    , ul [ class_ "calendar-conflict-list" ] (map (renderConflictItem items) resolution.groupIds)
    , div [ class_ "calendar-conflict-strategies" ]
        [ button
            [ class_ "btn btn-sm btn-outline-primary"
            , onClick (const (ConflictChooseStrategy StrategyShift30))
            ]
            [ text "Décaler de 30 min" ]
        , button
            [ class_ "btn btn-sm btn-outline-primary"
            , onClick (const (ConflictChooseStrategy StrategySwap))
            ]
            [ text "Échanger" ]
        ]
    , renderConfirmation resolution.pendingStrategy
    , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ConflictCancel) ] [ text "Fermer" ]
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

renderConfirmation :: forall w. Maybe ResolutionStrategy -> HTML w ConflictAction
renderConfirmation pending =
  case pending of
    Nothing -> text ""
    Just strategy ->
      div [ class_ "calendar-conflict-confirmation" ]
        [ div [ class_ "calendar-conflict-confirmation-text" ]
            [ text $ "Confirmer la stratégie: " <> show strategy <> " ?" ]
        , div [ class_ "calendar-conflict-confirmation-actions" ]
            [ button [ class_ "btn btn-sm btn-danger", onClick (const ConflictConfirm) ] [ text "Confirmer" ]
            , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ConflictCancel) ] [ text "Annuler" ]
            ]
        ]
