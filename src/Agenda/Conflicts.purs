module Agenda.Conflicts
  ( detectConflictIds
  , detectConflictGroups
  ) where

import Prelude

import Agenda.Model (CalendarItem(..), ItemType(..))
import Data.Array (elem, filter, find, length, mapMaybe, nub, uncons)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))


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
