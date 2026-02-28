module Calendar.Offline
  ( OfflineMutationResult
  , applyOfflineMutation
  , upsertPendingItem
  ) where

import Prelude

import Calendar.Model (CalendarItem(..))
import Data.Array (any)


type OfflineMutationResult =
  { items :: Array CalendarItem
  , pending :: Array CalendarItem
  }

applyOfflineMutation :: Boolean -> CalendarItem -> Array CalendarItem -> Array CalendarItem -> OfflineMutationResult
applyOfflineMutation offline item items pending =
  if offline then { items: items <> [ item ], pending: pending <> [ item ] }
  else { items, pending }

upsertPendingItem :: CalendarItem -> Array CalendarItem -> Array CalendarItem
upsertPendingItem item pending =
  case item of
    ServerCalendarItem { id } ->
      let
        hasSame =
          any
            ( \candidate -> case candidate of
                ServerCalendarItem { id: candidateId } -> candidateId == id
                _ -> false
            )
            pending
      in
        if hasSame then
          map
            ( \candidate -> case candidate of
                ServerCalendarItem payload | payload.id == id -> item
                _ -> candidate
            )
            pending
        else
          pending <> [ item ]
    _ -> pending
