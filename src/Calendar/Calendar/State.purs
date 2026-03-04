module Calendar.Calendar.State
  ( CalendarState
  , calendarInitialState
  , emptyDraft
  , _items
  , _draft
  , _validationError
  , _lastCreateType
  , _showConflictsOnlyS
  , _conflictResolutionS
  , _sortModeS
  , _draftTitleS
  , _draftItemTypeS
  , _draftWindowStartS
  , _draftWindowEndS
  , _draftCategoryS
  , _draftStatusS
  , _draftDurationS
  , _draftRecurrenceS
  ) where

import Prelude

import Calendar.Calendar.Types (ConflictResolution)
import Calendar.Model
  ( CalendarItem
  , IntentionDraft
  , ItemStatus(..)
  , ItemType(..)
  , RecurrenceDraft
  , SortMode(..)
  , defaultRecurrenceDraft
  )
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))

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
