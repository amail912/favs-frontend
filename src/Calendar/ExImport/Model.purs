module Calendar.ExImport.Model
  ( ItemType(..)
  , ItemStatus(..)
  , Item(..)
  ) where

import Prelude

import Calendar.Recurrence (RecurrenceRule)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

data ItemType
  = Task

derive instance itemTypeEq :: Eq ItemType
derive instance itemTypeGeneric :: Generic ItemType _
instance itemTypeShow :: Show ItemType where
  show = genericShow

data ItemStatus
  = Todo
  | InProgress
  | Done
  | Canceled

derive instance itemStatusEq :: Eq ItemStatus
derive instance itemStatusGeneric :: Generic ItemStatus _
instance itemStatusShow :: Show ItemStatus where
  show = genericShow

newtype Item = Item
  { itemType :: ItemType
  , title :: String
  , windowStart :: DateTime
  , windowEnd :: DateTime
  , status :: ItemStatus
  , category :: Maybe String
  , sourceItemId :: Maybe String
  , actualDurationMinutes :: Maybe Int
  , recurrenceRule :: Maybe RecurrenceRule
  , recurrenceExceptionDates :: Array Date
  }

derive newtype instance itemEq :: Eq Item
derive newtype instance itemShow :: Show Item
