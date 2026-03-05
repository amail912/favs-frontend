module Calendar.Export.Types
  ( ItemType(..)
  , ItemStatus(..)
  , ExportItem(..)
  , ExportFormat(..)
  , ExportFilter(..)
  ) where

import Prelude

import Calendar.Recurrence (RecurrenceRule)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

data ItemType
  = Intention
  | ScheduledBlock

derive instance exportItemTypeEq :: Eq ItemType
derive instance exportItemTypeGeneric :: Generic ItemType _
instance exportItemTypeShow :: Show ItemType where
  show = genericShow

data ItemStatus
  = Todo
  | EnCours
  | Fait
  | Annule

derive instance exportItemStatusEq :: Eq ItemStatus
derive instance exportItemStatusGeneric :: Generic ItemStatus _
instance exportItemStatusShow :: Show ItemStatus where
  show = genericShow

newtype ExportItem = ExportItem
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

derive newtype instance exportItemEq :: Eq ExportItem
derive newtype instance exportItemShow :: Show ExportItem

data ExportFormat = ExportCSV | ExportICS

derive instance exportFormatEq :: Eq ExportFormat
derive instance exportFormatGeneric :: Generic ExportFormat _
instance exportFormatShow :: Show ExportFormat where
  show = genericShow

newtype ExportFilter = ExportFilter
  { itemType :: Maybe ItemType
  , status :: Maybe ItemStatus
  , category :: Maybe String
  , startDate :: Maybe Date
  , endDate :: Maybe Date
  }
