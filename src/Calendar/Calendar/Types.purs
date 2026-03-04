module Calendar.Calendar.Types
  ( ConflictResolution
  , ResolutionStrategy(..)
  , PrimaryAction(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

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
