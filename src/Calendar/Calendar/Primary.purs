module Calendar.Calendar.Primary
  ( primaryActionFor
  ) where

import Prelude

import Calendar.Calendar.Types (PrimaryAction(..))
import Calendar.Model (CalendarItem(..), ItemType(..), ItemStatus(..))

primaryActionFor :: CalendarItem -> PrimaryAction
primaryActionFor (ServerCalendarItem { content }) =
  case content.itemType of
    Intention -> PrimaryPlanify
    ScheduledBlock ->
      if content.status /= Fait then PrimaryValidate else PrimaryNone
primaryActionFor _ = PrimaryNone
