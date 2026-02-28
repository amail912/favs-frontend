module Calendar.Commands
  ( Command(..)
  , SyncCommand(..)
  , DragCommand(..)
  , ViewCommand(..)
  , TemplateCommand(..)
  , ImportCommand(..)
  , tellCmd
  ) where

import Prelude

import Calendar.Model (CalendarItem, IntentionDraft)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Writer.Trans (WriterT)
import Control.Monad.State.Trans (StateT)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)

-- Commands emitted by domain handlers to update other sub-states or trigger effects.

data Command
  = SyncCmd SyncCommand
  | DragCmd DragCommand
  | ViewCmd ViewCommand
  | TemplateCmd TemplateCommand
  | ImportCmd ImportCommand


data SyncCommand
  = SyncSetItems (Array CalendarItem)
  | SyncCreateItem CalendarItem
  | SyncRefreshItems
  | SyncSubmitIntentionCmd
  | SyncRunPending (Array CalendarItem)


data DragCommand
  = DragSetItems (Array CalendarItem)
  | DragUpsertPending CalendarItem
  | DragSetUpdateError (Maybe String)
  | DragUpdateItem String CalendarItem
  | DragRefreshItems


data ViewCommand
  = ViewValidateItem String Int


data TemplateCommand
  = TemplateSetDraft IntentionDraft


data ImportCommand
  = ImportSetItems (Array CalendarItem)
  | ImportSetPending (Array CalendarItem)


tellCmd :: forall s. Command -> StateT s (WriterT (Array Command) Aff) Unit

tellCmd cmd = lift $ tell [ cmd ]
