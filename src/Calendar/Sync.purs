module Calendar.Sync
  ( SyncState(..)
  , SyncAction(..)
  , SyncCommand(..)
  , syncInitialState
  , handleSyncAction
  , renderOfflineToggle
  , renderSyncConflict
  , renderUpdateError
  , updateErrorMessage
  , _syncOfflineMode
  , _syncPendingSync
  , _syncConflict
  , _syncUpdateError
  ) where

import Prelude hiding (div)

import Calendar.Helpers (calendarItemContent)
import Calendar.Model (CalendarItem(..), CalendarItemContent, ItemStatus(..), ItemType(..))
import Calendar.Offline (applyOfflineMutation)
import Control.Monad.State.Trans (StateT, get, modify_)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Writer.Trans (WriterT)
import Data.Array (find, mapMaybe, null)
import Data.Lens (Lens', lens, (.~), (^.))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen.HTML (HTML, button, div, li, text, ul)
import Halogen.HTML.Events (onClick)
import Ui.Utils (class_)

newtype SyncState = SyncState
  { offlineMode :: Boolean
  , pendingSync :: Array CalendarItem
  , syncConflict :: Maybe (Array CalendarItem)
  , updateError :: Maybe String
  }

syncInitialState :: SyncState
syncInitialState =
  SyncState
    { offlineMode: false
    , pendingSync: []
    , syncConflict: Nothing
    , updateError: Nothing
    }

_syncOfflineMode :: Lens' SyncState Boolean
_syncOfflineMode =
  lens
    (\(SyncState state) -> state.offlineMode)
    (\(SyncState state) offlineMode -> SyncState (state { offlineMode = offlineMode }))

_syncPendingSync :: Lens' SyncState (Array CalendarItem)
_syncPendingSync =
  lens
    (\(SyncState state) -> state.pendingSync)
    (\(SyncState state) pendingSync -> SyncState (state { pendingSync = pendingSync }))

_syncConflict :: Lens' SyncState (Maybe (Array CalendarItem))
_syncConflict =
  lens
    (\(SyncState state) -> state.syncConflict)
    (\(SyncState state) syncConflict -> SyncState (state { syncConflict = syncConflict }))

_syncUpdateError :: Lens' SyncState (Maybe String)
_syncUpdateError =
  lens
    (\(SyncState state) -> state.updateError)
    (\(SyncState state) updateError -> SyncState (state { updateError = updateError }))

data SyncAction
  = SyncDraftTitleKeyDown String
  | SyncSubmitIntention
  | SyncPlanifyFrom String CalendarItemContent
  | SyncToggleOffline
  | SyncResolveKeepLocal
  | SyncResolveDiscardLocal
  | SyncDismissUpdateError

data SyncCommand
  = SyncSetItems (Array CalendarItem)
  | SyncCreateItem CalendarItem
  | SyncRefreshItems
  | SyncSubmitIntentionCmd
  | SyncRunPending (Array CalendarItem)

handleSyncAction :: Array CalendarItem -> SyncAction -> StateT SyncState (WriterT (Array SyncCommand) Aff) Unit
handleSyncAction items = case _ of
  SyncDraftTitleKeyDown key ->
    when (key == "Enter") (tell [ SyncSubmitIntentionCmd ])
  SyncSubmitIntention ->
    tell [ SyncSubmitIntentionCmd ]
  SyncPlanifyFrom sourceId content -> do
    syncState <- get
    let item = toScheduledBlock sourceId content
    if syncState ^. _syncOfflineMode then do
      let
        result = applyOfflineMutation true item items (syncState ^. _syncPendingSync)
      modify_ (_syncPendingSync .~ result.pending)
      tell [ SyncSetItems result.items ]
    else
      tell [ SyncCreateItem item ]
  SyncToggleOffline -> do
    syncState <- get
    if syncState ^. _syncOfflineMode then do
      modify_ (_syncOfflineMode .~ false)
      syncPending
    else modify_ (_syncOfflineMode .~ true)
  SyncResolveKeepLocal ->
    modify_ ((_syncConflict .~ Nothing) <<< (_syncOfflineMode .~ true))
  SyncResolveDiscardLocal -> do
    modify_ ((_syncConflict .~ Nothing) <<< (_syncPendingSync .~ []))
    tell [ SyncRefreshItems ]
  SyncDismissUpdateError ->
    modify_ (_syncUpdateError .~ Nothing)

toScheduledBlock :: String -> CalendarItemContent -> CalendarItem
toScheduledBlock sourceId content =
  NewCalendarItem
    { content:
        content
          { itemType = ScheduledBlock
          , status = Todo
          , sourceItemId = Just sourceId
          }
    }

renderOfflineToggle :: forall w. Boolean -> HTML w SyncAction
renderOfflineToggle offlineMode =
  div [ class_ "calendar-offline-toggle" ]
    [ button
        [ class_ $ "btn btn-sm " <> if offlineMode then "btn-outline-warning" else "btn-outline-secondary"
        , onClick (const SyncToggleOffline)
        ]
        [ text $ if offlineMode then "Mode hors ligne actif" else "Passer hors ligne" ]
    ]

renderUpdateError :: forall w. String -> HTML w SyncAction
renderUpdateError message =
  div [ class_ "calendar-error calendar-error--update" ]
    [ text message
    , button
        [ class_ "btn btn-sm btn-outline-secondary calendar-error-dismiss"
        , onClick (const SyncDismissUpdateError)
        ]
        [ text "OK" ]
    ]

updateErrorMessage :: Int -> String
updateErrorMessage status =
  "Echec de mise a jour de l'item (HTTP " <> show status <> ")."

renderSyncConflict :: forall w. Array CalendarItem -> HTML w SyncAction
renderSyncConflict pending =
  div [ class_ "calendar-sync-conflict" ]
    [ div [ class_ "calendar-conflict-title" ] [ text "Conflit de synchronisation" ]
    , div [ class_ "calendar-conflict-subtitle" ]
        [ text "Choisissez comment resoudre la synchronisation des changements locaux." ]
    , ul [ class_ "calendar-conflict-list" ] (map (renderConflictItem pending) pendingIds)
    , div [ class_ "calendar-conflict-confirmation-actions" ]
        [ button [ class_ "btn btn-sm btn-danger", onClick (const SyncResolveDiscardLocal) ] [ text "Abandonner local" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const SyncResolveKeepLocal) ] [ text "Conserver local" ]
        ]
    ]
  where
  pendingIds = mapMaybe extractId pending
  extractId (ServerCalendarItem { id }) = Just id
  extractId _ = Nothing

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

syncPending :: StateT SyncState (WriterT (Array SyncCommand) Aff) Unit
syncPending = do
  syncState <- get
  if null (syncState ^. _syncPendingSync) then
    tell [ SyncRefreshItems ]
  else
    tell [ SyncRunPending (syncState ^. _syncPendingSync) ]
