module Agenda.Sync
  ( SyncState
  , SyncAction(..)
  , syncInitialState
  , handleSyncAction
  , renderOfflineToggle
  , renderSyncConflict
  , renderUpdateError
  , updateErrorMessage
  , _syncOfflineModeS
  , _syncPendingSyncS
  , _syncConflictS
  , _syncUpdateErrorS
  ) where

import Prelude hiding (div)

import Agenda.Commands (SyncCommand(..), Command(..), tellCmd)
import Agenda.Helpers (calendarItemContent)
import Agenda.Model (CalendarItem(..), CalendarItemContent, ItemStatus(..), ItemType(..))
import Agenda.Offline (applyOfflineMutation)
import Control.Monad.State.Trans (StateT, get, modify_)
import Control.Monad.Writer.Trans (WriterT)
import Data.Array (find, mapMaybe, null)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen.HTML (HTML, button, div, li, text, ul)
import Halogen.HTML.Events (onClick)
import Ui.Utils (class_)
import Type.Proxy (Proxy(..))


type SyncState =
  { offlineMode :: Boolean
  , pendingSync :: Array CalendarItem
  , syncConflict :: Maybe (Array CalendarItem)
  , updateError :: Maybe String
  }


syncInitialState :: SyncState
syncInitialState =
  { offlineMode: false
  , pendingSync: []
  , syncConflict: Nothing
  , updateError: Nothing
  }


_syncOfflineModeS :: Lens' SyncState Boolean
_syncOfflineModeS = prop (Proxy :: _ "offlineMode")

_syncPendingSyncS :: Lens' SyncState (Array CalendarItem)
_syncPendingSyncS = prop (Proxy :: _ "pendingSync")

_syncConflictS :: Lens' SyncState (Maybe (Array CalendarItem))
_syncConflictS = prop (Proxy :: _ "syncConflict")

_syncUpdateErrorS :: Lens' SyncState (Maybe String)
_syncUpdateErrorS = prop (Proxy :: _ "updateError")


data SyncAction
  = SyncDraftTitleKeyDown String
  | SyncSubmitIntention
  | SyncPlanifyFrom String CalendarItemContent
  | SyncToggleOffline
  | SyncResolveKeepLocal
  | SyncResolveDiscardLocal
  | SyncDismissUpdateError


handleSyncAction :: Array CalendarItem -> SyncAction -> StateT SyncState (WriterT (Array Command) Aff) Unit
handleSyncAction items = case _ of
  SyncDraftTitleKeyDown key ->
    when (key == "Enter") (tellCmd $ SyncCmd SyncSubmitIntentionCmd)
  SyncSubmitIntention ->
    tellCmd $ SyncCmd SyncSubmitIntentionCmd
  SyncPlanifyFrom sourceId content -> do
    syncState <- get
    let item = toScheduledBlock sourceId content
    if syncState ^. _syncOfflineModeS then do
      let
        result = applyOfflineMutation true item items (syncState ^. _syncPendingSyncS)
      modify_ (_syncPendingSyncS .~ result.pending)
      tellCmd $ SyncCmd (SyncSetItems result.items)
    else do
      tellCmd $ SyncCmd (SyncCreateItem item)
  SyncToggleOffline -> do
    syncState <- get
    if syncState ^. _syncOfflineModeS then do
      modify_ (_syncOfflineModeS .~ false)
      syncPending
    else modify_ (_syncOfflineModeS .~ true)
  SyncResolveKeepLocal ->
    modify_ ((_syncConflictS .~ Nothing) <<< (_syncOfflineModeS .~ true))
  SyncResolveDiscardLocal -> do
    modify_ ((_syncConflictS .~ Nothing) <<< (_syncPendingSyncS .~ []))
    tellCmd $ SyncCmd SyncRefreshItems
  SyncDismissUpdateError ->
    modify_ (_syncUpdateErrorS .~ Nothing)


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


renderOfflineToggle :: forall w action. (SyncAction -> action) -> Boolean -> HTML w action
renderOfflineToggle onAction offlineMode =
  div [ class_ "agenda-offline-toggle" ]
    [ button
        [ class_ $ "btn btn-sm " <> if offlineMode then "btn-outline-warning" else "btn-outline-secondary"
        , onClick (const (onAction SyncToggleOffline))
        ]
        [ text $ if offlineMode then "Mode hors ligne actif" else "Passer hors ligne" ]
    ]

renderUpdateError :: forall w action. (SyncAction -> action) -> String -> HTML w action
renderUpdateError onAction message =
  div [ class_ "agenda-error agenda-error--update" ]
    [ text message
    , button
        [ class_ "btn btn-sm btn-outline-secondary agenda-error-dismiss"
        , onClick (const (onAction SyncDismissUpdateError))
        ]
        [ text "OK" ]
    ]

updateErrorMessage :: Int -> String
updateErrorMessage status =
  "Echec de mise a jour de l'item (HTTP " <> show status <> ")."

renderSyncConflict :: forall w action. (SyncAction -> action) -> Array CalendarItem -> HTML w action
renderSyncConflict onAction pending =
  div [ class_ "agenda-sync-conflict" ]
    [ div [ class_ "agenda-conflict-title" ] [ text "Conflit de synchronisation" ]
    , div [ class_ "agenda-conflict-subtitle" ]
        [ text "Choisissez comment resoudre la synchronisation des changements locaux." ]
    , ul [ class_ "agenda-conflict-list" ] (map (renderConflictItem pending) pendingIds)
    , div [ class_ "agenda-conflict-confirmation-actions" ]
        [ button [ class_ "btn btn-sm btn-danger", onClick (const (onAction SyncResolveDiscardLocal)) ] [ text "Abandonner local" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (onAction SyncResolveKeepLocal)) ] [ text "Conserver local" ]
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
        li [ class_ "agenda-conflict-item" ]
          [ div [ class_ "agenda-conflict-item-title" ] [ text content.title ]
          , div [ class_ "agenda-conflict-item-window" ]
              [ text $ content.windowStart <> " → " <> content.windowEnd ]
          ]
    Nothing -> text ""
  where
  matchId id (ServerCalendarItem { id: candidate }) = id == candidate
  matchId _ _ = false

syncPending :: StateT SyncState (WriterT (Array Command) Aff) Unit
syncPending = do
  syncState <- get
  if null (syncState ^. _syncPendingSyncS) then
    tellCmd $ SyncCmd SyncRefreshItems
  else
    tellCmd $ SyncCmd (SyncRunPending (syncState ^. _syncPendingSyncS))
