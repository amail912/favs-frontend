module Agenda.Sync
  ( SyncState
  , SyncAction(..)
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


_syncOfflineMode :: Lens' SyncState Boolean
_syncOfflineMode = prop (Proxy :: _ "offlineMode")

_syncPendingSync :: Lens' SyncState (Array CalendarItem)
_syncPendingSync = prop (Proxy :: _ "pendingSync")

_syncConflict :: Lens' SyncState (Maybe (Array CalendarItem))
_syncConflict = prop (Proxy :: _ "syncConflict")

_syncUpdateError :: Lens' SyncState (Maybe String)
_syncUpdateError = prop (Proxy :: _ "updateError")


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
    if syncState ^. _syncOfflineMode then do
      let
        result = applyOfflineMutation true item items (syncState ^. _syncPendingSync)
      modify_ (_syncPendingSync .~ result.pending)
      tellCmd $ SyncCmd (SyncSetItems result.items)
    else do
      tellCmd $ SyncCmd (SyncCreateItem item)
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
    tellCmd $ SyncCmd SyncRefreshItems
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
  div [ class_ "agenda-offline-toggle" ]
    [ button
        [ class_ $ "btn btn-sm " <> if offlineMode then "btn-outline-warning" else "btn-outline-secondary"
        , onClick (const SyncToggleOffline)
        ]
        [ text $ if offlineMode then "Mode hors ligne actif" else "Passer hors ligne" ]
    ]

renderUpdateError :: forall w. String -> HTML w SyncAction
renderUpdateError message =
  div [ class_ "agenda-error agenda-error--update" ]
    [ text message
    , button
        [ class_ "btn btn-sm btn-outline-secondary agenda-error-dismiss"
        , onClick (const SyncDismissUpdateError)
        ]
        [ text "OK" ]
    ]

updateErrorMessage :: Int -> String
updateErrorMessage status =
  "Echec de mise a jour de l'item (HTTP " <> show status <> ")."

renderSyncConflict :: forall w. Array CalendarItem -> HTML w SyncAction
renderSyncConflict pending =
  div [ class_ "agenda-sync-conflict" ]
    [ div [ class_ "agenda-conflict-title" ] [ text "Conflit de synchronisation" ]
    , div [ class_ "agenda-conflict-subtitle" ]
        [ text "Choisissez comment resoudre la synchronisation des changements locaux." ]
    , ul [ class_ "agenda-conflict-list" ] (map (renderConflictItem pending) pendingIds)
    , div [ class_ "agenda-conflict-confirmation-actions" ]
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
  if null (syncState ^. _syncPendingSync) then
    tellCmd $ SyncCmd SyncRefreshItems
  else
    tellCmd $ SyncCmd (SyncRunPending (syncState ^. _syncPendingSync))
