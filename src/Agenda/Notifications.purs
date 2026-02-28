module Agenda.Notifications
  ( NotificationState
  , NotificationAction(..)
  , NotificationEditor
  , notificationInitialState
  , handleNotificationAction
  , renderNotificationsPanel
  , renderNotificationsContent
  , reminderTimesForIntention
  ) where

import Prelude hiding (div)

import Agenda.Commands (Command)
import Agenda.Helpers (combineDateWithTime, isTimeLocal, parsePositiveInt, shiftMinutes)
import Agenda.Model
  ( CalendarItem(..)
  , CalendarItemContent
  , ItemType(..)
  , NotificationDefaults
  , NotificationOverride
  , ReminderTime
  , defaultNotificationDefaults
  )
import Control.Monad.State.Trans (StateT, get, modify_)
import Control.Monad.Writer.Trans (WriterT)
import Data.Array (catMaybes, filter, find, null)
import Data.Lens (Lens', (.~), (%~), (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Aff (Aff)
import Halogen.HTML (HTML, button, div, input, section, text)
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (type_, value)
import Type.Proxy (Proxy(..))
import Ui.AgendaRender (renderPanelHeader)
import DOM.HTML.Indexed.InputType (InputType(..))
import Ui.Utils (class_)


type NotificationState =
  { notificationDefaults :: NotificationDefaults
  , notificationOverrides :: Array NotificationOverride
  , notificationPanelOpen :: Boolean
  , notificationEditor :: Maybe NotificationEditor
  }


type NotificationEditor =
  { itemId :: String
  , startTime :: String
  , beforeEndRaw :: String
  }


notificationInitialState :: NotificationState
notificationInitialState =
  { notificationDefaults: defaultNotificationDefaults
  , notificationOverrides: []
  , notificationPanelOpen: false
  , notificationEditor: Nothing
  }


_notificationDefaultsS :: Lens' NotificationState NotificationDefaults
_notificationDefaultsS = prop (Proxy :: _ "notificationDefaults")

_notificationOverridesS :: Lens' NotificationState (Array NotificationOverride)
_notificationOverridesS = prop (Proxy :: _ "notificationOverrides")

_notificationPanelOpenS :: Lens' NotificationState Boolean
_notificationPanelOpenS = prop (Proxy :: _ "notificationPanelOpen")

_notificationEditorS :: Lens' NotificationState (Maybe NotificationEditor)
_notificationEditorS = prop (Proxy :: _ "notificationEditor")


data NotificationAction
  = NotificationTogglePanel
  | NotificationDefaultStartTimeChanged String
  | NotificationDefaultBeforeEndChanged String
  | NotificationOpenEditor String
  | NotificationStartTimeChanged String
  | NotificationBeforeEndChanged String
  | NotificationSaveOverride
  | NotificationResetOverride String
  | NotificationCancelOverride


handleNotificationAction :: NotificationAction -> StateT NotificationState (WriterT (Array Command) Aff) Unit
handleNotificationAction = case _ of
  NotificationTogglePanel ->
    modify_ (_notificationPanelOpenS %~ not)
  NotificationDefaultStartTimeChanged raw ->
    if isTimeLocal raw then
      modify_ (_notificationDefaultsS <<< prop (Proxy :: _ "startDayTime") .~ raw)
    else
      pure unit
  NotificationDefaultBeforeEndChanged raw ->
    case parsePositiveInt raw of
      Just hours ->
        modify_ (_notificationDefaultsS <<< prop (Proxy :: _ "beforeEndHours") .~ hours)
      Nothing -> pure unit
  NotificationOpenEditor itemId -> do
    st <- get
    let
      existing = lookupNotificationOverride itemId (st ^. _notificationOverridesS)
      startTime =
        fromMaybe (st ^. _notificationDefaultsS).startDayTime (existing >>= _.startDayTime)
      beforeEnd =
        fromMaybe (st ^. _notificationDefaultsS).beforeEndHours (existing >>= _.beforeEndHours)
    modify_ (_notificationEditorS .~ Just { itemId, startTime, beforeEndRaw: show beforeEnd })
  NotificationStartTimeChanged raw ->
    modify_ (_notificationEditorS %~ map (\editor -> editor { startTime = raw }))
  NotificationBeforeEndChanged raw ->
    modify_ (_notificationEditorS %~ map (\editor -> editor { beforeEndRaw = raw }))
  NotificationSaveOverride -> do
    st <- get
    case st ^. _notificationEditorS of
      Nothing -> pure unit
      Just editor -> do
        let
          cleanedTime = if isTimeLocal editor.startTime then Just editor.startTime else Nothing
          cleanedHours = parsePositiveInt editor.beforeEndRaw
        modify_
          ( (_notificationOverridesS %~ upsertNotificationOverride editor.itemId cleanedTime cleanedHours)
              <<< (_notificationEditorS .~ Nothing)
          )
  NotificationResetOverride itemId ->
    modify_
      ( (_notificationOverridesS %~ removeNotificationOverride itemId)
          <<< (_notificationEditorS .~ Nothing)
      )
  NotificationCancelOverride ->
    modify_ (_notificationEditorS .~ Nothing)


renderNotificationsPanel
  :: forall w action
   . (NotificationAction -> action)
  -> Boolean
  -> NotificationDefaults
  -> Array NotificationOverride
  -> Maybe NotificationEditor
  -> Array CalendarItem
  -> HTML w action
renderNotificationsPanel onAction isOpen defaults overrides editor intentions =
  if null intentions then text ""
  else
    section [ class_ "agenda-notifications" ]
      [ renderPanelHeader
          "agenda-notifications"
          "Rappels des intentions non planifiees"
          "Les rappels par defaut s'appliquent aux intentions non planifiees."
          [ button
              [ class_ $ "btn btn-sm agenda-notifications-toggle" <> if isOpen then " btn-outline-primary" else " btn-outline-secondary"
              , onClick (const (onAction NotificationTogglePanel))
              ]
              [ text $ if isOpen then "Masquer" else "Configurer" ]
          ]
      , if isOpen then renderNotificationDefaults onAction defaults else text ""
      , if isOpen then renderNotificationList onAction defaults overrides editor intentions else text ""
      ]

renderNotificationsContent
  :: forall w action
   . (NotificationAction -> action)
  -> NotificationDefaults
  -> Array NotificationOverride
  -> Maybe NotificationEditor
  -> Array CalendarItem
  -> HTML w action
renderNotificationsContent onAction defaults overrides editor intentions =
  if null intentions then
    div [ class_ "agenda-modal-empty" ]
      [ text "Aucune intention non planifiee." ]
  else
    div [ class_ "agenda-notifications-modal" ]
      [ renderNotificationDefaults onAction defaults
      , renderNotificationList onAction defaults overrides editor intentions
      ]

renderNotificationDefaults :: forall w action. (NotificationAction -> action) -> NotificationDefaults -> HTML w action
renderNotificationDefaults onAction defaults =
  div [ class_ "agenda-notifications-defaults" ]
    [ div [ class_ "agenda-notifications-section-title" ] [ text "Rappels par defaut" ]
    , div [ class_ "agenda-notifications-controls" ]
        [ div [ class_ "agenda-notifications-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Jour de debut" ]
            , input
                [ class_ "form-control agenda-input"
                , type_ InputTime
                , value defaults.startDayTime
                , onValueChange (onAction <<< NotificationDefaultStartTimeChanged)
                ]
            ]
        , div [ class_ "agenda-notifications-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Avant fin (heures)" ]
            , input
                [ class_ "form-control agenda-input"
                , type_ InputNumber
                , value (show defaults.beforeEndHours)
                , onValueChange (onAction <<< NotificationDefaultBeforeEndChanged)
                ]
            ]
        ]
    ]

renderNotificationList
  :: forall w action
   . (NotificationAction -> action)
  -> NotificationDefaults
  -> Array NotificationOverride
  -> Maybe NotificationEditor
  -> Array CalendarItem
  -> HTML w action
renderNotificationList onAction defaults overrides editor intentions =
  div [ class_ "agenda-notifications-list" ]
    (map (renderNotificationItem onAction defaults overrides editor) intentions)

renderNotificationItem
  :: forall w action
   . (NotificationAction -> action)
  -> NotificationDefaults
  -> Array NotificationOverride
  -> Maybe NotificationEditor
  -> CalendarItem
  -> HTML w action
renderNotificationItem onAction defaults overrides editor item =
  case item of
    ServerCalendarItem { id, content } | content.itemType == Intention ->
      let
        override = lookupNotificationOverride id overrides
        reminders = reminderTimesForIntention defaults override content
        editorForItem = editor >>= \current -> if current.itemId == id then Just current else Nothing
        hasOverride = case override of
          Nothing -> false
          Just _ -> true
      in
        div [ class_ "agenda-notification-item" ]
          [ div [ class_ "agenda-notification-header" ]
              [ div []
                  [ div [ class_ "agenda-notification-title" ] [ text content.title ]
                  , div [ class_ "agenda-notification-window" ] [ text $ content.windowStart <> " → " <> content.windowEnd ]
                  ]
              , div [ class_ "agenda-notification-actions" ]
                  [ div [ class_ $ "agenda-notification-badge" <> if hasOverride then " agenda-notification-badge--custom" else "" ]
                      [ text $ if hasOverride then "Personnalise" else "Par defaut" ]
                  , button
                      [ class_ "btn btn-sm btn-outline-secondary"
                      , onClick (const $ onAction (NotificationOpenEditor id))
                      ]
                      [ text "Personnaliser" ]
                  ]
              ]
          , renderReminderTimes reminders
          , maybe (text "") (renderNotificationEditor onAction id) editorForItem
          ]
    _ -> text ""

renderReminderTimes :: forall w action. Array ReminderTime -> HTML w action
renderReminderTimes reminders =
  div [ class_ "agenda-notification-times" ]
    (map (\reminder -> div [ class_ "agenda-notification-time" ] [ text $ reminder.label <> ": " <> reminder.at ]) reminders)

renderNotificationEditor :: forall w action. (NotificationAction -> action) -> String -> NotificationEditor -> HTML w action
renderNotificationEditor onAction itemId editor =
  div [ class_ "agenda-notification-editor" ]
    [ div [ class_ "agenda-notifications-section-title" ] [ text "Surcharge de rappel" ]
    , div [ class_ "agenda-notifications-controls" ]
        [ div [ class_ "agenda-notifications-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Jour de debut" ]
            , input
                [ class_ "form-control agenda-input"
                , type_ InputTime
                , value editor.startTime
                , onValueChange (onAction <<< NotificationStartTimeChanged)
                ]
            ]
        , div [ class_ "agenda-notifications-control" ]
            [ div [ class_ "agenda-notifications-label" ] [ text "Avant fin (heures)" ]
            , input
                [ class_ "form-control agenda-input"
                , type_ InputNumber
                , value editor.beforeEndRaw
                , onValueChange (onAction <<< NotificationBeforeEndChanged)
                ]
            ]
        ]
    , div [ class_ "agenda-notification-editor-actions" ]
        [ button [ class_ "btn btn-sm btn-success", onClick (const (onAction NotificationSaveOverride)) ] [ text "Enregistrer" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (onAction NotificationCancelOverride)) ] [ text "Annuler" ]
        , button [ class_ "btn btn-sm btn-outline-danger", onClick (const $ onAction (NotificationResetOverride itemId)) ] [ text "Reinitialiser" ]
        ]
    ]

reminderTimesForIntention :: NotificationDefaults -> Maybe NotificationOverride -> CalendarItemContent -> Array ReminderTime
reminderTimesForIntention defaults override content =
  if content.itemType /= Intention then []
  else
    let
      startTime = fromMaybe defaults.startDayTime (override >>= _.startDayTime)
      beforeEndHours = fromMaybe defaults.beforeEndHours (override >>= _.beforeEndHours)
      startReminder = combineDateWithTime content.windowStart startTime <#> \at -> { label: "Jour de debut", at }
      beforeEndReminder = shiftMinutes (negate (beforeEndHours * 60)) content.windowEnd <#> \at -> { label: show beforeEndHours <> "h avant fin", at }
    in
      catMaybes [ startReminder, beforeEndReminder ]

lookupNotificationOverride :: String -> Array NotificationOverride -> Maybe NotificationOverride
lookupNotificationOverride itemId overrides =
  find (\override -> override.itemId == itemId) overrides

upsertNotificationOverride :: String -> Maybe String -> Maybe Int -> Array NotificationOverride -> Array NotificationOverride
upsertNotificationOverride itemId startTime beforeEnd overrides =
  let
    cleaned =
      case { start: startTime, end: beforeEnd } of
        { start: Nothing, end: Nothing } -> Nothing
        { start, end } -> Just { itemId, startDayTime: start, beforeEndHours: end }
  in
    case cleaned of
      Nothing -> removeNotificationOverride itemId overrides
      Just entry ->
        case find (\override -> override.itemId == itemId) overrides of
          Nothing -> overrides <> [ entry ]
          Just _ -> map (\override -> if override.itemId == itemId then entry else override) overrides

removeNotificationOverride :: String -> Array NotificationOverride -> Array NotificationOverride
removeNotificationOverride itemId overrides =
  filter (\override -> override.itemId /= itemId) overrides
