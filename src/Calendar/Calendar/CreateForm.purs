module Calendar.Calendar.CreateForm
  ( CreateFormAction(..)
  , applyCreateFormAction
  , renderCreateContent
  ) where

import Prelude hiding (div)

import Calendar.Calendar.State
  ( CalendarState
  , _draftCategoryS
  , _draftDurationS
  , _draftItemTypeS
  , _draftRecurrenceS
  , _draftStatusS
  , _draftTitleS
  , _draftWindowEndS
  , _draftWindowStartS
  , _lastCreateType
  , _validationError
  )
import Calendar.Model (IntentionDraft, ItemStatus(..), ItemType(..))
import Calendar.RecurrenceEditor (RecurrenceAction, applyRecurrenceAction, renderRecurrenceEditor)
import Calendar.Sync (SyncAction(..))
import Data.Lens ((.~), (%~))
import Data.Maybe (Maybe(..), maybe)
import Halogen.HTML (HTML, button, div, input, option, select, text)
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events (onClick, onKeyDown, onValueChange)
import Halogen.HTML.Properties (attr, placeholder, type_, value)
import DOM.HTML.Indexed.InputType (InputType(..))
import Web.UIEvent.KeyboardEvent as KE
import Ui.Utils (class_)

data CreateFormAction
  = CreateFormDraftTitleChanged String
  | CreateFormDraftTypeChanged ItemType
  | CreateFormDraftStartChanged String
  | CreateFormDraftEndChanged String
  | CreateFormDraftCategoryChanged String
  | CreateFormDraftStatusChanged String
  | CreateFormDraftDurationChanged String
  | CreateFormDraftRecurrenceChanged RecurrenceAction
  | CreateFormSync SyncAction

applyCreateFormAction :: CreateFormAction -> CalendarState -> CalendarState
applyCreateFormAction action dataState =
  case action of
    CreateFormDraftTitleChanged title ->
      ((_draftTitleS .~ title) <<< (_validationError .~ Nothing)) dataState
    CreateFormDraftTypeChanged itemType ->
      ((_draftItemTypeS .~ itemType) <<< (_lastCreateType .~ itemType) <<< (_validationError .~ Nothing)) dataState
    CreateFormDraftStartChanged windowStart ->
      ((_draftWindowStartS .~ windowStart) <<< (_validationError .~ Nothing)) dataState
    CreateFormDraftEndChanged windowEnd ->
      ((_draftWindowEndS .~ windowEnd) <<< (_validationError .~ Nothing)) dataState
    CreateFormDraftCategoryChanged category ->
      ((_draftCategoryS .~ category) <<< (_validationError .~ Nothing)) dataState
    CreateFormDraftStatusChanged raw ->
      ((_draftStatusS .~ parseStatus raw) <<< (_validationError .~ Nothing)) dataState
    CreateFormDraftDurationChanged raw ->
      ((_draftDurationS .~ raw) <<< (_validationError .~ Nothing)) dataState
    CreateFormDraftRecurrenceChanged recurrenceAction ->
      ((_draftRecurrenceS %~ applyRecurrenceAction recurrenceAction) <<< (_validationError .~ Nothing)) dataState
    CreateFormSync _ ->
      dataState

renderCreateContent
  :: forall w
   . IntentionDraft
  -> Maybe String
  -> HTML w CreateFormAction
renderCreateContent draft validationError =
  div [ class_ "calendar-modal-stack" ]
    [ field "Type" typeInput
    , field "Titre" titleInput
    , dateTimeField "Début" CreateFormDraftStartChanged draft.windowStart
    , dateTimeField "Fin" CreateFormDraftEndChanged draft.windowEnd
    , field "Catégorie" categoryInput
    , field "Statut" statusInput
    , field "Durée réelle (minutes)" durationInput
    , recurrenceInput
    , errorInput
    ]
  where
  field label content =
    div [ class_ "calendar-modal-field" ]
      [ div [ class_ "calendar-notifications-label" ] [ text label ]
      , content
      ]

  dateTimeField label onChange currentValue =
    field label $
      input
        [ class_ "form-control calendar-input"
        , type_ InputDatetimeLocal
        , attr (AttrName "lang") "fr"
        , placeholder label
        , onValueChange onChange
        , value currentValue
        ]

  typeInput =
    div [ class_ "btn-group w-100", attr (AttrName "role") "group" ]
      [ toggleButton Intention "Intention"
      , toggleButton ScheduledBlock "Bloc planifié"
      ]

  titleInput =
    input
      [ class_ "form-control calendar-input"
      , placeholder "Titre"
      , onValueChange CreateFormDraftTitleChanged
      , onKeyDown (\ev -> CreateFormSync (SyncDraftTitleKeyDown (KE.key ev)))
      , value draft.title
      ]

  categoryInput =
    input
      [ class_ "form-control calendar-input"
      , placeholder "Catégorie"
      , onValueChange CreateFormDraftCategoryChanged
      , value draft.category
      ]

  statusInput =
    select
      [ class_ "form-select calendar-input"
      , onValueChange CreateFormDraftStatusChanged
      , value (statusValue draft.status)
      ]
      [ option [ value "todo" ] [ text "À faire" ]
      , option [ value "progress" ] [ text "En cours" ]
      , option [ value "done" ] [ text "Fait" ]
      , option [ value "canceled" ] [ text "Annulé" ]
      ]

  durationInput =
    input
      [ class_ "form-control calendar-input"
      , type_ InputNumber
      , placeholder "Ex: 30"
      , onValueChange CreateFormDraftDurationChanged
      , value draft.actualDurationMinutes
      ]

  recurrenceInput =
    map CreateFormDraftRecurrenceChanged (renderRecurrenceEditor draft.recurrence)

  errorInput =
    maybe (text "") (\msg -> div [ class_ "calendar-error" ] [ text msg ]) validationError

  toggleButton :: ItemType -> String -> HTML w CreateFormAction
  toggleButton itemType label =
    button
      [ class_ $ "btn btn-sm " <> if draft.itemType == itemType then "btn-primary" else "btn-outline-secondary"
      , onClick (const (CreateFormDraftTypeChanged itemType))
      ]
      [ text label ]

statusValue :: ItemStatus -> String
statusValue status =
  case status of
    Todo -> "todo"
    EnCours -> "progress"
    Fait -> "done"
    Annule -> "canceled"

parseStatus :: String -> ItemStatus
parseStatus raw =
  case raw of
    "progress" -> EnCours
    "done" -> Fait
    "canceled" -> Annule
    _ -> Todo
