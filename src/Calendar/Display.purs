module Calendar.Display
  ( ViewState
  , ViewAction(..)
  , ViewCommand(..)
  , AgendaModal(..)
  , ValidationPanel
  , EditPanel
  , viewInitialState
  , viewTitle
  , parseAgendaView
  , renderViewSelector
  , renderMobileTools
  , renderValidationPanel
  , renderEditContent
  , renderToolsContent
  , handleViewAction
  , _viewModeS
  , _viewFocusDate
  , _viewActiveModalS
  , _viewValidationPanelS
  , _viewEditPanelS
  , _viewIsMobileS
  , _viewLastTapAtS
  ) where

import Prelude hiding (div)

import Calendar.Edit (EditDraft, EditError(..), applyEditDraft, buildEditDraft)
import Calendar.Helpers (parsePositiveInt, suggestDurationMinutes)
import Calendar.Model (AgendaView(..), CalendarItem, CalendarItemContent, ItemStatus(..), ItemType(..), ValidationError(..))
import Calendar.RecurrenceEditor (RecurrenceAction, applyRecurrenceAction, renderRecurrenceEditor)
import Control.Alt ((<|>))
import Control.Monad.State.Trans (StateT, get, modify_)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Writer.Trans (WriterT)
import Data.Either (Either(..))
import Data.Lens (Lens', (.~), (%~), (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.DateTime.Instant (Instant, diff)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now as Now
import Halogen.HTML (HTML, button, div, input, option, select, text)
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (attr, placeholder, type_, value)
import Web.HTML (window)
import Web.HTML.Window as Window
import Ui.Utils (class_)
import Type.Proxy (Proxy(..))
import DOM.HTML.Indexed.InputType (InputType(..))

data AgendaModal
  = ModalNotifications
  | ModalTemplates
  | ModalImportCsv
  | ModalImportIcs
  | ModalExport
  | ModalTools
  | ModalFilters
  | ModalCreateItem
  | ModalEditItem

derive instance eqAgendaModal :: Eq AgendaModal

type ValidationPanel =
  { itemId :: String
  , proposedMinutes :: Maybe Int
  , inputValue :: String
  }

type ViewState =
  { viewMode :: AgendaView
  , focusDate :: String
  , activeModal :: Maybe AgendaModal
  , validationPanel :: Maybe ValidationPanel
  , editPanel :: Maybe EditPanel
  , isMobile :: Boolean
  , lastTapAt :: Maybe Instant
  }

viewInitialState :: ViewState
viewInitialState =
  { viewMode: ViewDay
  , focusDate: ""
  , activeModal: Nothing
  , validationPanel: Nothing
  , editPanel: Nothing
  , isMobile: false
  , lastTapAt: Nothing
  }

_viewModeS :: Lens' ViewState AgendaView
_viewModeS = prop (Proxy :: _ "viewMode")

_viewFocusDate :: Lens' ViewState String
_viewFocusDate = prop (Proxy :: _ "focusDate")

_viewActiveModalS :: Lens' ViewState (Maybe AgendaModal)
_viewActiveModalS = prop (Proxy :: _ "activeModal")

_viewValidationPanelS :: Lens' ViewState (Maybe ValidationPanel)
_viewValidationPanelS = prop (Proxy :: _ "validationPanel")

_viewEditPanelS :: Lens' ViewState (Maybe EditPanel)
_viewEditPanelS = prop (Proxy :: _ "editPanel")

_viewIsMobileS :: Lens' ViewState Boolean
_viewIsMobileS = prop (Proxy :: _ "isMobile")

_viewLastTapAtS :: Lens' ViewState (Maybe Instant)
_viewLastTapAtS = prop (Proxy :: _ "lastTapAt")

type EditPanel =
  { item :: CalendarItem
  , draft :: EditDraft
  , validationError :: Maybe String
  }

data ViewAction
  = ViewOpenValidation String CalendarItemContent
  | ViewValidationMinutesChanged String
  | ViewConfirmValidation
  | ViewCancelValidation
  | ViewChangedAction String
  | ViewFocusDateChanged String
  | ViewOpenModal AgendaModal
  | ViewCloseModal
  | ViewOpenCreate
  | ViewCloseCreate
  | ViewOpenEdit CalendarItem
  | ViewOpenEditFromDoubleClick CalendarItem
  | ViewMobileTap CalendarItem
  | ViewEditTitleChanged String
  | ViewEditStartChanged String
  | ViewEditEndChanged String
  | ViewEditCategoryChanged String
  | ViewEditStatusChanged String
  | ViewEditDurationChanged String
  | ViewEditRecurrence RecurrenceAction
  | ViewEditSave
  | ViewEditCancel
  | ViewSetIsMobile Boolean

data ViewCommand
  = ViewValidateItem String Int
  | ViewUpdateItem String CalendarItem

handleViewAction :: ViewAction -> StateT ViewState (WriterT (Array ViewCommand) Aff) Unit
handleViewAction = case _ of
  ViewOpenValidation itemId content -> do
    suggested <- liftEffect $ suggestDurationMinutes content.windowStart
    modify_ (_viewValidationPanelS .~ Just { itemId, proposedMinutes: suggested, inputValue: "" })
  ViewValidationMinutesChanged raw ->
    modify_ (_viewValidationPanelS %~ map (_ { inputValue = raw }))
  ViewConfirmValidation -> do
    viewState <- get
    case viewState ^. _viewValidationPanelS of
      Nothing -> pure unit
      Just panel -> do
        let duration = parsePositiveInt panel.inputValue <|> panel.proposedMinutes
        case duration of
          Nothing -> pure unit
          Just minutes -> do
            modify_ (_viewValidationPanelS .~ Nothing)
            tell [ ViewValidateItem panel.itemId minutes ]
  ViewCancelValidation ->
    modify_ (_viewValidationPanelS .~ Nothing)
  ViewChangedAction raw ->
    modify_ (_viewModeS .~ parseAgendaView raw)
  ViewFocusDateChanged raw ->
    modify_ (_viewFocusDate .~ raw)
  ViewOpenModal modal ->
    modify_ (_viewActiveModalS .~ Just modal)
  ViewCloseModal ->
    modify_ (_viewActiveModalS .~ Nothing)
  ViewOpenCreate ->
    modify_ (_viewActiveModalS .~ Just ModalCreateItem)
  ViewCloseCreate ->
    modify_ (_viewActiveModalS .~ Nothing)
  ViewOpenEdit item ->
    case buildEditDraft item of
      Nothing -> pure unit
      Just draft ->
        modify_
          ( (_viewEditPanelS .~ Just { item, draft, validationError: Nothing })
              <<< (_viewActiveModalS .~ Just ModalEditItem)
              <<< (_viewLastTapAtS .~ Nothing)
          )
  ViewOpenEditFromDoubleClick item -> do
    viewport <- liftEffect $ window >>= Window.innerWidth
    if viewport <= 768 then
      handleViewAction (ViewOpenEdit item)
    else
      pure unit
  ViewMobileTap item -> do
    viewState <- get
    if viewState ^. _viewIsMobileS then do
      now <- liftEffect Now.now
      case viewState ^. _viewLastTapAtS of
        Nothing ->
          modify_ (_viewLastTapAtS .~ Just now)
        Just previous ->
          let
            elapsedMs = case diff now previous of Milliseconds ms -> ms
          in
            if elapsedMs <= 350.0 then
              modify_ (_viewLastTapAtS .~ Nothing) *> handleViewAction (ViewOpenEdit item)
            else
              modify_ (_viewLastTapAtS .~ Just now)
    else
      pure unit
  ViewEditTitleChanged raw ->
    modify_ (_viewEditPanelS %~ map (\panel -> panel { draft = panel.draft { title = raw }, validationError = Nothing }))
  ViewEditStartChanged raw ->
    modify_ (_viewEditPanelS %~ map (\panel -> panel { draft = panel.draft { windowStart = raw }, validationError = Nothing }))
  ViewEditEndChanged raw ->
    modify_ (_viewEditPanelS %~ map (\panel -> panel { draft = panel.draft { windowEnd = raw }, validationError = Nothing }))
  ViewEditCategoryChanged raw ->
    modify_ (_viewEditPanelS %~ map (\panel -> panel { draft = panel.draft { category = raw }, validationError = Nothing }))
  ViewEditStatusChanged raw ->
    modify_ (_viewEditPanelS %~ map (\panel -> panel { draft = panel.draft { status = parseStatus raw }, validationError = Nothing }))
  ViewEditDurationChanged raw ->
    modify_ (_viewEditPanelS %~ map (\panel -> panel { draft = panel.draft { actualDurationMinutes = raw }, validationError = Nothing }))
  ViewEditRecurrence action ->
    modify_ (_viewEditPanelS %~ map (\panel -> panel { draft = panel.draft { recurrence = applyRecurrenceAction action panel.draft.recurrence }, validationError = Nothing }))
  ViewEditSave -> do
    viewState <- get
    case viewState ^. _viewEditPanelS of
      Nothing -> pure unit
      Just panel ->
        case applyEditDraft panel.draft panel.item of
          Left err ->
            modify_ (_viewEditPanelS .~ Just (panel { validationError = Just (editErrorMessage err) }))
          Right updatedItem -> do
            tell [ ViewUpdateItem panel.draft.itemId updatedItem ]
            modify_ ((_viewEditPanelS .~ Nothing) <<< (_viewActiveModalS .~ Nothing))
  ViewEditCancel ->
    modify_ ((_viewEditPanelS .~ Nothing) <<< (_viewActiveModalS .~ Nothing))
  ViewSetIsMobile isMobile ->
    modify_ (_viewIsMobileS .~ isMobile)

viewTitle :: AgendaView -> String
viewTitle viewMode =
  case viewMode of
    ViewDay -> "Vue Jour"
    ViewWeek -> "Vue Semaine"
    ViewMonth -> "Vue Mois"

parseAgendaView :: String -> AgendaView
parseAgendaView raw =
  case raw of
    "week" -> ViewWeek
    "month" -> ViewMonth
    _ -> ViewDay

renderViewSelector :: forall w. AgendaView -> String -> HTML w ViewAction
renderViewSelector viewMode focusDate =
  div [ class_ "calendar-view-selector" ]
    [ div [ class_ "calendar-view-buttons" ]
        [ button
            [ class_ $ "btn btn-sm " <> if viewMode == ViewDay then "btn-primary" else "btn-outline-secondary"
            , onClick (const (ViewChangedAction "day"))
            ]
            [ text "Jour" ]
        , button
            [ class_ $ "btn btn-sm " <> if viewMode == ViewWeek then "btn-primary" else "btn-outline-secondary"
            , onClick (const (ViewChangedAction "week"))
            ]
            [ text "Semaine" ]
        , button
            [ class_ $ "btn btn-sm " <> if viewMode == ViewMonth then "btn-primary" else "btn-outline-secondary"
            , onClick (const (ViewChangedAction "month"))
            ]
            [ text "Mois" ]
        ]
    , div [ class_ "calendar-view-date-field" ]
        [ div [ class_ "calendar-notifications-label" ] [ text "Date de référence" ]
        , input
            [ class_ "form-control calendar-input calendar-view-date"
            , type_ InputDate
            , attr (AttrName "lang") "fr"
            , value focusDate
            , onValueChange ViewFocusDateChanged
            ]
        ]
    ]

renderMobileTools :: forall w. AgendaView -> HTML w ViewAction
renderMobileTools _ =
  div [ class_ "calendar-mobile-tools" ]
    [ button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (ViewOpenModal ModalFilters)) ] [ text "Filtres" ]
    , button [ class_ "btn btn-sm btn-primary", onClick (const (ViewOpenModal ModalTools)) ] [ text "Outils" ]
    ]

renderToolsContent :: forall w. HTML w ViewAction
renderToolsContent =
  div [ class_ "calendar-modal-stack" ]
    [ button [ class_ "btn btn-outline-secondary", onClick (const (ViewOpenModal ModalNotifications)) ] [ text "Rappels" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const (ViewOpenModal ModalTemplates)) ] [ text "Templates" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const (ViewOpenModal ModalImportCsv)) ] [ text "Import CSV" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const (ViewOpenModal ModalImportIcs)) ] [ text "Import ICS" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const (ViewOpenModal ModalExport)) ] [ text "Export" ]
    ]

renderEditContent :: forall w. EditPanel -> HTML w ViewAction
renderEditContent panel =
  let
    draft = panel.draft
  in
    div [ class_ "calendar-modal-stack" ]
      [ div [ class_ "calendar-modal-field" ]
          [ div [ class_ "calendar-notifications-label" ] [ text "Type" ]
          , div [ class_ "badge rounded-pill text-bg-secondary" ]
              [ text $ case draft.itemType of
                  Intention -> "Intention"
                  ScheduledBlock -> "Bloc planifié"
              ]
          ]
      , div [ class_ "calendar-modal-field" ]
          [ div [ class_ "calendar-notifications-label" ] [ text "Titre" ]
          , input
              [ class_ "form-control calendar-input"
              , placeholder "Titre"
              , onValueChange ViewEditTitleChanged
              , value draft.title
              ]
          ]
      , div [ class_ "calendar-modal-field" ]
          [ div [ class_ "calendar-notifications-label" ] [ text "Début" ]
          , input
              [ class_ "form-control calendar-input"
              , type_ InputDatetimeLocal
              , attr (AttrName "lang") "fr"
              , placeholder "Début"
              , onValueChange ViewEditStartChanged
              , value draft.windowStart
              ]
          ]
      , div [ class_ "calendar-modal-field" ]
          [ div [ class_ "calendar-notifications-label" ] [ text "Fin" ]
          , input
              [ class_ "form-control calendar-input"
              , type_ InputDatetimeLocal
              , attr (AttrName "lang") "fr"
              , placeholder "Fin"
              , onValueChange ViewEditEndChanged
              , value draft.windowEnd
              ]
          ]
      , div [ class_ "calendar-modal-field" ]
          [ div [ class_ "calendar-notifications-label" ] [ text "Catégorie" ]
          , input
              [ class_ "form-control calendar-input"
              , placeholder "Catégorie"
              , onValueChange ViewEditCategoryChanged
              , value draft.category
              ]
          ]
      , div [ class_ "calendar-modal-field" ]
          [ div [ class_ "calendar-notifications-label" ] [ text "Statut" ]
          , select
              [ class_ "form-select calendar-input"
              , onValueChange ViewEditStatusChanged
              , value (statusValue draft.status)
              ]
              [ option [ value "todo" ] [ text "À faire" ]
              , option [ value "progress" ] [ text "En cours" ]
              , option [ value "done" ] [ text "Fait" ]
              , option [ value "canceled" ] [ text "Annulé" ]
              ]
          ]
      , div [ class_ "calendar-modal-field" ]
          [ div [ class_ "calendar-notifications-label" ] [ text "Durée réelle (minutes)" ]
          , input
              [ class_ "form-control calendar-input"
              , type_ InputNumber
              , placeholder "Ex: 30"
              , onValueChange ViewEditDurationChanged
              , value draft.actualDurationMinutes
              ]
          ]
      , map ViewEditRecurrence (renderRecurrenceEditor draft.recurrence)
      , maybe (text "") (\msg -> div [ class_ "calendar-error" ] [ text msg ]) panel.validationError
      ]

renderValidationPanel :: forall w. ValidationPanel -> HTML w ViewAction
renderValidationPanel panel =
  div [ class_ "calendar-validation-panel" ]
    [ div [ class_ "calendar-conflict-title" ] [ text "Valider la tâche" ]
    , div [ class_ "calendar-conflict-subtitle" ]
        [ text "Saisissez la durée réelle (minutes) ou acceptez la proposition." ]
    , maybe (text "") (\minutes -> div [ class_ "calendar-validation-proposal" ] [ text $ "Proposition: " <> show minutes <> " min" ]) panel.proposedMinutes
    , input
        [ class_ "form-control calendar-input"
        , placeholder "Durée réelle (minutes)"
        , onValueChange ViewValidationMinutesChanged
        , value panel.inputValue
        ]
    , div [ class_ "calendar-conflict-confirmation-actions" ]
        [ button [ class_ "btn btn-sm btn-success", onClick (const ViewConfirmValidation) ] [ text "Confirmer" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ViewCancelValidation) ] [ text "Annuler" ]
        ]
    ]

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

editErrorMessage :: EditError -> String
editErrorMessage err =
  case err of
    EditValidation validation -> validationErrorMessage validation
    EditRecurrence msg -> msg
    EditDuration msg -> msg
    EditUnsupported -> "Impossible de modifier cet item."

validationErrorMessage :: ValidationError -> String
validationErrorMessage err =
  case err of
    TitleEmpty -> "Le titre est obligatoire."
    WindowStartInvalid -> "La date de début est invalide."
    WindowEndInvalid -> "La date de fin est invalide."
    WindowOrderInvalid -> "La fin doit être après le début."
    WindowTooShort -> "La fin doit être au minimum 5 minutes après le début."
