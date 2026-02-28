module Agenda.Display
  ( ViewState
  , ViewAction(..)
  , AgendaModal(..)
  , ValidationPanel
  , viewInitialState
  , viewTitle
  , parseAgendaView
  , renderViewSelector
  , renderMobileTools
  , renderValidationPanel
  , renderToolsContent
  , handleViewAction
  , _viewModeS
  , _viewFocusDateS
  , _viewActiveModalS
  , _viewValidationPanelS
  ) where

import Prelude hiding (div)

import Agenda.Commands (ViewCommand(..), Command(..), tellCmd)
import Agenda.Helpers (parsePositiveInt, suggestDurationMinutes)
import Agenda.Model (AgendaView(..), CalendarItemContent)
import Control.Alt ((<|>))
import Control.Monad.State.Trans (StateT, get, modify_)
import Control.Monad.Writer.Trans (WriterT)
import Data.Lens (Lens', (.~), (%~), (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen.HTML (HTML, button, div, input, text)
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (attr, placeholder, type_, value)
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
  | ModalDateTime

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
  }


viewInitialState :: ViewState
viewInitialState =
  { viewMode: ViewDay
  , focusDate: ""
  , activeModal: Nothing
  , validationPanel: Nothing
  }


_viewModeS :: Lens' ViewState AgendaView
_viewModeS = prop (Proxy :: _ "viewMode")

_viewFocusDateS :: Lens' ViewState String
_viewFocusDateS = prop (Proxy :: _ "focusDate")

_viewActiveModalS :: Lens' ViewState (Maybe AgendaModal)
_viewActiveModalS = prop (Proxy :: _ "activeModal")

_viewValidationPanelS :: Lens' ViewState (Maybe ValidationPanel)
_viewValidationPanelS = prop (Proxy :: _ "validationPanel")


data ViewAction
  = ViewOpenValidation String CalendarItemContent
  | ViewValidationMinutesChanged String
  | ViewConfirmValidation
  | ViewCancelValidation
  | ViewChangedAction String
  | ViewFocusDateChanged String
  | ViewOpenModal AgendaModal
  | ViewCloseModal


handleViewAction :: ViewAction -> StateT ViewState (WriterT (Array Command) Aff) Unit
handleViewAction = case _ of
  ViewOpenValidation itemId content -> do
    suggested <- liftEffect $ suggestDurationMinutes content.windowStart
    modify_ (_viewValidationPanelS .~ Just { itemId, proposedMinutes: suggested, inputValue: "" })
  ViewValidationMinutesChanged raw ->
    modify_ (_viewValidationPanelS %~ map (\panel -> panel { inputValue = raw }))
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
            tellCmd $ ViewCmd (ViewValidateItem panel.itemId minutes)
  ViewCancelValidation ->
    modify_ (_viewValidationPanelS .~ Nothing)
  ViewChangedAction raw ->
    modify_ (_viewModeS .~ parseAgendaView raw)
  ViewFocusDateChanged raw ->
    modify_ (_viewFocusDateS .~ raw)
  ViewOpenModal modal ->
    modify_ (_viewActiveModalS .~ Just modal)
  ViewCloseModal ->
    modify_ (_viewActiveModalS .~ Nothing)


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

renderViewSelector :: forall w action. (ViewAction -> action) -> AgendaView -> String -> HTML w action
renderViewSelector onAction viewMode focusDate =
  div [ class_ "agenda-view-selector" ]
    [ div [ class_ "agenda-view-buttons" ]
        [ button
            [ class_ $ "btn btn-sm " <> if viewMode == ViewDay then "btn-primary" else "btn-outline-secondary"
            , onClick (const $ onAction (ViewChangedAction "day"))
            ]
            [ text "Jour" ]
        , button
            [ class_ $ "btn btn-sm " <> if viewMode == ViewWeek then "btn-primary" else "btn-outline-secondary"
            , onClick (const $ onAction (ViewChangedAction "week"))
            ]
            [ text "Semaine" ]
        , button
            [ class_ $ "btn btn-sm " <> if viewMode == ViewMonth then "btn-primary" else "btn-outline-secondary"
            , onClick (const $ onAction (ViewChangedAction "month"))
            ]
            [ text "Mois" ]
        ]
    , div [ class_ "agenda-view-date-field" ]
        [ div [ class_ "agenda-notifications-label" ] [ text "Date de reference" ]
        , input
            [ class_ "form-control agenda-input agenda-view-date"
            , type_ InputDate
            , attr (AttrName "lang") "fr"
            , value focusDate
            , onValueChange (onAction <<< ViewFocusDateChanged)
            ]
        ]
    ]

renderMobileTools :: forall w action. (ViewAction -> action) -> AgendaView -> HTML w action
renderMobileTools onAction viewMode =
  if viewMode /= ViewDay then text ""
  else
    div [ class_ "agenda-mobile-tools" ]
      [ button [ class_ "btn btn-sm btn-outline-secondary", onClick (const $ onAction (ViewOpenModal ModalFilters)) ] [ text "Filtres" ]
      , button [ class_ "btn btn-sm btn-primary", onClick (const $ onAction (ViewOpenModal ModalTools)) ] [ text "Outils" ]
      ]

renderToolsContent :: forall w action. (ViewAction -> action) -> HTML w action
renderToolsContent onAction =
  div [ class_ "agenda-modal-stack" ]
    [ button [ class_ "btn btn-outline-secondary", onClick (const $ onAction (ViewOpenModal ModalNotifications)) ] [ text "Rappels" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const $ onAction (ViewOpenModal ModalTemplates)) ] [ text "Templates" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const $ onAction (ViewOpenModal ModalImportCsv)) ] [ text "Import CSV" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const $ onAction (ViewOpenModal ModalImportIcs)) ] [ text "Import ICS" ]
    , button [ class_ "btn btn-outline-secondary", onClick (const $ onAction (ViewOpenModal ModalExport)) ] [ text "Export" ]
    ]

renderValidationPanel :: forall w action. (ViewAction -> action) -> ValidationPanel -> HTML w action
renderValidationPanel onAction panel =
  div [ class_ "agenda-validation-panel" ]
    [ div [ class_ "agenda-conflict-title" ] [ text "Valider la tache" ]
    , div [ class_ "agenda-conflict-subtitle" ]
        [ text "Saisissez la duree reelle (minutes) ou acceptez la proposition." ]
    , maybe (text "") (\minutes -> div [ class_ "agenda-validation-proposal" ] [ text $ "Proposition: " <> show minutes <> " min" ]) panel.proposedMinutes
    , input
        [ class_ "form-control agenda-input"
        , placeholder "Duree reelle (minutes)"
        , onValueChange (onAction <<< ViewValidationMinutesChanged)
        , value panel.inputValue
        ]
    , div [ class_ "agenda-conflict-confirmation-actions" ]
        [ button [ class_ "btn btn-sm btn-success", onClick (const (onAction ViewConfirmValidation)) ] [ text "Confirmer" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (onAction ViewCancelValidation)) ] [ text "Annuler" ]
        ]
    ]
