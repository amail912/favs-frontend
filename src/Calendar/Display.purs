module Calendar.Display
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
  , _viewFocusDate
  , _viewActiveModalS
  , _viewValidationPanelS
  ) where

import Prelude hiding (div)

import Calendar.Commands (ViewCommand(..), Command(..), tellCmd)
import Calendar.Helpers (parsePositiveInt, suggestDurationMinutes)
import Calendar.Model (AgendaView(..), CalendarItemContent)
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

_viewFocusDate :: Lens' ViewState String
_viewFocusDate = prop (Proxy :: _ "focusDate")

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
    modify_ (_viewFocusDate .~ raw)
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
        [ div [ class_ "calendar-notifications-label" ] [ text "Date de reference" ]
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
renderMobileTools viewMode =
  if viewMode /= ViewDay then text ""
  else
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

renderValidationPanel :: forall w. ValidationPanel -> HTML w ViewAction
renderValidationPanel panel =
  div [ class_ "calendar-validation-panel" ]
    [ div [ class_ "calendar-conflict-title" ] [ text "Valider la tache" ]
    , div [ class_ "calendar-conflict-subtitle" ]
        [ text "Saisissez la duree reelle (minutes) ou acceptez la proposition." ]
    , maybe (text "") (\minutes -> div [ class_ "calendar-validation-proposal" ] [ text $ "Proposition: " <> show minutes <> " min" ]) panel.proposedMinutes
    , input
        [ class_ "form-control calendar-input"
        , placeholder "Duree reelle (minutes)"
        , onValueChange ViewValidationMinutesChanged
        , value panel.inputValue
        ]
    , div [ class_ "calendar-conflict-confirmation-actions" ]
        [ button [ class_ "btn btn-sm btn-success", onClick (const ViewConfirmValidation) ] [ text "Confirmer" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ViewCancelValidation) ] [ text "Annuler" ]
        ]
    ]
