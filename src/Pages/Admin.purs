module Pages.Admin
  ( component
  , PendingSectionState(..)
  , PendingActionKind(..)
  , PendingActionState
  , actionButtonsDisabled
  , applyPendingLoadSuccess
  , applyPendingLoadFailure
  , beginPendingAction
  , finishPendingAction
  ) where

import Prelude hiding (div)

import Affjax (Error)
import Affjax.Web (Response)
import Api.Admin (PendingSignup(..), PendingSignupApprovalPayload(..), approvePendingSignupResponse, deletePendingSignupResponse, getPendingSignupsResponse)
import Control.Monad.Except (ExceptT(..), withExceptT)
import Control.Monad.RWS (modify_)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Array (null)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval) as H
import Halogen.HTML (button, div, h1, h2, li, section, span, text, ul)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (disabled)
import Data.Show.Generic (genericShow)
import Ui.Errors (FatalError, handleError, toFatalError)
import Ui.Utils (class_)

component :: forall q i. H.Component q i Void Aff
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }

type PendingActionState =
  { username :: String
  , action :: PendingActionKind
  }

type State =
  { pendingSection :: PendingSectionState
  , pendingAction :: Maybe PendingActionState
  , pendingFeedback :: Maybe String
  }

data PendingSectionState
  = PendingLoading
  | PendingLoadError String
  | PendingLoaded (Array PendingSignup)

data PendingActionKind
  = PendingApprove
  | PendingDelete

derive instance pendingSectionStateGeneric :: Generic PendingSectionState _
derive instance pendingActionKindGeneric :: Generic PendingActionKind _
derive instance pendingSectionStateEq :: Eq PendingSectionState
derive instance pendingActionKindEq :: Eq PendingActionKind

instance showPendingSectionState :: Show PendingSectionState where
  show = genericShow

instance showPendingActionKind :: Show PendingActionKind where
  show = genericShow

data Action
  = Initialize
  | RetryPendingLoad
  | ApprovePendingSignup String
  | DeletePendingSignup String

type AdminAppM = H.HalogenM State Action () Void Aff
type ErrorAdminAppM = ExceptT FatalError AdminAppM

initialState :: State
initialState =
  { pendingSection: PendingLoading
  , pendingAction: Nothing
  , pendingFeedback: Nothing
  }

applyPendingLoadSuccess :: Array PendingSignup -> State -> State
applyPendingLoadSuccess pendingSignups state =
  state
    { pendingSection = PendingLoaded pendingSignups
    , pendingAction = Nothing
    , pendingFeedback = Nothing
    }

applyPendingLoadFailure :: String -> State -> State
applyPendingLoadFailure message state =
  state
    { pendingSection = PendingLoadError message
    , pendingAction = Nothing
    , pendingFeedback = Nothing
    }

beginPendingAction :: String -> PendingActionKind -> State -> State
beginPendingAction username action state =
  state
    { pendingAction = Just { username, action }
    , pendingFeedback = Nothing
    }

finishPendingAction :: Maybe String -> State -> State
finishPendingAction maybeFeedback state =
  state
    { pendingAction = Nothing
    , pendingFeedback = maybeFeedback
    }

actionButtonsDisabled :: State -> Boolean
actionButtonsDisabled = isJust <<< _.pendingAction

render :: State -> H.ComponentHTML Action () Aff
render state =
  div [ class_ "row justify-content-center mt-4" ]
    [ div [ class_ "col-12 col-lg-8" ]
        [ div [ class_ "card shadow-sm border-0" ]
            [ div [ class_ "card-body p-4" ]
                [ h1 [ class_ "h3 mb-3" ] [ text "Administration utilisateurs" ]
                , div [ class_ "text-muted mb-4" ] [ text "Cet espace servira a gerer les comptes en attente et les utilisateurs existants." ]
                , renderPendingSection state
                , section [ class_ "mb-1" ]
                    [ h2 [ class_ "h5 mb-2" ] [ text "Utilisateurs existants" ]
                    , div [ class_ "text-muted" ] [ text "La gestion des suppressions sera ajoutee dans une story dediee." ]
                    ]
                ]
            ]
        ]
    ]

renderPendingSection :: State -> H.ComponentHTML Action () Aff
renderPendingSection state =
  section [ class_ "mb-4 admin-pending-section" ]
    ( [ h2 [ class_ "h5 mb-2" ] [ text "Comptes en attente" ] ]
        <> renderPendingFeedback state.pendingFeedback
        <> case state.pendingSection of
          PendingLoading ->
            [ div [ class_ "text-muted admin-pending-loading" ] [ text "Chargement..." ] ]
          PendingLoadError message ->
            [ div [ class_ "alert alert-danger d-flex flex-column align-items-start gap-2 admin-pending-error" ]
                [ div_ [ text message ]
                , button [ class_ "btn btn-outline-danger btn-sm", onClick (const RetryPendingLoad), disabled (actionButtonsDisabled state) ] [ text "Reessayer" ]
                ]
            ]
          PendingLoaded pendingSignups | null pendingSignups ->
            [ div [ class_ "text-muted admin-pending-empty" ] [ text "Aucun compte en attente." ] ]
          PendingLoaded pendingSignups ->
            [ ul [ class_ "list-group admin-pending-list" ] (map (renderPendingSignupRow state) pendingSignups) ]
    )

div_ :: forall i. Array (H.ComponentHTML i () Aff) -> H.ComponentHTML i () Aff
div_ = div []

renderPendingFeedback :: Maybe String -> Array (H.ComponentHTML Action () Aff)
renderPendingFeedback =
  case _ of
    Nothing -> []
    Just message -> [ div [ class_ "alert alert-danger admin-pending-action-feedback" ] [ text message ] ]

renderPendingSignupRow :: State -> PendingSignup -> H.ComponentHTML Action () Aff
renderPendingSignupRow state (PendingSignup { username }) =
  li [ class_ "list-group-item d-flex justify-content-between align-items-center gap-3 flex-wrap admin-pending-row" ]
    [ span [ class_ "fw-semibold admin-pending-username" ] [ text username ]
    , div [ class_ "d-flex gap-2" ]
        [ button
            [ class_ "btn btn-sm btn-primary"
            , onClick (const (ApprovePendingSignup username))
            , disabled (actionButtonsDisabled state)
            ]
            [ text (if isPendingAction state username PendingApprove then "Approbation..." else "Approuver") ]
        , button
            [ class_ "btn btn-sm btn-outline-danger"
            , onClick (const (DeletePendingSignup username))
            , disabled (actionButtonsDisabled state)
            ]
            [ text (if isPendingAction state username PendingDelete then "Suppression..." else "Supprimer") ]
        ]
    ]

isPendingAction :: State -> String -> PendingActionKind -> Boolean
isPendingAction { pendingAction } username action =
  case pendingAction of
    Just current -> current.username == username && current.action == action
    Nothing -> false

handleAction :: Action -> AdminAppM Unit
handleAction action = handleError case action of
  Initialize -> loadPendingSignups
  RetryPendingLoad -> loadPendingSignups
  ApprovePendingSignup username -> do
    modify_ (beginPendingAction username PendingApprove)
    result <- liftAff $ approvePendingSignupResponse (PendingSignupApprovalPayload { username })
    handleRowActionResult result
  DeletePendingSignup username -> do
    modify_ (beginPendingAction username PendingDelete)
    result <- liftAff $ deletePendingSignupResponse username
    handleRowActionResult result

handleRowActionResult :: Either Error (Response String) -> ErrorAdminAppM Unit
handleRowActionResult result =
  case result of
    Left err ->
      lift $ modify_ (finishPendingAction (Just (show (toFatalError err))))
    Right response ->
      if unwrap response.status >= 200 && unwrap response.status < 300 then
        loadPendingSignups
      else do
        let message = responseMessageFromTextResponse "Impossible de traiter la demande." response
        lift $ modify_ (finishPendingAction (Just message))

loadPendingSignups :: ErrorAdminAppM Unit
loadPendingSignups = do
  lift $ modify_ _ { pendingSection = PendingLoading, pendingFeedback = Nothing }
  response <- withExceptT toFatalError $ ExceptT $ liftAff getPendingSignupsResponse
  if unwrap response.status >= 200 && unwrap response.status < 300 then
    case lmap show (decodeJson response.body) of
      Right pendingSignups ->
        lift $ modify_ (applyPendingLoadSuccess pendingSignups)
      Left _ ->
        lift $ modify_ (applyPendingLoadFailure "Impossible de decoder les comptes en attente.")
  else
    lift $ modify_ (applyPendingLoadFailure (responseMessageFromJsonResponse "Impossible de charger les comptes en attente." response))

responseMessageFromJsonResponse :: String -> Response Json -> String
responseMessageFromJsonResponse fallback response =
  case decodeJson response.body :: Either _ { message :: String } of
    Right { message } -> message
    Left _ -> fallback

responseMessageFromTextResponse :: String -> Response String -> String
responseMessageFromTextResponse fallback response =
  case parseJson response.body >>= \json -> decodeJson json :: Either _ { message :: String } of
    Right { message } -> message
    Left _ ->
      if response.body == "" then fallback else response.body
