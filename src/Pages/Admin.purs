module Pages.Admin
  ( component
  , PendingSectionState(..)
  , PendingActionKind(..)
  , PendingActionState
  , ApprovedSectionState(..)
  , actionButtonsDisabled
  , applyPendingLoadSuccess
  , applyPendingLoadFailure
  , beginPendingAction
  , finishPendingAction
  , applyApprovedLoadSuccess
  , applyApprovedLoadFailure
  , openApprovedDeleteConfirmation
  , closeApprovedDeleteConfirmation
  , beginApprovedDelete
  , finishApprovedDelete
  , isApprovedUserDeletable
  ) where

import Prelude hiding (div)

import Affjax (Error, printError)
import Affjax.Web (Response)
import Api.Admin
  ( ApprovedUser(..)
  , PendingSignup(..)
  , PendingSignupApprovalPayload(..)
  , approvePendingSignupResponse
  , deleteApprovedUserResponse
  , deletePendingSignupResponse
  , getApprovedUsersResponse
  , getPendingSignupsResponse
  )
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Array (null)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval) as H
import Halogen.HTML (button, div, h1, h2, li, section, span, text, ul)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (disabled)
import Control.Monad.RWS (get, modify_)
import Ui.Modal as Modal
import Ui.Utils (class_)

type Input =
  { currentUsername :: String
  }

type PendingActionState =
  { username :: String
  , action :: PendingActionKind
  }

type State =
  { currentUsername :: String
  , pendingSection :: PendingSectionState
  , pendingAction :: Maybe PendingActionState
  , pendingFeedback :: Maybe String
  , approvedSection :: ApprovedSectionState
  , approvedDeleteTarget :: Maybe String
  , approvedDeleteInFlight :: Maybe String
  , approvedFeedback :: Maybe String
  }

data PendingSectionState
  = PendingLoading
  | PendingLoadError String
  | PendingLoaded (Array PendingSignup)

data ApprovedSectionState
  = ApprovedLoading
  | ApprovedLoadError String
  | ApprovedLoaded (Array ApprovedUser)

data PendingActionKind
  = PendingApprove
  | PendingDelete

derive instance pendingSectionStateGeneric :: Generic PendingSectionState _
derive instance approvedSectionStateGeneric :: Generic ApprovedSectionState _
derive instance pendingActionKindGeneric :: Generic PendingActionKind _
derive instance pendingSectionStateEq :: Eq PendingSectionState
derive instance approvedSectionStateEq :: Eq ApprovedSectionState
derive instance pendingActionKindEq :: Eq PendingActionKind

instance showPendingSectionState :: Show PendingSectionState where
  show = genericShow

instance showApprovedSectionState :: Show ApprovedSectionState where
  show = genericShow

instance showPendingActionKind :: Show PendingActionKind where
  show = genericShow

data Action
  = Initialize
  | RetryPendingLoad
  | ApprovePendingSignup String
  | DeletePendingSignup String
  | RetryApprovedLoad
  | RequestDeleteApprovedUser String
  | CancelDeleteApprovedUser
  | ConfirmDeleteApprovedUser

type AdminAppM = H.HalogenM State Action () Void Aff

component :: forall q. H.Component q Input Void Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }

initialState :: Input -> State
initialState { currentUsername } =
  { currentUsername
  , pendingSection: PendingLoading
  , pendingAction: Nothing
  , pendingFeedback: Nothing
  , approvedSection: ApprovedLoading
  , approvedDeleteTarget: Nothing
  , approvedDeleteInFlight: Nothing
  , approvedFeedback: Nothing
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

applyApprovedLoadSuccess :: Array ApprovedUser -> State -> State
applyApprovedLoadSuccess approvedUsers state =
  state
    { approvedSection = ApprovedLoaded approvedUsers
    , approvedDeleteTarget = Nothing
    , approvedDeleteInFlight = Nothing
    , approvedFeedback = Nothing
    }

applyApprovedLoadFailure :: String -> State -> State
applyApprovedLoadFailure message state =
  state
    { approvedSection = ApprovedLoadError message
    , approvedDeleteTarget = Nothing
    , approvedDeleteInFlight = Nothing
    , approvedFeedback = Nothing
    }

openApprovedDeleteConfirmation :: String -> State -> State
openApprovedDeleteConfirmation username state =
  state
    { approvedDeleteTarget = Just username
    , approvedFeedback = Nothing
    }

closeApprovedDeleteConfirmation :: State -> State
closeApprovedDeleteConfirmation state =
  state { approvedDeleteTarget = Nothing }

beginApprovedDelete :: String -> State -> State
beginApprovedDelete username state =
  state
    { approvedDeleteTarget = Nothing
    , approvedDeleteInFlight = Just username
    , approvedFeedback = Nothing
    }

finishApprovedDelete :: Maybe String -> State -> State
finishApprovedDelete maybeFeedback state =
  state
    { approvedDeleteTarget = Nothing
    , approvedDeleteInFlight = Nothing
    , approvedFeedback = maybeFeedback
    }

actionButtonsDisabled :: State -> Boolean
actionButtonsDisabled = isJust <<< _.pendingAction

isApprovedUserDeletable :: String -> ApprovedUser -> Boolean
isApprovedUserDeletable currentUsername (ApprovedUser { username }) = username /= currentUsername

render :: State -> H.ComponentHTML Action () Aff
render state =
  div [ class_ "row justify-content-center mt-4" ]
    [ div [ class_ "col-12 col-lg-8" ]
        [ div [ class_ "card shadow-sm border-0" ]
            [ div [ class_ "card-body p-4" ]
                ( [ h1 [ class_ "h3 mb-3" ] [ text "Administration utilisateurs" ]
                  , div [ class_ "text-muted mb-4" ] [ text "Cet espace servira a gerer les comptes en attente et les utilisateurs existants." ]
                  , renderPendingSection state
                  , renderApprovedSection state
                  ]
                    <> renderDeleteConfirmationModal state
                )
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

renderApprovedSection :: State -> H.ComponentHTML Action () Aff
renderApprovedSection state =
  section [ class_ "mb-1 admin-approved-section" ]
    ( [ h2 [ class_ "h5 mb-2" ] [ text "Utilisateurs existants" ] ]
        <> renderApprovedFeedback state.approvedFeedback
        <> case state.approvedSection of
          ApprovedLoading ->
            [ div [ class_ "text-muted admin-approved-loading" ] [ text "Chargement..." ] ]
          ApprovedLoadError message ->
            [ div [ class_ "alert alert-danger d-flex flex-column align-items-start gap-2 admin-approved-error" ]
                [ div_ [ text message ]
                , button
                    [ class_ "btn btn-outline-danger btn-sm"
                    , onClick (const RetryApprovedLoad)
                    , disabled (approvedDeleteButtonsDisabled state)
                    ]
                    [ text "Reessayer" ]
                ]
            ]
          ApprovedLoaded approvedUsers | null approvedUsers ->
            [ div [ class_ "text-muted admin-approved-empty" ] [ text "Aucun utilisateur existant." ] ]
          ApprovedLoaded approvedUsers ->
            [ ul [ class_ "list-group admin-approved-list" ] (map (renderApprovedUserRow state) approvedUsers) ]
    )

renderPendingFeedback :: Maybe String -> Array (H.ComponentHTML Action () Aff)
renderPendingFeedback =
  case _ of
    Nothing -> []
    Just message -> [ div [ class_ "alert alert-danger admin-pending-action-feedback" ] [ text message ] ]

renderApprovedFeedback :: Maybe String -> Array (H.ComponentHTML Action () Aff)
renderApprovedFeedback =
  case _ of
    Nothing -> []
    Just message -> [ div [ class_ "alert alert-danger admin-approved-action-feedback" ] [ text message ] ]

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

renderApprovedUserRow :: State -> ApprovedUser -> H.ComponentHTML Action () Aff
renderApprovedUserRow state approvedUser@(ApprovedUser { username }) =
  li [ class_ "list-group-item d-flex justify-content-between align-items-center gap-3 flex-wrap admin-approved-row" ]
    [ div [ class_ "d-flex flex-column gap-1" ]
        [ span [ class_ "fw-semibold admin-approved-username" ] [ text username ]
        , if isApprovedUserDeletable state.currentUsername approvedUser then
            text ""
          else
            span [ class_ "text-muted small admin-approved-protected" ] [ text "Suppression indisponible pour votre compte." ]
        ]
    , button
        [ class_ "btn btn-sm btn-outline-danger"
        , onClick (const (RequestDeleteApprovedUser username))
        , disabled (approvedDeleteButtonDisabled state approvedUser)
        ]
        [ text (if isApprovedDeleteInFlight state username then "Suppression..." else "Supprimer") ]
    ]

renderDeleteConfirmationModal :: State -> Array (H.ComponentHTML Action () Aff)
renderDeleteConfirmationModal { approvedDeleteTarget } =
  case approvedDeleteTarget of
    Nothing -> []
    Just username ->
      [ div [ class_ "admin-delete-confirm-modal" ]
          [ Modal.renderModalWithActionState
              "Confirmer la suppression"
              [ div [ class_ "text-muted" ]
                  [ text ("Voulez-vous vraiment supprimer l'utilisateur " <> username <> " ? Cette action est irreversible.") ]
              ]
              CancelDeleteApprovedUser
              { action: ConfirmDeleteApprovedUser
              , disabled: false
              , label: "Supprimer"
              }
          ]
      ]

div_ :: forall i. Array (H.ComponentHTML i () Aff) -> H.ComponentHTML i () Aff
div_ = div []

isPendingAction :: State -> String -> PendingActionKind -> Boolean
isPendingAction { pendingAction } username action =
  case pendingAction of
    Just current -> current.username == username && current.action == action
    Nothing -> false

approvedDeleteButtonsDisabled :: State -> Boolean
approvedDeleteButtonsDisabled = isJust <<< _.approvedDeleteInFlight

approvedDeleteButtonDisabled :: State -> ApprovedUser -> Boolean
approvedDeleteButtonDisabled state approvedUser =
  approvedDeleteButtonsDisabled state || not (isApprovedUserDeletable state.currentUsername approvedUser)

isApprovedDeleteInFlight :: State -> String -> Boolean
isApprovedDeleteInFlight { approvedDeleteInFlight } username =
  case approvedDeleteInFlight of
    Just currentUsername -> currentUsername == username
    Nothing -> false

handleAction :: Action -> AdminAppM Unit
handleAction = case _ of
  Initialize -> do
    loadPendingSignups
    loadApprovedUsers
  RetryPendingLoad -> loadPendingSignups
  ApprovePendingSignup username -> do
    modify_ (beginPendingAction username PendingApprove)
    result <- liftAff $ approvePendingSignupResponse (PendingSignupApprovalPayload { username })
    handlePendingRowActionResult result
  DeletePendingSignup username -> do
    modify_ (beginPendingAction username PendingDelete)
    result <- liftAff $ deletePendingSignupResponse username
    handlePendingRowActionResult result
  RetryApprovedLoad -> loadApprovedUsers
  RequestDeleteApprovedUser username ->
    modify_ (openApprovedDeleteConfirmation username)
  CancelDeleteApprovedUser ->
    modify_ closeApprovedDeleteConfirmation
  ConfirmDeleteApprovedUser -> do
    state <- get
    case state.approvedDeleteTarget of
      Nothing -> pure unit
      Just username -> do
        modify_ (beginApprovedDelete username)
        result <- liftAff $ deleteApprovedUserResponse username
        handleApprovedDeleteResult result

handlePendingRowActionResult :: Either Error (Response String) -> AdminAppM Unit
handlePendingRowActionResult result =
  case result of
    Left err ->
      modify_ (finishPendingAction (Just (printError err)))
    Right response ->
      if statusOk response then
        loadPendingSignups
      else do
        let message = responseMessageFromTextResponse "Impossible de traiter la demande." response
        modify_ (finishPendingAction (Just message))

handleApprovedDeleteResult :: Either Error (Response String) -> AdminAppM Unit
handleApprovedDeleteResult result =
  case result of
    Left err ->
      modify_ (finishApprovedDelete (Just (printError err)))
    Right response ->
      if statusOk response then
        loadApprovedUsers
      else do
        let message = responseMessageFromTextResponse "Impossible de supprimer l'utilisateur." response
        modify_ (finishApprovedDelete (Just message))

loadPendingSignups :: AdminAppM Unit
loadPendingSignups = do
  modify_ _ { pendingSection = PendingLoading, pendingFeedback = Nothing }
  result <- liftAff getPendingSignupsResponse
  case result of
    Left err ->
      modify_ (applyPendingLoadFailure (printError err))
    Right response ->
      if statusOk response then
        case lmap show (decodeJson response.body) of
          Right pendingSignups ->
            modify_ (applyPendingLoadSuccess pendingSignups)
          Left _ ->
            modify_ (applyPendingLoadFailure "Impossible de decoder les comptes en attente.")
      else
        modify_ (applyPendingLoadFailure (responseMessageFromJsonResponse "Impossible de charger les comptes en attente." response))

loadApprovedUsers :: AdminAppM Unit
loadApprovedUsers = do
  modify_ _ { approvedSection = ApprovedLoading, approvedFeedback = Nothing, approvedDeleteTarget = Nothing }
  result <- liftAff getApprovedUsersResponse
  case result of
    Left err ->
      modify_ (applyApprovedLoadFailure (printError err))
    Right response ->
      if statusOk response then
        case lmap show (decodeJson response.body) of
          Right approvedUsers ->
            modify_ (applyApprovedLoadSuccess approvedUsers)
          Left _ ->
            modify_ (applyApprovedLoadFailure "Impossible de decoder les utilisateurs existants.")
      else
        modify_ (applyApprovedLoadFailure (responseMessageFromJsonResponse "Impossible de charger les utilisateurs existants." response))

statusOk :: forall a. Response a -> Boolean
statusOk response = unwrap response.status >= 200 && unwrap response.status < 300

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
