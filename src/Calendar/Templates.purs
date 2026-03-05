module Calendar.Templates
  ( module Calendar.Templates.Types
  , TemplatesOutput(..)
  , component
  , addTemplate
  , updateTemplate
  , removeTemplate
  , templateSummary
  , nextTemplateId
  , instantiateRoutine
  , applyDependencies
  ) where

import Prelude hiding (div)

import Data.Array (elem, filter, find, null)
import Data.DateTime (adjust, diff)
import Data.Int as Int
import Data.Lens (Lens', (.~), (%~), (^.), lens)
import Data.Maybe (Maybe(..), fromMaybe, maybe, maybe')
import Data.String.Common as StringCommon
import Data.Time.Duration (Minutes(..))
import Effect.Aff (Aff)
import Calendar.Templates.Types (RoutineInstance, RoutineInstanceStep, RoutineTemplate, RoutineTemplateStep, StepDependency(..), TaskTemplate, TemplateDraft, TemplateState(..), _templateDraftCategory, _templateDraftDuration, _templateDraftTitle, emptyTemplateDraft, templateInitialState, resetTemplateDraft)
import Helpers.DateTime as DateTime
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, mkComponent, mkEval, modify_, raise) as H
import Halogen.HTML (HTML, button, div, input, section, text)
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (placeholder, type_, value)
import DOM.HTML.Indexed.InputType (InputType(..))
import Ui.AgendaRender (renderPanelHeader)
import Ui.Utils (class_)

_templates :: Lens' TemplateState (Array TaskTemplate)
_templates = lens
  (\(TemplateState state) -> state.templates)
  (\(TemplateState state) templates -> TemplateState (state { templates = templates }))

_templateDraft :: Lens' TemplateState TemplateDraft
_templateDraft = lens
  (\(TemplateState state) -> state.templateDraft)
  (\(TemplateState state) templateDraft -> TemplateState (state { templateDraft = templateDraft }))

_editingTemplateId :: Lens' TemplateState (Maybe String)
_editingTemplateId = lens
  (\(TemplateState state) -> state.editingTemplateId)
  (\(TemplateState state) editingTemplateId -> TemplateState (state { editingTemplateId = editingTemplateId }))

data TemplateAction
  = Receive TemplateState
  | TemplateTitleChangedAction String
  | TemplateDurationChangedAction String
  | TemplateCategoryChangedAction String
  | TemplateSubmit
  | TemplateEdit String
  | TemplateCancelEdit
  | TemplateDelete String
  | TemplateUse String

data TemplatesOutput
  = TemplatesStateChanged TemplateState
  | TemplatesUse TaskTemplate

component :: forall q. H.Component q TemplateState TemplatesOutput Aff
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

handleAction :: TemplateAction -> H.HalogenM TemplateState TemplateAction () TemplatesOutput Aff Unit
handleAction = case _ of
  Receive st -> H.modify_ (const st)
  TemplateTitleChangedAction title -> modifyAndRaise (_templateDraft <<< _templateDraftTitle .~ title)
  TemplateDurationChangedAction duration -> modifyAndRaise (_templateDraft <<< _templateDraftDuration .~ duration)
  TemplateCategoryChangedAction category -> modifyAndRaise (_templateDraft <<< _templateDraftCategory .~ category)
  TemplateSubmit -> do
    st <- H.get
    let draft = st ^. _templateDraft
    parsePositiveInt draft.durationMinutes # maybe (pure unit) \duration ->
      if StringCommon.trim draft.title == "" then pure unit
      else do
        let
          template =
            { id: fromMaybe "" (st ^. _editingTemplateId)
            , title: draft.title
            , durationMinutes: duration
            , category: draft.category
            }
          nextTemplates = maybe' (const $ addTemplate template (st ^. _templates))
            (const $ updateTemplate template (st ^. _templates))
            (st ^. _editingTemplateId)
        modifyAndRaise $
          (_templates .~ nextTemplates) <<< (_templateDraft .~ emptyTemplateDraft) <<< (_editingTemplateId .~ Nothing)
  TemplateEdit templateId -> do
    st <- H.get
    find (\tpl -> tpl.id == templateId) (st ^. _templates) # maybe (pure unit) \template ->
      modifyAndRaise $
        (_templateDraft <<< _templateDraftTitle .~ template.title)
          <<< (_templateDraft <<< _templateDraftDuration .~ show template.durationMinutes)
          <<< (_templateDraft <<< _templateDraftCategory .~ template.category)
          <<< (_editingTemplateId .~ Just template.id)
  TemplateCancelEdit -> modifyAndRaise ((_templateDraft .~ emptyTemplateDraft) <<< (_editingTemplateId .~ Nothing))
  TemplateDelete templateId -> modifyAndRaise $
    (_templates %~ removeTemplate templateId) <<< (_templateDraft .~ emptyTemplateDraft) <<< (_editingTemplateId .~ Nothing)
  TemplateUse templateId -> do
    st <- H.get
    find (\tpl -> tpl.id == templateId) (st ^. _templates) # maybe (pure unit) \template -> H.raise (TemplatesUse template)
  where
  modifyAndRaise f = do
    H.modify_ f
    H.get >>= H.raise <<< TemplatesStateChanged

render :: forall m. TemplateState -> H.ComponentHTML TemplateAction () m
render (TemplateState { templates, templateDraft, editingTemplateId }) =
  section [ class_ "calendar-templates" ]
    [ renderHeader
    , renderForm templateDraft editingTemplateId
    , renderTemplatesList templates
    ]

renderHeader :: forall w. HTML w TemplateAction
renderHeader =
  renderPanelHeader
    { baseClass: "calendar-templates"
    , title: "Templates de tâches"
    , subtitle: "Créez des templates réutilisables pour accélérer la saisie."
    }
    []

renderForm :: forall w. TemplateDraft -> Maybe String -> HTML w TemplateAction
renderForm draft editingId =
  div [ class_ "calendar-templates-form" ]
    [ input
        [ class_ "form-control calendar-input"
        , placeholder "Titre du template"
        , value draft.title
        , onValueChange TemplateTitleChangedAction
        ]
    , div [ class_ "calendar-templates-row" ]
        [ input
            [ class_ "form-control calendar-input"
            , type_ InputNumber
            , placeholder "Durée (minutes)"
            , value draft.durationMinutes
            , onValueChange TemplateDurationChangedAction
            ]
        , input
            [ class_ "form-control calendar-input"
            , placeholder "Catégorie (optionnelle)"
            , value draft.category
            , onValueChange TemplateCategoryChangedAction
            ]
        ]
    , renderFormActions editingId
    ]

renderFormActions :: forall w. Maybe String -> HTML w TemplateAction
renderFormActions editingId =
  div [ class_ "calendar-templates-actions" ] $
    [ button [ class_ "btn btn-sm btn-primary", onClick (const TemplateSubmit) ] $
        [ text $ if editingId == Nothing then "Ajouter" else "Mettre à jour" ]
    ]
      <>
        if editingId == Nothing then []
        else
          [ button [ class_ "btn btn-sm btn-outline-secondary", onClick (const TemplateCancelEdit) ]
              [ text "Annuler" ]
          ]

renderTemplatesList :: forall w. Array TaskTemplate -> HTML w TemplateAction
renderTemplatesList templates =
  if null templates then
    div [ class_ "calendar-templates-empty" ] [ text "Aucun template pour l'instant." ]
  else
    div [ class_ "calendar-templates-list" ] (map renderTemplateCard templates)

renderTemplateCard :: forall w. TaskTemplate -> HTML w TemplateAction
renderTemplateCard template =
  div [ class_ "calendar-template-card" ]
    [ div [ class_ "calendar-template-main" ]
        [ div [ class_ "calendar-template-title" ] [ text template.title ]
        , div [ class_ "calendar-template-summary" ] [ text (templateSummary template) ]
        ]
    , div [ class_ "calendar-template-actions" ]
        [ button [ class_ "btn btn-sm btn-outline-primary", onClick (const (TemplateUse template.id)) ] [ text "Utiliser" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (TemplateEdit template.id)) ] [ text "Editer" ]
        , button [ class_ "btn btn-sm btn-outline-danger", onClick (const (TemplateDelete template.id)) ] [ text "Supprimer" ]
        ]
    ]

addTemplate :: TaskTemplate -> Array TaskTemplate -> Array TaskTemplate
addTemplate template templates =
  let
    nextId = if template.id == "" then nextTemplateId templates else template.id
  in
    templates <> [ template { id = nextId } ]

updateTemplate :: TaskTemplate -> Array TaskTemplate -> Array TaskTemplate
updateTemplate template templates =
  map (\existing -> if existing.id == template.id then template else existing) templates

removeTemplate :: String -> Array TaskTemplate -> Array TaskTemplate
removeTemplate templateId templates =
  filter (\template -> template.id /= templateId) templates

templateSummary :: TaskTemplate -> String
templateSummary template =
  let
    duration = show template.durationMinutes <> " min"
    category = StringCommon.trim template.category
  in
    if category == "" then duration else duration <> " • " <> category

nextTemplateId :: Array TaskTemplate -> String
nextTemplateId templates =
  let
    existing = map _.id templates
    findId n =
      let
        candidate = "tpl-" <> show n
      in
        if elem candidate existing then findId (n + 1) else candidate
  in
    findId 1

instantiateRoutine :: RoutineTemplate -> RoutineInstance
instantiateRoutine template =
  { templateId: template.id
  , steps: applyDependencies template.steps $ map toInstance template.steps
  }
  where
  toInstance step =
    { id: step.id
    , title: step.title
    , windowStart: step.windowStart
    , windowEnd: step.windowEnd
    , sourceStepId: step.id
    }

applyDependencies :: Array RoutineTemplateStep -> Array RoutineInstanceStep -> Array RoutineInstanceStep
applyDependencies templateSteps instanceSteps =
  map (applyDependency templateSteps instanceSteps) instanceSteps

applyDependency :: Array RoutineTemplateStep -> Array RoutineInstanceStep -> RoutineInstanceStep -> RoutineInstanceStep
applyDependency templateSteps instanceSteps step =
  find (\templateStep -> templateStep.id == step.sourceStepId) templateSteps # maybe step \templateStep ->
    templateStep.dependsOn # maybe step \dependency ->
      case dependency of
        StartAfterEnd { stepId, offsetMinutes } -> updateFromBase stepId offsetMinutes _.windowEnd
        StartBeforeStart { stepId, offsetMinutes } -> updateFromBase stepId (-offsetMinutes) _.windowStart
  where
  updateFromBase baseId offset selectBase =
    find (\candidate -> candidate.id == baseId) instanceSteps # maybe step \base ->
      let
        duration = durationMinutesBetween step.windowStart step.windowEnd
        newStart = shiftMinutes offset (selectBase base)
        newEnd = newStart >>= \start -> duration >>= \mins -> shiftMinutes mins start
      in
        case { start: newStart, end: newEnd } of
          { start: Just start, end: Just end } -> step { windowStart = start, windowEnd = end }
          _ -> step

parsePositiveInt :: String -> Maybe Int
parsePositiveInt raw =
  Int.fromString (StringCommon.trim raw) >>= \val ->
    if val > 0 then Just val else Nothing

shiftMinutes :: Int -> String -> Maybe String
shiftMinutes offset start = do
  dt <- DateTime.parseLocalDateTime start
  newDt <- adjust (Minutes (Int.toNumber offset)) dt
  pure $ DateTime.formatLocalDateTime newDt

durationMinutesBetween :: String -> String -> Maybe Int
durationMinutesBetween start end = do
  startDt <- DateTime.parseLocalDateTime start
  endDt <- DateTime.parseLocalDateTime end
  let
    Minutes n = diff endDt startDt
    minutes = Int.floor n
  pure $ max 1 minutes
