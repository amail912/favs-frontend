module Calendar.Templates
  ( TemplateState
  , TemplateAction(..)
  , templateInitialState
  , handleTemplateAction
  , renderTemplatesPanel
  , applyTemplateToDraft
  , addTemplate
  , updateTemplate
  , removeTemplate
  , templateSummary
  , instantiateRoutine
  ) where

import Prelude hiding (div)

import Calendar.Commands (TemplateCommand(..), Command(..), tellCmd)
import Calendar.Helpers (durationMinutesBetween, formatDateTimeLocal, parsePositiveInt, shiftMinutes)
import Calendar.Model
  ( IntentionDraft
  , RoutineInstance
  , RoutineInstanceStep
  , RoutineTemplate
  , RoutineTemplateStep
  , StepDependency(..)
  , TaskTemplate
  , TemplateDraft
  , emptyTemplateDraft
  )
import Control.Monad.State.Trans (StateT, get, modify_)
import Control.Monad.Writer.Trans (WriterT)
import Data.Array (elem, filter, find, null)
import Data.Lens (Lens', (.~), (%~), (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common as StringCommon
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Halogen.HTML (HTML, button, div, input, section, text)
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (placeholder, type_, value)
import Type.Proxy (Proxy(..))
import DOM.HTML.Indexed.InputType (InputType(..))
import Ui.AgendaRender (renderPanelHeader)
import Ui.Utils (class_)


type TemplateState =
  { templates :: Array TaskTemplate
  , templateDraft :: TemplateDraft
  , editingTemplateId :: Maybe String
  }


templateInitialState :: TemplateState
templateInitialState =
  { templates: []
  , templateDraft: emptyTemplateDraft
  , editingTemplateId: Nothing
  }


_templatesS :: Lens' TemplateState (Array TaskTemplate)
_templatesS = prop (Proxy :: _ "templates")

_templateDraftS :: Lens' TemplateState TemplateDraft
_templateDraftS = prop (Proxy :: _ "templateDraft")

_editingTemplateIdS :: Lens' TemplateState (Maybe String)
_editingTemplateIdS = prop (Proxy :: _ "editingTemplateId")

_templateDraftTitleS :: Lens' TemplateDraft String
_templateDraftTitleS = prop (Proxy :: _ "title")

_templateDraftDurationS :: Lens' TemplateDraft String
_templateDraftDurationS = prop (Proxy :: _ "durationMinutes")

_templateDraftCategoryS :: Lens' TemplateDraft String
_templateDraftCategoryS = prop (Proxy :: _ "category")


data TemplateAction
  = TemplateTitleChangedAction String
  | TemplateDurationChangedAction String
  | TemplateCategoryChangedAction String
  | TemplateSubmit
  | TemplateEdit String
  | TemplateCancelEdit
  | TemplateDelete String
  | TemplateUse String


handleTemplateAction :: TemplateAction -> StateT TemplateState (WriterT (Array Command) Aff) Unit
handleTemplateAction = case _ of
  TemplateTitleChangedAction title ->
    modify_ (_templateDraftS <<< _templateDraftTitleS .~ title)
  TemplateDurationChangedAction duration ->
    modify_ (_templateDraftS <<< _templateDraftDurationS .~ duration)
  TemplateCategoryChangedAction category ->
    modify_ (_templateDraftS <<< _templateDraftCategoryS .~ category)
  TemplateSubmit -> do
    st <- get
    let
      draft = st ^. _templateDraftS
      duration = parsePositiveInt draft.durationMinutes
    case duration of
      Nothing -> pure unit
      Just minutes -> do
        let
          template =
            { id: fromMaybe "" (st ^. _editingTemplateIdS)
            , title: StringCommon.trim draft.title
            , durationMinutes: minutes
            , category: draft.category
            }
        if template.title == "" then pure unit
        else do
          let
            nextTemplates =
              case st ^. _editingTemplateIdS of
                Nothing -> addTemplate template (st ^. _templatesS)
                Just _ -> updateTemplate template (st ^. _templatesS)
          modify_
            ( (_templatesS .~ nextTemplates)
                <<< (_templateDraftS .~ emptyTemplateDraft)
                <<< (_editingTemplateIdS .~ Nothing)
            )
  TemplateEdit templateId -> do
    st <- get
    case find (\tpl -> tpl.id == templateId) (st ^. _templatesS) of
      Nothing -> pure unit
      Just template ->
        modify_
          ( (_templateDraftS <<< _templateDraftTitleS .~ template.title)
              <<< (_templateDraftS <<< _templateDraftDurationS .~ show template.durationMinutes)
              <<< (_templateDraftS <<< _templateDraftCategoryS .~ template.category)
              <<< (_editingTemplateIdS .~ Just template.id)
          )
  TemplateCancelEdit ->
    modify_ ((_templateDraftS .~ emptyTemplateDraft) <<< (_editingTemplateIdS .~ Nothing))
  TemplateDelete templateId ->
    modify_
      ( (_templatesS %~ removeTemplate templateId)
          <<< (_templateDraftS .~ emptyTemplateDraft)
          <<< (_editingTemplateIdS .~ Nothing)
      )
  TemplateUse templateId -> do
    st <- get
    case find (\tpl -> tpl.id == templateId) (st ^. _templatesS) of
      Nothing -> pure unit
      Just template -> do
        now <- liftEffect nowDateTime
        let startStr = formatDateTimeLocal now
        let endStr = fromMaybe startStr (shiftMinutes template.durationMinutes startStr)
        tellCmd $ TemplateCmd (TemplateSetDraft (applyTemplateToDraft template startStr endStr))


renderTemplatesPanel
  :: forall w
   . Array TaskTemplate
  -> TemplateDraft
  -> Maybe String
  -> HTML w TemplateAction
renderTemplatesPanel templates draft editingId =
  section [ class_ "agenda-templates" ]
    [ renderPanelHeader
        "agenda-templates"
        "Templates de taches"
        "Creez des templates reutilisables pour accelerer la saisie."
        []
    , div [ class_ "agenda-templates-form" ]
        [ input
            [ class_ "form-control agenda-input"
            , placeholder "Titre du template"
            , value draft.title
            , onValueChange TemplateTitleChangedAction
            ]
        , div [ class_ "agenda-templates-row" ]
            [ input
                [ class_ "form-control agenda-input"
                , type_ InputNumber
                , placeholder "Duree (minutes)"
                , value draft.durationMinutes
                , onValueChange TemplateDurationChangedAction
                ]
            , input
                [ class_ "form-control agenda-input"
                , placeholder "Categorie (optionnelle)"
                , value draft.category
                , onValueChange TemplateCategoryChangedAction
                ]
            ]
        , div [ class_ "agenda-templates-actions" ]
            ( [ button
                  [ class_ "btn btn-sm btn-primary"
                  , onClick (const TemplateSubmit)
                  ]
                  [ text $ if editingId == Nothing then "Ajouter" else "Mettre a jour" ]
              ] <>
                if editingId == Nothing then []
                else
                  [ button
                      [ class_ "btn btn-sm btn-outline-secondary"
                      , onClick (const TemplateCancelEdit)
                      ]
                      [ text "Annuler" ]
                  ]
            )
        ]
    , renderTemplatesList templates
    ]

renderTemplatesList :: forall w. Array TaskTemplate -> HTML w TemplateAction
renderTemplatesList templates =
  if null templates then
    div [ class_ "agenda-templates-empty" ] [ text "Aucun template pour l'instant." ]
  else
    div [ class_ "agenda-templates-list" ] (map renderTemplateCard templates)

renderTemplateCard :: forall w. TaskTemplate -> HTML w TemplateAction
renderTemplateCard template =
  div [ class_ "agenda-template-card" ]
    [ div [ class_ "agenda-template-main" ]
        [ div [ class_ "agenda-template-title" ] [ text template.title ]
        , div [ class_ "agenda-template-summary" ] [ text (templateSummary template) ]
        ]
    , div [ class_ "agenda-template-actions" ]
        [ button [ class_ "btn btn-sm btn-outline-primary", onClick (const (TemplateUse template.id)) ] [ text "Utiliser" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const (TemplateEdit template.id)) ] [ text "Editer" ]
        , button [ class_ "btn btn-sm btn-outline-danger", onClick (const (TemplateDelete template.id)) ] [ text "Supprimer" ]
        ]
    ]


applyTemplateToDraft :: TaskTemplate -> String -> String -> IntentionDraft
applyTemplateToDraft template windowStart windowEnd =
  { title: template.title
  , windowStart
  , windowEnd
  , category: template.category
  }

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
  let
    steps = map toInstance template.steps
    withDeps = applyDependencies template.steps steps
  in
    { templateId: template.id
    , steps: withDeps
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
  case find (\templateStep -> templateStep.id == step.sourceStepId) templateSteps of
    Nothing -> step
    Just templateStep ->
      case templateStep.dependsOn of
        Nothing -> step
        Just dependency ->
          case dependency of
            StartAfterEnd { stepId, offsetMinutes } ->
              updateFromBase stepId offsetMinutes _.windowEnd
            StartBeforeStart { stepId, offsetMinutes } ->
              updateFromBase stepId (-offsetMinutes) _.windowStart
  where
  updateFromBase baseId offset selectBase =
    case find (\candidate -> candidate.id == baseId) instanceSteps of
      Nothing -> step
      Just base ->
        let
          duration = durationMinutesBetween step.windowStart step.windowEnd
          newStart = shiftMinutes offset (selectBase base)
          newEnd = newStart >>= \start -> duration >>= \mins -> shiftMinutes mins start
        in
          case { start: newStart, end: newEnd } of
            { start: Just start, end: Just end } -> step { windowStart = start, windowEnd = end }
            _ -> step
