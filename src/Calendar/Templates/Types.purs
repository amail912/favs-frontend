module Calendar.Templates.Types
  ( StepDependency(..)
  , RoutineTemplate
  , RoutineTemplateStep
  , RoutineInstance
  , RoutineInstanceStep
  , TaskTemplate
  , TemplateDraft
  , emptyTemplateDraft
  , _templateDraftTitle
  , _templateDraftDuration
  , _templateDraftCategory
  , TemplateState(..)
  , templateInitialState
  , resetTemplateDraft
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy(..))

data StepDependency
  = StartAfterEnd { stepId :: String, offsetMinutes :: Int }
  | StartBeforeStart { stepId :: String, offsetMinutes :: Int }

derive instance stepDependencyGeneric :: Generic StepDependency _
derive instance stepDependencyEq :: Eq StepDependency
instance stepDependencyShow :: Show StepDependency where
  show = genericShow

type RoutineTemplate =
  { id :: String
  , name :: String
  , steps :: Array RoutineTemplateStep
  }

type RoutineTemplateStep =
  { id :: String
  , title :: String
  , windowStart :: String
  , windowEnd :: String
  , dependsOn :: Maybe StepDependency
  }

type RoutineInstance =
  { templateId :: String
  , steps :: Array RoutineInstanceStep
  }

type RoutineInstanceStep =
  { id :: String
  , title :: String
  , windowStart :: String
  , windowEnd :: String
  , sourceStepId :: String
  }

type TaskTemplate =
  { id :: String
  , title :: String
  , durationMinutes :: Int
  , category :: String
  }

type TemplateDraft =
  { title :: String
  , durationMinutes :: String
  , category :: String
  }

emptyTemplateDraft :: TemplateDraft
emptyTemplateDraft =
  { title: ""
  , durationMinutes: ""
  , category: ""
  }

_templateDraftTitle :: Lens' TemplateDraft String
_templateDraftTitle = prop (Proxy :: _ "title")

_templateDraftDuration :: Lens' TemplateDraft String
_templateDraftDuration = prop (Proxy :: _ "durationMinutes")

_templateDraftCategory :: Lens' TemplateDraft String
_templateDraftCategory = prop (Proxy :: _ "category")

newtype TemplateState = TemplateState
  { templates :: Array TaskTemplate
  , templateDraft :: TemplateDraft
  , editingTemplateId :: Maybe String
  }

templateInitialState :: TemplateState
templateInitialState = TemplateState
  { templates: []
  , templateDraft: emptyTemplateDraft
  , editingTemplateId: Nothing
  }

resetTemplateDraft :: TemplateState -> TemplateState
resetTemplateDraft (TemplateState state) =
  TemplateState (state { templateDraft = emptyTemplateDraft, editingTemplateId = Nothing })
