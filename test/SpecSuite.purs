module Test.SpecSuite (runSpecSuite) where

import Prelude

import Effect.Aff (Aff)
import Test.Calendar.DragSpec as DragSpec
import Test.Domain.Calendar.Spec as CalendarSpec
import Test.Pages.AdminSpec as AdminSpec
import Test.Pages.AppSpec as AppSpec
import Test.Pages.CalendarSpec as CalendarPageSpec
import Test.Pages.ChecklistsSpec as ChecklistsSpec
import Test.Pages.NotesSpec as NotesSpec
import Test.Ui.ErrorsSpec as ErrorsSpec
import Test.Ui.PageFlowSpec as PageFlowSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

runSpecSuite :: Aff Unit
runSpecSuite = runSpec [ consoleReporter ] do
  DragSpec.spec
  AdminSpec.spec
  AppSpec.spec
  NotesSpec.spec
  ChecklistsSpec.spec
  CalendarPageSpec.spec
  CalendarSpec.spec
  ErrorsSpec.spec
  PageFlowSpec.spec
