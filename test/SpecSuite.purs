module Test.SpecSuite (runSpecSuite) where

import Prelude

import Effect.Aff (Aff)
import Test.Api.AgendaContractSpec as AgendaContractSpec
import Test.Api.ChecklistsContractSpec as ChecklistsContractSpec
import Test.Api.NotesContractSpec as NotesContractSpec
import Test.Agenda.DragSpec as DragSpec
import Test.Domain.Agenda.Spec as AgendaSpec
import Test.Pages.ChecklistsSpec as ChecklistsSpec
import Test.Pages.NotesSpec as NotesSpec
import Test.Ui.ErrorsSpec as ErrorsSpec
import Test.Ui.PageFlowSpec as PageFlowSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

runSpecSuite :: Aff Unit
runSpecSuite = runSpec [ consoleReporter ] do
  NotesContractSpec.spec
  ChecklistsContractSpec.spec
  DragSpec.spec
  NotesSpec.spec
  ChecklistsSpec.spec
  AgendaContractSpec.spec
  AgendaSpec.spec
  ErrorsSpec.spec
  PageFlowSpec.spec
