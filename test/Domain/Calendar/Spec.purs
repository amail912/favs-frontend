module Test.Domain.Calendar.Spec (spec) where

import Prelude

import Agenda.Model (CalendarItem(..), IntentionDraft, ItemStatus(..), ItemType(..), RecurrenceRule(..), RoutineTemplate, SortMode(..), StepDependency(..), ValidationError(..), defaultNotificationDefaults)
import Agenda.Conflicts (detectConflictGroups, detectConflictIds)
import Agenda.Calendar (toNewIntention, validateIntention)
import Agenda.Exports (exportItemsToCsv, exportItemsToIcs, filterItemsForExport)
import Agenda.Helpers (durationMinutesBetween, sortItems)
import Agenda.Imports (parseCsvImport, parseIcsImport)
import Agenda.Notifications (reminderTimesForIntention)
import Agenda.Offline (applyOfflineMutation)
import Agenda.Recurrence (generateOccurrencesForMonth)
import Agenda.Templates (applyTemplateToDraft, instantiateRoutine, templateSummary, addTemplate, removeTemplate, updateTemplate)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (head, length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Common as StringCommon
import Data.String.Pattern (Pattern(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Support.Builders (agendaContent, serverAgendaItem)

spec :: Spec Unit
spec =
  describe "Calendar intentions" do
    it "round-trips a new intention" do
      let
        draft :: IntentionDraft
        draft =
          { title: "Deep work"
          , windowStart: "2026-02-19T09:00"
          , windowEnd: "2026-02-19T10:00"
          , category: ""
          }
        newItem = toNewIntention draft
      case decodeJson (encodeJson newItem) of
        Right decoded -> decoded `shouldEqual` newItem
        Left err -> fail $ "Decoding encoded intention failed: " <> show err

    it "round-trips a scheduled block with source item id" do
      let
        scheduled =
          NewCalendarItem
            { content:
                { itemType: ScheduledBlock
                , title: "Planifie"
                , windowStart: "2026-02-19T11:00"
                , windowEnd: "2026-02-19T12:00"
                , status: Todo
                , sourceItemId: Just "source-1"
                , actualDurationMinutes: Nothing
                , category: Nothing
                , recurrenceRule: Nothing
                , recurrenceExceptionDates: []
                }
            }
      case decodeJson (encodeJson scheduled) of
        Right decoded -> decoded `shouldEqual` scheduled
        Left err -> fail $ "Decoding encoded scheduled block failed: " <> show err

    it "fails validation when the title is empty" do
      let
        draft :: IntentionDraft
        draft =
          { title: "   "
          , windowStart: "2026-02-19T09:00"
          , windowEnd: "2026-02-19T10:00"
          , category: ""
          }
      validateIntention draft `shouldEqual` Left TitleEmpty

    it "fails validation when the end is before start" do
      let
        draft :: IntentionDraft
        draft =
          { title: "Focus"
          , windowStart: "2026-02-19T10:00"
          , windowEnd: "2026-02-19T09:00"
          , category: ""
          }
      validateIntention draft `shouldEqual` Left WindowOrderInvalid

    it "accepts a valid intention" do
      let
        draft :: IntentionDraft
        draft =
          { title: "Sprint"
          , windowStart: "2026-02-19T09:00"
          , windowEnd: "2026-02-19T10:00"
          , category: ""
          }
      validateIntention draft `shouldEqual` Right draft

    it "flags conflicts between scheduled blocks" do
      let
        itemA = serverAgendaItem "a" (agendaContent ScheduledBlock "A" "2026-02-19T09:00" "2026-02-19T10:00")
        itemB = serverAgendaItem "b" (agendaContent ScheduledBlock "B" "2026-02-19T09:30" "2026-02-19T10:30")
      detectConflictIds [ itemA, itemB ] `shouldEqual` [ "a", "b" ]

    it "groups chained conflicts into a single group" do
      let
        itemA = serverAgendaItem "a" (agendaContent ScheduledBlock "A" "2026-02-19T09:00" "2026-02-19T10:00")
        itemB = serverAgendaItem "b" (agendaContent ScheduledBlock "B" "2026-02-19T09:30" "2026-02-19T10:30")
        itemC = serverAgendaItem "c" (agendaContent ScheduledBlock "C" "2026-02-19T10:15" "2026-02-19T11:00")
      detectConflictGroups [ itemA, itemB, itemC ] `shouldEqual` [ [ "a", "b", "c" ] ]

    it "queues items locally when offline" do
      let
        item = NewCalendarItem { content: agendaContent Intention "Offline" "2026-02-19T09:00" "2026-02-19T10:00" }
        result = applyOfflineMutation true item [] []
      result.items `shouldEqual` [ item ]
      result.pending `shouldEqual` [ item ]

    it "computes duration between window start and end" do
      durationMinutesBetween "2026-02-19T09:00" "2026-02-19T10:30" `shouldEqual` Just 90

    it "computes default reminders for an intention" do
      let
        content = agendaContent Intention "Notif" "2026-02-19T09:00" "2026-02-20T12:00"
        reminders = reminderTimesForIntention defaultNotificationDefaults Nothing content
      reminders `shouldEqual`
        [ { label: "Jour de debut", at: "2026-02-19T06:00" }
        , { label: "24h avant fin", at: "2026-02-19T12:00" }
        ]

    it "computes overridden reminders for an intention" do
      let
        content = agendaContent Intention "Notif" "2026-02-19T09:00" "2026-02-20T12:00"
        override =
          Just
            { itemId: "x"
            , startDayTime: Just "08:30"
            , beforeEndHours: Just 12
            }
        reminders = reminderTimesForIntention defaultNotificationDefaults override content
      reminders `shouldEqual`
        [ { label: "Jour de debut", at: "2026-02-19T08:30" }
        , { label: "12h avant fin", at: "2026-02-20T00:00" }
        ]

    it "applies a template to a draft" do
      let
        template =
          { id: "tpl-1"
          , title: "Template"
          , durationMinutes: 45
          , category: "Deep"
          }
        draft = applyTemplateToDraft template "2026-02-19T09:00" "2026-02-19T09:45"
      draft.title `shouldEqual` "Template"
      draft.category `shouldEqual` "Deep"
      draft.windowStart `shouldEqual` "2026-02-19T09:00"
      draft.windowEnd `shouldEqual` "2026-02-19T09:45"

    it "adds, updates, and removes templates" do
      let
        template =
          { id: ""
          , title: "Morning"
          , durationMinutes: 30
          , category: "Rituel"
          }
        added = addTemplate template []
      case head added of
        Just first -> do
          first.id `shouldEqual` "tpl-1"
          let updated = updateTemplate (first { title = "Updated" }) added
          case head updated of
            Just updatedFirst -> updatedFirst.title `shouldEqual` "Updated"
            Nothing -> fail "Expected updated template"
          removeTemplate first.id updated `shouldEqual` []
        Nothing -> fail "Expected added template"

    it "summarizes a template" do
      let
        template =
          { id: "tpl-1"
          , title: "Summary"
          , durationMinutes: 90
          , category: "Focus"
          }
      templateSummary template `shouldEqual` "90 min • Focus"

    it "parses a valid CSV import" do
      let
        csv =
          "type,titre,fenetre_debut,fenetre_fin,categorie,statut\n" <>
            "INTENTION,Focus,2026-02-19T09:00,2026-02-19T10:00,Deep,TODO"
        result = parseCsvImport csv
      length result.items `shouldEqual` 1
      length result.errors `shouldEqual` 0

    it "reports errors for invalid CSV rows" do
      let
        csv =
          "type,titre,fenetre_debut,fenetre_fin\n" <>
            "INTENTION,,2026-02-19T09:00,2026-02-19T08:00"
        result = parseCsvImport csv
      length result.items `shouldEqual` 0
      length result.errors `shouldEqual` 1

    it "parses a valid ICS import" do
      let
        ics =
          "BEGIN:VCALENDAR\n"
            <> "BEGIN:VEVENT\n"
            <> "SUMMARY:Meeting\n"
            <> "DTSTART:20260219T090000\n"
            <> "DTEND:20260219T100000\n"
            <> "END:VEVENT\n"
            <>
              "END:VCALENDAR"
        result = parseIcsImport ics
      length result.items `shouldEqual` 1
      length result.errors `shouldEqual` 0

    it "reports errors for invalid ICS events" do
      let
        ics =
          "BEGIN:VCALENDAR\n"
            <> "BEGIN:VEVENT\n"
            <> "DTSTART:20260219T090000\n"
            <> "DTEND:20260219T100000\n"
            <> "END:VEVENT\n"
            <>
              "END:VCALENDAR"
        result = parseIcsImport ics
      length result.items `shouldEqual` 0
      length result.errors `shouldEqual` 1

    it "filters items for export" do
      let
        itemA = serverAgendaItem "a" (agendaContent Intention "A" "2026-02-19T09:00" "2026-02-19T10:00")
        itemB = serverAgendaItem "b" (agendaContent ScheduledBlock "B" "2026-02-20T11:00" "2026-02-20T12:00")
        filter =
          { itemType: Just Intention
          , status: Just Todo
          , category: Nothing
          , startDate: Nothing
          , endDate: Nothing
          }
      filterItemsForExport filter [ itemA, itemB ] `shouldEqual` [ itemA ]

    it "exports items to CSV and ICS" do
      let
        item = serverAgendaItem "a" (agendaContent Intention "A" "2026-02-19T09:00" "2026-02-19T10:00")
        csv = exportItemsToCsv [ item ]
        ics = exportItemsToIcs [ item ]
      (length (StringCommon.split (Pattern "type,titre") csv) > 1) `shouldEqual` true
      (length (StringCommon.split (Pattern "BEGIN:VEVENT") ics) > 1) `shouldEqual` true

    it "sorts items by status" do
      let
        itemTodo = serverAgendaItem "todo" (agendaContent ScheduledBlock "Todo" "2026-02-19T09:00" "2026-02-19T10:00")
        itemDone =
          ServerCalendarItem
            { id: "done"
            , content:
                (agendaContent ScheduledBlock "Done" "2026-02-19T11:00" "2026-02-19T12:00")
                  { status = Fait }
            }
      sortItems SortByStatus [] [ itemDone, itemTodo ] `shouldEqual` [ itemTodo, itemDone ]

    it "generates weekly occurrences and skips exceptions" do
      let
        occurrences = generateOccurrencesForMonth RecurrenceWeekly [ "2026-02-19" ] "2026-02-05T09:00"
      occurrences `shouldEqual` [ "2026-02-05", "2026-02-12", "2026-02-26" ]

    it "instantiates a routine with dependent steps" do
      let
        template :: RoutineTemplate
        template =
          { id: "r1"
          , name: "Routine"
          , steps:
              [ { id: "a"
                , title: "Etape A"
                , windowStart: "2026-02-19T09:00"
                , windowEnd: "2026-02-19T10:00"
                , dependsOn: Nothing
                }
              , { id: "b"
                , title: "Etape B"
                , windowStart: "2026-02-19T10:00"
                , windowEnd: "2026-02-19T10:30"
                , dependsOn: Just (StartAfterEnd { stepId: "a", offsetMinutes: 15 })
                }
              ]
          }
        routineInstance = instantiateRoutine template
      case routineInstance.steps of
        [ stepA, stepB ] -> do
          stepA.windowStart `shouldEqual` "2026-02-19T09:00"
          stepB.windowStart `shouldEqual` "2026-02-19T10:15"
          stepB.windowEnd `shouldEqual` "2026-02-19T10:45"
        _ -> fail "Expected two routine steps"
