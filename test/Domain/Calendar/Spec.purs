module Test.Domain.Calendar.Spec (spec) where

import Prelude

import Calendar.Model (CalendarItem(..), IntentionDraft, ItemStatus(..), ItemType(..), RecurrenceRule(..), RoutineTemplate, SortMode(..), StepDependency(..), ValidationError(..), defaultNotificationDefaults, defaultRecurrenceDraft)
import Calendar.Conflicts (detectConflictGroups, detectConflictIds)
import Calendar.Calendar (PrimaryAction(..), primaryActionFor, toNewIntention)
import Calendar.Edit (EditError(..), applyEditDraft, buildEditDraft)
import Calendar.Exports (exportItemsToCsv, exportItemsToIcs, filterItemsForExport)
import Calendar.Helpers (durationMinutesBetween, sortItems, validateIntention)
import Calendar.Imports (parseCsvImport, parseIcsImport)
import Calendar.Notifications (reminderTimesForIntention)
import Calendar.Offline (applyOfflineMutation)
import Calendar.Recurrence (generateOccurrencesForMonth)
import Calendar.Templates (applyTemplateToDraft, instantiateRoutine, templateSummary, addTemplate, removeTemplate, updateTemplate)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (head, length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Common as StringCommon
import Data.String.Pattern (Pattern(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Support.Builders (calendarContent, serverCalendarItem)

spec :: Spec Unit
spec = do
  describe "Calendar creation" do
    it "encodes/decodes an intention created from a draft" do
      let
        draft :: IntentionDraft
        draft =
          { itemType: Intention
          , title: "Deep work"
          , windowStart: "2026-02-19T09:00"
          , windowEnd: "2026-02-19T10:00"
          , category: ""
          , status: Todo
          , actualDurationMinutes: ""
          , recurrence: defaultRecurrenceDraft
          }
      case toNewIntention draft of
        Left err -> fail $ "Creation failed: " <> err
        Right newItem ->
          case decodeJson (encodeJson newItem) of
            Right decoded -> decoded `shouldEqual` newItem
            Left err -> fail $ "Decoding encoded intention failed: " <> show err

    it "creates a scheduled block when draft itemType is ScheduledBlock" do
      let
        draft :: IntentionDraft
        draft =
          { itemType: ScheduledBlock
          , title: "Planifié"
          , windowStart: "2026-02-19T13:00"
          , windowEnd: "2026-02-19T14:00"
          , category: ""
          , status: Todo
          , actualDurationMinutes: ""
          , recurrence: defaultRecurrenceDraft
          }
      case toNewIntention draft of
        Left err -> fail $ "Creation failed: " <> err
        Right (NewCalendarItem { content }) -> content.itemType `shouldEqual` ScheduledBlock
        Right _ -> fail "Expected new calendar item for scheduled block"

    it "encodes/decodes a scheduled block with sourceItemId" do
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

  describe "Calendar validation" do
    it "fails validation when title is blank" do
      let
        draft :: IntentionDraft
        draft =
          { itemType: Intention
          , title: "   "
          , windowStart: "2026-02-19T09:00"
          , windowEnd: "2026-02-19T10:00"
          , category: ""
          , status: Todo
          , actualDurationMinutes: ""
          , recurrence: defaultRecurrenceDraft
          }
      validateIntention draft `shouldEqual` Left TitleEmpty

    it "fails validation when windowEnd is before windowStart" do
      let
        draft :: IntentionDraft
        draft =
          { itemType: Intention
          , title: "Focus"
          , windowStart: "2026-02-19T10:00"
          , windowEnd: "2026-02-19T09:00"
          , category: ""
          , status: Todo
          , actualDurationMinutes: ""
          , recurrence: defaultRecurrenceDraft
          }
      validateIntention draft `shouldEqual` Left WindowOrderInvalid

    it "fails validation when duration is < 5 minutes" do
      let
        draft :: IntentionDraft
        draft =
          { itemType: Intention
          , title: "Court"
          , windowStart: "2026-02-19T10:00"
          , windowEnd: "2026-02-19T10:04"
          , category: ""
          , status: Todo
          , actualDurationMinutes: ""
          , recurrence: defaultRecurrenceDraft
          }
      validateIntention draft `shouldEqual` Left WindowTooShort

    it "accepts draft when title and window are valid" do
      let
        draft :: IntentionDraft
        draft =
          { itemType: Intention
          , title: "Sprint"
          , windowStart: "2026-02-19T09:00"
          , windowEnd: "2026-02-19T10:00"
          , category: ""
          , status: Todo
          , actualDurationMinutes: ""
          , recurrence: defaultRecurrenceDraft
          }
      validateIntention draft `shouldEqual` Right draft

  describe "Calendar edit" do
    it "applyEditDraft updates title, actualDurationMinutes, and recurrence fields" do
      let
        content =
          (calendarContent Intention "Intention" "2026-02-19T09:00" "2026-02-19T10:00")
            { actualDurationMinutes = Just 25
            , recurrenceRule = Just RecurrenceWeekly
            , recurrenceExceptionDates = [ "2026-02-26" ]
            }
        item = serverCalendarItem "edit-1" content
      case buildEditDraft item of
        Nothing -> fail "Expected edit draft"
        Just draft -> do
          draft.actualDurationMinutes `shouldEqual` "25"
          let
            updatedDraft = draft
              { title = "Mise a jour"
              , actualDurationMinutes = "40"
              }
          case applyEditDraft updatedDraft item of
            Left err -> fail $ "Edit failed: " <> show err
            Right (ServerCalendarItem { content: updated }) -> do
              updated.title `shouldEqual` "Mise a jour"
              updated.actualDurationMinutes `shouldEqual` Just 40
              updated.recurrenceRule `shouldEqual` Just RecurrenceWeekly
              updated.recurrenceExceptionDates `shouldEqual` [ "2026-02-26" ]
            Right _ -> fail "Expected server item"

    it "applyEditDraft rejects non-positive actual duration" do
      let
        content = calendarContent Intention "Intention" "2026-02-19T09:00" "2026-02-19T10:00"
        item = serverCalendarItem "edit-2" content
      case buildEditDraft item of
        Nothing -> fail "Expected edit draft"
        Just draft -> do
          let updatedDraft = draft { actualDurationMinutes = "0" }
          applyEditDraft updatedDraft item `shouldEqual` Left (EditDuration "Durée réelle invalide.")

  describe "Calendar primary actions" do
    it "primaryActionFor returns Planify for Intention items" do
      let
        item = serverCalendarItem "a" (calendarContent Intention "Intention" "2026-02-19T09:00" "2026-02-19T10:00")
      primaryActionFor item `shouldEqual` PrimaryPlanify

    it "primaryActionFor returns Validate for scheduled blocks in non-done status" do
      let
        item = serverCalendarItem "b" (calendarContent ScheduledBlock "Bloc" "2026-02-19T09:00" "2026-02-19T10:00")
      primaryActionFor item `shouldEqual` PrimaryValidate

    it "primaryActionFor returns None for done scheduled blocks" do
      let
        content = (calendarContent ScheduledBlock "Bloc" "2026-02-19T09:00" "2026-02-19T10:00") { status = Fait }
        item = serverCalendarItem "c" content
      primaryActionFor item `shouldEqual` PrimaryNone

  describe "Calendar conflicts" do
    it "detectConflictIds returns both ids for overlapping scheduled blocks" do
      let
        itemA = serverCalendarItem "a" (calendarContent ScheduledBlock "A" "2026-02-19T09:00" "2026-02-19T10:00")
        itemB = serverCalendarItem "b" (calendarContent ScheduledBlock "B" "2026-02-19T09:30" "2026-02-19T10:30")
      detectConflictIds [ itemA, itemB ] `shouldEqual` [ "a", "b" ]

    it "detectConflictGroups chains overlapping blocks into a single group" do
      let
        itemA = serverCalendarItem "a" (calendarContent ScheduledBlock "A" "2026-02-19T09:00" "2026-02-19T10:00")
        itemB = serverCalendarItem "b" (calendarContent ScheduledBlock "B" "2026-02-19T09:30" "2026-02-19T10:30")
        itemC = serverCalendarItem "c" (calendarContent ScheduledBlock "C" "2026-02-19T10:15" "2026-02-19T11:00")
      detectConflictGroups [ itemA, itemB, itemC ] `shouldEqual` [ [ "a", "b", "c" ] ]

  describe "Calendar offline" do
    it "applyOfflineMutation enqueues items and pending when offline" do
      let
        item = NewCalendarItem { content: calendarContent Intention "Offline" "2026-02-19T09:00" "2026-02-19T10:00" }
        result = applyOfflineMutation true item [] []
      result.items `shouldEqual` [ item ]
      result.pending `shouldEqual` [ item ]

  describe "Calendar time helpers" do
    it "durationMinutesBetween returns minutes between start and end" do
      durationMinutesBetween "2026-02-19T09:00" "2026-02-19T10:30" `shouldEqual` Just 90

  describe "Calendar notifications" do
    it "reminderTimesForIntention uses default notification settings" do
      let
        content = calendarContent Intention "Notif" "2026-02-19T09:00" "2026-02-20T12:00"
        reminders = reminderTimesForIntention defaultNotificationDefaults Nothing content
      reminders `shouldEqual`
        [ { label: "Jour de début", at: "2026-02-19T06:00" }
        , { label: "24h avant fin", at: "2026-02-19T12:00" }
        ]

    it "reminderTimesForIntention uses override startDayTime and beforeEndHours" do
      let
        content = calendarContent Intention "Notif" "2026-02-19T09:00" "2026-02-20T12:00"
        override =
          Just
            { itemId: "x"
            , startDayTime: Just "08:30"
            , beforeEndHours: Just 12
            }
        reminders = reminderTimesForIntention defaultNotificationDefaults override content
      reminders `shouldEqual`
        [ { label: "Jour de début", at: "2026-02-19T08:30" }
        , { label: "12h avant fin", at: "2026-02-20T00:00" }
        ]

  describe "Calendar templates" do
    it "applyTemplateToDraft sets title/category/window from template" do
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

    it "template helpers add, update, and remove entries" do
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

    it "templateSummary includes duration and category when present" do
      let
        template =
          { id: "tpl-1"
          , title: "Summary"
          , durationMinutes: 90
          , category: "Focus"
          }
      templateSummary template `shouldEqual` "90 min • Focus"

  describe "Calendar imports" do
    it "parseCsvImport returns one item and no errors for valid input" do
      let
        csv =
          "type,titre,fenetre_debut,fenetre_fin,categorie,statut\n" <>
            "INTENTION,Focus,2026-02-19T09:00,2026-02-19T10:00,Deep,TODO"
        result = parseCsvImport csv
      length result.items `shouldEqual` 1
      length result.errors `shouldEqual` 0

    it "parseCsvImport reports errors for invalid rows" do
      let
        csv =
          "type,titre,fenetre_debut,fenetre_fin\n" <>
            "INTENTION,,2026-02-19T09:00,2026-02-19T08:00"
        result = parseCsvImport csv
      length result.items `shouldEqual` 0
      length result.errors `shouldEqual` 1

    it "parseIcsImport returns one item and no errors for valid input" do
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

    it "parseIcsImport reports errors for invalid events" do
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

  describe "Calendar exports" do
    it "filterItemsForExport keeps only items matching the filter" do
      let
        itemA = serverCalendarItem "a" (calendarContent Intention "A" "2026-02-19T09:00" "2026-02-19T10:00")
        itemB = serverCalendarItem "b" (calendarContent ScheduledBlock "B" "2026-02-20T11:00" "2026-02-20T12:00")
        filter =
          { itemType: Just Intention
          , status: Just Todo
          , category: Nothing
          , startDate: Nothing
          , endDate: Nothing
          }
      filterItemsForExport filter [ itemA, itemB ] `shouldEqual` [ itemA ]

    it "exportItemsToCsv/ToIcs include expected markers" do
      let
        item = serverCalendarItem "a" (calendarContent Intention "A" "2026-02-19T09:00" "2026-02-19T10:00")
        csv = exportItemsToCsv [ item ]
        ics = exportItemsToIcs [ item ]
      (length (StringCommon.split (Pattern "type,titre") csv) > 1) `shouldEqual` true
      (length (StringCommon.split (Pattern "BEGIN:VEVENT") ics) > 1) `shouldEqual` true

  describe "Calendar sorting" do
    it "sortItems SortByStatus orders Todo before Done" do
      let
        itemTodo = serverCalendarItem "todo" (calendarContent ScheduledBlock "Todo" "2026-02-19T09:00" "2026-02-19T10:00")
        itemDone =
          ServerCalendarItem
            { id: "done"
            , content:
                (calendarContent ScheduledBlock "Done" "2026-02-19T11:00" "2026-02-19T12:00")
                  { status = Fait }
            }
      sortItems SortByStatus [] [ itemDone, itemTodo ] `shouldEqual` [ itemTodo, itemDone ]

  describe "Calendar recurrence" do
    it "generateOccurrencesForMonth excludes exception dates" do
      let
        occurrences = generateOccurrencesForMonth RecurrenceWeekly [ "2026-02-19" ] "2026-02-05T09:00"
      occurrences `shouldEqual` [ "2026-02-05", "2026-02-12", "2026-02-26" ]

  describe "Calendar routines" do
    it "instantiateRoutine applies StartAfterEnd offsets" do
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
