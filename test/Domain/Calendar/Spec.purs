module Test.Domain.Calendar.Spec (spec) where

import Prelude

import Calendar.ExImport.Export as Export
import Calendar.ExImport.Import (parseCsv, parseIcs)
import Pages.Calendar
  ( CalendarItem(..)
  , TaskDraft
  , ItemStatus(..)
  , ItemType(..)
  , SortMode(..)
  , ValidationError(..)
  , toNewTask
  , primaryActionFor
  , PrimaryAction(..)
  , buildTimelineLayout
  , buildMobileOverlapStacks
  , toTimelineBlock
  , EditError(..)
  , applyEditDraft
  , buildEditDraft
  , durationMinutesBetween
  , sortItems
  , validateTask
  , DayFocusTarget(..)
  , computeDayFocusTarget
  )
import Calendar.Recurrence (RecurrenceRule(..), defaultRecurrenceDraft, generateOccurrencesForMonth)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Common as StringCommon
import Data.String.Pattern (Pattern(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Support.Builders (calendarContent, serverCalendarItem, unsafeDate, unsafeDateTime)

spec :: Spec Unit
spec = do
  describe "Calendar creation" do
    it "encodes/decodes a task created from a draft" do
      let
        draft :: TaskDraft
        draft =
          { itemType: Task
          , title: "Deep work"
          , windowStart: "2026-02-19T09:00"
          , windowEnd: "2026-02-19T10:00"
          , category: ""
          , status: Todo
          , actualDurationMinutes: ""
          , recurrence: defaultRecurrenceDraft
          }
      case toNewTask draft of
        Left err -> fail $ "Creation failed: " <> err
        Right newItem ->
          case decodeJson (encodeJson newItem) of
            Right decoded -> decoded `shouldEqual` newItem
            Left err -> fail $ "Decoding encoded task failed: " <> show err

    it "decodes legacy scheduled-block payloads as tasks" do
      let
        scheduled =
          NewCalendarItem
            { content:
                { itemType: Task
                , title: "Task"
                , windowStart: unsafeDateTime "2026-02-19T11:00"
                , windowEnd: unsafeDateTime "2026-02-19T12:00"
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
        Left err -> fail $ "Decoding encoded task failed: " <> show err

  describe "Calendar validation" do
    it "fails validation when title is blank" do
      let
        draft :: TaskDraft
        draft =
          { itemType: Task
          , title: "   "
          , windowStart: "2026-02-19T09:00"
          , windowEnd: "2026-02-19T10:00"
          , category: ""
          , status: Todo
          , actualDurationMinutes: ""
          , recurrence: defaultRecurrenceDraft
          }
      validateTask draft `shouldEqual` Left TitleEmpty

    it "fails validation when windowEnd is before windowStart" do
      let
        draft :: TaskDraft
        draft =
          { itemType: Task
          , title: "Focus"
          , windowStart: "2026-02-19T10:00"
          , windowEnd: "2026-02-19T09:00"
          , category: ""
          , status: Todo
          , actualDurationMinutes: ""
          , recurrence: defaultRecurrenceDraft
          }
      validateTask draft `shouldEqual` Left WindowOrderInvalid

    it "fails validation when duration is < 5 minutes" do
      let
        draft :: TaskDraft
        draft =
          { itemType: Task
          , title: "Court"
          , windowStart: "2026-02-19T10:00"
          , windowEnd: "2026-02-19T10:04"
          , category: ""
          , status: Todo
          , actualDurationMinutes: ""
          , recurrence: defaultRecurrenceDraft
          }
      validateTask draft `shouldEqual` Left WindowTooShort

    it "accepts draft when title and window are valid" do
      let
        draft :: TaskDraft
        draft =
          { itemType: Task
          , title: "Sprint"
          , windowStart: "2026-02-19T09:00"
          , windowEnd: "2026-02-19T10:00"
          , category: ""
          , status: Todo
          , actualDurationMinutes: ""
          , recurrence: defaultRecurrenceDraft
          }
      validateTask draft `shouldEqual` Right draft

  describe "Calendar edit" do
    it "applyEditDraft updates title, actualDurationMinutes, and recurrence fields" do
      let
        content =
          (calendarContent Task "Task" "2026-02-19T09:00" "2026-02-19T10:00")
            { actualDurationMinutes = Just 25
            , recurrenceRule = Just Weekly
            , recurrenceExceptionDates = [ unsafeDate "2026-02-26" ]
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
              updated.recurrenceRule `shouldEqual` Just Weekly
              updated.recurrenceExceptionDates `shouldEqual` [ unsafeDate "2026-02-26" ]
            Right _ -> fail "Expected server item"

    it "applyEditDraft rejects non-positive actual duration" do
      let
        content = calendarContent Task "Task" "2026-02-19T09:00" "2026-02-19T10:00"
        item = serverCalendarItem "edit-2" content
      case buildEditDraft item of
        Nothing -> fail "Expected edit draft"
        Just draft -> do
          let updatedDraft = draft { actualDurationMinutes = "0" }
          applyEditDraft updatedDraft item `shouldEqual` Left (EditDuration "Durée réelle invalide.")

  describe "Calendar primary actions" do
    it "primaryActionFor returns Validate for non-done tasks" do
      let
        item = serverCalendarItem "a" (calendarContent Task "Task" "2026-02-19T09:00" "2026-02-19T10:00")
      primaryActionFor item `shouldEqual` PrimaryValidate

    it "primaryActionFor returns None for done tasks" do
      let
        content = (calendarContent Task "Task" "2026-02-19T09:00" "2026-02-19T10:00") { status = Done }
        item = serverCalendarItem "c" content
      primaryActionFor item `shouldEqual` PrimaryNone

  describe "Calendar timeline layout" do
    it "clamps end time when it is before the start" do
      let
        item = serverCalendarItem "t1" (calendarContent Task "Late" "2026-02-19T23:00" "2026-02-19T22:00")
      toTimelineBlock item
        `shouldEqual`
          Just { item, startMin: 1380, endMin: 1440 }

    it "assigns distinct columns for overlapping items" do
      let
        itemA = serverCalendarItem "t2" (calendarContent Task "A" "2026-02-19T09:00" "2026-02-19T10:00")
        itemB = serverCalendarItem "t3" (calendarContent Task "B" "2026-02-19T09:30" "2026-02-19T10:30")
        layout = buildTimelineLayout [ itemA, itemB ]
      length layout `shouldEqual` 2
      case layout of
        [ first, second ] -> do
          first.columnCount `shouldEqual` 2
          second.columnCount `shouldEqual` 2
          (first.columnIndex == second.columnIndex) `shouldEqual` false
        _ -> fail "Expected exactly two timeline items"

    it "builds a single mobile stack for overlapping items" do
      let
        itemA = serverCalendarItem "stack-a" (calendarContent Task "A" "2026-02-19T09:00" "2026-02-19T10:00")
        itemB = serverCalendarItem "stack-b" (calendarContent Task "B" "2026-02-19T09:30" "2026-02-19T10:30")
        stacks = buildMobileOverlapStacks [ itemA, itemB ]
      case stacks of
        [ stack ] -> do
          stack.topItem `shouldEqual` itemA
          stack.hiddenItems `shouldEqual` [ itemB ]
          stack.startMin `shouldEqual` 540
          stack.duration `shouldEqual` 60
          stack.hiddenCount `shouldEqual` 1
        _ -> fail "Expected exactly one mobile overlap stack"

    it "keeps non-overlapping items in separate mobile stacks" do
      let
        itemA = serverCalendarItem "stack-c" (calendarContent Task "A" "2026-02-19T09:00" "2026-02-19T10:00")
        itemB = serverCalendarItem "stack-d" (calendarContent Task "B" "2026-02-19T10:00" "2026-02-19T11:00")
        stacks = buildMobileOverlapStacks [ itemA, itemB ]
      length stacks `shouldEqual` 2

    it "chooses the earliest end time when overlapping items start together" do
      let
        longItem = serverCalendarItem "stack-e" (calendarContent Task "Long" "2026-02-19T09:00" "2026-02-19T10:30")
        shortItem = serverCalendarItem "stack-f" (calendarContent Task "Short" "2026-02-19T09:00" "2026-02-19T09:45")
        stacks = buildMobileOverlapStacks [ longItem, shortItem ]
      case stacks of
        [ stack ] -> do
          stack.topItem `shouldEqual` shortItem
          stack.hiddenItems `shouldEqual` [ longItem ]
        _ -> fail "Expected exactly one mobile overlap stack"

    it "uses a stable identity fallback when start and end times match" do
      let
        itemB = serverCalendarItem "stack-z" (calendarContent Task "B" "2026-02-19T09:00" "2026-02-19T10:00")
        itemA = serverCalendarItem "stack-a" (calendarContent Task "A" "2026-02-19T09:00" "2026-02-19T10:00")
        stacks = buildMobileOverlapStacks [ itemB, itemA ]
      case stacks of
        [ stack ] -> do
          stack.topItem `shouldEqual` itemA
          stack.hiddenItems `shouldEqual` [ itemB ]
        _ -> fail "Expected exactly one mobile overlap stack"

  describe "Calendar time helpers" do
    it "durationMinutesBetween returns minutes between start and end" do
      durationMinutesBetween (unsafeDateTime "2026-02-19T09:00") (unsafeDateTime "2026-02-19T10:30") `shouldEqual` 90

  describe "Calendar day initial focus" do
    it "targets current time for today with tasks" do
      let
        now = unsafeDateTime "2026-02-19T14:30"
        items =
          [ serverCalendarItem "focus-1" (calendarContent Task "Task" "2026-02-19T09:00" "2026-02-19T10:00") ]
      computeDayFocusTarget "2026-02-19" now items `shouldEqual` FocusCurrentTime

    it "targets current time for today without tasks" do
      let
        now = unsafeDateTime "2026-02-19T14:30"
      computeDayFocusTarget "2026-02-19" now [] `shouldEqual` FocusCurrentTime

    it "targets first task for another day with tasks" do
      let
        now = unsafeDateTime "2026-02-19T14:30"
        items =
          [ serverCalendarItem "focus-2" (calendarContent Task "Task" "2026-02-20T09:00" "2026-02-20T10:00") ]
      computeDayFocusTarget "2026-02-20" now items `shouldEqual` FocusFirstTask

    it "targets the top for another day without tasks" do
      let
        now = unsafeDateTime "2026-02-19T14:30"
      computeDayFocusTarget "2026-02-20" now [] `shouldEqual` FocusTop

  describe "Calendar imports" do
    it "parseCsv returns one item and no errors for valid input" do
      let
        csv =
          "type,titre,fenetre_debut,fenetre_fin,categorie,statut,source_item_id,actual_duration_minutes,recurrence_rule_type,recurrence_rule_interval_days,recurrence_exception_dates\n" <>
            "TASK,Focus,2026-02-19T09:00,2026-02-19T10:00,Deep,TODO,,,,,"
        result = parseCsv csv
      length result.items `shouldEqual` 1
      length result.errors `shouldEqual` 0

    it "parseCsv reports errors for invalid rows" do
      let
        csv =
          "type,titre,fenetre_debut,fenetre_fin\n" <>
            "TASK,,2026-02-19T09:00,2026-02-19T08:00"
        result = parseCsv csv
      length result.items `shouldEqual` 0
      length result.errors `shouldEqual` 1

    it "parseIcs returns one item and no errors for valid input" do
      let
        ics =
          "BEGIN:VCALENDAR\n"
            <> "BEGIN:VEVENT\n"
            <> "SUMMARY:Meeting\n"
            <> "DTSTART:20260219T090000\n"
            <> "DTEND:20260219T100000\n"
            <> "X-FAVS-TYPE:TASK\n"
            <> "X-FAVS-STATUS:TODO\n"
            <> "X-FAVS-SOURCE-ITEM-ID:\n"
            <> "X-FAVS-ACTUAL-DURATION-MINUTES:\n"
            <> "END:VEVENT\n"
            <>
              "END:VCALENDAR"
        result = parseIcs ics
      length result.items `shouldEqual` 1
      length result.errors `shouldEqual` 0

    it "parseIcs reports errors for invalid events" do
      let
        ics =
          "BEGIN:VCALENDAR\n"
            <> "BEGIN:VEVENT\n"
            <> "DTSTART:20260219T090000\n"
            <> "DTEND:20260219T100000\n"
            <> "X-FAVS-TYPE:TASK\n"
            <> "X-FAVS-STATUS:TODO\n"
            <> "X-FAVS-SOURCE-ITEM-ID:\n"
            <> "X-FAVS-ACTUAL-DURATION-MINUTES:\n"
            <> "END:VEVENT\n"
            <>
              "END:VCALENDAR"
        result = parseIcs ics
      length result.items `shouldEqual` 0
      length result.errors `shouldEqual` 1

  describe "Calendar exports" do
    it "filterItemsForExport keeps only items matching the filter" do
      let
        itemA =
          Export.Item
            { itemType: Export.Task
            , title: "A"
            , windowStart: unsafeDateTime "2026-02-19T09:00"
            , windowEnd: unsafeDateTime "2026-02-19T10:00"
            , status: Export.Todo
            , category: Just "Focus"
            , sourceItemId: Nothing
            , actualDurationMinutes: Nothing
            , recurrenceRule: Nothing
            , recurrenceExceptionDates: []
            }
        itemB =
          Export.Item
            { itemType: Export.Task
            , title: "B"
            , windowStart: unsafeDateTime "2026-02-20T11:00"
            , windowEnd: unsafeDateTime "2026-02-20T12:00"
            , status: Export.Todo
            , category: Nothing
            , sourceItemId: Nothing
            , actualDurationMinutes: Nothing
            , recurrenceRule: Nothing
            , recurrenceExceptionDates: []
            }
        filter =
          { status: Nothing
          , category: Just "Focus"
          , startDate: Nothing
          , endDate: Nothing
          }
      Export.filterItemsForExport filter [ itemA, itemB ] `shouldEqual` [ itemA ]

    it "exportItemsToCsv/ToIcs include expected markers" do
      let
        item =
          Export.Item
            { itemType: Export.Task
            , title: "A"
            , windowStart: unsafeDateTime "2026-02-19T09:00"
            , windowEnd: unsafeDateTime "2026-02-19T10:00"
            , status: Export.Todo
            , category: Nothing
            , sourceItemId: Nothing
            , actualDurationMinutes: Nothing
            , recurrenceRule: Nothing
            , recurrenceExceptionDates: []
            }
        csv = Export.exportItemsToCsv [ item ]
        ics = Export.exportItemsToIcs [ item ]
      (length (StringCommon.split (Pattern "source_item_id") csv) > 1) `shouldEqual` true
      (length (StringCommon.split (Pattern "X-FAVS-TYPE") ics) > 1) `shouldEqual` true

  describe "Calendar sorting" do
    it "sortItems SortByStatus orders Todo before Done" do
      let
        itemTodo = serverCalendarItem "todo" (calendarContent Task "Todo" "2026-02-19T09:00" "2026-02-19T10:00")
        itemDone =
          ServerCalendarItem
            { id: "done"
            , content:
                (calendarContent Task "Done" "2026-02-19T11:00" "2026-02-19T12:00")
                  { status = Done }
            }
      sortItems SortByStatus [ itemDone, itemTodo ] `shouldEqual` [ itemTodo, itemDone ]

  describe "Calendar recurrence" do
    it "generateOccurrencesForMonth excludes exception dates" do
      let
        occurrences = generateOccurrencesForMonth Weekly [ "2026-02-19" ] "2026-02-05T09:00"
      occurrences `shouldEqual` [ "2026-02-05", "2026-02-12", "2026-02-26" ]
