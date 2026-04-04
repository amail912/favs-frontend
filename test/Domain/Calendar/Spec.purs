module Test.Domain.Calendar.Spec (spec) where

import Prelude

import Calendar.ExImport.Export as Export
import Calendar.ExImport.Import (parseCsv, parseIcs)
import Pages.Calendar
  ( CalendarItem(..)
  , CalendarItemContent(..)
  , TaskDraft
  , TripDraft
  , ItemStatus(..)
  , ItemType(..)
  , SortMode(..)
  , ValidationError(..)
  , TripValidationError(..)
  , toNewTask
  , toNewTrip
  , buildTimelineLayout
  , buildMobileOverlapStacks
  , applyMobileOverlapPromotions
  , toTimelineBlock
  , EditDraft(..)
  , EditError(..)
  , applyEditDraft
  , buildEditDraft
  , durationMinutesBetween
  , sortItems
  , validateTask
  , validateTrip
  , calendarItemPrimaryText
  , calendarItemSecondaryText
  , calendarItemCardClass
  , calendarItemTimelineCardClass
  , calendarItemSupportsEdit
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
import Test.Support.Builders (calendarContent, tripCalendarContent, serverCalendarItem, unsafeDate, unsafeDateTime)

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
        scheduledContent =
          case calendarContent Task "Task" "2026-02-19T11:00" "2026-02-19T12:00" of
            TaskCalendarItemContent content ->
              TaskCalendarItemContent (content { sourceItemId = Just "source-1" })
            TripCalendarItemContent _ ->
              calendarContent Task "Task" "2026-02-19T11:00" "2026-02-19T12:00"
        scheduled =
          NewCalendarItem { content: scheduledContent }
      case decodeJson (encodeJson scheduled) of
        Right decoded -> decoded `shouldEqual` scheduled
        Left err -> fail $ "Decoding encoded task failed: " <> show err

    it "encodes and decodes trip payloads with trip-specific fields" do
      let
        trip =
          ServerCalendarItem
            { id: "trip-1"
            , content: tripCalendarContent "Paris" "St Clair" "2026-02-19T13:15" "2026-02-19T15:00"
            }
      case decodeJson (encodeJson trip) of
        Right decoded -> decoded `shouldEqual` trip
        Left err -> fail $ "Decoding encoded trip failed: " <> show err

    it "renders trip display helpers with route, hours, styling, and no edit support" do
      let
        trip = serverCalendarItem "trip-render" (tripCalendarContent "Paris" "Le Mesnil" "2026-02-19T08:30" "2026-02-19T09:45")
      calendarItemPrimaryText trip `shouldEqual` "Paris → Le Mesnil"
      calendarItemSecondaryText trip `shouldEqual` "Départ 08:30 · Arrivée 09:45"
      calendarItemCardClass trip `shouldEqual` "row list-group-item entity-card calendar-card calendar-card--trip"
      calendarItemTimelineCardClass trip `shouldEqual` "calendar-calendar-card calendar-calendar-item--trip"
      calendarItemSupportsEdit trip `shouldEqual` true

    it "renders task secondary text with the shared French display format" do
      let
        task = serverCalendarItem "task-render" (calendarContent Task "Deep work" "2026-02-19T08:30" "2026-02-19T09:45")
      calendarItemPrimaryText task `shouldEqual` "Deep work"
      calendarItemSecondaryText task `shouldEqual` "19/02/2026 08:30 → 19/02/2026 09:45"

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

    it "fails trip validation when departure is missing" do
      let
        draft :: TripDraft
        draft =
          { departurePlaceId: ""
          , arrivalPlaceId: "Paris"
          , windowStart: "2026-02-19T09:00"
          , windowEnd: "2026-02-19T10:00"
          }
      validateTrip draft `shouldEqual` Left TripDeparturePlaceMissing

    it "fails trip validation when places match" do
      let
        draft :: TripDraft
        draft =
          { departurePlaceId: "Paris"
          , arrivalPlaceId: "Paris"
          , windowStart: "2026-02-19T09:00"
          , windowEnd: "2026-02-19T10:00"
          }
      validateTrip draft `shouldEqual` Left TripPlacesMustDiffer

    it "encodes and decodes a trip created from a draft" do
      let
        draft :: TripDraft
        draft =
          { departurePlaceId: "Paris"
          , arrivalPlaceId: "Lyon"
          , windowStart: "2026-02-19T09:00"
          , windowEnd: "2026-02-19T11:00"
          }
      case toNewTrip draft of
        Left err -> fail $ "Trip creation failed: " <> err
        Right newItem ->
          case decodeJson (encodeJson newItem) of
            Right decoded -> decoded `shouldEqual` newItem
            Left err -> fail $ "Decoding encoded trip draft failed: " <> show err

  describe "Calendar edit" do
    it "applyEditDraft updates title, actualDurationMinutes, and recurrence fields" do
      let
        content =
          case calendarContent Task "Task" "2026-02-19T09:00" "2026-02-19T10:00" of
            TaskCalendarItemContent taskContent ->
              TaskCalendarItemContent
                ( taskContent
                    { actualDurationMinutes = Just 25
                    , recurrenceRule = Just Weekly
                    , recurrenceExceptionDates = [ unsafeDate "2026-02-26" ]
                    }
                )
            TripCalendarItemContent _ ->
              calendarContent Task "Task" "2026-02-19T09:00" "2026-02-19T10:00"
        item = serverCalendarItem "edit-1" content
      case buildEditDraft item of
        Nothing -> fail "Expected edit draft"
        Just (EditTaskDraft draft) -> do
          draft.actualDurationMinutes `shouldEqual` "25"
          let
            updatedDraft =
              EditTaskDraft
                ( draft
                    { title = "Mise a jour"
                    , actualDurationMinutes = "40"
                    }
                )
          case applyEditDraft updatedDraft item of
            Left err -> fail $ "Edit failed: " <> show err
            Right (ServerCalendarItem { content: TaskCalendarItemContent updated }) -> do
              updated.title `shouldEqual` "Mise a jour"
              updated.actualDurationMinutes `shouldEqual` Just 40
              updated.recurrenceRule `shouldEqual` Just Weekly
              updated.recurrenceExceptionDates `shouldEqual` [ unsafeDate "2026-02-26" ]
            Right _ -> fail "Expected server item"
        Just _ -> fail "Expected task edit draft"

    it "applyEditDraft rejects non-positive actual duration" do
      let
        content = calendarContent Task "Task" "2026-02-19T09:00" "2026-02-19T10:00"
        item = serverCalendarItem "edit-2" content
      case buildEditDraft item of
        Nothing -> fail "Expected edit draft"
        Just draft -> do
          let
            updatedDraft = case draft of
              EditTaskDraft taskDraft -> EditTaskDraft (taskDraft { actualDurationMinutes = "0" })
              _ -> draft
          applyEditDraft updatedDraft item `shouldEqual` Left (EditDuration "Durée réelle invalide.")

    it "buildEditDraft exposes trip items to the trip editor and applyEditDraft updates route and hours" do
      let
        trip = serverCalendarItem "trip-edit" (tripCalendarContent "Paris" "St Clair" "2026-02-19T09:00" "2026-02-19T10:30")
      case buildEditDraft trip of
        Nothing -> fail "Expected trip edit draft"
        Just (EditTripDraft draft) -> do
          draft.departurePlaceId `shouldEqual` "Paris"
          draft.arrivalPlaceId `shouldEqual` "St Clair"
          let
            updatedDraft =
              EditTripDraft
                ( draft
                    { departurePlaceId = "Lyon"
                    , arrivalPlaceId = "Nice"
                    , windowStart = "2026-02-19T11:00"
                    , windowEnd = "2026-02-19T13:00"
                    }
                )
          case applyEditDraft updatedDraft trip of
            Left err -> fail $ "Trip edit failed: " <> show err
            Right (ServerCalendarItem { content: TripCalendarItemContent updated }) -> do
              updated.departurePlaceId `shouldEqual` "Lyon"
              updated.arrivalPlaceId `shouldEqual` "Nice"
              updated.windowStart `shouldEqual` unsafeDateTime "2026-02-19T11:00"
              updated.windowEnd `shouldEqual` unsafeDateTime "2026-02-19T13:00"
            Right _ -> fail "Expected updated trip item"
        Just _ -> fail "Expected trip edit draft"

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
          stack.groupKey `shouldEqual` "server:stack-a|server:stack-b"
          stack.items `shouldEqual` [ itemA, itemB ]
          stack.topItem `shouldEqual` itemA
          map _.item stack.hiddenCards `shouldEqual` [ itemB ]
          map _.startMin stack.hiddenCards `shouldEqual` [ 570 ]
          map _.duration stack.hiddenCards `shouldEqual` [ 60 ]
          stack.startMin `shouldEqual` 540
          stack.topStartMin `shouldEqual` 540
          stack.topDuration `shouldEqual` 60
          stack.duration `shouldEqual` 90
          stack.hiddenCount `shouldEqual` 1
        _ -> fail "Expected exactly one mobile overlap stack"

    it "keeps non-overlapping items in separate mobile stacks" do
      let
        itemA = serverCalendarItem "stack-c" (calendarContent Task "A" "2026-02-19T09:00" "2026-02-19T10:00")
        itemB = serverCalendarItem "stack-d" (calendarContent Task "B" "2026-02-19T10:00" "2026-02-19T11:00")
        stacks = buildMobileOverlapStacks [ itemA, itemB ]
      length stacks `shouldEqual` 2
      case stacks of
        [ first, second ] -> do
          first.hiddenCount `shouldEqual` 0
          second.hiddenCount `shouldEqual` 0
        _ -> fail "Expected exactly two mobile overlap stacks"

    it "chooses the earliest end time when overlapping items start together" do
      let
        longItem = serverCalendarItem "stack-e" (calendarContent Task "Long" "2026-02-19T09:00" "2026-02-19T10:30")
        shortItem = serverCalendarItem "stack-f" (calendarContent Task "Short" "2026-02-19T09:00" "2026-02-19T09:45")
        stacks = buildMobileOverlapStacks [ longItem, shortItem ]
      case stacks of
        [ stack ] -> do
          stack.topItem `shouldEqual` shortItem
          map _.item stack.hiddenCards `shouldEqual` [ longItem ]
          map _.duration stack.hiddenCards `shouldEqual` [ 90 ]
          stack.duration `shouldEqual` 90
        _ -> fail "Expected exactly one mobile overlap stack"

    it "uses a stable identity fallback when start and end times match" do
      let
        itemB = serverCalendarItem "stack-z" (calendarContent Task "B" "2026-02-19T09:00" "2026-02-19T10:00")
        itemA = serverCalendarItem "stack-a" (calendarContent Task "A" "2026-02-19T09:00" "2026-02-19T10:00")
        stacks = buildMobileOverlapStacks [ itemB, itemA ]
      case stacks of
        [ stack ] -> do
          stack.topItem `shouldEqual` itemA
          map _.item stack.hiddenCards `shouldEqual` [ itemB ]
        _ -> fail "Expected exactly one mobile overlap stack"

    it "keeps chronological full-group ordering for overlap sheet display" do
      let
        itemB = serverCalendarItem "stack-k" (calendarContent Task "B" "2026-02-19T09:10" "2026-02-19T10:00")
        itemA = serverCalendarItem "stack-j" (calendarContent Task "A" "2026-02-19T09:00" "2026-02-19T09:45")
        itemC = serverCalendarItem "stack-l" (calendarContent Task "C" "2026-02-19T09:20" "2026-02-19T09:50")
        stacks = buildMobileOverlapStacks [ itemC, itemB, itemA ]
      case stacks of
        [ stack ] -> do
          stack.items `shouldEqual` [ itemA, itemB, itemC ]
          stack.hiddenCount `shouldEqual` 2
        _ -> fail "Expected exactly one mobile overlap stack"

    it "preserves hidden card start and end geometry for staggered overlaps" do
      let
        topItem = serverCalendarItem "stack-g" (calendarContent Task "Top" "2026-02-19T09:00" "2026-02-19T09:45")
        hiddenEarly = serverCalendarItem "stack-h" (calendarContent Task "Hidden 1" "2026-02-19T09:10" "2026-02-19T10:10")
        hiddenLate = serverCalendarItem "stack-i" (calendarContent Task "Hidden 2" "2026-02-19T09:20" "2026-02-19T09:50")
        stacks = buildMobileOverlapStacks [ hiddenLate, hiddenEarly, topItem ]
      case stacks of
        [ stack ] -> do
          stack.topItem `shouldEqual` topItem
          map _.startMin stack.hiddenCards `shouldEqual` [ 550, 560 ]
          map _.duration stack.hiddenCards `shouldEqual` [ 60, 30 ]
          map _.stackIndex stack.hiddenCards `shouldEqual` [ 1, 2 ]
          stack.startMin `shouldEqual` 540
          stack.duration `shouldEqual` 70
        _ -> fail "Expected exactly one mobile overlap stack"

    it "promotes a selected hidden item to the top of its mobile stack" do
      let
        itemA = serverCalendarItem "stack-promote-a" (calendarContent Task "A" "2026-02-19T09:00" "2026-02-19T09:45")
        itemB = serverCalendarItem "stack-promote-b" (calendarContent Task "B" "2026-02-19T09:10" "2026-02-19T10:00")
        itemC = serverCalendarItem "stack-promote-c" (calendarContent Task "C" "2026-02-19T09:20" "2026-02-19T09:50")
        stacks =
          applyMobileOverlapPromotions
            [ { groupKey: "server:stack-promote-a|server:stack-promote-b|server:stack-promote-c"
              , itemIdentity: "server:stack-promote-c"
              }
            ]
            (buildMobileOverlapStacks [ itemA, itemB, itemC ])
      case stacks of
        [ stack ] -> do
          stack.topItem `shouldEqual` itemC
          map _.item stack.hiddenCards `shouldEqual` [ itemA, itemB ]
          map _.stackIndex stack.hiddenCards `shouldEqual` [ 1, 2 ]
        _ -> fail "Expected exactly one promoted mobile overlap stack"

    it "keeps promotions scoped to their overlap group" do
      let
        groupOneA = serverCalendarItem "stack-group-1a" (calendarContent Task "Group 1A" "2026-02-19T09:00" "2026-02-19T09:45")
        groupOneB = serverCalendarItem "stack-group-1b" (calendarContent Task "Group 1B" "2026-02-19T09:10" "2026-02-19T10:00")
        groupTwoA = serverCalendarItem "stack-group-2a" (calendarContent Task "Group 2A" "2026-02-19T11:00" "2026-02-19T11:45")
        groupTwoB = serverCalendarItem "stack-group-2b" (calendarContent Task "Group 2B" "2026-02-19T11:10" "2026-02-19T12:00")
        stacks =
          applyMobileOverlapPromotions
            [ { groupKey: "server:stack-group-1a|server:stack-group-1b"
              , itemIdentity: "server:stack-group-1b"
              }
            ]
            (buildMobileOverlapStacks [ groupOneA, groupOneB, groupTwoA, groupTwoB ])
      case stacks of
        [ first, second ] -> do
          first.topItem `shouldEqual` groupOneB
          second.topItem `shouldEqual` groupTwoA
        _ -> fail "Expected exactly two mobile overlap stacks"

    it "falls back to default ordering when the promoted identity is missing" do
      let
        itemA = serverCalendarItem "stack-fallback-a" (calendarContent Task "A" "2026-02-19T09:00" "2026-02-19T09:45")
        itemB = serverCalendarItem "stack-fallback-b" (calendarContent Task "B" "2026-02-19T09:10" "2026-02-19T10:00")
        stacks =
          applyMobileOverlapPromotions
            [ { groupKey: "server:stack-fallback-a|server:stack-fallback-b"
              , itemIdentity: "server:missing"
              }
            ]
            (buildMobileOverlapStacks [ itemA, itemB ])
      case stacks of
        [ stack ] -> do
          stack.topItem `shouldEqual` itemA
          map _.item stack.hiddenCards `shouldEqual` [ itemB ]
        _ -> fail "Expected exactly one fallback mobile overlap stack"

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
          case calendarContent Task "Done" "2026-02-19T11:00" "2026-02-19T12:00" of
            TaskCalendarItemContent content ->
              ServerCalendarItem
                { id: "done"
                , content: TaskCalendarItemContent (content { status = Done })
                }
            TripCalendarItemContent _ ->
              serverCalendarItem "done" (calendarContent Task "Done" "2026-02-19T11:00" "2026-02-19T12:00")
      sortItems SortByStatus [ itemDone, itemTodo ] `shouldEqual` [ itemTodo, itemDone ]

    it "sortItems SortByTime orders trips and tasks by windowStart" do
      let
        task = serverCalendarItem "task" (calendarContent Task "Task" "2026-02-19T10:00" "2026-02-19T11:00")
        trip = serverCalendarItem "trip" (tripCalendarContent "Paris" "St Clair" "2026-02-19T08:00" "2026-02-19T09:30")
      sortItems SortByTime [ task, trip ] `shouldEqual` [ trip, task ]

  describe "Calendar recurrence" do
    it "generateOccurrencesForMonth excludes exception dates" do
      let
        occurrences = generateOccurrencesForMonth Weekly [ "2026-02-19" ] "2026-02-05T09:00"
      occurrences `shouldEqual` [ "2026-02-05", "2026-02-12", "2026-02-26" ]
