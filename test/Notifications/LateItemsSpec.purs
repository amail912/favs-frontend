module Test.Notifications.LateItemsSpec (spec) where

import Prelude

import Data.Array (length, replicate)
import Notifications.LateItems as LateItems
import Pages.Calendar (CalendarItem, CalendarItemContent(..), ItemStatus(..), ItemType(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Support.Builders (calendarContent, serverCalendarItem, tripCalendarContent, unsafeDateTime)

spec :: Spec Unit
spec =
  describe "Late items reminder derivation" do
    it "keeps only ended non-trip items not done/canceled" do
      let
        now = unsafeDateTime "2026-04-10T12:00"
        eligibleTodo = mkTaskItem "todo" Todo "Todo late" "2026-04-10T08:00" "2026-04-10T09:00"
        eligibleInProgress = mkTaskItem "in-progress" InProgress "In progress late" "2026-04-10T09:00" "2026-04-10T10:00"
        excludedDone = mkTaskItem "done" Done "Done late" "2026-04-10T07:00" "2026-04-10T08:00"
        excludedCanceled = mkTaskItem "canceled" Canceled "Canceled late" "2026-04-10T06:00" "2026-04-10T07:00"
        excludedFuture = mkTaskItem "future" Todo "Future task" "2026-04-10T12:30" "2026-04-10T13:30"
        excludedTrip = serverCalendarItem "trip" (tripCalendarContent "Paris" "Lyon" "2026-04-10T07:00" "2026-04-10T08:00")
        lateItems =
          LateItems.deriveLateItems now
            [ eligibleTodo
            , eligibleInProgress
            , excludedDone
            , excludedCanceled
            , excludedFuture
            , excludedTrip
            ]
      map _.title lateItems `shouldEqual` [ "In progress late", "Todo late" ]

    it "orders late items by descending end time" do
      let
        now = unsafeDateTime "2026-04-10T14:00"
        older = mkTaskItem "older" Todo "Older" "2026-04-10T08:00" "2026-04-10T09:00"
        newer = mkTaskItem "newer" Todo "Newer" "2026-04-10T10:00" "2026-04-10T11:00"
        newest = mkTaskItem "newest" Todo "Newest" "2026-04-10T12:00" "2026-04-10T13:00"
        lateItems = LateItems.deriveLateItems now [ older, newest, newer ]
      map _.title lateItems `shouldEqual` [ "Newest", "Newer", "Older" ]
      map _.day lateItems `shouldEqual` [ "2026-04-10", "2026-04-10", "2026-04-10" ]

    it "supports first-50 pagination and has-more detection" do
      let
        now = unsafeDateTime "2026-04-10T14:00"
        seed = mkTaskItem "seed" Todo "Late task" "2026-04-10T08:00" "2026-04-10T09:00"
        lateItems = LateItems.deriveLateItems now (replicate 55 seed)
      length (LateItems.visibleLateItems 50 lateItems) `shouldEqual` 50
      LateItems.hasMoreLateItems 50 lateItems `shouldEqual` true
      LateItems.hasMoreLateItems 100 lateItems `shouldEqual` false

mkTaskItem :: String -> ItemStatus -> String -> String -> String -> CalendarItem
mkTaskItem id status title start end =
  serverCalendarItem id (setStatus status (calendarContent Task title start end))

setStatus :: ItemStatus -> CalendarItemContent -> CalendarItemContent
setStatus status = case _ of
  TaskCalendarItemContent content -> TaskCalendarItemContent (content { status = status })
  TripCalendarItemContent content -> TripCalendarItemContent content
