# Initial Focus

## Goal
As a Day view user, I want the screen to open near the useful part of the day, so I do not have to manually scroll through irrelevant hours before reaching the information that matters. On today, that means landing near the current time. On another day, that means landing near the first task of the day or staying at the top when the day is empty.

Relevant screenshots:
- [DayCalendar top area](../screenshots/daycalendar-top-mobile.png)
- [DayCalendar dense overlaps](../screenshots/daycalendar-dense-overlap-mobile.png)

## How This Story Achieves The Goal
The Day view should automatically focus the timeline when it becomes active and when the selected Day date changes. If the selected date is today, the view should open near the current local time. If the selected date is another day with tasks, the view should open near the earliest task by start time. If the selected date has no task, the view should remain at the top.

This auto-focus should run again when the user switches away from Day view and comes back or when the selected Day date changes. Once the auto-focus has been applied for that rendered Day/date state, normal user scrolling should take precedence and passive rerenders or routine data refreshes should not snap the view back.

## Technical Details
This story applies to Day view in general, not mobile only. The implementation should introduce a pure helper that computes a semantic focus target from the selected date, the local current datetime, and the sorted tasks for that date. That helper should return one of three outcomes: focus near the current time, focus near the first task, or stay at the top.

The Day view rendering layer should translate that semantic target into a scroll offset after the timeline container and its items are mounted. The auto-focus logic should only run for Day view, not Week or Month. The implementation should also keep enough local UI state to avoid reapplying the automatic scroll after the user has already scrolled within the same rendered Day/date state.

Changing the selected Day date or re-entering Day view should reset that guard for the new Day context. The likely implementation area is `src/Pages/Calendar.purs`. CSS changes should remain minimal unless a dedicated scroll anchor or helper element is required.

## Tests
- Unit tests for the focus-strategy helper covering: today with tasks, today without tasks, another day with tasks, and another day without tasks.
- Integration tests for Day view ensuring the focus target is recomputed when Day view becomes active, when the selected Day date changes, and when the user switches away from Day view and comes back.
- Integration tests ensuring passive rerenders or routine data refreshes do not reapply the auto-focus after the user has manually scrolled in the same Day/date state.
- E2E tests verifying that on today the Day view opens near the current time, that on another day with tasks it opens near the earliest task, and that on an empty non-today day it remains at the top.
