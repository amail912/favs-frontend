# Mobile Actions Sheet

## Goal
As a mobile user, I want one compact entrypoint for secondary calendar actions, so I can keep the top area light while still reaching filters and tools quickly.

Relevant screenshots:
- [DayCalendar top area](../screenshots/daycalendar-top-mobile.png)
- [DayCalendar tools modal](../screenshots/daycalendar-tools-modal-mobile.png)

## How This Story Achieves The Goal
The mobile calendar header should expose a single secondary-actions trigger instead of separate `Filters` and `Tools` buttons. That trigger should open a touch-friendly mobile actions sheet that groups filters and current tool actions in a faster, clearer, and easier-to-dismiss surface.

## Technical Details
This story owns only mobile secondary actions. It is in scope to replace the current dual-button mobile entrypoint and the confirm-style modal flow with a single compact trigger and a mobile actions sheet. It is out of scope to redesign the global app shell, change date wording, change initial timeline scroll, or change overlap rendering. The likely implementation area is the mobile view action rendering in `src/Pages/Calendar.purs`, plus shared modal or sheet UI used to expose filters and tool actions through one mobile surface.

## Tests
- Unit tests for any helper that groups mobile secondary actions behind the single trigger.
- Integration tests ensuring the compact trigger is rendered on mobile and that all existing filter and tool actions remain exposed through the new surface.
- E2E tests verifying that the separate `Filters` and `Tools` buttons are replaced by one mobile secondary-actions trigger, that the mobile sheet can open and close cleanly, and that a user can still access filters, templates, imports, and export from that surface.
