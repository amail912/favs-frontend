# Mobile Actions Sheet

## Goal
As a mobile user, I want one compact entrypoint for secondary calendar actions, so I can keep the top area light while still reaching import and export utilities quickly.

Relevant screenshots:
- [DayCalendar top area](../screenshots/daycalendar-top-mobile.png)
- [DayCalendar tools modal](../screenshots/daycalendar-tools-modal-mobile.png)

## How This Story Achieves The Goal
The mobile calendar header should expose a single secondary-actions trigger instead of a larger set of top-level utility controls. That trigger should open a touch-friendly mobile actions sheet focused on the remaining utility workflows: `Import CSV`, `Import ICS`, and `Export`.

## Technical Details
This story owns only mobile secondary actions. It is in scope to expose the remaining utility workflows behind a single compact trigger and mobile actions sheet. It is out of scope to redesign the global app shell, change date wording, change initial timeline scroll, or change overlap rendering. The likely implementation area is the mobile view action rendering in `src/Pages/Calendar.purs`, plus shared modal or sheet UI used to expose import and export actions through one mobile surface.

## Tests
- Unit tests for any helper that groups the remaining mobile utility actions behind the single trigger.
- Integration tests ensuring the compact trigger is rendered on mobile and that `Import CSV`, `Import ICS`, and `Export` remain exposed through the new surface.
- E2E tests verifying that the mobile sheet can open and close cleanly and that a user can still access `Import CSV`, `Import ICS`, and `Export` from that surface.

## Cancel reason
Not useful anymore after cleanup.
