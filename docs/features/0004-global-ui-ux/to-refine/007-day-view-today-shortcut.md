# Day View Today Shortcut

## Goal
As a calendar user, I want a quick `Aujourd'hui` action in Day view so I can return to the current day without manually browsing dates.

## How This Story Achieves The Goal
It adds a dedicated `Aujourd'hui` control in the Day view navigation area. The action jumps directly to the current local day using the same consulted-day behavior as existing Day controls, so users get a fast and predictable way back to today.

## Technical Details
- Surface: Calendar Day view header controls.
- Visibility: show the control only in `Jour`, hide it in `Semaine` and `Mois`.
- State: clicking `Aujourd'hui` updates canonical consulted day state (`focusDate`) to the current local day.
- Consistency: day label and day-bound data loading continue to derive from the same state path already used by date picker and arrows.
- Responsiveness: keep the control reachable on desktop and mobile without layout regression.

## Tests
- Integration tests for `Aujourd'hui` visibility in Day view only.
- Integration tests for `Aujourd'hui` action updating `focusDate` to current local day.
- Integration tests for synchronized day label and day-bound content after the action.
- E2E test on desktop: navigate away from today, click `Aujourd'hui`, verify day restore.
- E2E test on mobile layout: same restore-to-today flow.
