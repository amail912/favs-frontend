# Day View Today Shortcut

## Goal
As a calendar user, I want a quick `Aujourd'hui` action in Day view so I can return to the current day without manually browsing dates.

## How This Story Achieves The Goal
It adds a dedicated `Aujourd'hui` control in the Day view navigation area. The action jumps directly to the current local day using the same consulted-day behavior as existing Day controls, so users get a fast and predictable way back to today.

## Technical Details
- Surface: Calendar Day view header controls.
- Placement: render `Aujourd'hui` in the existing Day navigation cluster with previous/day-trigger/next controls, without introducing a new toolbar or modal.
- Visibility: show the control only in `Jour`, hide it in `Semaine` and `Mois`.
- Accessibility: expose a clear aria label (`Aujourd'hui`) and keep keyboard activation behavior aligned with other Day nav buttons.
- Selector stability: add a stable selector/class for E2E targeting (`calendar-view-day-nav__today`).
- State: clicking `Aujourd'hui` updates canonical consulted day state (`focusDate`) to the browser-local current day.
- URL/history consistency: reuse the current Day route sync behavior (`?day=YYYY-MM-DD`) already used by date picker/arrows.
- No-op policy: when consulted day is already today, clicking `Aujourd'hui` must not create duplicate history entries.
- Consistency: day label and day-bound data loading continue to derive from the same canonical state path.
- Responsiveness: keep the control reachable on desktop and mobile without layout regression.

## Tests
- Integration tests for `Aujourd'hui` visibility in Day view only (hidden in Week/Month).
- Integration tests for `Aujourd'hui` action updating `focusDate` to local current day.
- Integration tests for route synchronization after click (Day URL reflects today).
- Integration tests for no-op behavior when already on today (no duplicate history push).
- Integration tests for synchronized day label and day-bound content after the action.
- E2E desktop scenario: navigate away from today, click `Aujourd'hui`, verify label, URL, and day content restoration.
- E2E mobile scenario: same restore-to-today behavior with control reachable in Day header.
