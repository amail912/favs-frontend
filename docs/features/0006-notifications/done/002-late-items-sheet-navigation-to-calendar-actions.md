# Late-Items Sheet Navigation To Calendar Actions

## Goal
As a user, I want to jump directly from a late reminder entry to the corresponding Calendar context so I can act without searching manually.

## Outcome
Selecting a late item from the reminder sheet routes to the appropriate Calendar day and opens that item's actions directly.

## In Scope
- Tap/click behavior for late-item rows in reminder bottom sheet.
- Route contract extension needed to target both consulted day and item identity.
- Calendar-side handling of targeted item opening in day context.
- Graceful fallback when targeted item no longer exists.

## Out Of Scope
- Changes to calendar item action content.
- New Calendar editing workflows.
- Reminder refresh policy details (handled in a dedicated story).

## Technical Details
- Row selection routes to Calendar with:
  - consulted day aligned with selected item,
  - item identifier included for direct actions opening.
- On Calendar arrival:
  - app restores/sets the target day,
  - opens item action surface for target item.
- If target item is absent, Calendar remains on targeted day with no blocking error modal.

## Acceptance Criteria
- From any authenticated page, selecting a late-item row navigates to Calendar route.
- Calendar opens focused on selected item's day.
- Item actions surface opens automatically for the targeted item.
- If targeted item is not found at render time, Calendar displays day normally and remains interactive.
- Browser history remains coherent (back returns to previous page/sheet context per existing app navigation rules).

## Tests
- Unit/integration tests for route parse/print with day + item targeting.
- Integration test for Calendar target resolution from route input.
- E2E test: open reminder sheet on non-Calendar page, select row, verify Calendar day focus and item actions open.
- E2E fallback test where targeted item is missing, verifying no hard failure and usable Calendar view.
