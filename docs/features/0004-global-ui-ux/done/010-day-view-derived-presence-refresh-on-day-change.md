# Day View Derived Presence Refresh On Day Change

## Goal
As a calendar user, I want the Day view presence rail to always reflect the currently consulted day so location segments update immediately when I change day.

## Outcome
Changing day in Day view recomputes derived presence from the selected day’s trip period data, and stale day data is no longer shown in the rail.

## In Scope
- Day view only.
- Day changes triggered by:
  - previous/next arrows
  - date picker/date input
  - `Aujourd'hui`
  - router/history day restoration (`?day=YYYY-MM-DD`)
- Recompute pipeline for shared presence rail from selected day period trips.
- Protection against stale async responses overriding the latest selected day.

## Out Of Scope
- Week/Month presence behavior.
- Visual redesign of navigation or rail.
- Backend contract changes.

## Technical Details
- Ensure all Day day-change entrypoints run the same day-bound presence refresh path.
- Keep existing route sync behavior; fix concerns load/recompute timing only.
- The recomputation must use selected-day period bounds (`start/end`) and update rail segments atomically for that day.
- If multiple day changes happen quickly, only latest request result can commit rail state.
- Preserve existing error states/messages for load failures.

## Acceptance Criteria
- From Day view, clicking next/previous changes rail segments to match the new day.
- Returning to a prior day restores that day’s expected rail segments without full page reload.
- Reloading the page on a day still produces same rail result as navigation to that day.
- Browser back/forward day restoration also refreshes rail for restored day.
- Rapid day switching never leaves rail stuck on an older day.

## Tests
- Integration: previous/next day actions trigger presence reload/recompute for selected day.
- Integration: date input and `Aujourd'hui` trigger the same recompute path.
- Integration: router `ReceiveInput` day change triggers same recompute path.
- Integration: stale-response guard keeps only latest day result.
- E2E: mock distinct period-trips payloads for consecutive days; verify rail segment changes with next/previous.
- E2E: verify same day reached by reload and by navigation yields same rail segments.
