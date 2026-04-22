# Quick Complete Late Items Via Status Update

## Goal
As a user, I want to resolve late items from reminders even after the legacy `/validate` endpoint removal, while keeping clear and reliable in-sheet feedback.

## Outcome
Quick complete uses the calendar item update contract (`POST /api/v1/calendar-items`) by changing item status, and the reminder state updates from the backend response with explicit behavior for items that remain late.

## In Scope
- Replace quick-complete contract from `/validate` to status update through `POST /api/v1/calendar-items`.
- Keep duration prompt in quick-complete flow (`duree_reelle_minutes` remains user-entered).
- Define state transition rules in reminder sheet based on API response body.
- Explicit handling for late items that remain late after update.
- Define required coverage updates (unit/integration/E2E) for this behavior.

## Out Of Scope
- New backend endpoints.
- Reminder-scope changes beyond late calendar items.
- Polling or background refresh redesign.

## Backend Contract Input
- Existing list endpoint: `GET /api/v1/calendar-items`.
- Quick-complete update target: `POST /api/v1/calendar-items`.
- Success responses are expected to return an updated calendar item payload.

## Technical Details
- Quick complete keeps the existing inline duration prompt and validation rules.
- On confirm, frontend sends an item update payload through `POST /api/v1/calendar-items`, with status change (completion path) and `duree_reelle_minutes` from prompt.
- Reminder state update is response-driven:
  - `2xx` + decodable updated item: apply local patch from returned item.
  - If updated item is no longer late (`DONE`, `CANCELED`, or otherwise not eligible), remove it from late list and update chip count.
  - If updated item is still late, keep it in late list and refresh row data/order from returned item.
  - `2xx` + non-decodable body: keep current row state and show inline actionable error.
  - Non-`2xx` or network failure: keep prompt open, show inline error, allow retry/cancel.
- Late-items sheet remains open throughout this flow.

## Acceptance Criteria
- Quick-complete story no longer references `/api/v1/calendar-items/:id/validate`.
- Quick-complete contract is defined with `POST /api/v1/calendar-items` and status update semantics.
- Duration prompt remains part of the quick-complete UX.
- On successful update with usable body:
  - terminal status update removes item from reminders,
  - non-terminal still-late update keeps item in reminders with refreshed row data.
- On failure or unusable success body, user sees inline feedback and can retry without leaving context.
- Sheet stays open during and after quick-complete attempts.

## Tests
- Unit: reminder eligibility and local patch behavior from returned updated item, including still-late vs no-longer-late branches.
- Integration: quick-complete state transitions for success decodable, success non-decodable, and failure/retry.
- E2E: replace `/validate` mocks with `POST /api/v1/calendar-items` in late reminder coverage scenarios and verify chip/list coherence.

## Assumptions
- Backend accepts status update for quick-complete through `POST /api/v1/calendar-items`.
- Backend returns updated item payload on successful update for response-driven local state updates.
