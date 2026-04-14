# Quick Complete Late Items With Duration Prompt

## Goal
As a user, I want to complete late items directly from reminders with minimal friction while still controlling actual duration.

## Outcome
The reminder sheet supports an in-context quick complete action that asks for duration, prefills it from planned item length, validates the item, and keeps the sheet open for batch processing.

## In Scope
- Row-level quick complete action in late-item sheet.
- Duration prompt UX and validation.
- Prefill rule for duration from planned `windowStart`/`windowEnd`.
- Completion call through existing validate endpoint.
- Post-success sheet refresh while keeping sheet open.

## Out Of Scope
- Auto-completing without user confirmation.
- Status-only update path as alternative completion contract.
- New backend endpoints.

## Technical Details
- Quick complete opens a confirmation/prompt surface requesting duration in minutes.
- Default input value is computed from planned duration (`windowEnd - windowStart`), clamped to a positive integer.
- Submit triggers existing validate API (`/api/v1/calendar-items/:id/validate`) with `duree_reelle_minutes`.
- On success:
  - target item leaves late list if no longer eligible,
  - sheet stays open,
  - list/count refreshes in-place.
- On error:
  - actionable inline feedback appears,
  - user remains in flow and can retry/cancel.

## Acceptance Criteria
- Each late-item row exposes a clear quick complete action.
- Triggering quick complete opens duration prompt before API call.
- Prompt input is prefilled with planned duration in minutes.
- Confirming with valid duration sends validate request for selected item.
- Successful completion updates reminder list/count without closing the sheet.
- Invalid duration blocks submission with clear feedback.
- Backend/network failure displays error feedback and preserves user context.

## Tests
- Unit tests for planned-duration-to-prefill conversion, including boundary values.
- Integration tests for prompt open/close, validation guard, and success/error state transitions.
- Integration test verifying sheet remains open and list refreshes after success.
- E2E test completing multiple late items sequentially from the same open sheet.
- E2E test for API failure feedback and retry path.
