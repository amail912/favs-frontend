# Trip Validation And Editing

## Goal
As a user, I want invalid trips to be rejected clearly so I can trust the location information derived from my calendar.

## Outcome
Trip creation and editing become usable from the existing agenda create and edit flow. The UI stays aligned with the current modal pattern, but it becomes variant-aware so tasks and trips can each expose the right fields and validation rules.

## In Scope
- Extend the existing agenda create entrypoint so the modal can create either a task or a trip.
- Keep the existing modal pattern for editing and add a trip-specific edit variant instead of introducing a separate trip flow.
- Load place choices from `GET /api/v1/trip-places`.
- Treat the backend `name` field as both identifier and visible label for this iteration.
- Constrain departure and arrival to catalog-backed select fields instead of free text.
- Support trip creation with the fields:
  - departure place
  - arrival place
  - start
  - end
- Support trip editing with the same fields as trip creation.
- Keep task create and edit behavior unchanged.
- Surface the backend validation rules clearly in the UI:
  - departure place must be selected
  - arrival place must be selected
  - departure and arrival must differ
  - end must be strictly after start
  - trip windows for the same owner must not overlap
- Preserve the same rules for editing as for creation.

## Technical Details
- The page state should gain a trip-places cache with:
  - loading state
  - loaded list of place names
  - optional load error
- The create and edit modal state should become variant-aware rather than task-only:
  - task branch keeps the current task draft and edit draft behavior
  - trip branch introduces a dedicated trip draft with `departurePlaceId`, `arrivalPlaceId`, `windowStart`, and `windowEnd`
- The existing create entrypoint should still default to task, with an explicit type switch inside the modal to choose trip.
- Trip forms should not display task-only fields such as category, status, actual duration, or recurrence.
- Trip create should encode `NewCalendarItem { content = TripCalendarItemContent ... }`.
- Trip edit should update the existing item with `TripCalendarItemContent ...`.
- Trip save should be disabled until the place list is loaded and the local draft is valid.
- Backend rejections should be mapped to understandable UI feedback for:
  - invalid place reference
  - same departure and arrival
  - invalid time order
  - overlapping trip window
  - generic non-success write failure
- The timeline drag and drop flow should remain out of scope for trips in this story. The supported edit surface is the modal.

## Out Of Scope
- Sharing lists.
- Shared presence derivation.
- Day view presence cues.
- Import and export support for trips.
- Timeline drag and drop editing for trips.

## Dependencies
- Depends on `002 Trip Calendar Item`.

## Acceptance Criteria
- A user can open the existing create flow, switch it to trip, and create a trip by selecting two catalog places and a valid time window.
- A user can open an existing trip from the agenda and edit it through the same modal pattern.
- Task creation and task editing continue to behave as before.
- The trip form uses select controls for places rather than free-text inputs.
- The trip form disables save until required values are loaded and valid.
- The UI blocks obvious invalid states before submit where practical.
- Backend rejections are mapped to understandable feedback when the server refuses a trip write.
- A trip form with a failed place-catalog load shows a visible blocking error instead of allowing an invalid save attempt.

## Tests
- Integration tests covering trip-place response decoding.
- Integration tests covering place-catalog loading states in the trip form.
- Integration tests covering local trip validation for missing place, same place, invalid date, and invalid window order.
- Integration tests covering trip create and trip update payload encoding through `CalendarItem`.
- Integration tests covering trip edit draft creation and round-trip edit behavior.
- Integration tests covering backend error mapping for same-place, invalid window order, and overlapping trip windows.
- Integration tests confirming the existing task create and edit flow remains unchanged.
- E2E tests confirming valid trips can be created from the existing create entrypoint and edited from an existing trip item.
- E2E tests confirming invalid trips cannot be saved and produce understandable feedback.
- E2E tests covering trip-place load failure in the modal.
