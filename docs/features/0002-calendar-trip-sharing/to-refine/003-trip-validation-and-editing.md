# Trip Validation And Editing

## Goal
As a user, I want invalid trips to be rejected clearly so I can trust the location information derived from my calendar.

## Outcome
Trip creation and editing become usable from the calendar UI with constrained place selection and clear feedback on the validation rules already enforced by the backend.

## In Scope
- Load place choices from `GET /api/v1/trip-places`.
- Treat the backend `name` field as the selectable place value for this iteration.
- Add create and edit affordances for trip items on top of the trip item model from `002`.
- Constrain departure and arrival to the catalog instead of free text.
- Surface the backend validation rules clearly in the UI:
  - both places must be selected
  - departure and arrival must differ
  - end must be strictly after start
  - a trip must not overlap another trip owned by the same user
- Preserve the same rules for editing as for creation.

## Out Of Scope
- Sharing lists.
- Shared presence derivation.
- Day view presence cues.

## Dependencies
- Depends on `002 Trip Calendar Item`.

## Acceptance Criteria
- A user can create a trip by selecting two catalog places and a valid time window.
- A user can edit an existing trip with the same constraints.
- The UI prevents obvious invalid states before submit where practical.
- Backend rejections are mapped to understandable feedback when the server refuses a trip write.

## Tests
- Integration tests covering place-catalog loading and constrained place selection in create and edit flows.
- Integration tests covering validation feedback for same-place, invalid window order, and overlapping trip windows.
- E2E tests confirming valid trips can be created and edited.
- E2E tests confirming invalid trips cannot be saved and produce understandable feedback.
