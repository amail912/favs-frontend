# Trip Calendar Item

## Goal
As a user, I want a trip to appear as a distinct calendar item with a readable route so I can understand it immediately in the agenda.

## Outcome
Trips become a first-class frontend calendar item variant. Existing agenda cards remain the rendering surface, but trip cards expose the route clearly enough to distinguish them from normal calendar items at a glance.

## In Scope
- Decode trip items returned by `/api/v1/calendar-items` using the trip-specific fields `windowStart`, `windowEnd`, `departurePlaceId`, and `arrivalPlaceId`.
- Store trips as a dedicated frontend variant instead of forcing them into the existing task-only content shape.
- Encode trip items back to the backend using the same trip payload shape.
- Render trip cards in agenda lists and day surfaces with readable route information based on departure and arrival place names.
- Keep existing non-trip item rendering unchanged.

## Out Of Scope
- Trip create and edit form controls.
- Loading the predefined place catalog.
- Validation messaging for invalid trip writes.

These remain in `0002-003`.

## Dependencies
- No functional dependency inside feature `0002`.
- This story is a prerequisite for `003`, `006`, and any Day view presence work.

## Acceptance Criteria
- A trip item is decoded as a distinct frontend item type.
- A normal calendar item still decodes and renders exactly as before.
- A trip card shows the route in a readable form using both departure and arrival place names.
- The trip card remains editable through the same calendar surfaces that expose existing items.

## Tests
- Integration tests ensuring trip items are decoded and stored distinctly from normal items.
- Integration tests ensuring trip payload encoding preserves the current backend field names for trips.
- Integration tests ensuring calendar item cards render trip route information while non-trip items keep their current rendering behavior.
- E2E tests loading a trip from the backend and seeing it rendered as a trip with readable route information in the agenda.
