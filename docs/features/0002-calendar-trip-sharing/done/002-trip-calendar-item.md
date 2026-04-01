# Trip Calendar Item

## Goal
As a user, I want a trip to appear as a distinct calendar item with a readable route so I can understand it immediately in the agenda.

## Outcome
Trips become a first-class frontend calendar item variant. Existing agenda cards remain the rendering surface, but trip cards expose the route, the departure time, and the arrival time clearly enough to distinguish them from normal calendar items at a glance.

## In Scope
- Replace the current task-only frontend content shape in `Pages.Calendar` with a sum type that can represent both standard calendar items and trips.
- Decode trip items returned by `/api/v1/calendar-items` using the trip-specific fields `windowStart`, `windowEnd`, `departurePlaceId`, and `arrivalPlaceId`.
- Keep standard calendar items decoded with the existing fields `titre`, `fenetre_debut`, `fenetre_fin`, `statut`, `categorie`, recurrence fields, and current behavior.
- Encode trip items back to the backend using the same trip payload shape, without leaking task-only fields into trip payloads.
- Add total helper accessors for common scheduling behavior so sorting, timeline layout, day filtering, and list rendering work for both item variants without duplicating logic.
- Render trip cards in agenda lists and day surfaces with:
  - the route as the primary readable content
  - the departure and arrival hours explicitly visible
  - a dedicated trip visual treatment distinct from other calendar items
- Keep existing non-trip item rendering unchanged.

## Technical Details
- The frontend type introduced by this story should be decision-complete:
  - `TaskCalendarItemContent TaskCalendarFields`
  - `TripCalendarItemContent TripCalendarFields`
- `TripCalendarFields` should contain:
  - `windowStart :: DateTime`
  - `windowEnd :: DateTime`
  - `departurePlaceId :: String`
  - `arrivalPlaceId :: String`
- `CalendarItem = NewCalendarItem | ServerCalendarItem` remains unchanged so item identity, list state, and sync plumbing stay stable.
- Shared helpers should be introduced for cross-cutting calendar behavior that currently assumes task fields directly, especially window access and ordering.
- Rendering rules for a trip card are fixed in this story:
  - primary line: `departurePlaceId -> arrivalPlaceId`
  - secondary line: explicit departure and arrival times derived from `windowStart` and `windowEnd`
  - dedicated trip class and color distinct from other calendar items
  - no task-only metadata such as category or status on the trip card
- `departurePlaceId` and `arrivalPlaceId` are rendered directly as readable place labels in this story. Place catalog lookup remains deferred to `003`.

## Out Of Scope
- Trip create and edit form controls.
- Loading the predefined place catalog.
- Validation messaging for invalid trip writes.
- CSV or ICS import or export expansion for trips.
- Any shared presence, period query, or Day view rail behavior.

These remain in `0002-003`.

## Dependencies
- No functional dependency inside feature `0002`.
- This story is a prerequisite for `003`, `006`, and any Day view presence work.

## Acceptance Criteria
- A trip item is decoded as a distinct frontend item type.
- A normal calendar item still decodes and renders exactly as before.
- A mixed response containing standard calendar items and trip items decodes successfully.
- A trip item encodes back to the backend with `type = "trip"`, `windowStart`, `windowEnd`, `departurePlaceId`, and `arrivalPlaceId`.
- A trip card shows the route in a readable form using both departure and arrival place names.
- A trip card shows departure and arrival hours explicitly.
- A trip card has a dedicated visual treatment, including a trip-specific color distinct from standard calendar items.
- Existing sorting, day filtering, and timeline placement still work for trips through shared scheduling helpers.
- Trip cards do not open a broken task-only editor in this story. Trip editing remains deferred to `003`.

## Tests
- Integration tests ensuring trip items are decoded and stored distinctly from normal items.
- Integration tests ensuring legacy standard calendar items still decode and encode unchanged.
- Integration tests ensuring a mixed array of standard items and trips decodes successfully.
- Integration tests ensuring trip payload encoding preserves the current backend field names for trips.
- Integration tests ensuring sorting and timeline helpers order and place trips from `windowStart` and `windowEnd`.
- Integration tests ensuring calendar item cards render trip route information, departure and arrival hours, and trip-specific styling while non-trip items keep their current rendering behavior.
- Integration tests ensuring trip cards do not expose unsupported task-only edit behavior.
- E2E tests loading a trip from the backend and seeing it rendered as a trip with route, hours, and trip-specific visual treatment in the agenda.
