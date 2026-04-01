# Period Trips Query

## Goal
As the frontend, I want a specialized shared-trips query for a period so I can derive each followed user's location over that period without reconstructing the whole calendar history.

## Outcome
The frontend can request and normalize the shared trips needed for a given period without coupling presence derivation to the generic calendar-items response.

## In Scope
- Query `GET /api/v1/trip-sharing/period-trips?start=<iso>&end=<iso>`.
- Decode the grouped response shape `{ username, trips }`.
- Preserve backend ordering by `windowStart` inside each user group.
- Normalize the response into a frontend shape ready for presence derivation.
- Respect the current backend behavior:
  - at most one seed trip before the requested start per user
  - in-period trips start within the requested bounds
  - users with no usable seed trip and no in-period trip are omitted from the response

## Out Of Scope
- The derivation algorithm itself.
- Day view rendering decisions.

## Dependencies
- Depends on `002 Trip Calendar Item`.
- Depends functionally on `004` and `005` for realistic end-to-end visibility scenarios.
- Blocks `007 Presence Derivation`.

## Acceptance Criteria
- The frontend can load grouped shared trips for a requested period.
- The normalized data structure preserves username grouping and trip ordering.
- Omitted users are treated as having no visible data for the period rather than as errors.

## Tests
- Integration tests for decoding and normalizing the grouped ordered response.
- Integration tests covering the presence of a seed trip before the requested start.
- Integration tests covering omission of users with no visible trips in or before the period.
- E2E tests covering a shared-period query flow that feeds presence derivation.
