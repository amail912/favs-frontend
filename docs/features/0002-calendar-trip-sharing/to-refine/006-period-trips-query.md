# Period Trips Query

## Goal
As the frontend, I want a specialized shared-trips query for a period so I can derive each followed user's location over that period without reconstructing the whole calendar history.

## How This Story Achieves The Goal
The frontend should request just enough ordered trip data to seed location at the period start and then apply the trip changes inside the period. This keeps the query narrow while preserving correct frontend derivation under the current rules.

## Technical Details
Consume the dedicated period-based shared-trips query and normalize the grouped ordered response for presence derivation. The frontend should rely on one seed trip before the requested start and the trips starting inside the requested period.

## Tests
- Integration tests for consuming the grouped ordered response in the calendar frontend.
- E2E tests covering shared presence derivation from a period-based trips response.
