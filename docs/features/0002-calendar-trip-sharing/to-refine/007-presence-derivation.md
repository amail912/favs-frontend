# Presence Derivation

## Goal
As a calendar viewer, I want the application to infer where the other person is throughout the day so I do not have to read every trip manually.

## Outcome
The frontend derives a deterministic timeline of location states from grouped ordered trips. This turns trip events into a view-model usable by Day view features without mixing derivation logic into rendering code.

## In Scope
- Derive initial state from the optional seed trip before the requested period.
- Derive `unknown` when no earlier visible trip establishes a location.
- Derive `at place` after a completed trip.
- Derive `in transit` while a trip is active.
- Apply later trips in chronological order.
- Produce a derived structure suitable for Day view rendering and inspection.

## Out Of Scope
- The visual form of the Day view rail.
- Interaction details for inspection.

## Dependencies
- Depends on `006 Period Trips Query`.
- Enables `008`, `009`, `011`, and `012`.

## Acceptance Criteria
- A user with no seed trip starts in an unknown state until the first trip provides context.
- A completed seed trip yields the arrival place as the initial known state.
- A seed trip that overlaps the period start yields an in-transit state until its end.
- Later trips update the state in the same order as the backend response.

## Tests
- Unit tests for derivation with no seed trip.
- Unit tests for a completed seed trip before the period.
- Unit tests for a seed trip still active at the period start.
- Unit tests for multiple in-period trips applied in order.
- Unit tests for stable arrival-place state after a trip ends.
