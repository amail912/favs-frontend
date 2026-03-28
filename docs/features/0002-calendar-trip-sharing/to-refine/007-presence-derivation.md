# Presence Derivation

## Goal
As a calendar viewer, I want the application to infer where the other person is throughout the day so I do not have to read every trip manually.

## How This Story Achieves The Goal
The frontend should derive user presence from the ordered trips returned for the requested period. This transforms raw trip events into a continuous location state that the Day view can render.

## Technical Details
Derive an initial state from the seed trip before the period, then apply later trips in chronological order. The derived state must support both stable presence at a place and in-transit periods between departure and arrival.

## Tests
- Unit tests for derivation with no seed trip.
- Unit tests for a completed seed trip before the period.
- Unit tests for a seed trip still active at the period start.
- Unit tests for multiple in-period trips applied in order.
