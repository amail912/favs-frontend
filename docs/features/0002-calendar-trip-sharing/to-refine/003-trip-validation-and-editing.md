# Trip Validation And Editing

## Goal
As a user, I want invalid trips to be rejected clearly so I can trust the location information derived from my calendar.

## How This Story Achieves The Goal
Trip creation and editing should enforce valid time windows and valid place transitions. The UI should make those rules understandable before bad data reaches sharing and presence cues.

## Technical Details
Use constrained predefined-place choices in the create and edit flows, then validate that both places are selected, that departure and arrival differ, and that the end is after the start. Editing should preserve the same rules as creation and keep the failure states understandable.

## Tests
- Integration tests covering constrained place selection and validation feedback in trip create and edit flows.
- E2E tests confirming invalid trips cannot be saved, especially when departure and arrival would resolve to the same place.
