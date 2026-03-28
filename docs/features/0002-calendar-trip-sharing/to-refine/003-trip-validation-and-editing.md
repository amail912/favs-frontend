# Trip Validation And Editing

## Goal
As a user, I want invalid trips to be rejected clearly so I can trust the location information derived from my calendar.

## How This Story Achieves The Goal
Trip creation and editing should enforce valid time windows and valid place transitions. The UI should make those rules understandable before bad data reaches sharing and presence cues.

## Technical Details
Validate that both places exist, that departure and arrival differ, and that the end is after the start. Editing should preserve the same rules as creation and stay aligned with the current calendar update contract.

## Tests
- Integration tests covering validation feedback in trip create and edit flows.
- E2E tests confirming invalid trips cannot be saved.
