# Day View Location Inspection

## Goal
As a calendar viewer, I want to inspect the exact location behind the presence cue so I can read explicit place names when I need them.

## How This Story Achieves The Goal
The presence cue should stay compact by default, but it must reveal the exact location through interaction. The interaction needs to work consistently on desktop, mobile, and keyboard navigation.

## Technical Details
Support hover on desktop, tap on mobile, and focus-driven inspection for accessibility. The interaction should expose the current location or in-transit state for the targeted time segment without forcing permanent labels into the Day view.

## Tests
- Integration tests for hover, tap, and focus interactions on the rail.
- Integration tests ensuring explicit location text matches the derived state.
- E2E tests covering desktop and mobile inspection flows.
