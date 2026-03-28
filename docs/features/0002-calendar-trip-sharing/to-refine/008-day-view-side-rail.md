# Day View Side Rail

## Goal
As a calendar owner, I want an easy-to-spot presence cue in Day view so I can understand where shared users are without the cue competing with agenda items.

## How This Story Achieves The Goal
The Day view should render a compact side rail that communicates derived presence over time. The cue should stay identifiable while preserving the current readability of timeline cards and drag interactions.

## Technical Details
Use the Day view timeline structure as the main rendering surface and add a thin segmented rail aligned to time. The rail should support multiple visible users while keeping the agenda grid readable on desktop and mobile.

## Tests
- Integration tests for rail rendering from derived presence segments.
- Integration tests for multiple visible users within the supported readability target.
- E2E tests verifying the rail appears and updates with shared trip data.
