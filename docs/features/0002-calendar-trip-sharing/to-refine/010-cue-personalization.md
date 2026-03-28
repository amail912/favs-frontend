# Cue Personalization

## Goal
As a calendar owner, I want to personalize shared presence cues so I can recognize each person and place combination quickly.

## How This Story Achieves The Goal
The owner should be able to configure cue colors by shared person and by place. This lets the visual language stay meaningful to the owner without changing the shared trip data itself.

## Technical Details
Store personalization as owner-scoped preferences rather than trip metadata. The rendering logic should resolve colors from the current shared user and derived place for each visible segment.

## Tests
- Unit tests for preference resolution by user and place.
- Integration tests ensuring configured colors appear in Day view cues.
- E2E tests for changing a cue preference and seeing the Day view update.
