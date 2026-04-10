# Desktop Day View Timeline Time Alignment

## Goal
As a desktop calendar user, I want Day view card position to match item start and end times so time interpretation is accurate.

## How This Story Achieves The Goal
It removes the desktop Day timeline drift so item cards map exactly to their start and end times. Users can then trust the vertical position of cards for quick time reading.

## Technical Details
- Surface: desktop Day timeline card positioning.
- Mapping rule: card top aligns to exact start-minute position.
- Mapping rule: card height aligns to exact duration minutes.
- Regression boundary: keep overlap handling and interaction affordances stable.
- Platform boundary: apply fix to desktop path; mobile Day behavior remains unchanged in this story.

## Tests
- Unit tests for minute-index and duration mapping helpers used by desktop positioning.
- Integration tests for desktop card top/height computed from exact start/end times.
- Integration tests ensuring mobile rendering path remains unchanged for the same fixture data.
- E2E test on desktop with known `16:00` to `19:00` fixture validating grid alignment.
