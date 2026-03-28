# Unknown Location States

## Goal
As a calendar viewer, I want the application to show when location cannot yet be inferred so I do not mistake missing data for a confirmed place.

## How This Story Achieves The Goal
The shared presence model should distinguish explicit known places from unknown states. This prevents the rail and inspection UI from over-claiming certainty when no prior trip exists.

## Technical Details
If no seed trip exists for a user and no earlier visible trip establishes a location, presence should remain unknown until the first usable trip context appears. The Day view should render a clear but non-intrusive unknown state.

## Tests
- Unit tests for derivation with no prior trip context.
- Integration tests for unknown state rendering in the Day view.
- E2E tests covering a followed user with no derivable location at period start.
