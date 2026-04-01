# Unknown Location States

## Goal
As a calendar viewer, I want the application to show when location cannot yet be inferred so I do not mistake missing data for a confirmed place.

## Outcome
Unknown location becomes an explicit derived and rendered state. The UI does not silently imply a place when the current period lacks enough visible trip history.

## In Scope
- Keep `unknown` as a first-class presence state in the derivation output.
- Render that state distinctly in Day view.
- Make inspection text explicit when location is unknown rather than omitting content.
- Preserve the transition from unknown to known or in-transit once trips establish context.

## Out Of Scope
- The entire derivation algorithm.
- General rail layout.

## Dependencies
- Depends on `007 Presence Derivation`.
- Refines `008 Day View Side Rail` and `009 Day View Location Inspection`.

## Acceptance Criteria
- A user with no prior visible trip context is rendered as unknown at period start.
- Unknown remains visible until the first applicable trip changes the state.
- Inspection text does not confuse unknown with in-transit or at-place states.

## Tests
- Unit tests for derivation with no prior trip context.
- Integration tests for unknown state rendering in the Day view.
- Integration tests for inspection text when location is unknown.
- E2E tests covering a followed user with no derivable location at period start.
