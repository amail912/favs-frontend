# Day View Location Inspection

## Goal
As a calendar viewer, I want to inspect the exact location behind the presence cue so I can read explicit place names when I need them.

## Outcome
The rail stays compact by default, but the viewer can reveal explicit derived location details when needed without turning the baseline Day view into a label-heavy layout.

## In Scope
- Support hover inspection on desktop.
- Support tap inspection on mobile.
- Support focus-driven inspection for keyboard and accessibility use.
- Reveal the exact place name or in-transit state for the targeted segment.
- Keep inspection text sourced from the already-derived presence state rather than from raw trip parsing in the view layer.

## Out Of Scope
- The base rail rendering without interaction.
- Cue personalization.

## Dependencies
- Depends on `007 Presence Derivation` and `008 Day View Side Rail`.

## Acceptance Criteria
- Hover, tap, and focus reveal a readable inspection state for the targeted segment.
- The inspection content matches the derived state at that time.
- Inspection does not permanently expand every segment label in the Day view.

## Tests
- Integration tests for hover, tap, and focus interactions on the rail.
- Integration tests ensuring explicit location text matches known-place, in-transit, and unknown derived states.
- E2E tests covering desktop and mobile inspection flows.
