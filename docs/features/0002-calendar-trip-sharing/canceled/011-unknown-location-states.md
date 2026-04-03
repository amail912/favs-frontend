# Unknown Location States

## Goal
As a calendar viewer, I want the application to show when location cannot yet be inferred so I do not mistake missing data for a confirmed place.

## Cancellation Reason
This refinement was absorbed by already-delivered stories instead of shipping as a standalone increment.

The intended behavior is already covered by:
- `007 Presence Derivation`, which derives `unknown` when no prior visible trip establishes a location.
- `008 Day View Side Rail`, which renders an explicit unknown segment in Day view.
- `009 Day View Location Inspection`, which exposes explicit inspection text for the unknown state.

## Outcome
The backlog does not keep `011` as a separate delivery item because it no longer represents remaining implementation work.

## Notes
- This cancellation does not reject the behavior.
- It records that the behavior shipped through adjacent stories rather than through a dedicated follow-up story.
