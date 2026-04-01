# Shared Users Readability

## Goal
As a calendar owner, I want shared presence to remain readable when several users are visible so the Day view stays useful instead of becoming a color puzzle.

## Outcome
The Day view presence design gets an explicit supported-user target and a defined fallback once that target is exceeded, so the feature remains predictable instead of degrading accidentally.

## In Scope
- Define the supported simultaneous-user target for the Day view rail.
- Design the rail and inspection affordances for that supported target.
- Define fallback behavior beyond the supported target instead of leaving it implicit.
- Keep the fallback compatible with both desktop and mobile layouts.

## Out Of Scope
- Personalization rules for colors.
- Presence derivation itself.

## Dependencies
- Depends on `008 Day View Side Rail`.
- Depends on `009 Day View Location Inspection` when the fallback changes how details are accessed.

## Acceptance Criteria
- The docs freeze a supported target of about three simultaneously visible shared users unless implementation constraints justify a stricter limit.
- The Day view remains understandable at the supported target.
- Behavior beyond the target is explicit, not accidental.

## Tests
- Integration tests for the supported small-multi-user layout.
- Integration tests for fallback behavior beyond the readability target.
- E2E tests verifying the Day view remains understandable with multiple visible shared users.
