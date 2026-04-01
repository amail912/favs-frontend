# Cue Personalization

## Goal
As a calendar owner, I want to personalize shared presence cues so I can recognize each person and place combination quickly.

## Outcome
After the base Day view cues are stable, the owner can configure cue colors in a way that remains local to their account and does not alter shared trip data.

## In Scope
- Define owner-scoped cue preferences keyed by shared user and place.
- Resolve display colors from those preferences when rendering Day view presence cues.
- Keep personalization separate from the trip-sharing backend contract.

## Out Of Scope
- Base shared presence support.
- Share and subscription management.

## Dependencies
- Depends on `008 Day View Side Rail`.
- Depends on `009 Day View Location Inspection` if the chosen settings UI lives in the same Day view feature area.
- Should be delivered after the base cue language is stable.

## Acceptance Criteria
- A preference can change the cue color for a given shared user and place combination.
- Missing preferences fall back to the default cue language.
- Personalization changes do not mutate shared trip payloads or sharing rules.

## Tests
- Unit tests for preference resolution by user and place.
- Integration tests ensuring configured colors appear in Day view cues.
- E2E tests for changing a cue preference and seeing the Day view update.
