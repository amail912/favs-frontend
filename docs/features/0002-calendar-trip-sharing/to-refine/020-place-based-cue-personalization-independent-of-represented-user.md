# Place-Based Cue Personalization Independent Of Represented User

## Goal
As a calendar user, I want cue color preferences to be associated with places only, so the same place uses the same cue regardless of which user is represented.

## Outcome
Cue personalization no longer depends on represented username. A place gets one color preference reused across self and shared presence rails.

## In Scope
- Change cue preference keying model from user+place to place-only.
- Keep cue editor available from both self and shared inspections.
- Apply selected place color consistently across self rail and shared rails.
- Migrate existing stored cue preferences to the new place-only model with safe fallback.

## Out Of Scope
- New cue token palette.
- Backend persistence of cue preferences.
- Week and Month presence cues.

## Dependencies
- Depends on `010 Cue Personalization`.
- Must remain coherent with `018 Connected User Presence Rail`.

## Implementation Decisions
- Preference key policy:
  - one key per place id, independent of represented username
- Resolution policy:
  - if a place has a configured color, that color is applied for every represented user
  - default tone class remains fallback when no place color exists
- Migration policy:
  - read legacy preference payloads when present
  - convert to place-only shape at load time and persist normalized payload

## Acceptance Criteria
- Setting cue color for a place in one inspection is reflected for the same place in all represented users.
- Self rail and shared rails resolve the same place color consistently.
- Existing users with legacy cue preferences keep a valid, non-breaking experience after migration.
- Cue editor interactions remain unchanged on desktop and mobile.

## Tests
- Unit/integration:
  - preference lookup ignores represented username and keys by place only
  - migration from legacy user+place preferences to place-only preferences
  - fallback to default tone when place preference missing
- E2E:
  - set color for a place from one user inspection and verify same color on another user for same place
  - verify same color consistency between self rail and shared rail for same place
