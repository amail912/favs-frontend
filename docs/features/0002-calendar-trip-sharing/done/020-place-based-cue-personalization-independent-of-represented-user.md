# Place-Based Cue Personalization Independent Of Represented User

## Goal
As a calendar user, I want cue color preferences to be associated with places only, so the same place uses the same cue regardless of which user is represented.

## Outcome
Cue personalization no longer depends on represented username. A place gets one color preference reused across self and shared presence rails.

## In Scope
- Change cue preference keying model from user+place to place-only.
- Keep cue editor available from both self and shared inspections.
- Apply selected place color consistently across self rail and shared rails.
- Replace preference JSON shape with place-only entries.
- Stop supporting legacy user+place preference payloads.

## Out Of Scope
- New cue token palette.
- Backend persistence of cue preferences.
- Week and Month presence cues.
- Legacy preference migration.

## Dependencies
- Depends on `010 Cue Personalization`.
- Must remain coherent with `018 Connected User Presence Rail`.

## Implementation Decisions
- Preference key policy:
  - one key per place id, independent of represented username
- Resolution policy:
  - if a place has a configured color, that color is applied for every represented user
  - default tone class remains fallback when no place color exists
- Storage policy:
  - keep owner-scoped local storage key (`favs.calendar.presence-cues.<owner>`)
  - store only place-based preference entries in that owner payload
- Legacy policy:
  - do not read legacy user+place entries
  - do not attempt migration or compatibility rewrite

## Acceptance Criteria
- Setting cue color for a place in one inspection is reflected for the same place in all represented users.
- Self rail and shared rails resolve the same place color consistently.
- Cue editor interactions remain unchanged on desktop and mobile.
- If no place preference exists, tone fallback remains unchanged.
- Legacy preference payloads are ignored by the new decoder and do not affect rendered colors.

## Tests
- Unit/integration:
  - preference lookup ignores represented username and keys by place only
  - JSON encode/decode for place-only preference payloads
  - fallback to default tone when place preference missing
  - transit and unknown segments remain on default tone behavior
  - legacy payload shape does not produce applied place color
- E2E:
  - set color for a place from one user inspection and verify same color on another user for same place
  - verify same color consistency between self rail and shared rail for same place
  - verify configured place color persists after reload with place-only payload
