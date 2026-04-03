# Desktop Hover Continuity For Presence Inspection

## Goal
As a calendar user on desktop, I want the Day view presence inspection to stay open when moving from the rail segment to the inspection panel so I can read details and change cue preferences without fighting hover flicker.

## Outcome
Presence inspection becomes reliably interactive in desktop hover mode. Moving pointer from rail segment to panel no longer closes the panel prematurely.

## In Scope
- Define desktop hover continuity behavior between rail segment and inspection panel.
- Define close behavior when pointer leaves the interaction zone.
- Keep cue preference controls usable in hover mode.

## Out Of Scope
- Mobile bottom-sheet interaction behavior.
- Presence derivation logic and rail segment generation.
- Any backend/API change.
- Viewport clamping and boundary placement strategy (covered by `016`).

## Dependencies
- Depends on `008 Day View Side Rail`.
- Depends on `009 Day View Location Inspection`.
- Must remain compatible with `010 Cue Personalization`.

## Acceptance Criteria
- Hovering a rail segment opens the inspection panel as today.
- Moving pointer from segment to panel keeps the panel visible.
- Cue preference actions in the panel are clickable in hover mode.
- Panel closes only after pointer leaves both segment and panel interaction zones.
- Existing desktop click-to-pin behavior remains available.

## Tests
- Desktop E2E scenario:
  - hover rail segment
  - move pointer into inspection panel
  - click a cue preference option successfully
- Desktop E2E scenario:
  - leave both segment and panel
  - inspection closes
- Regression E2E:
  - click-to-pin behavior still works.
