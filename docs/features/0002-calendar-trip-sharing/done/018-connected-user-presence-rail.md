# Connected User Presence Rail

## Goal
As a calendar user, I want a thin dedicated rail for my own presence on Day view so I can compare my own timeline with shared users at a glance.

## Outcome
Day view renders a dedicated authenticated-user rail on the left of existing shared rails, with inspection and cue editing behavior aligned with current shared-rail interactions.

## In Scope
- Add a dedicated, thinner self rail to the left of current shared-presence rails.
- Reuse current presence derivation states (`Lieu inconnu`, `Trajet`, `Lieu`) for the authenticated user.
- Render self rail in Day view when the authenticated user has at least one trip in history.
- Keep desktop and mobile interaction parity with existing inspection behavior.
- Keep compatibility with current shared-user overflow behavior.
- Keep cue editor available in self inspection with the same UI behavior as shared inspection.

## Out Of Scope
- Week and Month view presence cues.
- Backend-only authorization or admin logic.
- Date-time formatting policy.
- Migration of cue-color model to place-only keying (covered by follow-up story `020`).

## Dependencies
- Depends on `007 Presence Derivation`.
- Depends on `008 Day View Side Rail`.
- Depends on `009 Day View Location Inspection`.
- Must remain compatible with `010 Cue Personalization`, `012 Shared Users Readability`, and `017 Mobile Inspection Visibility And Interaction Stability`.
- Must remain coherent with follow-up `020 Place-Based Cue Personalization Independent Of Represented User`.

## Implementation Decisions
- Self rail visual hierarchy:
  - placed left of shared rails
  - thinner than shared rails
  - never hidden by shared overflow logic
- Self rail visibility:
  - evaluated in Day view only
  - enabled when authenticated user has at least one trip in history
- Presence semantics:
  - same derivation rules as shared users
  - same state labels and time ranges
- Interaction policy:
  - desktop: hover/focus/tap behavior matches existing inspection rules
  - mobile: tap opens bottom-sheet inspection with same interaction stability guarantees
- Cue editor policy:
  - self inspection exposes cue editor controls with the same behavior as shared inspection
  - cue-key model change remains out of scope of this story

## Acceptance Criteria
- Day view displays a dedicated self rail to the left of shared rails.
- Self rail is rendered when authenticated user has at least one trip in history, even if no shared-user rail is displayed.
- Self rail uses the same semantic states and time windows as other rails.
- Self rail remains visible when shared rails overflow.
- Inspecting self rail segments is possible on desktop and mobile with the same interaction model as existing rails.
- Self inspection exposes cue editor controls and remains usable under existing desktop/mobile inspection stability rules.
- Existing shared-rail behavior and shared overflow behavior remain unchanged.

## Tests
- Unit/integration:
  - self rail visibility gating from authenticated user's historical trip existence
  - self rail layout placement and width relative to shared rails
  - self rail excluded from shared overflow count
  - self rail segment state derivation parity with existing logic
  - self inspection includes cue editor controls and parity interactions
- E2E desktop:
  - self rail renders and inspection opens on hover/focus
  - cue editor remains usable from self inspection
- E2E mobile:
  - tap opens self inspection sheet and keeps interactions stable
  - cue editor remains usable in self inspection sheet
  - backdrop closes sheet
