# Connected User Presence Rail

## Goal
As a calendar user, I want a thin dedicated rail for my own presence on Day view so I can compare my own timeline with shared users at a glance.

## Outcome
Day view renders a new authenticated-user rail on the left of existing shared rails, with the same presence derivation semantics and inspection behavior.

## In Scope
- Add a dedicated, thinner self rail to the left of current shared-presence rails.
- Reuse current presence derivation states (`Lieu inconnu`, `Trajet`, `Lieu`) for the authenticated user.
- Keep desktop and mobile interaction parity with existing inspection behavior.
- Keep compatibility with current shared-user overflow behavior.

## Out Of Scope
- Week and Month view presence cues.
- Backend-only authorization or admin logic.
- Date-time formatting policy.

## Dependencies
- Depends on `007 Presence Derivation`.
- Depends on `008 Day View Side Rail`.
- Depends on `009 Day View Location Inspection`.
- Must remain compatible with `010 Cue Personalization`, `012 Shared Users Readability`, and `017 Mobile Inspection Visibility And Interaction Stability`.

## Implementation Decisions
- Self rail visual hierarchy:
  - placed left of shared rails
  - thinner than shared rails
  - never hidden by shared overflow logic
- Presence semantics:
  - same derivation rules as shared users
  - same state labels and time ranges
- Interaction policy:
  - desktop: hover/focus/tap behavior matches existing inspection rules
  - mobile: tap opens bottom-sheet inspection with same interaction stability guarantees

## Acceptance Criteria
- Day view displays a dedicated self rail to the left of shared rails.
- Self rail uses the same semantic states and time windows as other rails.
- Self rail remains visible when shared rails overflow.
- Inspecting self rail segments is possible on desktop and mobile with the same interaction model as existing rails.
- Existing shared-rail behavior and cue personalization remain unchanged.

## Tests
- Unit/integration:
  - self rail layout placement and width relative to shared rails
  - self rail excluded from shared overflow count
  - self rail segment state derivation parity with existing logic
- E2E desktop:
  - self rail renders and inspection opens on hover/focus
- E2E mobile:
  - tap opens self inspection sheet and keeps interactions stable
  - backdrop closes sheet
