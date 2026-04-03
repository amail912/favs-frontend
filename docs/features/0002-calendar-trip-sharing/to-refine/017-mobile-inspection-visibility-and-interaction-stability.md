# Mobile Inspection Visibility And Interaction Stability

## Goal
As a mobile calendar user, I want the Day view presence inspection surface to remain reachable and stable so I can consult state details and edit cue preferences reliably on small screens.

## Outcome
Mobile inspection behavior follows the same visibility and interaction stability principles as desktop, adapted to the bottom-sheet interaction model.

This pass is documentation refinement only. No code or test changes are included in this story write-up.

## In Scope
- Define visibility and reachability expectations for mobile presence inspection content.
- Define interaction stability expectations while scrolling or tapping inside the inspection surface.
- Ensure cue preference actions remain usable on mobile.

## Out Of Scope
- Desktop hover continuity mechanics (covered by `015`).
- Desktop viewport clamping behavior (covered by `016`).
- Any backend/API change.

## Dependencies
- Depends on `009 Day View Location Inspection`.
- Must remain compatible with `010 Cue Personalization`.
- Must remain coherent with `012 Shared Users Readability` overflow interactions.

## Acceptance Criteria
- Mobile inspection surface remains fully usable on small viewports.
- Presence state/time details remain readable and reachable without layout break.
- Cue preference controls remain reachable and actionable.
- Interaction inside inspection content does not trigger accidental dismissal.
- Existing tap-to-open model remains the primary mobile interaction.

## Tests
- Mobile E2E scenario:
  - open inspection from a segment near end-of-day view
  - verify content remains reachable (scrollable if needed)
- Mobile E2E scenario:
  - change cue preference from inspection
  - verify action applies correctly
- Mobile regression E2E:
  - inspection does not close unexpectedly during normal content interaction.
