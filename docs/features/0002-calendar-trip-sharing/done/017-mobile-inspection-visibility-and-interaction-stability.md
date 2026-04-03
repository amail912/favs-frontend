# Mobile Inspection Visibility And Interaction Stability

## Goal
As a mobile calendar user, I want the Day view presence inspection surface to remain reachable and stable so I can consult state details and edit cue preferences reliably on small screens.

## Outcome
Mobile inspection behavior follows the same visibility and interaction stability principles as desktop, adapted to the bottom-sheet interaction model.

This pass is documentation refinement only. No code or test changes are included in this story write-up.

## In Scope
- Mobile Day view inspection as a bottom-sheet-first interaction.
- Viewport-safe sheet sizing with internal content scroll.
- Stable interaction rules while tapping or scrolling inside the sheet.
- Cue preference controls remaining reachable and actionable on small screens.

## Out Of Scope
- Desktop hover continuity mechanics (covered by `015`).
- Desktop viewport clamping behavior (covered by `016`).
- Any backend/API change.

## Dependencies
- Depends on `009 Day View Location Inspection`.
- Must remain compatible with `010 Cue Personalization`.
- Must remain coherent with `012 Shared Users Readability` overflow interactions.

## Implementation Decisions
- Mobile close policy: backdrop tap closes the sheet.
- Interaction safety: taps and scrolls inside `.app-bottom-sheet__dialog` never close the sheet.
- Scrolling policy: internal sheet scroll, not page-level scroll.
- Header stability: title and close button remain visible while body content scrolls.
- Existing tap-to-open from rail segment remains the primary mobile entry path.

## Acceptance Criteria
- Mobile inspection opens as a bottom sheet from segment tap.
- Sheet stays fully usable on small viewports with constrained height and internal scroll.
- Presence state/time details remain readable and reachable without layout break.
- Cue preference controls remain reachable and actionable even when content is long.
- Interaction inside sheet content does not dismiss the inspection.
- Backdrop tap dismisses the inspection.

## Tests
- Mobile E2E scenario:
  - open inspection from a segment near end-of-day view
  - verify sheet is visible and content is reachable through internal sheet scroll
- Mobile E2E scenario:
  - change cue preference from inspection after scrolling in sheet
  - verify action applies correctly on rail segment
- Mobile E2E scenario:
  - tap inside sheet content and header controls
  - verify sheet stays open
- Mobile E2E scenario:
  - tap backdrop
  - verify sheet closes
- Mobile regression E2E:
  - tap-to-open flow and mobile overflow sheet flow remain unchanged.
