# Viewport-Safe Stable Presence Panel

## Goal
As a calendar user on desktop, I want the Day view presence inspection panel to remain visible in the viewport even when I am deep in the timeline, while keeping panel motion stable and predictable.

## Outcome
Inspection panel placement becomes viewport-aware: always visible, mostly stable, and only repositioned when approaching viewport boundaries.

This pass is documentation refinement only. No code or test changes are included in this story write-up.

## In Scope
- Define viewport visibility requirements for desktop inspection panel placement.
- Define stability expectations to avoid continuous movement on mouse move or scroll.
- Define boundary-driven repositioning behavior near viewport edges.

## Out Of Scope
- Hover continuity behavior between segment and panel (covered by `015`).
- Mobile bottom-sheet behavior (covered by `017`).
- Any backend/API change.

## Dependencies
- Depends on `008 Day View Side Rail`.
- Depends on `009 Day View Location Inspection`.
- Should stay consistent with `012 Shared Users Readability` and `010 Cue Personalization`.

## Acceptance Criteria
- Inspection panel stays visible within viewport bounds when opened from any visible rail segment.
- Panel position stays stable for a given inspected segment.
- Panel does not continuously track pointer movement.
- Panel repositions only when needed to avoid clipping near top or bottom viewport limits.
- Visual anchoring remains understandable when clamped near boundaries.

## Tests
- Unit/integration tests for placement/clamping behavior:
  - centered segment
  - near-top segment
  - near-bottom segment
- Desktop E2E scenario:
  - scroll deep in Day view
  - hover segment
  - panel remains visible in viewport
- Desktop E2E regression:
  - panel does not jitter during normal hover interactions.
