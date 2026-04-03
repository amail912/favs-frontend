# Viewport-Safe Stable Presence Panel

## Goal
As a calendar user on desktop, I want the Day view presence inspection panel to remain visible in the viewport even when I am deep in the timeline, while keeping panel motion stable and predictable.

## Outcome
Inspection panel placement is viewport-aware: visible by default, stable while interacting, and repositioned only on boundary pressure.

This pass is documentation refinement only. No code or test changes are included in this story write-up.

## In Scope
- Desktop Day view inspection panel placement.
- Deterministic and stable open-time clamp strategy.
- Boundary-triggered re-clamp rules.
- Reusable clamp helper contract (isolated behavior).

## Out Of Scope
- Hover continuity behavior between segment and panel (covered by `015`).
- Mobile bottom-sheet behavior (covered by `017`).
- Any backend/API change.

## Dependencies
- Depends on `008 Day View Side Rail`.
- Depends on `009 Day View Location Inspection`.
- Should stay consistent with `012 Shared Users Readability` and `010 Cue Personalization`.

## Implementation Decisions
- Positioning strategy: **open-time clamp**.
- Anchor definition: segment midpoint in viewport coordinates.
- Desired position: panel vertically centered on anchor.
- Clamp bounds:
  - `minTop = viewportTop + safePaddingPx`
  - `maxTop = viewportBottom - safePaddingPx - panelHeightPx`
- Re-clamp triggers:
  - inspection open or inspected segment change
  - viewport resize/orientation change
  - scroll only when panel crosses a clamp boundary with hysteresis
- Stability rules:
  - no recompute on mouse move
  - no continuous tracking while scrolling in safe zone
  - one snap when crossing boundary threshold

## Reusable Clamp Isolation (Required)
- Clamp behavior must be isolated in a dedicated pure helper, not spread in render/update branches.
- Helper input is generic geometry, not calendar-specific state:
  - `anchorY`, `panelHeight`, `viewportTop`, `viewportBottom`, `safePadding`, `hysteresis`
- Helper output is reusable placement metadata:
  - `panelTop`
  - `clampMode` (`None | TopClamped | BottomClamped`)
  - `anchorOffset` (for pointer/arrow visual anchoring)
- All desktop inspection call paths must consume this helper; no duplicate clamp math is allowed.

## Acceptance Criteria
- Opening inspection from any visible rail segment keeps the panel fully visible in viewport.
- Panel stays position-stable for the same inspected segment while pointer moves.
- Panel does not continuously move during normal scroll interactions.
- Panel repositions only when crossing top/bottom viewport safety boundaries.
- When clamped, visual anchoring remains understandable.
- Clamp behavior is implemented via one reusable helper API consumed by all relevant call paths.

## Tests
- Unit tests for isolated clamp helper:
  - centered anchor => no clamp
  - near-top anchor => top clamp
  - near-bottom anchor => bottom clamp
  - oversized panel => bounded visible placement
  - hysteresis edge behavior => no oscillation around threshold
- Desktop E2E:
  - deep timeline hover opens inspection fully visible
  - small scroll in safe zone keeps panel stable
  - boundary-crossing scroll re-clamps once and remains visible
  - cue editor remains usable after clamping

## Defaults
- `safePaddingPx = 12`
- `hysteresisPx = 8`
