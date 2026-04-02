# Day View Side Rail

## Goal
As a calendar owner, I want an easy-to-spot presence cue in Day view so I can understand where shared users are without the cue competing with agenda items.

## Outcome
Day view gains a compact rendering surface for derived shared presence. The rail is intentionally separate from agenda cards so owner tasks and shared-presence cues remain legible together.

## In Scope
- Render derived presence on the Day view timeline as a compact rail aligned to time.
- Keep the rail visually separate from agenda cards and drag interactions.
- Support both desktop and mobile layouts.
- Render known-place, in-transit, and unknown segments using the derived state from `007`.

## Out Of Scope
- Exact inspection interaction details.
- Personalization rules.
- Behavior beyond the supported simultaneous-user readability target.

## Dependencies
- Depends on `007 Presence Derivation`.
- `011` and `012` refine this story's rendering states and multi-user limits.

## Acceptance Criteria
- The rail appears only when shared presence data is available for the selected day.
- Time alignment matches the derived segments.
- The agenda remains readable and interactive with the rail present.
- Mobile and desktop layouts both keep the rail understandable.

## Tests
- Integration tests for rail rendering from derived presence segments.
- Integration tests for known-place, in-transit, and unknown visual states.
- E2E tests verifying the rail appears and updates with shared trip data.
