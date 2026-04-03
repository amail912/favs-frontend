# Shared French Date-Time Display Policy

## Goal
As a user, I want consistent French date-time formatting everywhere so reading dates and times is immediate and predictable.

## Outcome
Frontend uses one reusable display policy for date-time rendering with the reference format `dd/MM/yyyy HH:mm`.

## In Scope
- Define reusable date-time display helpers/components.
- Apply policy to both authenticated and unauthenticated pages.
- Ensure consistent rendering for pure text displays and non-editable labels.

## Out Of Scope
- Desktop picker interaction behavior.
- Backend storage format changes.
- Localization to other languages.

## Dependencies
- Depends on current date/time utility layer.
- Should remain compatible with existing parser/validator contracts.

## Implementation Decisions
- Display policy:
  - reference format `dd/MM/yyyy HH:mm`
  - one reusable helper/component API used across domains
- Fallback behavior:
  - invalid/unknown raw values remain safely renderable without runtime crashes

## Acceptance Criteria
- Date-time labels across app use the same French display policy.
- No mixed legacy display formats remain on covered surfaces.
- UIs without auth (signup/signin/not-found) follow the same policy when date-time is present.

## Tests
- Unit:
  - format output for representative values
  - invalid input fallback behavior
- Integration/E2E:
  - sample pages show consistent formatted date-time output
