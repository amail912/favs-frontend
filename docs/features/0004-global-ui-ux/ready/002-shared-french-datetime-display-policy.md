# Shared French Date-Time Display Policy

## Goal
As a user, I want all visible dates and times to follow one French rendering policy so reading scheduling information is immediate and predictable across the app.

## Outcome
Frontend uses one shared display policy for visible date/time labels, with reusable helpers that render French-friendly text while keeping existing ISO-local formats for parsing, validation, and submit payloads.

## In Scope
- Define reusable display helpers for visible date/time rendering.
- Standardize visible rendering for:
  - full date-time labels
  - date-only labels
  - time-only labels
- Apply the policy to authenticated and unauthenticated UI surfaces when they display user-facing date/time text.
- Keep invalid raw values safely renderable without runtime crashes.

## Out Of Scope
- Desktop picker interaction behavior covered by `003`.
- Native or custom input fields used for editing.
- Backend storage or payload formats.
- CSV, ICS, or other technical export formats.
- Localization to other languages.

## Dependencies
- Depends on the current helper layer in `Helpers.DateTime`.
- Must remain compatible with existing ISO-local parser/validator contracts already used by Calendar and import/export flows.

## Implementation Decisions
- Separation of responsibilities:
  - existing ISO-local helpers keep serving parse, validation, storage, and input compatibility
  - new shared display helpers serve only user-visible rendering
- Reference display policy:
  - full date-time labels use `dd/MM/yyyy HH:mm`
  - date-only labels use `dd/MM/yyyy`
  - time-only labels use `HH:mm`
- Fallback policy:
  - if a raw value cannot be parsed for display, the helper returns the original raw string
  - helpers must never crash on invalid input
- Adoption policy for this story:
  - cover visible date/time labels already rendered in UI text
  - do not replace `datetime-local`, `date`, or `time` input values with display-formatted strings
- Module boundary:
  - shared formatting lives in `Helpers.DateTime` or an adjacent shared helper module, not inside `Pages.Calendar`

## Acceptance Criteria
- Covered UI date/time labels use one French display policy instead of raw ISO-local strings.
- Full datetime labels render as `dd/MM/yyyy HH:mm`.
- Date-only labels render as `dd/MM/yyyy`.
- Time-only labels render as `HH:mm`.
- Invalid display inputs remain visible without crashing and fall back to the raw value.
- Input fields and submit payloads continue to use their existing ISO-local formats.

## Tests
- Unit:
  - full datetime formatting for representative valid values
  - date-only formatting for representative valid values
  - time-only formatting for representative valid values
  - raw fallback for invalid values
- Integration:
  - visible calendar labels covered by the story no longer show raw ISO-local datetime strings
  - helpers remain distinct from input-format helpers used by validation and submit logic
- E2E:
  - covered visible calendar labels render in French-friendly format instead of `YYYY-MM-DDTHH:mm`
