# Desktop Custom Date-Time Picker

## Goal
As a desktop user, I want to enter date-time values quickly and reliably without relying on weak native desktop picker ergonomics.

## Outcome
A reusable desktop custom date-time input component is available and adopted in form flows requiring date-time entry. Mobile continues to use native inputs.

## In Scope
- Design and implement one reusable desktop custom date-time picker component.
- Keep mobile input behavior native.
- Ensure compatibility with existing validation and submit payload formats.

## Out Of Scope
- Full calendar redesign.
- Timezone conversion policy changes.
- Backend contract changes for date-time write format.

## Dependencies
- Depends on `002 Shared French Date-Time Display Policy`.
- Must remain coherent with current ISO-local payload expectations.

## Implementation Decisions
- Platform behavior:
  - desktop uses custom picker component
  - mobile keeps native `datetime-local` interaction
- Output behavior:
  - component emits values compatible with existing backend payload contract

## Acceptance Criteria
- Desktop users can set date and time with fewer interaction steps than current native desktop flow.
- Mobile users keep native date-time input behavior.
- Form validation and submission behavior remain functionally equivalent.
- Component is reusable and not calendar-domain specific.

## Tests
- Unit/integration:
  - component input/output mapping and validation interaction
  - desktop/mobile behavior switching
- E2E:
  - desktop create/edit flows using custom picker
  - mobile create/edit flows still using native behavior
