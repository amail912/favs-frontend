# Desktop Custom Date-Time Picker

## Goal
As a desktop user, I want faster and clearer date-time entry than the current native desktop `datetime-local` experience, while mobile keeps its native behavior.

## Outcome
A reusable desktop-first date-time input component replaces the current desktop `datetime-local` fields in calendar create/edit flows, using separate synchronized date and time controls while preserving the existing ISO-local value contract.

## In Scope
- Implement one reusable desktop custom date-time picker component.
- Use the component on desktop in the current calendar create/edit modals.
- Keep mobile and touch contexts on native input behavior.
- Preserve compatibility with existing validation and submit payload formats.

## Out Of Scope
- Full calendar redesign.
- New timezone behavior.
- Backend contract changes for date-time writes.
- Adoption outside the current calendar create/edit modal flows.

## Dependencies
- Depends on `002 Shared French Date-Time Display Policy`.
- Must remain coherent with the existing ISO-local payload contract `YYYY-MM-DDTHH:mm`.
- Must integrate with the current Calendar draft/edit state without changing domain validation rules.

## Implementation Decisions
- Adoption scope:
  - only replace the two current `datetime-local` usages in calendar create/edit flows
  - no broader app-wide adoption in this story
- Desktop component shape:
  - component renders one date field and one time field
  - both fields stay synchronized with one combined local ISO datetime value
- Platform behavior:
  - desktop uses the custom split component
  - mobile/touch keeps native `datetime-local`
- Value contract:
  - component input is the same local ISO datetime string already used today
  - component output is the same local ISO datetime string `YYYY-MM-DDTHH:mm`
  - no new conversion layer is introduced between picker output and existing form state
- Update behavior:
  - changing date or time emits a complete recombined ISO-local datetime string
  - invalid or incomplete intermediate input must remain recoverable without crashing the form
- Reusability boundary:
  - component is shared UI, not a Calendar-specific inline helper
  - Calendar remains responsible for business validation and submit behavior

## Acceptance Criteria
- Desktop calendar create flow uses the custom split date-time component instead of native `datetime-local`.
- Desktop calendar edit flow uses the custom split date-time component instead of native `datetime-local`.
- Mobile/touch contexts continue using native input behavior.
- Changing either the date or time part updates the combined ISO-local value used by the form.
- Existing validation and submit behavior remain functionally equivalent.
- Component is reusable outside Calendar even if only Calendar adopts it in this story.

## Tests
- Unit/integration:
  - initializes correctly from a valid ISO-local datetime string
  - recombines date + time into `YYYY-MM-DDTHH:mm`
  - handles invalid or incomplete input without runtime failure
  - desktop/mobile behavior switching selects the correct control family
- Integration:
  - calendar create/edit drafts still receive ISO-local datetime strings after picker interaction
  - existing validation messages still trigger against the same underlying data contract
- E2E:
  - desktop create flow uses the custom split controls successfully
  - desktop edit flow uses the custom split controls successfully
  - mobile create/edit flows keep native inputs
