# Browser Back Closes Modal Without Validating Action

## Goal
As a user, I want the browser back button to close an open modal or sheet before navigating away, and never validate the pending action.

## Outcome
When a modal surface is open, browser back closes it with cancel semantics. Validation actions are not triggered by back navigation.

## In Scope
- App modal surfaces rendered through shared modal primitives.
- Calendar modal and sheet flows.
- Admin delete confirmation modal flow.
- Browser back handling on desktop and mobile.

## Out Of Scope
- Modal visual redesign.
- Backend contract changes.

## Technical Details
- Opening a modal pushes a same-URL history entry.
- Back `popstate` closes the currently open modal via cancel behavior.
- Manual modal close consumes the modal history entry.
- Back-triggered close never dispatches validate/confirm actions.

## Acceptance Criteria
- With an open modal, pressing browser back closes the modal and keeps the user on the same page.
- Back on Calendar create/edit/delete-related modal paths cancels the modal and does not validate.
- Back on Admin delete confirmation closes the confirmation and does not send delete request.
- Closing modals with UI controls keeps history coherent (no stale modal back step).

## Tests
- E2E: Calendar create modal closes on browser back with no create request.
- E2E: Admin delete confirmation closes on browser back with no delete request.
