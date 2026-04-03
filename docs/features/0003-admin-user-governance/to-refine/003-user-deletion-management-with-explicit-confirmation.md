# User Deletion Management With Explicit Confirmation

## Goal
As an admin, I need to delete user accounts when required, with strong confirmation safeguards to avoid accidental destructive actions.

## Outcome
Admin page provides a user list with delete action guarded by explicit confirmation.

## In Scope
- List existing users in admin page context.
- Per-user delete action.
- Explicit confirmation step before deletion.
- UI recovery on cancellation and API failure.

## Out Of Scope
- User editing or role assignment.
- Soft-delete lifecycle policy.
- Audit dashboard UX.

## Dependencies
- Depends on `001 Admin Role Gating And Route Access`.
- Depends on backend user-list and delete endpoints.

## Implementation Decisions
- Safety policy:
  - deletion always requires explicit confirmation
  - destructive action remains disabled until confirmation is complete
- Feedback:
  - deleted user disappears from list on success
  - failure leaves row visible with clear error feedback

## Acceptance Criteria
- Admin can open user list and trigger delete flow on a user.
- Delete cannot proceed without explicit confirmation.
- Successful deletion updates list immediately.
- Canceling confirmation leaves data unchanged.
- Non-admin users cannot access deletion UI.

## Tests
- Integration:
  - confirmation gating logic
  - row state transitions during deletion
- E2E:
  - admin confirms deletion and sees user removed
  - admin cancels deletion and user remains
  - delete API failure shows recoverable error state
