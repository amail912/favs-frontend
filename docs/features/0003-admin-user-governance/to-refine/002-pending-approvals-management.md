# Pending Approvals Management

## Goal
As an admin, I want to process pending account approvals quickly so legitimate users can sign in without manual backend operations.

## Outcome
Admin page shows pending accounts and supports two actions: approve account and delete pending account.

## In Scope
- Pending accounts list UI with loading/empty/error states.
- Approve action per pending account.
- Delete action per pending account.
- Immediate UI refresh after action completion.

## Out Of Scope
- Editing account profile details.
- Bulk actions in this iteration.
- Notification system.

## Dependencies
- Depends on `001 Admin Role Gating And Route Access`.
- Depends on backend endpoints for pending list, approve, and delete.

## Implementation Decisions
- Processing model:
  - per-row actions with explicit pending state
  - prevent double-submit while one row action is running
- Feedback:
  - success and failure feedback at row/page level
  - deterministic refresh of pending list after each action

## Acceptance Criteria
- Admin sees list of pending accounts with clear action controls.
- Approve action removes account from pending list after success.
- Delete action removes account from pending list after success.
- Errors are visible and retryable without page reload.
- Non-admin users cannot execute these actions.

## Tests
- Integration:
  - pending list state transitions (loading, empty, error, loaded)
  - approve/delete row state handling
- E2E:
  - admin approves one pending account and sees list update
  - admin deletes one pending account and sees list update
  - API error path shows actionable feedback
