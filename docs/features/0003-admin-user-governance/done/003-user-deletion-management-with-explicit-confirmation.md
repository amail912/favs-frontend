# User Deletion Management With Explicit Confirmation

## Goal
As an admin, I want to manage existing approved users from the admin page and delete an account only after an explicit destructive-action confirmation.

## Outcome
The admin page section `Utilisateurs existants` displays approved users from the backend and supports one per-row `Supprimer` flow guarded by a confirmation modal.

## In Scope
- Replace the approved-users placeholder in the admin page with a live section.
- Load approved users from the backend when the admin page is opened.
- Render the section with explicit `loading`, `empty`, `error`, and `loaded` states.
- Display one row per approved user using the backend authenticated-profile payload.
- Support one per-row delete action with explicit confirmation.
- Refresh the approved-users list deterministically after each successful deletion.
- Keep `Comptes en attente` behavior unchanged from `002`.

## Out Of Scope
- Pending approval actions already covered by `002`.
- Editing user profile details.
- Role assignment or role-management UX.
- Soft-delete lifecycle policy.
- Bulk deletion actions.
- Audit or activity-history UX.

## Dependencies
- Depends on `001 Admin Role Gating And Route Access`.
- Depends on `002 Pending Approvals Management`.
- Depends on the existing admin backend contract already available in `../foucl2`.

## Backend Contract
- `GET /api/v1/admin/users`
  - returns a JSON array of authenticated-profile objects
  - payload shape per item:
    - `{ "username": String, "roles": Array String, "approved": Boolean }`
- `DELETE /api/v1/admin/users/:username`
  - deletes the approved user matching the path username
  - returns `200` with empty response on success
- Relevant backend error responses already implemented:
  - `401` with message `Not authenticated`
  - `403` with message `Admin privileges required`
  - `404` with message `Not found`
  - `409` with message `Cannot delete bootstrap admin`
  - `409` with message `Cannot delete your own account`
  - `500` with message `Unable to process authentication`

## Implementation Decisions
- Rendering scope:
  - the admin page keeps the title and section structure introduced in `001`
  - only the `Utilisateurs existants` section becomes interactive in this story
- Approved-users list rendering:
  - render one row per approved user keyed by `username`
  - each row shows at minimum the username and a `Supprimer` action
  - role badges or extra metadata are not required in this story
- Processing model:
  - only one delete flow may be in progress at a time
  - while one delete flow is active, duplicate submission is prevented
  - other rows remain visible; the implementation may disable all delete buttons globally if that is the simplest way to guarantee single-flight behavior
- Confirmation policy:
  - clicking `Supprimer` opens a confirmation modal
  - the modal shows clear destructive-action copy naming the target username
  - the modal has two actions: cancel and final `Supprimer`
  - deletion does not start before the final confirmation click
  - cancel closes the modal and leaves the list unchanged
- Protected-user policy:
  - the current authenticated admin account is visible in the list but its delete button is disabled with explanatory text
  - the backend contract does not expose which user is the bootstrap admin, so this story does not require pre-disabling that row in the frontend
  - if the admin attempts to delete the bootstrap admin, the backend `409` message must be surfaced as recoverable feedback
- Refresh policy:
  - after a successful delete action, the frontend refetches the approved-users list from `GET /api/v1/admin/users`
  - the row is considered successfully removed only after the refetch completes
- Feedback policy:
  - successful deletion is reflected implicitly by the row disappearing from the refreshed list
  - section-level loading error must display a visible retry control
  - delete failure must display actionable feedback without requiring a page reload
  - section-level feedback is acceptable for delete failures; row-level feedback is optional
- Module boundaries:
  - `Api.*` remains purely technical and owns payload types and JSON encoding/decoding
  - admin UI state, confirmation flow, and action decisions remain in the admin page module

## Acceptance Criteria
- Admin opening `/admin` sees the `Utilisateurs existants` section load approved users from the backend.
- When approved users exist, admin sees one row per username with a `Supprimer` control.
- When no approved users exist, the section shows an explicit empty state.
- If approved-users loading fails, the section shows a visible error state with a retry action.
- Clicking `Supprimer` on a deletable user opens an explicit confirmation modal.
- Canceling the confirmation modal closes it and leaves the list unchanged.
- Confirming the deletion removes the user from the section after a successful refresh.
- The current authenticated admin account is visible but cannot be deleted from the UI.
- If deletion fails with a backend conflict or technical error, the admin remains on the page, the list remains coherent, and the error is visible and retryable.
- This story does not change pending-signup behavior and does not add role-management controls.

## Tests
- Integration:
  - approved-users decoding uses `{ username, roles, approved }` payloads
  - approved-users section transitions through `loading`, `empty`, `error`, and `loaded`
  - clicking a delete button opens the confirmation modal for the matching username
  - cancel from the confirmation modal closes it without issuing a delete request
  - confirm from the modal triggers delete and refetch on success
  - the current authenticated admin row renders as non-deletable
  - delete failure with `409` leaves the row visible and exposes recoverable feedback
  - retry from initial load error refetches the approved-users list
- E2E:
  - admin opens `/admin`, sees approved users, opens delete confirmation, cancels, and the user remains
  - admin opens delete confirmation for an approved user, confirms, and sees the user disappear
  - current admin account is visible and not deletable from the UI
  - backend error during list load or delete shows actionable feedback without page reload
