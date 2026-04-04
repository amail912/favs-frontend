# Pending Approvals Management

## Goal
As an admin, I want to process pending account approvals quickly so legitimate users can sign in without manual backend operations.

## Outcome
The admin page section `Comptes en attente` displays pending accounts and supports two per-row actions: approve the account or delete the pending account.

## In Scope
- Replace the pending-accounts placeholder in the admin page with a live section.
- Load pending accounts from the backend when the admin page is opened.
- Render the section with explicit `loading`, `empty`, `error`, and `loaded` states.
- Support one per-row `Approuver` action.
- Support one per-row `Supprimer` action for pending accounts.
- Refresh the pending list deterministically after each successful action.
- Keep the `Utilisateurs existants` section non-functional in this story.

## Out Of Scope
- Approved-user deletion flows.
- Editing account profile details.
- Bulk actions.
- Notification center or cross-page toast system.
- Any admin route or role gating behavior already covered by `001`.

## Dependencies
- Depends on `001 Admin Role Gating And Route Access`.
- Depends on the existing admin backend contract already available in `../foucl2`.

## Backend Contract
- `GET /api/v1/admin/pending-signups`
  - returns a JSON array of pending signup objects
  - payload shape per item: `{ "username": String }`
- `POST /api/v1/admin/pending-signups/approve`
  - expects JSON body `{ "username": String }`
  - returns `200` with empty response on success
- `DELETE /api/v1/admin/pending-signups/:username`
  - deletes the pending signup matching the path username
  - returns `200` with empty response on success
- Relevant backend error responses already implemented:
  - `401` with message `Not authenticated`
  - `403` with message `Admin privileges required`
  - `404` with message `Not found`
  - `400` with message `username is required` for invalid approve payload
  - `500` with message `Unable to process authentication`

## Implementation Decisions
- Rendering scope:
  - the admin page keeps the title and section structure introduced in `001`
  - only the `Comptes en attente` section becomes interactive in this story
- Pending list rendering:
  - each pending signup is rendered as one row keyed by `username`
  - each row shows the username and two actions: `Approuver` and `Supprimer`
- Processing model:
  - only one row action may be in progress at a time
  - while one row is processing, both actions for that row are disabled
  - other rows remain visible; the implementation may disable all row actions globally if that is the simplest way to guarantee single-flight behavior
- Delete safety policy:
  - deleting a pending signup is a direct action in this story
  - no extra confirmation step is required here
- Refresh policy:
  - after a successful approve or delete action, the frontend refetches the pending list from `GET /api/v1/admin/pending-signups`
  - the row is considered successfully handled only after the refetch completes
- Feedback policy:
  - successful actions are reflected implicitly by the row disappearing from the refreshed list
  - section-level loading error must display a visible retry control
  - action failure must display actionable feedback without requiring a page reload
  - prefer row-level feedback for row actions; section-level feedback is acceptable if row-local feedback is not practical
- Module boundaries:
  - `Api.*` remains purely technical and owns payload types and JSON encoding/decoding
  - admin UI state, rendering, and action decisions remain in the admin page module

## Acceptance Criteria
- Admin opening `/admin` sees the `Comptes en attente` section load pending signups from the backend.
- When pending signups exist, admin sees one row per username with `Approuver` and `Supprimer` controls.
- When no pending signups exist, the section shows an explicit empty state.
- If pending-signups loading fails, the section shows a visible error state with a retry action.
- Approving a pending signup removes it from the section after a successful refresh.
- Deleting a pending signup removes it from the section after a successful refresh.
- While an approve or delete request is in flight, duplicate submission is prevented.
- If an approve or delete request fails, the admin remains on the page, the list remains coherent, and the error is visible and retryable.
- This story does not expose any approved-user deletion controls.

## Tests
- Integration:
  - pending section transitions through `loading`, `empty`, `error`, and `loaded`
  - pending list decoding uses `{ username }` payloads
  - approve action disables duplicate submission and triggers refetch on success
  - delete action disables duplicate submission and triggers refetch on success
  - approve failure leaves the row visible and exposes recoverable feedback
  - delete failure leaves the row visible and exposes recoverable feedback
  - retry from initial load error refetches the pending list
- E2E:
  - admin opens `/admin`, sees one pending account, approves it, and sees it disappear
  - admin opens `/admin`, sees one pending account, deletes it, and sees it disappear
  - empty pending list shows explicit empty-state messaging
  - backend error during list load or row action shows actionable feedback without page reload
