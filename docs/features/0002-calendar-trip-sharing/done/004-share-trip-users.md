# Share Trip Users

## Goal
As a user, I want to control who may see my trips so shared travel information stays intentional.

## Outcome
The user can manage the backend share list that grants trip visibility to other users. The UI makes it explicit that this list controls outbound visibility only.

## In Scope
- Read the share list from `GET /api/v1/trip-sharing/shares`.
- Add a user through `POST /api/v1/trip-sharing/shares`.
- Remove a user through `DELETE /api/v1/trip-sharing/shares/:username`.
- Show the current list in a manageable owner-facing UI.
- Explain that this list alone does not make another user's trips visible in return.

## Out Of Scope
- Subscription list management.
- Deriving or rendering shared presence.

## Dependencies
- No hard dependency on `002` or `003`.
- Combined with `005`, this story enables meaningful shared-trip visibility.

## Acceptance Criteria
- The user can see who currently has access to their trips.
- Adding a username updates the rendered list after a successful backend write.
- Removing a username updates the rendered list after a successful backend write.
- The screen wording makes clear that this list controls who may see the user's trips.

## Tests
- Integration tests for rendering the share list response.
- Integration tests for successful add and remove flows.
- E2E tests covering adding and removing a user from the share list.
