# Subscribe To Trip Users

## Goal
As a user, I want to choose whose trips I follow so my calendar only surfaces the travel information that matters to me.

## Outcome
The user can manage the backend subscription list that declares whose trips they want to follow. The UI makes it explicit that subscription alone is insufficient without reciprocal sharing.

## In Scope
- Read the subscription list from `GET /api/v1/trip-sharing/subscriptions`.
- Add a user through `POST /api/v1/trip-sharing/subscriptions`.
- Remove a user through `DELETE /api/v1/trip-sharing/subscriptions/:username`.
- Show the current list in a manageable owner-facing UI.
- Explain that visible shared trips require both a subscription and a reciprocal share from the other user.

## Out Of Scope
- Share list management.
- Deriving or rendering shared presence.

## Dependencies
- No hard dependency on `002` or `003`.
- Combined with `004`, this story enables meaningful shared-trip visibility.

## Acceptance Criteria
- The user can see who they currently follow.
- Adding a username updates the rendered list after a successful backend write.
- Removing a username updates the rendered list after a successful backend write.
- The screen wording makes clear that following a user is not enough if they do not share back.

## Tests
- Integration tests for rendering the subscription list response.
- Integration tests for successful add and remove flows.
- E2E tests covering adding and removing a user from the subscription list.
