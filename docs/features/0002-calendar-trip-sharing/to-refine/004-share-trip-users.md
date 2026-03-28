# Share Trip Users

## Goal
As a user, I want to control who may see my trips so shared travel information stays intentional.

## How This Story Achieves The Goal
The application should let each user maintain the list of people they share their trips with. This is one of the two user-managed lists that drive shared trip visibility.

## Technical Details
Provide a manageable UI for viewing, adding, and removing users from the share list without attaching share state to individual trips. The screen should make it clear that this list controls who may receive the user's trips.

## Tests
- Integration tests for rendering and updating the share list UI.
- E2E tests covering adding and removing a user from the share list.
