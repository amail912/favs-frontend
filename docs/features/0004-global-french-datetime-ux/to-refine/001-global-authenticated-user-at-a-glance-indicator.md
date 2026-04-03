# Global Authenticated User At-A-Glance Indicator

## Goal
As a user, I want to identify the active authenticated account instantly from the app shell.

## Outcome
A reusable global identity indicator is visible across authenticated pages, and remains coherent with session state and role-aware navigation.

## In Scope
- Add a persistent at-a-glance authenticated identity indicator in global app shell.
- Keep identity visibility on desktop and mobile.
- Ensure consistency after signin, reload, and signout.

## Out Of Scope
- Admin management actions.
- User profile editing.
- Domain-specific presence rendering logic.

## Dependencies
- Depends on frontend auth session/profile loading flow.
- Must remain coherent with role-aware navigation behavior.

## Implementation Decisions
- Identity source:
  - authenticated profile/session data as source of truth
- Visibility policy:
  - shown on authenticated surfaces
  - not shown on unauthenticated screens

## Acceptance Criteria
- Active username is visible at a glance on authenticated pages.
- Indicator updates correctly after signin/signout.
- Indicator remains stable after reload with an active session.
- No ambiguity between authenticated identity and other usernames displayed in page content.

## Tests
- Integration:
  - shell rendering by auth state
  - identity update on auth transitions
- E2E:
  - signin shows indicator
  - signout hides indicator
