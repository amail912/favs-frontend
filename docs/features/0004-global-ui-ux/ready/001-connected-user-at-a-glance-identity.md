# Connected User At-A-Glance Identity

## Goal
As an app user, I want to see immediately which account is connected so I avoid ambiguity across pages and shared-user contexts.

## Outcome
A global shell indicator shows the authenticated identity as `Connecté: <username>` on app pages when authenticated, and is hidden when unauthenticated.

## In Scope
- Display a global authenticated identity indicator in the app shell across pages.
- Use a single label format: `Connecté: <username>`.
- Keep indicator visible while navigating between pages, including calendar.
- Hide indicator on unauthenticated pages (signup/signin and other non-authenticated contexts).

## Out Of Scope
- Role/permission management.
- Admin workflows.
- Calendar-specific rail behavior (covered by `0002-018`).

## Dependencies
- Depends on frontend auth session/profile source used to resolve authenticated username.
- Must remain coherent with `0002-018` self-rail username semantics.

## Implementation Decisions
- Identity source:
  - authenticated username from existing auth session/profile flow
- Visibility policy:
  - authenticated: indicator visible
  - unauthenticated: indicator hidden
- Rendering policy:
  - exact visible text `Connecté: <username>`
  - indicator remains in global shell context, not inside domain-specific panels

## Acceptance Criteria
- On authenticated pages, user sees `Connecté: <username>` at a glance.
- On unauthenticated pages, no identity indicator is shown.
- Indicator remains consistent after signin, reload, and signout.
- Navigating between pages preserves identity consistency with no visual ambiguity.

## Tests
- Integration:
  - shell renders indicator for authenticated state
  - shell hides indicator for unauthenticated state
  - indicator updates correctly on signin/signout transitions
- E2E:
  - signin flow shows `Connecté: <username>`
  - signout removes indicator
  - page navigation keeps indicator visible and consistent while authenticated
