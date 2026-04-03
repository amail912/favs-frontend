# Connected User At-A-Glance Identity In Calendar Context

## Goal
As a calendar user, I want to see immediately which authenticated account is active so I do not confuse my own data with shared-user data.

## Outcome
Calendar shell makes the authenticated identity explicit and always visible while browsing Day view and shared-presence interactions.

## In Scope
- Display authenticated username in a persistent, at-a-glance location in calendar context.
- Ensure visual distinction between authenticated identity and shared-user identities.
- Keep identity visible while interacting with presence rails and inspections.

## Out Of Scope
- Global non-calendar identity surfaces across other domains.
- Role/permission management.
- Backend admin workflows.

## Dependencies
- Depends on current auth session storage (`favs.auth.username`) behavior.
- Must remain coherent with `018 Connected User Presence Rail`.

## Implementation Decisions
- Identity source:
  - use authenticated session identity already stored client-side
  - display fallback-safe state when identity is temporarily unavailable
- Visual behavior:
  - identity stays visible in calendar shell on desktop and mobile
  - identity label is not mixed into shared overflow content
- Naming consistency:
  - same username string reused across calendar identity and self rail semantics

## Acceptance Criteria
- Authenticated username is visible at a glance in calendar context.
- Identity remains visible while switching calendar view modes and interacting with rails.
- Shared usernames remain clearly distinguishable from authenticated identity.
- No regression in existing calendar navigation and rail interactions.

## Tests
- Integration:
  - calendar shell renders authenticated identity from session state
  - fallback behavior when no username is available
- E2E:
  - authenticated user opens calendar and sees own identity immediately
  - identity remains visible through shared rail inspection flows
