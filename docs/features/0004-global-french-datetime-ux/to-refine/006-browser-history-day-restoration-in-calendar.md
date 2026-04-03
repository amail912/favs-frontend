# Browser History Day Restoration In Calendar

## Goal
As a calendar user, I want browser back/forward to restore previously consulted days so day navigation feels native and predictable.

## Outcome
Calendar Day view synchronizes consulted day with browser history using `?day=YYYY-MM-DD`, and back/forward restores that day.

## In Scope
- Synchronize selected calendar day with URL query param `day`.
- Push history entries for day changes from arrows and date picker.
- Restore day when navigating with browser back/forward.
- Keep behavior stable on reload with a valid `day` query param.

## Out Of Scope
- Persisting modal or transient UI state in history.
- Week/Month deep-linking policy.
- Backend API changes.

## Dependencies
- Depends on `005 Day Arrow Navigation In Calendar Day View`.
- Depends on app-level routing integration for query string parsing/serialization.

## Implementation Decisions
- URL policy:
  - source of truth for consulted day in calendar route is `?day=YYYY-MM-DD`
- History policy:
  - history entries are created when day changes by arrows or date picker
- Invalid input policy:
  - invalid `day` query falls back to current day

## Acceptance Criteria
- Opening `/calendar?day=YYYY-MM-DD` loads calendar on that day.
- Changing day via arrows or date picker updates URL query accordingly.
- Browser back/forward restores previously consulted days.
- Invalid `day` query does not break UI and falls back to a valid default day.

## Tests
- Unit:
  - parse/serialize `day` query value
  - fallback behavior for invalid day query values
- Integration:
  - day state and query synchronization in calendar route
- E2E:
  - navigate through several days then use browser back/forward to restore each consulted day
