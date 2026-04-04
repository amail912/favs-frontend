# Browser History Day Restoration In Calendar

## Goal
As a calendar user, I want browser back/forward to restore previously consulted days so Day navigation feels native and predictable.

## Outcome
Calendar Day view synchronizes the consulted day with browser history using `?day=YYYY-MM-DD`, and browser back/forward restores that day. The query param is present only in Day view and is removed when the user switches to Week or Month view.

## In Scope
- Synchronize consulted Day with URL query param `day`.
- Push history entries for day changes triggered from Day view interactions.
- Restore Day state when navigating with browser back/forward.
- Keep reload stable with a valid `day` query param in Day view.
- Remove `day` from the URL when leaving Day view for Week or Month.

## Out Of Scope
- Persisting modal, drag, or other transient Calendar UI state in history.
- Deep-linking Week or Month state.
- Backend API changes.

## Dependencies
- Depends on `005 Day Arrow Navigation In Calendar Day View`.
- Depends on app-level routing integration in `Pages.App`, which currently parses only route paths and not Calendar query params.

## Implementation Decisions
- URL policy:
  - source of truth for consulted Day in Day view is `?day=YYYY-MM-DD`
  - `day` exists only while Calendar is in `Jour`
  - switching to `Semaine` or `Mois` removes `day` from the URL
- History policy:
  - day changes triggered from Day view controls create browser history entries
  - browser back/forward restores previously consulted Day values
- Parsing policy:
  - valid `day` query initializes the consulted Day in Calendar Day view
  - invalid `day` query falls back to the current day without breaking the UI
- Scope boundary:
  - only consulted Day is synchronized
  - no other Calendar state is serialized into the URL in this story
- Technical boundary:
  - `Pages.App` route parsing/printing must evolve to preserve Calendar path plus optional `day` query
  - Calendar remains responsible for applying the resolved consulted day to its own state

## Acceptance Criteria
- Opening `/calendar?day=YYYY-MM-DD` in Day view loads Calendar on that consulted day.
- Changing day from Day view updates the URL query accordingly.
- Browser back/forward restores previously consulted days.
- Reload on a valid Day URL preserves the consulted day.
- Invalid `day` query falls back to a valid default day and does not break UI rendering.
- Switching from Day view to Week or Month removes the `day` query param from the URL.

## Tests
- Unit:
  - parse valid `day=YYYY-MM-DD`
  - reject invalid `day` values with fallback to current day
  - serialize Calendar Day URLs with `?day=YYYY-MM-DD`
- Integration:
  - Day state and URL query stay synchronized while in Day view
  - switching to Week/Month removes the query param
  - reload with valid query restores the consulted day
- E2E:
  - open `/calendar?day=YYYY-MM-DD` and observe the restored day
  - navigate through several days then use browser back/forward to restore each consulted day
  - switch to Week/Month and confirm the `day` query is removed
