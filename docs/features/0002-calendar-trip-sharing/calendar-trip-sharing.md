# Calendar Trip Sharing

This backlog captures trip sharing, derived presence, and Day view location cues between users.

## Goal
Let users share trip information in a controlled way so each person can understand where the other is over time from the calendar.

## Scope For This Iteration
- Trips are dedicated calendar items that reuse the standard calendar start and end times and add a departure place and an arrival place.
- Shared visibility is resolved from user-level sharing and subscription lists handled by the backend.
- The frontend derives location over a requested period from ordered trips returned by the backend.
- Day view is the only in-scope calendar surface for presence cues in this iteration.
- Backend payloads are aligned with the implementation currently available in `../foucl2`.

## Current Backend Constraints
- `GET /api/v1/trip-places` returns places by `name`. That name is the usable identifier for this iteration.
- Trip items are stored through `GET` and `POST /api/v1/calendar-items` with `type = "trip"`.
- Trip payloads use `windowStart`, `windowEnd`, `departurePlaceId`, and `arrivalPlaceId`.
- Trip writes are rejected when departure and arrival are the same, when the window is invalid, or when the trip overlaps another trip owned by the same user.
- Shared presence only exists when the viewer subscribes to a user and that user shares trips back with the viewer.

## Backend API Contract
- [API Contract](api-contract.md) - Backend endpoints, payload expectations, and frontend derivation assumptions.

## Recommended Delivery Order
1. `002` Trip item model and rendering.
2. `003` Trip create and edit validation.
3. `004` Share list management.
4. `005` Subscription list management.
5. `006` Period trips query consumption.
6. `007` Presence derivation.
7. `008` Day view side rail.
8. `009` Day view inspection.
9. `012` Multi-user readability rules.
10. `010` Cue personalization.
11. `015` Desktop hover continuity for presence inspection.
12. `016` Viewport-safe stable presence inspection panel.
13. `017` Mobile inspection visibility and interaction stability.
14. `018` Connected user presence rail.
15. `019` Connected user identity at a glance in calendar context.

## Stories

### Done
- [002 Trip Calendar Item](done/002-trip-calendar-item.md) - Introduce trips as a dedicated calendar item variant and render a readable route on existing agenda cards.
- [003 Trip Validation And Editing](done/003-trip-validation-and-editing.md) - Add trip create and edit flows backed by the place catalog and the backend validation rules.
- [004 Share Trip Users](done/004-share-trip-users.md) - Let a user manage who may see their trips through the share list endpoints.
- [005 Subscribe To Trip Users](done/005-subscribe-to-trip-users.md) - Let a user manage whose trips they follow through the subscription list endpoints.
- [006 Period Trips Query](done/006-period-trips-query.md) - Consume the dedicated grouped shared-trips query needed for presence derivation.
- [007 Presence Derivation](done/007-presence-derivation.md) - Derive deterministic location state from grouped ordered trips.
- [008 Day View Side Rail](done/008-day-view-side-rail.md) - Render derived presence in Day view without competing with the owner agenda.
- [009 Day View Location Inspection](done/009-day-view-location-inspection.md) - Reveal the exact derived location or in-transit state on interaction.
- [010 Cue Personalization](done/010-cue-personalization.md) - Let the owner configure cue colors without changing shared trip data.
- [012 Shared Users Readability](done/012-shared-users-readability.md) - Keep Day view readable with an explicit three-user rail limit and an inspectable overflow fallback.
- [015 Desktop Hover Continuity For Presence Inspection](done/015-desktop-hover-continuity-presence-inspection.md) - Keep desktop inspection usable when moving pointer from rail to panel.
- [016 Viewport-Safe Stable Presence Panel](done/016-viewport-safe-stable-presence-panel.md) - Keep the desktop inspection panel visible and stable near viewport limits.
- [017 Mobile Inspection Visibility And Interaction Stability](done/017-mobile-inspection-visibility-and-interaction-stability.md) - Ensure mobile inspection content remains reachable and stable for cue interactions.

### To Refine
- [018 Connected User Presence Rail](to-refine/018-connected-user-presence-rail.md) - Add a thin Day view rail for the authenticated user with the same presence semantics as shared rails.
- [019 Connected User At-A-Glance Identity In Calendar Context](to-refine/019-connected-user-at-a-glance-identity-in-calendar-context.md) - Keep the authenticated identity explicit in the calendar shell to remove ambiguity between self and shared users.

### Canceled
- [001 Predefined Places Catalog](canceled/001-predefined-places-catalog.md) - Absorbed into the trip item and trip validation stories instead of remaining a standalone frontend outcome.
- [011 Unknown Location States](canceled/011-unknown-location-states.md) - Absorbed by delivered derivation, rail, and inspection stories instead of shipping as a standalone increment.

### Postponed
- [013 Week View Presence Cues](postponed/013-week-view-presence-cues.md) - Track Week view presence as a later extension.
- [014 Month View Presence Cues](postponed/014-month-view-presence-cues.md) - Track Month view presence as a later extension.
