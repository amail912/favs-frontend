# Calendar Trip Sharing

This backlog captures trip sharing, derived presence, and Day view location cues between users.

## Goal
Let users share trip information in a controlled way so each person can understand where the other is over time from the calendar.

## Scope For This Iteration
- Trips are dedicated calendar items that reuse the standard calendar start and end times and add a departure place and an arrival place.
- Shared visibility is resolved from user-level sharing and subscription lists handled by the backend.
- The frontend derives location over a requested period from ordered trips returned by the backend.
- Day view is the only in-scope calendar surface for presence cues in this iteration.

## Backend API Contract
- [API Contract](api-contract.md) - Backend endpoints, payload expectations, and frontend derivation assumptions.

## Stories

### To Refine
- [002 Trip Calendar Item](to-refine/002-trip-calendar-item.md) - Model trips as first-class calendar items with coherent place fields in the frontend.
- [003 Trip Validation And Editing](to-refine/003-trip-validation-and-editing.md) - Keep trip creation and updates valid with constrained place choices and clear feedback.
- [004 Share Trip Users](to-refine/004-share-trip-users.md) - Let users manage who may see their trips.
- [005 Subscribe To Trip Users](to-refine/005-subscribe-to-trip-users.md) - Let users manage whose trips they want to follow.
- [006 Period Trips Query](to-refine/006-period-trips-query.md) - Return the ordered trips needed for frontend presence derivation.
- [007 Presence Derivation](to-refine/007-presence-derivation.md) - Compute location state over a period from the ordered trips.
- [008 Day View Side Rail](to-refine/008-day-view-side-rail.md) - Surface shared presence in Day view without competing with item cards.
- [009 Day View Location Inspection](to-refine/009-day-view-location-inspection.md) - Make the exact location readable on hover, tap, and focus.
- [010 Cue Personalization](to-refine/010-cue-personalization.md) - Let the calendar owner customize cue colors per person and place.
- [011 Unknown Location States](to-refine/011-unknown-location-states.md) - Make missing or not-yet-derivable location states explicit.
- [012 Shared Users Readability](to-refine/012-shared-users-readability.md) - Keep the Day view understandable with a small set of simultaneous users.

### Canceled
- [001 Predefined Places Catalog](canceled/001-predefined-places-catalog.md) - Absorbed into the trip item and trip validation stories instead of remaining a standalone frontend outcome.

### Postponed
- [013 Week View Presence Cues](postponed/013-week-view-presence-cues.md) - Track Week view presence as a later extension.
- [014 Month View Presence Cues](postponed/014-month-view-presence-cues.md) - Track Month view presence as a later extension.
