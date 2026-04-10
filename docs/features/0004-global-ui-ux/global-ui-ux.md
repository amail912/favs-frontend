# Global UI UX

This backlog captures cross-app UI and UX foundations shared by multiple domains.

## Goal
Provide coherent, reusable global UI/UX behavior across shell identity surfaces, navigation ergonomics, and date-time interactions.

## Scope For This Iteration
- Global authenticated identity indicator.
- Reusable French date-time display policy for authenticated and unauthenticated pages.
- Reusable desktop date-time input component with stronger ergonomics than native desktop pickers.
- Calendar Day view arrow navigation for previous/next day.
- Browser history restoration for previously consulted calendar day.
- Day view quick return to current day.
- Day view short-duration item readability fallback.
- Desktop Day timeline visual alignment with item times.
- Mobile behavior remains native input based.

## Backend Contract Input
- [Backend Evolution Needs: Auth Profile, Admin Governance, and Date-Time UX](../backend-evolution-needs-auth-admin-and-datetime-ux.md)

## Recommended Delivery Order
1. `001` Connected user identity at-a-glance across app pages.
2. `002` Shared French date-time display policy and reusable helpers.
3. `003` Desktop custom date-time picker component.
4. `004` Adoption across existing surfaces.
5. `005` Day arrow navigation in calendar day view.
6. `006` Browser history restoration for consulted day.
7. `007` Day view today shortcut.
8. `008` Day view short item title readability.
9. `009` Desktop day view timeline time alignment.

## Stories

### Done
- [001 Connected User At-A-Glance Identity](done/001-connected-user-at-a-glance-identity.md) - Show `Connecté: <username>` across app pages when authenticated, hidden when unauthenticated.
- [002 Shared French Date-Time Display Policy](done/002-shared-french-datetime-display-policy.md) - Standardize visible date/time rendering with shared French display helpers while keeping ISO-local formats for inputs and payloads.
- [003 Desktop Custom Date-Time Picker](done/003-desktop-custom-datetime-picker.md) - Add a reusable desktop split date+time picker while keeping mobile-native date-time inputs.
- [004 Date-Time UX Adoption Across The App](done/004-datetime-ux-adoption-across-the-app.md) - Adopt the shared date-time display and desktop input primitives across current app surfaces and list any remaining non-migrated ones.
- [005 Day Arrow Navigation In Calendar Day View](done/005-day-arrow-navigation-in-calendar-day-view.md) - Add previous/next day arrows in Calendar Day view, synchronized with the existing canonical day state.
- [006 Browser History Day Restoration In Calendar](done/006-browser-history-day-restoration-in-calendar.md) - Synchronize consulted Day view with `?day=YYYY-MM-DD` and restore it through browser back/forward.
- [007 Day View Today Shortcut](done/007-day-view-today-shortcut.md) - Add an `Aujourd'hui` Day-only control in Day view and keep state/URL synchronization coherent with existing day navigation.
- [008 Day View Short Item Title Readability](done/008-day-view-short-item-title-readability.md) - Add compact fallback rules for short Day cards with same-line time/title, hidden category, and an `11px` minimum title size.

### Ready

### To Refine
- [009 Desktop Day View Timeline Time Alignment](to-refine/009-desktop-day-view-timeline-time-alignment.md) - Fix desktop Day timeline vertical positioning so card top and bottom match item start/end times.

### Canceled

### Postponed
