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

## Stories

### Done
- [001 Connected User At-A-Glance Identity](done/001-connected-user-at-a-glance-identity.md) - Show `Connecté: <username>` across app pages when authenticated, hidden when unauthenticated.

### Ready
- [002 Shared French Date-Time Display Policy](ready/002-shared-french-datetime-display-policy.md) - Standardize visible date/time rendering with shared French display helpers while keeping ISO-local formats for inputs and payloads.
- [003 Desktop Custom Date-Time Picker](ready/003-desktop-custom-datetime-picker.md) - Add a reusable desktop split date+time picker while keeping mobile-native date-time inputs.
- [004 Date-Time UX Adoption Across The App](ready/004-datetime-ux-adoption-across-the-app.md) - Adopt the shared date-time display and desktop input primitives across current app surfaces and list any remaining non-migrated ones.
- [005 Day Arrow Navigation In Calendar Day View](ready/005-day-arrow-navigation-in-calendar-day-view.md) - Add previous/next day arrows in Calendar Day view, synchronized with the existing canonical day state.
- [006 Browser History Day Restoration In Calendar](ready/006-browser-history-day-restoration-in-calendar.md) - Synchronize consulted Day view with `?day=YYYY-MM-DD` and restore it through browser back/forward.

### To Refine

### Canceled

### Postponed
