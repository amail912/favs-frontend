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

### To Refine
- [002 Shared French Date-Time Display Policy](to-refine/002-shared-french-datetime-display-policy.md) - Standardize date-time rendering to `dd/MM/yyyy HH:mm` with reusable helpers/components.
- [003 Desktop Custom Date-Time Picker](to-refine/003-desktop-custom-datetime-picker.md) - Add a reusable desktop-first date-time picker while keeping mobile-native inputs.
- [004 Date-Time UX Adoption Across The App](to-refine/004-datetime-ux-adoption-across-the-app.md) - Migrate existing surfaces to shared display and input components.
- [005 Day Arrow Navigation In Calendar Day View](to-refine/005-day-arrow-navigation-in-calendar-day-view.md) - Add previous/next day arrows for quick day browsing in Day view.
- [006 Browser History Day Restoration In Calendar](to-refine/006-browser-history-day-restoration-in-calendar.md) - Keep consulted day aligned with browser back/forward using `?day=YYYY-MM-DD`.

### Canceled

### Postponed
