# Global French Date-Time UX

This backlog captures generic UX improvements for date-time input and display across the entire frontend.

## Goal
Make date-time interactions easier on desktop and consistently French-formatted across the application, while preserving mobile-native input behavior.

## Scope For This Iteration
- Global at-a-glance authenticated identity indicator.
- Reusable French date-time display policy for authenticated and unauthenticated pages.
- Reusable desktop date-time input component with stronger ergonomics than native desktop pickers.
- Day view arrow navigation for previous/next day.
- Browser history restoration for previously consulted day in calendar.
- Mobile behavior remains native input based.

## Backend Contract Input
- [Backend Evolution Needs: Auth Profile, Admin Governance, and Date-Time UX](../backend-evolution-needs-auth-admin-and-datetime-ux.md)

## Recommended Delivery Order
1. `001` Global authenticated user at-a-glance indicator.
2. `002` Shared French date-time display policy and reusable helpers.
3. `003` Desktop custom date-time picker component.
4. `004` Adoption across existing surfaces.
5. `005` Day arrow navigation in calendar day view.
6. `006` Browser history restoration for consulted day.

## Stories

### Done

### To Refine
- [001 Global Authenticated User At-A-Glance Indicator](to-refine/001-global-authenticated-user-at-a-glance-indicator.md) - Keep the active account explicit across the app shell.
- [002 Shared French Date-Time Display Policy](to-refine/002-shared-french-datetime-display-policy.md) - Standardize date-time rendering to `dd/MM/yyyy HH:mm` with reusable helpers/components.
- [003 Desktop Custom Date-Time Picker](to-refine/003-desktop-custom-datetime-picker.md) - Add a reusable desktop-first date-time picker while keeping mobile-native inputs.
- [004 Date-Time UX Adoption Across The App](to-refine/004-datetime-ux-adoption-across-the-app.md) - Migrate existing surfaces to shared display and input components.
- [005 Day Arrow Navigation In Calendar Day View](to-refine/005-day-arrow-navigation-in-calendar-day-view.md) - Add previous/next day arrows for quick day browsing in Day view.
- [006 Browser History Day Restoration In Calendar](to-refine/006-browser-history-day-restoration-in-calendar.md) - Keep consulted day aligned with browser back/forward using `?day=YYYY-MM-DD`.

### Canceled

### Postponed
