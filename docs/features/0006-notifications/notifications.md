# Notifications

This backlog captures cross-page reminder and notification UX, starting with late calendar items.

## Goal
Help users notice and resolve overdue work quickly from anywhere in the app, without forcing them to manually scan Calendar views.

## Scope For This Iteration
- Global late-items reminder visible across authenticated tabs.
- Late-items bottom sheet optimized for fast triage.
- Quick completion flow with duration confirmation.
- Route-driven refresh policy for notification freshness.
- Navigation from reminder entries to targeted Calendar context.
- Dedicated test coverage for reminder behaviors.

## Out Of Scope For This Iteration
- Browser push notifications.
- Email or external notification channels.
- Non-calendar notification types (for example due-soon, admin events, checklist reminders).

## Backend Contract Input
- Existing calendar list endpoint: `GET /api/v1/calendar-items`.
- Existing completion endpoint: `POST /api/v1/calendar-items/:id/validate`.

## Recommended Delivery Order
1. `001` Global late-items reminder surface.
2. `002` Late-items sheet interactions and navigation to Calendar actions.
3. `003` Quick complete flow with prefilled duration prompt.
4. `004` Route-change refresh and state coherence.
5. `005` Notification reminder test coverage matrix.

## Stories

### Done
- [001 Global Late-Items Reminder Surface](done/001-global-late-items-reminder-surface.md) - Show a cross-page late-items chip in shared app chrome and open a bottom sheet entrypoint when late items exist.
- [002 Late-Items Sheet Navigation To Calendar Actions](done/002-late-items-sheet-navigation-to-calendar-actions.md) - Let users open Calendar day context and item actions directly from a late-item row.
- [003 Quick Complete Late Items With Duration Prompt](done/003-quick-complete-late-items-with-duration-prompt.md) - Resolve late items from the reminder sheet via validate action with a prefilled duration prompt.
- [004 Late-Items Refresh On Route Changes](done/004-late-items-refresh-on-route-changes.md) - Keep reminder data fresh by recomputing notifications on every address-bar navigation change.
- [005 Late-Items Reminder Coverage Matrix](done/005-late-items-reminder-coverage-matrix.md) - Define required unit/integration/E2E coverage for late-items reminder reliability.

### Ready

### To Refine

### Canceled

### Postponed
