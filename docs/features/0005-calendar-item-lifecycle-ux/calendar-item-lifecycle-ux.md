# Calendar Item Lifecycle UX

This backlog captures direct lifecycle actions on calendar items with UX adapted for desktop and mobile.

## Goal
Allow users to manage calendar items directly where they are displayed, starting with safe and explicit deletion.

## Scope For This Iteration
- Direct calendar item deletion from item surfaces.
- UX adapted by device:
  - desktop direct action button
  - mobile action sheet entrypoint
- Explicit confirmation before destructive action.
- Coverage for both Task and Trip items.
- Contextual create defaults for item date-time entry.

## Backend Contract Input
- [Backend Evolution Needs: Auth Profile, Admin Governance, and Date-Time UX](../backend-evolution-needs-auth-admin-and-datetime-ux.md)

## Recommended Delivery Order
1. `001` Direct item deletion across day and range surfaces.
2. `002` Create item contextual date-time defaults.

## Stories

### Done
- [001 Direct Item Deletion Across Calendar Surfaces](done/001-direct-item-deletion-across-calendar-surfaces.md) - Delete server-backed tasks and trips directly from Calendar surfaces with desktop/mobile-adapted entrypoints and explicit confirmation.

### Ready

### To Refine
- [002 Create Item Contextual Date-Time Defaults](to-refine/002-create-item-contextual-datetime-defaults.md) - Pre-fill start date from consulted day and auto-fill end date-time to start + 1h when end time is empty.

### Canceled

### Postponed
