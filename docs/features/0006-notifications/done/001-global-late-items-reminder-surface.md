# Global Late-Items Reminder Surface

## Goal
As an authenticated user, I want to immediately see when I have late calendar items from any page so I can react without switching context manually.

## Outcome
A global reminder chip appears in shared app chrome when late items exist. The chip count reflects current late-item volume and opens a dedicated bottom sheet.

## In Scope
- Shared app-shell rendering on authenticated routes (`Notes`, `Checklists`, `Calendar`, `Admin`).
- Late-item chip placement between auth menu and tab navigation.
- Chip visibility policy: hidden when late count is zero.
- Bottom-sheet entrypoint from chip tap/click.
- Late-item list ordering by most recently ended first.
- Progressive rendering: first 50 items, then explicit "load more".

## Out Of Scope
- Reminder interactions that mutate items (handled by separate stories).
- Background push channels (browser push, email, mobile push).
- Notification types beyond late calendar items.

## Technical Details
- Late-item eligibility (for v1):
  - only non-trip calendar items,
  - `now` strictly after item `windowEnd`,
  - status not `Done`,
  - status not `Canceled`.
- Reminder surface must be rendered by shared page shell, not duplicated per page.
- Bottom sheet must be mobile-first and usable on desktop.
- Count and list derive from same filtered source to avoid mismatches.

## Acceptance Criteria
- On an authenticated page, when there is at least one eligible late item, a reminder chip is visible between auth controls and tabs.
- When there are zero eligible late items, no reminder chip is rendered.
- Chip label clearly exposes late-item count.
- Clicking/tapping the chip opens a bottom sheet listing late items.
- Bottom sheet list is ordered by descending `windowEnd` (most recently ended first).
- Bottom sheet initially shows at most 50 rows and exposes a clear "load more" control when additional rows exist.

## Tests
- Unit test for eligibility filtering on type, status, and `windowEnd < now`.
- Unit test for ordering by descending end time.
- Integration test for chip hidden state when filtered count is zero.
- Integration test for chip visible state and count rendering when filtered count is non-zero.
- Integration test for first-50 behavior and "load more" visibility.
- E2E test opening chip from each authenticated tab and verifying bottom-sheet visibility.
