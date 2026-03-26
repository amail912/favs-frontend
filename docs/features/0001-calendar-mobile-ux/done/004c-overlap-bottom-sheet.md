# Overlap Bottom Sheet

## Goal
As a mobile Day view user, I want a bottom sheet listing every item in an overlap group, so I can identify hidden items without relying only on the stack preview.

Relevant screenshots:
- [DayCalendar dense overlaps](../screenshots/daycalendar-dense-overlap-mobile.png)

## How This Story Achieves The Goal
When a mobile overlap stack contains hidden items, it should expose a compact summary entrypoint such as `+2`. Tapping that entrypoint should open a bottom sheet listing the full overlap group in chronological order.

This gives the user a reliable way to inspect dense overlaps while keeping the timeline itself readable.

## Technical Details
Show the `+N` summary entrypoint only when an overlap group has hidden items. The bottom sheet opened from that entrypoint should list all items in the overlap group in chronological order and preserve enough identifying information to distinguish them at a glance.

Each row in the bottom sheet should show at least the title and time range, plus the existing useful metadata already shown in the timeline when available. The likely implementation area is `src/Pages/Calendar.purs`, with any necessary mobile sheet styling in `static/app.css`.

This story applies only to mobile Day view. It does not decide which listed item becomes the top card after selection; that behavior belongs to the next story.

## Tests
- Unit tests for hidden-count computation and chronological ordering of overlap-group items.
- Integration tests ensuring the summary entrypoint appears only when hidden overlap items exist and that opening it shows the full group in chronological order.
- E2E tests validating that tapping the `+N` entrypoint opens the overlap bottom sheet on a mobile viewport.
