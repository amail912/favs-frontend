# Overlap Readability

## Goal
As a mobile user, I want overlapping calendar items to remain understandable, so I can still identify what is planned without unreadably thin cards when several items occupy the same time range.

Relevant screenshots:
- [DayCalendar dense overlaps](../screenshots/daycalendar-dense-overlap-mobile.png)

## How This Story Achieves The Goal
The mobile Day view should no longer reuse the desktop-style side-by-side overlap columns. Instead, overlapping items should use a summary-stack model: the earliest-starting item in the overlap group remains directly visible in the timeline, while the remaining items are represented by a visible summary entrypoint such as `+2`.

Tapping that summary entrypoint should open a bottom sheet showing the full overlap group in chronological order, with enough information and actions for the user to identify the hidden items and continue interacting with them. Desktop Day view should keep the current parallel-column layout unchanged.

## Technical Details
Keep the existing overlap grouping logic as the source of truth unless a small adaptation is needed to expose mobile-friendly grouped results. The mobile-only presentation layer should transform an overlap group into one primary visible timeline card plus one summary entrypoint representing the remaining items. The primary visible item must be selected deterministically by earliest start time.

The bottom sheet opened from the summary entrypoint should list all items in the overlap group in chronological order and preserve the essential details and existing actions needed to identify and use each item. The likely implementation area is `src/Pages/Calendar.purs`, with accompanying mobile styling in `static/app.css`. This story applies only to mobile Day view and does not redesign desktop overlap rendering.

## Tests
- Unit tests for any pure mobile-overlap presentation helper covering a single item, two overlapping items, dense overlap groups, and deterministic primary-card selection by earliest start time.
- Integration tests ensuring mobile Day view uses the summary-stack strategy while desktop keeps the current parallel-column layout.
- Integration tests ensuring the summary entrypoint appears only when hidden overlap items exist and that the bottom sheet lists all overlap-group items in chronological order.
- E2E tests validating that dense overlap groups no longer render as multiple unreadably thin side-by-side cards on a mobile viewport, that tapping the `+N` entrypoint opens the overlap bottom sheet, and that the full overlap group remains identifiable there.

## Cancel reason
This story was canceled because it mixed four separate concerns into one backlog item:
- mobile stacked overlap rendering
- hidden-card end visibility
- overlap inspection through a bottom sheet
- promotion of a hidden item to the top card

It has been replaced by the following focused stories:
- [004A Mobile Overlap Stack](../todo/004a-mobile-overlap-stack.md)
- [004B Hidden Cards Show End Position](../todo/004b-hidden-cards-show-end-position.md)
- [004C Overlap Bottom Sheet](../todo/004c-overlap-bottom-sheet.md)
- [004D Promote Hidden Item](../todo/004d-promote-hidden-item.md)
