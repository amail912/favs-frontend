# Mobile Overlap Stack

## Goal
As a mobile Day view user, I want overlapping items to render as a stacked deck instead of narrow side-by-side columns, so the primary item stays readable on a narrow screen.

Relevant screenshots:
- [DayCalendar dense overlaps](../screenshots/daycalendar-dense-overlap-mobile.png)

## How This Story Achieves The Goal
The mobile Day view should stop reusing the desktop overlap-column layout. Each overlap group should render as a deck of cards with one readable top card and the remaining overlapping items partially visible behind it.

Desktop Day view should keep the current parallel-column layout unchanged.

## Technical Details
Keep the existing overlap grouping logic as the source of truth unless a small adaptation is needed to expose grouped results for mobile rendering. For each overlap group on mobile, render one top card and render the remaining items as shadow cards behind it.

The default top card must be chosen deterministically by earliest start time. If multiple items start at the same minute, the one ending earliest should be chosen first. If another tie-break is still needed, use a stable deterministic fallback from the existing sorted order. The likely implementation area is `src/Pages/Calendar.purs`, with accompanying mobile styling in `static/app.css`.

This story applies only to mobile Day view and does not redesign desktop overlap rendering.

## Tests
- Unit tests for a pure mobile-overlap presentation helper covering a single item, two overlapping items, dense overlap groups, and deterministic top-card selection.
- Integration tests ensuring mobile Day view uses the stacked deck strategy while desktop keeps the current parallel-column layout.
- E2E tests validating that dense overlap groups no longer render as multiple unreadably thin side-by-side cards on a mobile viewport.
