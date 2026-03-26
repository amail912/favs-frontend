# Hidden Cards Show End Position

## Goal
As a mobile Day view user, I want hidden overlapping items to remain partially visible with their own vertical extent, so I can still see where they end even when they are not the top card.

Relevant screenshots:
- [DayCalendar dense overlaps](../screenshots/daycalendar-dense-overlap-mobile.png)

## How This Story Achieves The Goal
The mobile overlap stack should not hide the lower cards behind a generic stub. Hidden cards should remain visible enough to show that the overlap group has depth and that different items may end at different times.

The stack should read visually like a deck of cards rather than a collapsed badge or a set of narrow columns.

## Technical Details
This story owns the rendering of hidden cards inside the mobile overlap stack only. Hidden cards must remain partially visible behind the top card with offset and shadow treatment, and their visible extent must continue down the timeline far enough to indicate each hidden item's own end time.

The top card remains the only fully readable card in the timeline. The likely implementation area is the mobile overlap presentation helper in `src/Pages/Calendar.purs` and mobile-specific stack styling in `static/app.css`.

This story applies only to mobile Day view and does not change overlap grouping rules.

## Tests
- Unit tests for helper output that preserves per-hidden-item rendering extent.
- Integration tests ensuring hidden cards with different end times remain visually distinct inside the same mobile overlap stack.
- E2E tests validating that the stack visually communicates differing end positions on a mobile viewport.
