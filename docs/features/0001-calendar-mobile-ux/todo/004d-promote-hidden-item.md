# Promote Hidden Item

## Goal
As a mobile Day view user, I want to choose a hidden item from the overlap sheet and bring it to the top of the stack, so I can edit or drag the item I care about.

Relevant screenshots:
- [DayCalendar dense overlaps](../screenshots/daycalendar-dense-overlap-mobile.png)

## How This Story Achieves The Goal
The overlap bottom sheet should not be read-only. Tapping an item in that sheet should promote it to the top of the overlap stack so it becomes the visible timeline card for that group.

Once promoted, the item should behave like the normal top card and stay easy to act on.

## Technical Details
When the user taps an item in the overlap bottom sheet, close the sheet and promote that item to the top of the corresponding mobile overlap stack. The promoted item must then behave like the visible timeline card, including existing edit and drag-and-drop interactions.

The promoted selection should remain active until the user leaves Day view. Re-entering Day view should reset the stack to the default deterministic top-card selection. The likely implementation area is `src/Pages/Calendar.purs`, with mobile-only UI state keyed to overlap groups.

This story applies only to mobile Day view and does not change desktop behavior or business overlap rules.

## Tests
- Unit tests for promoted-item override behavior inside a mobile overlap group.
- Integration tests ensuring a selected sheet item becomes the visible top card and remains selected through rerenders in the current Day view session.
- E2E tests validating that a promoted item remains editable and draggable and that the promoted selection resets after leaving Day view.
