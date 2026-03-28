# Trip Calendar Item

## Goal
As a user, I want a trip to behave like a real calendar item so I can plan it with start and end times while still distinguishing it from other agenda items.

## How This Story Achieves The Goal
Trips should be first-class calendar items that reuse the standard calendar scheduling fields and add trip-specific place metadata rather than loose annotations on generic items. This gives the agenda a stable model for rendering, editing, and sharing trip data.

## Technical Details
Render and handle trips as a dedicated calendar item type in the frontend, with place references for departure and arrival while reusing the standard calendar item time fields for scheduling. The frontend should decode, store, and display those place values coherently wherever trip items are read.

## Tests
- Integration tests ensuring trip items are decoded, stored, and rendered distinctly from normal items with their departure and arrival places.
- E2E tests creating a trip and seeing it in the calendar.
