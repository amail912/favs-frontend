# Header Compaction

## Goal
As a mobile user, I want to reach the actual agenda content without scrolling through layers of navigation and controls first, so I can understand my day immediately.

Relevant screenshots:
- [DayCalendar top area](../screenshots/daycalendar-top-mobile.png)

## How This Story Achieves The Goal
The mobile Day view should present a tighter top section that keeps only the most important controls visible before the timeline. Secondary actions should move behind a mobile-specific interaction pattern so the first viewport gives direct value instead of mostly showing chrome.

## Technical Details
Review the mobile rendering of the calendar page header, view selector, date input area, and mobile tools group. The change will likely involve `src/Pages/Calendar.purs` and the mobile rules in `static/app.css`, especially the blocks currently stacking controls above the timeline.

## Tests
- Unit tests for any pure helper introduced to decide which controls remain directly visible on mobile.
- Integration tests covering the Day view mobile rendering path and ensuring the expected control groups still exist.
- E2E tests validating that the first mobile viewport shows the timeline sooner and that hidden actions remain reachable.

## Cancel reason
This story was canceled because it mixed three separate concerns into one backlog item:
- top-area density and visual compaction
- mobile view and date navigation
- mobile secondary actions

It has been replaced by the following focused stories:
- [006 Mobile Actions Sheet](../to-refine/006-mobile-actions-sheet.md)
- [008 Top Area Density](../to-refine/008-top-area-density.md)
- [009 Mobile View Navigation](../to-refine/009-mobile-view-navigation.md)
