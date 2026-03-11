# Top Area Density

## Goal
As a mobile user, I want meaningful calendar content to appear sooner when I open Day view, so I do not spend the first screenful on calendar-owned labels and spacing.

Relevant screenshots:
- [DayCalendar top area](../screenshots/daycalendar-top-mobile.png)

## How This Story Achieves The Goal
The calendar-owned top area should collapse into a tighter layout that removes or merges low-value vertical blocks before the timeline. The mobile Day view should surface useful agenda content faster without changing the global app shell that sits above the calendar page itself.

## Technical Details
This story owns the density of the calendar-specific mobile top area only. It is in scope to compact the Day view heading, tighten spacing between calendar-owned blocks, and remove or merge redundant calendar-local elements that do not justify their height. It is out of scope to redesign the global app shell, change the wording of date labels, or redesign the secondary mobile actions surface. The likely implementation area is the calendar page rendering in `src/Pages/Calendar.purs` and the mobile-specific layout rules in `static/app.css`.

## Tests
- Unit tests only if a pure helper is introduced to select or suppress calendar-local header elements on mobile.
- Integration tests covering the mobile Day view rendering path and ensuring required calendar-local controls still exist after compaction.
- E2E tests validating that on a `390x844` viewport the timeline begins materially higher than in the current design and that meaningful calendar content appears sooner without hiding required calendar functionality.
