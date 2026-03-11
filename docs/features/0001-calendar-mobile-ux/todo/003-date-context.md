# Date Context

## Goal
As a mobile user, I want the selected Day date to be expressed in a clear and human-friendly way, so I can understand it at a glance without parsing a raw `YYYY-MM-DD` string.

Relevant screenshots:
- [DayCalendar top area](../screenshots/daycalendar-top-mobile.png)

## How This Story Achieves The Goal
The Day header should stop showing a raw ISO date and instead use a localized full-date label such as `jeu. 12 mars`. The visible Day date entrypoint should also become a human-friendly button or chip rather than a raw visible date input. Both elements should represent the same selected Day date in a consistent way.

Activating the visible date button or chip should open the same native date picker flow already used by the page. After changing the date, both the Day header and the visible date entrypoint should update to the same selected date. This story applies to Day view only and does not redesign Week or Month date context.

## Technical Details
Introduce a thin formatting layer for user-facing Day date labels that delegates to an existing library or standard platform internationalization API. Do not implement custom weekday or month lookup tables, handwritten locale rules, or bespoke formatting logic.

Replace the currently visible raw native date input in Day view with a human-friendly button or chip while keeping the existing native date selection mechanism under the hood. The selected date should remain stored in the same canonical raw value already used by the page state and filtering logic, and both the Day header and the visible date entrypoint should derive from that same canonical selected date state.

The likely implementation area is `src/Pages/Calendar.purs`, with matching styling in `static/app.css`. The formatting helper introduced here should be reusable by later stories even though Week and Month are out of scope for this implementation.

## Tests
- Unit tests for the date-formatting wrapper covering a standard date in the current month, a date in another month, and a date in another year if the chosen localized output includes the year.
- Unit tests ensuring the formatting layer delegates to the chosen library or platform formatter rather than using custom formatting branches.
- Integration tests ensuring the Day header renders a localized full date instead of raw ISO and that the visible Day date entrypoint renders a human-friendly label from the same selected date state.
- Integration tests confirming that changing the date updates both the Day header and the visible date entrypoint consistently while keeping the canonical selected date state unchanged for filtering and data loading.
- E2E tests verifying that on mobile Day view the raw ISO date is no longer the primary visible date label, that tapping the visible date button or chip opens the date selection flow, and that the header and visible date entrypoint stay aligned after selecting another date.
