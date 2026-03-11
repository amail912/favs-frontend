# Creation Entrypoint

## Goal
As a mobile user, I want the creation entrypoint to stay easy to reach without covering important agenda content, so I can add items quickly while still reading the schedule.

Relevant screenshots:
- [DayCalendar top area](../screenshots/daycalendar-top-mobile.png)
- [DayCalendar dense overlaps](../screenshots/daycalendar-dense-overlap-mobile.png)

## How This Story Achieves The Goal
The add-item CTA should remain prominent but should no longer compete with the timeline for limited screen space. Its mobile behavior should feel intentional in both idle reading and active planning moments.

## Technical Details
Review the mobile role, placement, and spacing of the fixed creation CTA. The implementation will likely involve the Day view shell and the CSS around the floating action button so the CTA can coexist with dense content and mobile bottom space safely.

## Tests
- Unit tests only if a pure helper is introduced to control CTA visibility or placement modes.
- Integration tests ensuring the creation action remains available from Day view on mobile.
- E2E tests checking that the creation entrypoint is visible, does not obstruct key content, and still opens the create flow.
