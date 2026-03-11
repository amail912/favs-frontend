# Touch Accessibility

## Goal
As a mobile user, I want every important calendar action and timeline interaction to be reliably touchable, so I can operate the calendar without precision tapping or accidental misses.

Relevant screenshots:
- [DayCalendar top area](../screenshots/daycalendar-top-mobile.png)
- [DayCalendar dense overlaps](../screenshots/daycalendar-dense-overlap-mobile.png)
- [DayCalendar tools modal](../screenshots/daycalendar-tools-modal-mobile.png)

## How This Story Achieves The Goal
The mobile calendar should provide touch targets, spacing, and visual states that fit real thumb interaction. This includes buttons, view switches, timeline cards, and secondary action surfaces so the whole screen feels intentionally mobile-first.

## Technical Details
Audit the mobile touch target sizes, spacing, and focus or active states across the Day view and its action surfaces. This work will mainly affect mobile CSS sizing rules and may require small rendering changes where controls are currently too dense or too small.

## Tests
- Unit tests only if pure helpers are added for touch-specific presentation rules.
- Integration tests covering the presence and expected labels of mobile controls after layout changes.
- E2E tests validating tap behavior for the main mobile controls, timeline item interactions, and any replacement for the current tools modal.
