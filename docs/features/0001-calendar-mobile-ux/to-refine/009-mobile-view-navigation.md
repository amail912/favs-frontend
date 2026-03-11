# Mobile View Navigation

## Goal
As a mobile user, I want compact but direct access to the current day and other calendar views, so I can switch context quickly without filling the top area with oversized navigation controls.

Relevant screenshots:
- [DayCalendar top area](../screenshots/daycalendar-top-mobile.png)

## How This Story Achieves The Goal
The mobile calendar should keep Day view explicit, keep date switching one tap away through a compact visible date control, and move Week and Month behind a lighter `More views` path. This keeps primary navigation fast while reducing the height of the mobile calendar header area.

## Technical Details
This story owns the mobile model for view switching and direct date access. It is in scope to replace the full `Day / Week / Month` button row with a compact mobile navigation model where Day remains directly visible, Week and Month stay reachable through `More views`, and the date picker remains directly reachable through a compact visible date chip or button. It is out of scope to define the final date wording policy, which belongs to `003-date-context`, and out of scope to redesign secondary mobile actions, which belongs to `006-mobile-actions-sheet`. The likely implementation area is the view selector logic in `src/Pages/Calendar.purs` and related mobile styles in `static/app.css`.

## Tests
- Unit tests for any helper that maps available calendar views into the compact mobile navigation model.
- Integration tests covering mobile rendering of the visible Day control, the compact date control, and the `More views` entrypoint across Day, Week, and Month.
- E2E tests verifying that a mobile user can keep Day visible, open `More views` to reach Week and Month, and open the date picker directly from the compact date control.
