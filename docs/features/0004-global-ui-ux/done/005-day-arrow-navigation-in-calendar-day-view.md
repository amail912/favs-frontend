# Day Arrow Navigation In Calendar Day View

## Goal
As a calendar user, I want fast previous/next day controls in Day view so I can browse days without reopening the date picker each time.

## Outcome
Calendar Day view exposes previous and next day arrow controls around the existing day label/date trigger, and those controls update the same canonical `focusDate` already used by the current day picker and day-bound data loading.

## In Scope
- Add `jour précédent` and `jour suivant` controls in Calendar Day view.
- Keep arrow controls usable on desktop and mobile.
- Keep the existing date picker flow compatible and synchronized with arrow navigation.
- Keep day label and consulted Day dataset coherent after each arrow navigation.

## Out Of Scope
- Week or Month arrow semantics.
- Browser history or query param behavior covered by `006`.
- Date-time display policy changes covered by `002`.

## Dependencies
- Depends on the current `focusDate` state already used by Calendar Day view.
- Must remain coherent with the current Day label rendering policy and date picker trigger.

## Implementation Decisions
- Visibility policy:
  - arrows are shown only in `Jour`
  - arrows are hidden in `Semaine` and `Mois`
- Placement policy:
  - arrows sit around the existing Day label/date trigger in the Day view selector
  - no separate toolbar or modal is introduced
- Navigation policy:
  - each click/tap changes `focusDate` by exactly one calendar day
  - month and year boundaries must be handled correctly
- State policy:
  - arrows and date picker both update the same canonical `focusDate`
  - existing day-bound fetch and render logic continue to derive from `focusDate`
- Responsiveness policy:
  - controls remain reachable and usable on desktop and mobile without layout regression

## Acceptance Criteria
- In Day view, user can move to the previous day and the next day via dedicated arrow controls.
- Arrow navigation updates the visible Day label immediately.
- Arrow navigation updates the consulted Day dataset using the same canonical state as the date picker.
- Switching the date from the picker remains synchronized with arrow-driven navigation.
- No arrow controls are shown in Week or Month view.

## Tests
- Unit:
  - helper computes day -1 and day +1 across month boundaries
  - helper computes day -1 and day +1 across year boundaries
- Integration:
  - day state updates from arrow actions
  - Day label updates from the same canonical `focusDate`
  - arrows are hidden outside Day view
- E2E:
  - user navigates several consecutive days with arrows and sees coherent day label changes
  - day-bound content remains aligned with the consulted day after arrow navigation
