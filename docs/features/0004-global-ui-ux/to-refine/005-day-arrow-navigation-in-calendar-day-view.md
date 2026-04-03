# Day Arrow Navigation In Calendar Day View

## Goal
As a calendar user, I want quick previous/next arrows in Day view so I can browse days without opening the date picker each time.

## Outcome
Day view exposes two arrow controls that navigate to the previous and next day while keeping the current date model and rendering coherent.

## In Scope
- Add `previous day` and `next day` controls in calendar Day view.
- Keep arrow navigation available on desktop and mobile.
- Keep existing date picker flow compatible with arrow navigation.
- Keep day label and day-bound data refresh consistent after arrow navigation.

## Out Of Scope
- Week/Month arrow semantics.
- Browser history behavior (covered by `006`).
- Changes to date-time formatting policy.

## Dependencies
- Depends on calendar Day view date state and date parsing helpers.
- Must remain coherent with current Day label rendering policy.

## Implementation Decisions
- Scope policy:
  - arrows are shown only in Day view
  - arrows are hidden in Week/Month views
- Navigation granularity:
  - each click/tap changes selected day by exactly one calendar day
- Inputs parity:
  - arrow navigation and date picker update the same canonical day state

## Acceptance Criteria
- In Day view, user can navigate to previous and next day via arrows.
- Arrow navigation updates the visible day label and the consulted Day dataset.
- Date picker still works and remains synchronized with arrow-driven day changes.
- Controls are usable on desktop and mobile without layout regression.

## Tests
- Unit:
  - helper computes day -1 and day +1 across month/year boundaries
- Integration:
  - day state updates from arrow actions
  - day label updates from same canonical state
- E2E:
  - user navigates several days with arrows and sees consistent day content updates
