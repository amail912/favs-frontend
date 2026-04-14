# Late-Items Reminder Coverage Matrix

## Goal
As a team, we want explicit and sufficient test coverage for late-item reminders so behavior remains stable across UI, routing, and completion flows.

## Outcome
A mandatory coverage matrix defines what must be validated at unit, integration, and E2E levels before marking late-item reminder stories done.

## In Scope
- Coverage requirements for stories `001` to `004`.
- Mapping of behaviors to minimum test layers.
- Completion criteria for story acceptance.

## Out Of Scope
- Non-notification domains unrelated to late-item reminders.
- Performance/load benchmarking strategy.
- Tooling migration for test frameworks.

## Technical Details
- Coverage must include:
  - eligibility filtering and ordering logic,
  - global chip visibility and count behavior,
  - bottom-sheet rendering and pagination behavior,
  - row navigation to Calendar day + item actions,
  - quick complete prompt and validate flow,
  - route-change refresh behavior and stale-response safety.
- "Done" requires all mandatory tests passing in CI for impacted stories.

## Acceptance Criteria
- Every late-item reminder behavior in stories `001` to `004` is mapped to at least one test layer.
- Critical correctness rules (eligibility, navigation targeting, completion call semantics) are covered by automated tests.
- At least one E2E path validates end-to-end flow from chip visibility to successful quick completion.
- Story closure checklist references this matrix as mandatory gate.

## Tests
- Unit: eligibility, ordering, duration prefill computation.
- Integration: chip/sheet state transitions, route parsing/printing, refresh race handling.
- E2E:
  - cross-tab chip visibility and sheet opening,
  - navigation from sheet row to Calendar target actions,
  - quick complete success path and in-sheet refresh,
  - quick complete failure/retry path.
