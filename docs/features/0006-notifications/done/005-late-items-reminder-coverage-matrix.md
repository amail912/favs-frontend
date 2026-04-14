# Late-Items Reminder Coverage Matrix

## Goal
As a team, we want explicit and sufficient test coverage for late-item reminders so behavior remains stable across UI, routing, and completion flows.

## Outcome
A mandatory coverage matrix now maps each reminder behavior to concrete automated tests and acts as a closure gate for stories `001` to `004`.

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

## Coverage Matrix
- Eligibility filtering + descending ordering:
  - Unit: `test/Notifications/LateItemsSpec.purs`
- Pagination (first 50 + has more):
  - Unit: `test/Notifications/LateItemsSpec.purs`
  - E2E: `e2e/tests/notifications.reminder-coverage.spec.js` (`chip/sheet...pagination`)
- Quick-complete duration normalization + validation:
  - Unit: `test/Notifications/LateItemsSpec.purs`
- Reminder visibility policy and refresh routing gates:
  - Integration/pure: `test/Pages/AppSpec.purs`
- Route-change refresh stale-response protection:
  - Integration/pure: `test/Pages/AppSpec.purs` (`ignores stale late-items responses...`)
- Navigation from reminder row to Calendar day+item actions:
  - E2E: `e2e/tests/notifications.reminder-coverage.spec.js` (`selecting a row...`)
- Quick-complete success with in-sheet refresh:
  - E2E: `e2e/tests/notifications.reminder-coverage.spec.js` (`quick-complete success...`)
- Quick-complete failure and retry path:
  - E2E: `e2e/tests/notifications.reminder-coverage.spec.js` (`failure...retry succeeds`)

## Acceptance Criteria
- Every late-item reminder behavior in stories `001` to `004` is mapped to at least one test layer.
- Critical correctness rules (eligibility, navigation targeting, completion call semantics) are covered by automated tests.
- At least one E2E path validates end-to-end flow from chip visibility to successful quick completion.
- Story closure checklist references this matrix as mandatory gate.

## Tests
- `npm test`
- `npm run lint`
- `npm run format`
- `npx playwright test e2e/tests/notifications.reminder-coverage.spec.js`
- `npm run e2e`
