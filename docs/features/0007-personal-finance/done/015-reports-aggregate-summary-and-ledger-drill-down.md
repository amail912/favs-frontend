# Reports Aggregate Summary And Ledger Drill-Down

## Goal
As a finance user, I want a compact but explainable report surface over my transactions, so I can evaluate an aggregate and then jump directly to the underlying ledger entries.

## Outcome
The finance module exposes a first-pass reports surface that submits a date-scoped aggregate query to the current backend, shows `total` and `count`, and supports exact drill-down back into the ledger through the existing ledger date-context model.

## In Scope
- A reports workspace with required date-range controls:
  - `from`
  - `to`
- Aggregate summary rendering for:
  - `total`
  - `count`
- Exact ledger drill-down from a date-scoped report result back into the ledger.
- Restoration of supported ledger date context through the existing ledger filter model from `007`.
- Explicit report behavior for current backend report semantics:
  - transfer-linked transactions are excluded from the aggregate base query
  - split transactions contribute proportionally
- Empty and error states for the active report surface.

## Out Of Scope
- Direction filter controls.
- Account include/exclude controls.
- Category include/exclude controls.
- Chart-ready category breakdowns.
- Cashflow time series.
- Account balance cards.
- Period comparison.
- Net worth tracking.
- Exact richer drill-down beyond the current ledger filter contract.

## Dependencies
- Depends on `002` finance workspace shell and primary navigation.
- Depends on `004` finance backend contract adoption.
- Depends on `007` because drill-down must restore supported ledger context.
- Interacts with `014` because current backend report behavior excludes transfer-linked transactions from the aggregate base query.
- Defers richer report and drill-down parity work to postponed story `019`.

## Important Changes To Public Interfaces And Types
For this first ready version of reports, the active UI consumes only:
- required `from`
- required `to`

It still uses the existing backend response:
- `total`
- `count`
- `transactionIds`

But this story does not introduce:
- richer active report controls that the current ledger cannot represent exactly
- a hidden exact-transaction-set ledger mode
- route or URL state beyond the existing ledger date-context model owned by `007`

## Implementation Decisions
- Active first-pass report scope is intentionally narrower than the full backend query surface.
- The active report UI exposes only a date-range query so drill-down can remain exact through the current ledger filter contract.
- Active reports scope must still use the current backend report response shape: `total`, `count`, and `transactionIds`.
- The reports surface remains explainable by making drill-down to the matching ledger set easy.
- Drill-down reuses the existing ledger date-context model from `007`; this story does not invent a second hidden ledger mode.
- Transfer-linked transactions are excluded from the current report aggregate base query.
- Split transactions contribute proportionally according to the backend report semantics.
- Richer report filters and exact richer drill-down parity are deferred to a dedicated follow-up story.

## Acceptance Criteria
- A user can open reports and submit a date-range aggregate report query.
- The result shows aggregate `total` and `count` clearly.
- A user can drill from a report result back into the ledger using the same date context.
- The ledger shows the carried date scope through its active filter context.
- The active report surface does not expose direction, account include/exclude, or category include/exclude controls.
- Empty and error states are explicit and recoverable.
- The active story does not expose richer report controls whose exact drill-down cannot yet be represented by the active ledger contract.
- The active story does not promise chart, bucket, or balance-report surfaces that the backend report endpoint does not return.

## Tests
- Integration tests covering date-range report control state, aggregate result rendering, and empty/error state behavior.
- Integration tests ensuring drill-down restores the expected ledger date context.
- Integration tests covering transfer exclusion and split semantics as documented by the backend contract.
- E2E tests verifying that a user can submit a date-scoped report and drill into the supporting ledger context.

## Implementation Notes
- Added a dedicated `Pages.FinanceReports` workspace component with explicit date-range controls (`from`, `to`) and report states: idle, loading, error, empty, and summary.
- Report submission now calls the current backend aggregate endpoint through the existing contract using only active story controls:
  - required `from`
  - required `to`
  - no direction/account/category controls in the UI
- Aggregate result rendering now shows `total` and `count`, with explicit recoverable error and empty states.
- Added drill-down output from reports to app shell, mapped to existing ledger context route semantics:
  - `/finance/transactions?from=...&to=...`
- Replaced finance reports placeholder rendering in `Pages.App` with the new reports component slot and output handling.
- Added unit/integration coverage in `Test.Pages.FinanceReportsSpec` and wired it into `SpecSuite`.
- Updated routing/navigation E2E coverage to assert real reports workspace behavior and date-context drill-down.
