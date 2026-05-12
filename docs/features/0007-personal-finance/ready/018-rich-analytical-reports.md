# Rich Analytical Reports

## Goal
As a finance user, I want analytical views such as category breakdowns, cashflow timelines, and balance-oriented report cards, so I can understand patterns over time instead of only consulting one aggregate total.

## How This Story Achieves The Goal
This story expands finance reports from aggregate-only consultation to backend-backed analytics sections, so users can inspect category, timeline, and account-level patterns under explicit filter context.

The user can:
- Run an analytics report over a selected range.
- See category breakdown analytics.
- See cashflow series analytics.
- See account balance analytics cards.

## Technical Details
- Analytics endpoint:
  - `GET /api/v1/finance/report/analytics`
- Query model:
  - required: `from`, `to`
  - optional: `direction`, `accountId`, repeated `categoryIn`, repeated `categoryNotIn`, `amountMin`, `amountMax`, `search`
- Response sections:
  - `summary`
  - `categoryBreakdown`
  - `cashflowSeries`
  - `accountBalances`
- Current analytics response does not include `transactionIds`.
- Filter semantics align with current backend finance query parsing:
  - `direction` supports `sent`, `received`, `all`
  - category include/exclude overlap is invalid
  - `amountMin`/`amountMax` must be integer cents
  - `amountMin > amountMax` is invalid
  - `search` is case-insensitive over `counterparty` and `description`
- Analytics consistency expectations:
  - `summary` totals/counts align with equivalent aggregate report scope under the same filters
  - backend auto-selects `cashflowSeries` bucket granularity from effective range span
  - `cashflowSeries` buckets are contiguous, non-overlapping, and cover full `[from, to)` interval
- Out of scope:
  - exact rich drill-down parity and richer drill-down state model (`019`)
  - frontend-only analytical calculations that diverge from backend results

## Tests
- Integration tests for analytics request lifecycle states:
  - loading, success, empty, error
- Integration tests for analytics response rendering:
  - `summary`, `categoryBreakdown`, `cashflowSeries`, `accountBalances`
- Integration tests for filter propagation and validation error handling:
  - invalid `direction`
  - overlapping `categoryIn`/`categoryNotIn`
  - invalid/non-integer amount bounds
  - invalid range constraints
- Integration tests ensuring analytics summary parity with equivalent aggregate report scope.
- Integration tests ensuring timeline rendering honors backend bucket order and continuity.
- E2E tests validating filter application and coherent analytics presentation in reports UI.
- Explicit test note:
  - rich analytics bucket drill-down behavior remains deferred to `019`.
