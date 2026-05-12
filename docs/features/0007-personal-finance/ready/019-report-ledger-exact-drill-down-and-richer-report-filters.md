# Report/Ledger Exact Drill-Down And Richer Report Filters

## Goal
As a finance user, I want richer report controls and exact drill-down into the supporting transactions, so I can analyze beyond one date-scoped aggregate without the ledger view becoming approximate or misleading.

## How This Story Achieves The Goal
This story aligns report controls and ledger drill-down context on the same backend-supported filter surface so users can open richer report slices and drill to the exact supporting ledger set.

The user can:
- Run report queries with richer controls.
- Open drill-down with the full active report filter context preserved.
- See a ledger result set that reflects the same filter universe as the report.

## Technical Details
- Active report and ledger filtering are aligned to backend-supported shared dimensions:
  - `from`
  - `to`
  - `direction`
  - `accountId`
  - `categoryIn`
  - `categoryNotIn`
  - `amountMin`
  - `amountMax`
  - `search`
- Report endpoint:
  - `GET /api/v1/finance/report`
  - returns `total`, `count`, and ordered `transactionIds` for the aggregate set
- Ledger endpoint:
  - `GET /api/v1/finance/transactions`
  - consumes equivalent filter context and ordering semantics
- Exact drill-down behavior:
  - drill-down carries the full report filter context into ledger route/query state
  - no separate hidden transaction-set navigation mode is required
  - resulting ledger context remains explainable, reload-safe, and browser-history-safe
- Backend parity semantics that must remain explicit in UX and acceptance:
  - report filters share interpretation with ledger filters for overlapping dimensions
  - transfer-linked transactions are excluded from report aggregates
  - adjustment rows are included in report aggregates with backend-derived sign semantics
  - split/category filtering remains split-aware and consistent across report and ledger
  - `search` is case-insensitive over `counterparty` and `description`
  - category include/exclude overlap and invalid amount bounds are validation errors
- Out of scope:
  - new backend drill-down token mechanisms
  - analytics visualization expansion beyond scope already covered in `018`

## Tests
- Integration tests for report controls with richer filter combinations.
- Integration tests for drill-down context mapping:
  - report filter state serializes into ledger filter state without loss
- Integration tests for exactness checks:
  - ledger results under carried context match report supporting scope expectations
  - report `transactionIds` ordering and ledger ordering remain coherent
- Integration tests for parity-sensitive semantics:
  - transfer exclusion in reports
  - adjustment-row treatment
  - split/category behavior under include/exclude filters
- Integration tests for validation and edge cases:
  - invalid/repeated constrained query params
  - overlapping `categoryIn`/`categoryNotIn`
  - invalid `amountMin`/`amountMax`
  - range boundary behavior
- E2E tests validating:
  - user applies richer report controls
  - user drills down into ledger
  - preserved filter chips/context and supporting rows remain coherent across reload/back-forward
