# Extended Ledger Filtering

## Goal
As a finance user, I want more powerful ledger filtering, so I can narrow the transaction workspace by financial meaning rather than only by account and date.

## How This Story Achieves The Goal
This story extends the finance ledger filtering model from account/date-only constraints to backend-supported financial filters, while preserving explainable URL-backed context and reload-safe behavior.

The user can:
- Filter by transaction direction.
- Filter by category inclusion/exclusion.
- Filter by amount range.
- Filter by text search over transaction metadata.
- Combine these filters with existing account/date filters.

## Technical Details
- Ledger filtering is backed by `GET /api/v1/finance/transactions` with existing and extended query parameters:
  - existing: `accountId`, `from`, `to`
  - extended: `direction`, repeated `categoryIn`, repeated `categoryNotIn`, `amountMin`, `amountMax`, `search`
- Filter semantics:
  - `direction` supports `sent`, `received`, `all` and defaults to `all`
  - `from` is inclusive on `occurredAt`
  - `to` is exclusive on `occurredAt`
  - `search` is case-insensitive over `counterparty` and `description`
  - `amountMin` and `amountMax` are integer cents and compare against absolute row `amount`
- Category filter semantics:
  - when split is active, category filtering evaluates split categories
  - when split is not active, category filtering evaluates whole-transaction category
  - `categoryIn` and `categoryNotIn` must not overlap
- Direction/row behavior:
  - `direction=all` includes sent, received, and adjustment rows under current ledger behavior
  - ordering remains newest `occurredAt` first, then transaction id tie-breaker
- Validation and error behavior from backend:
  - invalid `direction` value => `400`
  - repeated single-value params (`accountId`, `from`, `to`, `direction`, `amountMin`, `amountMax`, `search`) => `400`
  - non-integer `amountMin`/`amountMax` => `400`
  - `amountMin > amountMax` => `400`
  - overlap between `categoryIn` and `categoryNotIn` => `400`
  - `from > to` => `400`
  - `from == to` => empty list
  - unknown/foreign `accountId` => empty list
- Out of scope:
  - analytical report datasets and charting (`018`)
  - richer report/ledger drill-down parity behavior beyond ledger filter adoption (`019`)

## Tests
- Integration tests for each extended filter input:
  - `direction`, `categoryIn`, `categoryNotIn`, `amountMin`, `amountMax`, `search`
- Integration tests for combined filters with existing account/date context.
- Integration tests for validation failures:
  - invalid/repeated single-value parameters
  - non-integer amount bounds
  - `amountMin > amountMax`
  - overlapping `categoryIn`/`categoryNotIn`
  - `from > to`
- Integration tests for boundary semantics:
  - `from == to` returns empty result
  - unknown `accountId` returns empty result
- Integration tests for split-aware category behavior and direction behavior including adjustment rows.
- E2E tests validating:
  - user can apply/reset extended filters from ledger UI
  - active filter context is visible and URL-backed
  - reload/back-forward restores the same extended context

## Implementation Notes (2026-05-12)
- Extended ledger context, route query parsing/printing, and transaction query encoding to include direction, category include/exclude, amount bounds, and search.
- Added new ledger filter controls and active filter pills with URL-backed apply/reset behavior and context restoration across navigation/reload.
- Added client-side validation for integer amount bounds, min/max ordering, and include/exclude category overlap before loading.
