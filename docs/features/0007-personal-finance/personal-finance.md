# Personal Finance

This backlog captures a finance workspace for recording, reviewing, interpreting, and analyzing financial transactions against the backend that already exists in `foucl2`, with active context established inside the finance workspace itself.

## Goal
Help users manage financial operations as explicit, explainable facts: capture transactions quickly, refine their interpretation safely, and drill down from aggregates back to the underlying operations.

## Scope For This Iteration
- Finance workspace shell and route entrypoints.
- Frontend adoption of the existing `/api/v1/finance` backend contract.
- Transaction ledger, detail, categorization, split, transfer linking, and note management aligned to the backend that already exists.
- Aggregate report consultation and ledger drill-down using the current backend report response.
- Responsive UX where mobile supports fast capture and desktop supports review and refinement.

## Out Of Scope For This Iteration
- Saved filter presets.
- Bulk actions.
- Offline transaction entry.
- Counterparty or description fields on transactions.
- Counterparty suggestions and counterparty-driven category suggestions.
- Generic multi-transaction linking beyond pairwise transfers.
- Rich analytical report datasets such as category buckets, time-series cashflow, or account-balance cards.
- Period comparison reports.
- Net worth tracking.
- Silent auto-categorization without explicit user confirmation.
- Unlinking existing transfer links.

## Implementation Principles
- The finance module is a workspace, not a passive archive.
- Analytical views must always support drill-down into the operational data behind the numbers.
- Important financial meaning stays explicit; the UI may suggest but must not silently infer key facts.
- Current workspace context should accelerate actions such as recording a transaction or opening a filtered ledger.
- Financial history is immutable; corrections are represented by additional events or interpretation changes rather than rewriting the original money movement.
- Active frontend scope must match the backend contract that exists today; broader ambitions stay postponed until the backend supports them.
- Mobile and desktop are both first-class, with different UX optimizations but consistent business rules.

## Current Backend Constraints
- Finance routes exist under `/api/v1/finance`.
- Transaction creation uses direction-specific endpoints and body fields `accountId`, `amount`, and optional `occurredAt`.
- Transaction reads currently expose `id`, `direction`, `accountId`, `amount`, `occurredAt`, `recordedAt`, `transfer`, `category`, `splits`, `notes`, and `adjustment`.
- Transaction list filtering currently supports `accountId`, `from`, and `to` only.
- Transfer linking currently supports only pairwise `transfer` links.
- Notes are mutable on the backend today: create, update, and delete are all supported.
- Report responses currently expose aggregate `total`, `count`, and `transactionIds`, not chart-ready analytical datasets.

## Backend API Contract
- [API Contract](api-contract.md) - Existing backend endpoints, payloads, validations, and current limitations for the finance frontend.

## Recommended Delivery Order
1. `001` Finance route entry and guarded navigation.
2. `002` Finance workspace shell and primary navigation.
3. `003` Finance nested flow context and return semantics.
4. `004` Finance backend contract adoption.
5. `006` Transaction ledger workspace.
6. `007` Ledger filtering and active-context persistence.
7. `011` Transaction detail inspection surface.
8. `008` Ledger indicators and navigation to transaction detail.
9. `009` Contextual record-transaction entrypoints.
10. `010` Record-transaction form validation and submission UX.
11. `012` Transaction categorization and notes management.
12. `013` Transaction split editor with explicit remainder.
13. `014` Transaction transfer linking.
14. `015` Reports aggregate summary and ledger drill-down.

## Stories

### Done
- [001 Finance Route Entry And Guarded Navigation](done/001-finance-route-entry-and-guarded-navigation.md) - Introduce `/finance`, `/finance/transactions`, and `/finance/reports`, expose one authenticated `Finance` tab, and normalize `/finance` to the transactions landing route.
- [002 Finance Workspace Shell And Primary Navigation](done/002-finance-workspace-shell-and-primary-navigation.md) - Add a dedicated finance-local subnav row for `Transactions` and `Reports`, with the finance `+` visible on `Transactions` only.
- [003 Finance Nested Flow Context And Return Semantics](done/003-finance-nested-flow-context-and-return-semantics.md) - Establish finance overlay open/close/back semantics through the create flow first, so later create and detail stories reuse one route-preserving modal-history contract.
- [004 Finance Backend Contract Adoption](done/004-finance-backend-contract-adoption.md) - Establish the initial `Api.Finance*` contract layer and shared finance types aligned to the existing `/api/v1/finance` backend.
- [006 Transaction Ledger Workspace](done/006-transaction-ledger-workspace.md) - Establish the base `Transactions` workspace with backend-backed newest-first ledger loading, core row facts, and explicit loading, empty, no-results, and error states.
- [007 Ledger Filtering And Active-Context Persistence](done/007-ledger-filtering-and-active-context-persistence.md) - Add account/date filtering with URL-backed finance context, visible active state, reset actions, and browser-history restoration.
- [008 Ledger Indicators And Navigation To Transaction Detail](done/008-ledger-indicators-and-navigation-to-transaction-detail.md) - Add structural ledger indicators and whole-row detail opening while preserving filters, ordering, and scroll position.
- [009 Contextual Record-Transaction Entrypoints](done/009-contextual-record-transaction-entrypoints.md) - Add a desktop-dropdown/mobile-sheet create chooser for `Expense` and `Income`, with supported ledger context forwarded into create.
- [010 Record-Transaction Form Validation And Submission UX](done/010-record-transaction-form-validation-and-submission-ux.md) - Add backend-compatible create form validation, idempotent submit lifecycle, desktop repeat-entry behavior, and mobile success toast confirmation.
- [011 Transaction Detail Inspection Surface](done/011-transaction-detail-inspection-surface.md) - Expose a focused read-only detail surface for inspecting backend-supported transaction facts, categorization state, transfers, splits, notes, and adjustments.
- [012 Transaction Categorization And Notes Management](done/012-transaction-categorization-and-notes-management.md) - Allow category refinement and mutable note management from transaction detail while matching current backend capabilities.
- [013 Transaction Split Editor With Explicit Remainder](done/013-transaction-split-editor-with-explicit-remainder.md) - Add a split editor with empty unsplit start, live remainder feedback, and save-back-to-detail behavior using the current backend split write shape.

### Ready
- [014 Transaction Transfer Linking](ready/014-transaction-transfer-linking.md) - Add in-flow target selection for explicit pairwise transfer linking, with backend-validated linking and linked-state reflection in detail and ledger.
- [015 Reports Aggregate Summary And Ledger Drill-Down](ready/015-reports-aggregate-summary-and-ledger-drill-down.md) - Add a first-pass date-range aggregate report with exact drill-down back into the ledger’s current date-context model.

### To Refine

### Canceled
- [005 Account Context Surface And Handoff](canceled/005-account-context-surface-and-handoff.md) - Canceled because account context comes from the `Transactions` page account filter; its useful behavior was redistributed into ledger filtering, create prefill, and report drill-down.

### Postponed
- [016 Rich Transaction Metadata](postponed/016-rich-transaction-metadata.md) - Track counterparty, description, counterparty suggestions, and counterparty-driven category suggestions once the backend exposes the required data.
- [017 Extended Ledger Filtering](postponed/017-extended-ledger-filtering.md) - Track direction, category, amount-range, and text-search ledger filtering beyond the backend-supported account/date filters.
- [018 Rich Analytical Reports](postponed/018-rich-analytical-reports.md) - Track chart-ready category breakdowns, time-series cashflow, account-balance reporting, and other analytical datasets that the current backend report endpoint does not return.
- [019 Report/Ledger Exact Drill-Down And Richer Report Filters](postponed/019-report-ledger-exact-drill-down-and-richer-report-filters.md) - Track richer report controls and exact drill-down beyond the current ledger filter contract, including why this was deferred from `007` and the first-pass `015`.
