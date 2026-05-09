# Ledger Filtering And Active-Context Persistence

## Goal
As a finance user, I want to keep my current ledger context while I work, so I can narrow the transaction workspace without repeatedly rebuilding the same account and date view.

## Outcome
The ledger exposes the backend-supported account and date-range filters, treats them as the canonical source of active finance context, keeps that context visible, and persists it through the `Transactions` URL so reload and browser history restore the same ledger state.

## In Scope
- Filter controls for:
  - account
  - date range via `from` and `to`
- URL-backed persistence of supported ledger context on the `Transactions` route.
- Hydration of ledger state from URL query params on direct entry, reload, and browser back/forward.
- Active-context visibility.
- Quick clearing of individual supported filters.
- Full reset back to unfiltered ledger context.
- Forwarding the active account and eligible date context into transaction creation.
- Restoring account and date context when re-entering the ledger from report drill-down.

## Out Of Scope
- Category, direction, amount-range, counterparty, or text-search filters.
- Saved filter presets.
- Browser storage persistence outside the URL.
- Cross-feature deep-linking beyond the `Transactions` URL contract.
- Bulk actions derived from a filtered selection.
- Separate account-entry or account-launcher surfaces.

## Dependencies
- Depends on `006` ledger workspace and ledger data loading.
- Depends on `001` and `002` implicitly because `Transactions` route ownership and shell already exist.
- Interacts with `009` because supported ledger context can prefill transaction creation.
- Interacts with `015` because report drill-down must restore supported ledger context.
- Interacts with `003` because create and detail overlays must preserve the active ledger context once established.

## Important Changes To Public Interfaces And Types
This story needs an explicit route and query contract for the `Transactions` page.

Required route behavior:
- `Transactions` route accepts optional query params:
  - `accountId`
  - `from`
  - `to`
- These query params are the canonical persisted representation of supported ledger context.
- Filter UI state derives from the parsed route state rather than from a separate hidden persistence layer.

Expected supporting interface changes:
- route parse and print support for `Transactions` query params
- a finance ledger context type or equivalent view state representing:
  - optional account id
  - optional `from`
  - optional `to`
- mapping from parsed route context into the transaction-list request already established by `004`

Do not introduce:
- additional filter params beyond backend-supported ones
- local-storage-backed persistence for ledger context
- a separate finance route just for filtered ledger views

## Implementation Decisions
- The `Transactions` URL query params are the canonical source of supported ledger context.
- The ledger UI reflects and edits that context; it does not maintain a separate authoritative persistence layer.
- Active ledger scope should only promise the account and date filters that the backend list endpoint supports directly.
- The ledger account filter is the canonical source of account context in this iteration.
- Use backend-shaped query params directly:
  - `accountId`
  - `from`
  - `to`
- Date values use the same raw ISO timestamp shape expected by the backend list API.
- Each committed account or date filter change pushes a new browser-history entry.
- Browser back and forward restore prior supported ledger contexts.
- Absence of a query param means that filter is not active.
- Account-only, date-only, and combined account-plus-date contexts are all valid.
- `from` and `to` remain independent optional bounds.
- The ledger must make active context obvious and must provide a fast reset path.
- Eligible supported context that should prefill transaction creation is account and single-day date.
- If the active date filter represents a single day, transaction creation should inherit that day; otherwise creation defaults to now.
- No separate finance account launcher or account-entry surface exists in active scope.
- When reports drill into the ledger with supported account or date scope, they land on the same `Transactions` URL contract.
- Invalid or unusable URL context must not break the ledger page; route handling stays recoverable and coherent even when query values are malformed.

## Acceptance Criteria
- A user can filter the ledger by account and date range.
- Applying supported filters updates the `Transactions` URL query params.
- Reloading a filtered `Transactions` URL restores the same supported ledger context.
- Browser back and forward restore prior supported ledger contexts after filter changes.
- Active context is visibly represented in the ledger workspace.
- A user can clear individual supported filters and reset the full supported context.
- Entering from an account or report drill-down opens the ledger with the expected account/date context already active.
- If an account filter is active, that account becomes the active finance account context used by later create and drill-down flows.
- If supported report drill-down enters the ledger with account or date scope, the ledger opens with the corresponding supported filters already active.
- Filtered results update without a full page workflow reset.
- Unsupported filter capabilities are not exposed by this story.

## Tests
- Route parse and print integration tests for:
  - `/finance/transactions`
  - `/finance/transactions?accountId=...`
  - `/finance/transactions?from=...&to=...`
  - combined supported query params
- Integration tests for ledger filter behavior:
  - account filter only
  - date-range filter only
  - combined account and date filters
  - clear-one-filter and reset-all behavior
- Integration tests for persistence and restoration:
  - reload with supported query params restores the same ledger context
  - browser back and forward restore prior contexts after filter changes
- Integration tests for downstream context use:
  - active `accountId` is exposed to create prefill
  - eligible single-day date context is exposed to create prefill
  - report drill-down restores ledger context through the same supported model
- Integration tests for invalid query handling:
  - malformed or unusable query values do not crash the ledger workspace
- E2E tests:
  - apply account and date filters and observe URL updates
  - reload and observe restored ledger context
  - navigate through several filter changes and use browser back and forward to restore each context
  - drill down from reports into a filtered ledger context

## Implementation Notes
- Implemented `Transactions` route state as `{ accountId :: Maybe String, from :: Maybe String, to :: Maybe String }` in `Pages.App`, with parse/print support for `/finance/transactions` query params.
- `Pages.FinanceTransactions` now treats this route state as canonical ledger context, loads transactions with that context, and raises route-sync outputs when filters are applied/cleared/reset.
- Added visible filter controls and active-filter chips for `accountId`, `from`, and `to`, plus `Apply` and full `Reset`.
- Added route-level unit coverage in `AppSpec` for finance query parsing/printing and added navigation E2E coverage for URL persistence through reload and back navigation.
