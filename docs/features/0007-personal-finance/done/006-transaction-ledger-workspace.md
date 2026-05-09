# Transaction Ledger Workspace

## Goal
As a finance user, I want a dedicated transaction workspace where I can browse and understand my money movements quickly, so I can decide what needs review or action.

## Outcome
The finance module exposes `Transactions` as the primary operational workspace, loads transactions from the current backend list endpoint, renders a responsive ledger with stable newest-first ordering, and handles explicit loading, empty, no-results, and error states.

## In Scope
- Base `Transactions` workspace content for the finance route already established by `001` and `002`.
- Initial transaction loading from the finance transaction list endpoint using the active backend contract from `004`.
- Responsive ledger rendering that may use table-like or list-like presentation depending on device and layout.
- Default ordering by newest `occurredAt` first, matching backend behavior.
- Base row rendering for backend-supported facts:
  - amount
  - direction indicator
  - account label when available, otherwise account id
  - occurred date-time
  - current category when present
  - split presence as plain base row information only
  - transfer presence as plain base row information only
  - note presence as plain base row information only
  - adjustment state when present
- Explicit workspace states:
  - initial loading
  - loaded with rows
  - empty when no transactions exist at all
  - no-results when supported context matches nothing
  - error with retry
- Visible ledger-anchored create entrypoint presence, without defining its behavior beyond discoverability.

## Out Of Scope
- Filter controls, context persistence, and reset behavior.
- Detail opening, row selection behavior, or visible open-detail affordances.
- Specialized indicators beyond readable base row facts.
- Inline editing.
- Bulk actions.
- Rich transaction metadata that the backend does not expose.
- Client-side reordering or alternate sort modes.

## Dependencies
- Depends on `002` finance workspace shell and primary navigation.
- Depends on `004` finance backend contract adoption.
- Precedes `007`, which adds the supported filter model and active-context persistence.
- Precedes `008`, which adds navigational row semantics and richer structural indicators.
- Interacts with `009` only for presence of the visible create entrypoint, not for create behavior itself.

## Important Changes To Public Interfaces And Types
This story should rely on the finance API seam introduced by `004`, not define a new contract.

Expected minimum consumed interface:
- transaction list request helper from `Api.Finance`
- shared `FinanceTransaction` read type from `Api.FinanceContract`

Behavioral contract to make explicit:
- `006` consumes unfiltered list loading by default
- `006` may also render no-results when later-supported context is present, but it does not own the filter controls that produce that context
- `006` does not add new route state, new query params, or new API surface

## Implementation Decisions
- The ledger is the primary finance workspace and should be treated as the default operational screen.
- Base list loading should rely on backend ordering rather than re-sorting client-side unless needed for deterministic rendering after decode.
- Desktop and mobile can use different list presentations, but must expose the same core facts and ordering.
- The ledger must render meaningful transaction rows even before `008` adds navigation-specific indicators.
- Base row policy:
  - show readable core facts only
  - do not visually imply clickability or drill-in in this story
  - do not invent counterparty or description fields
- Account display policy:
  - prefer resolved account name when the account reference data is already available from the adopted finance API layer
  - fall back to account id if name resolution is not yet available in the rendering path
- State policy:
  - `empty` means there are no transactions in the dataset
  - `no-results` means a later-supported account/date context yields zero matches
  - `error` must expose a visible retry path
- Create entrypoint policy:
  - the workspace visibly exposes the create affordance already promised by `002`
  - action semantics, menu content, and contextual defaults remain owned by `009`

## Acceptance Criteria
- Opening `Transactions` shows a finance ledger workspace under the existing finance shell.
- By default, the workspace loads transactions from the backend list endpoint and renders them newest first.
- Each visible row shows the backend-supported transaction facts needed for basic review.
- The ledger does not display unsupported fields such as counterparty or description.
- Initial load shows a loading state until data resolves.
- If the user has no transactions, the workspace shows an explicit empty state.
- If supported account or date context yields no matching transactions, the workspace shows an explicit no-results state distinct from the empty state.
- If loading fails, the workspace shows an explicit error state with a retry action.
- A create-transaction entrypoint is visibly present from the ledger workspace.
- Base ledger rows do not yet promise detail-opening affordance in this story.

## Tests
- Integration tests covering workspace state transitions:
  - loading to loaded
  - loading to empty
  - loading to error with retry
  - loaded to no-results when supported context is applied by surrounding state
- Integration tests ensuring default ordering is newest first by `occurredAt`.
- Integration tests ensuring row rendering includes required backend-supported fields and excludes unsupported fields.
- Integration tests ensuring account label fallback behavior is coherent if name resolution is unavailable.
- Integration tests ensuring the create affordance is visible on the `Transactions` workspace.
- E2E tests:
  - open finance `Transactions` and see transactions ordered from newest to oldest
  - see explicit empty-state messaging when no transactions exist
  - see recoverable error behavior when list loading fails
  - see the create entrypoint present from the ledger workspace

## Implementation Notes (2026-05-09)
- Added a dedicated `Pages.FinanceTransactions` component and mounted it under `/finance/transactions` from `Pages.App`.
- Implemented backend loading through `Api.Finance.getTransactions` and account-name resolution through `Api.Finance.getAccounts` with fallback to `accountId`.
- Added explicit ledger states: loading, loaded rows, empty, no-results (context-aware), and error with retry.
- Added unit/integration coverage in `Test.Pages.FinanceTransactionsSpec` and adjusted finance navigation E2E expectations to target the ledger workspace instead of the old placeholder.
