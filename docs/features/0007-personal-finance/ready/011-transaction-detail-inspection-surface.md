# Transaction Detail Inspection Surface

## Goal
As a finance user, I want to inspect a transaction fully from within the finance workspace, so I can understand its facts and structure without losing the surrounding ledger context.

## Outcome
Opening a transaction exposes a focused read-only detail surface that presents the backend-supported transaction facts, current categorization state, transfer state, split information, notes, and adjustment context without inventing a separate detail-read contract.

## In Scope
- A transaction detail surface opened from the ledger.
- Core fact display for:
  - amount
  - direction
  - account
  - occurred-at date-time
  - recorded-at date-time
- Categorization area showing either a single category or split breakdown.
- Transfer area showing linked transfer details when present.
- Notes area showing note history in read-only inspection form.
- Adjustment display when the transaction is a snapshot adjustment.
- Rendering the selected transaction from the current finance transaction read model already adopted in `004`.
- Read-only loaded inspection state plus recoverable fallback behavior when the targeted transaction cannot be resolved.

## Out Of Scope
- Counterparty or description display.
- Unlinking linked transfers.
- Categorization launch affordances.
- Split-edit launch affordances.
- Transfer-link launch affordances.
- Note-management launch affordances.
- Story-owned saving or mutation-busy states.

## Dependencies
- Depends on `003` because detail presentation must preserve finance return semantics defined by the shared overlay contract.
- Depends on `004` because the active inspection surface must match the adopted `FinanceTransaction` read model.
- Interacts with `008`, which later makes ledger rows open this detail surface.
- Interacts with `012`, `013`, and `014`, which separately introduce mutation affordances from inside the detail surface.

## Important Changes To Public Interfaces And Types
This story should rely on the current `FinanceTransaction` read model already adopted in `004`, rather than inventing a new detail API.

Required behavior boundary:
- no dedicated `GET /transactions/:id` detail endpoint is required in active scope
- detail content is rendered from the selected ledger transaction snapshot
- if the targeted transaction snapshot is unavailable when detail opens or restores, the detail surface shows recoverable fallback behavior

The inspection surface covers these existing read-model facts:
- amount
- direction
- account
- occurred-at
- recorded-at
- category or split breakdown
- transfer information when present
- notes content or history in read-only form
- adjustment context when present

## Implementation Decisions
- The detail surface should open over the ledger workspace rather than replace it conceptually.
- `011` becomes the first visible detail-content consumer of the overlay contract introduced in `003`.
- Desktop may use modal behavior with ESC and outside-click close semantics.
- Mobile may use a device-appropriate close affordance while keeping the same underlying transaction scope.
- The active detail view must reflect only the facts the backend currently returns.
- `011` is inspection-only and does not own mutation entrypoints or saving states.
- If the transaction is split, inspection shows the split breakdown instead of a misleading single-category summary.
- If the selected transaction cannot be resolved from the active transaction data, the detail surface should fail recoverably rather than break the finance workspace.

## Acceptance Criteria
- Opening a transaction shows its backend-supported core facts.
- If the transaction is split, the detail surface shows a split breakdown rather than a misleading single-category view.
- If the transaction is transfer-linked, the detail surface shows the linked transfer information.
- Existing notes are visible in the detail surface.
- Adjustment transactions display their adjustment context clearly.
- If the targeted transaction cannot be resolved, the detail surface shows explicit recoverable fallback behavior.
- The active story does not claim categorization, split-edit, transfer-link, or note-management entrypoints.

## Tests
- Integration tests covering inspection rendering of core facts, split state, transfer state, notes, and adjustment state.
- Integration tests covering recoverable fallback behavior when the targeted transaction snapshot is unavailable.
- Integration tests ensuring unsupported fields such as counterparty and description are not assumed by the active detail model.
- E2E tests verifying that a user can open transaction detail from the ledger, inspect its structure, and close it without losing context.
