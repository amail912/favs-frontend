# Transaction Transfer Linking

## Goal
As a finance user, I want to explicitly link two transactions that represent a transfer, so the system can distinguish transfers from ordinary spending or income without hiding how that relationship was created.

## Outcome
Transaction detail exposes a transfer-link affordance that opens a dedicated in-flow transaction selector, lets the user choose one target transaction, submits a pairwise `transfer` link request, and then reflects the resulting linked state back into transaction detail and ledger indicators.

## In Scope
- Transfer-link launch affordance from transaction detail.
- Dedicated overlay or nested selector flow for choosing the target transaction within finance.
- Candidate browsing or searching inside that selector.
- Single-target selection for pairwise transfer linking only.
- Light prefiltering of obviously impossible targets before submit.
- Using the current backend transfer link type.
- Final backend validation and visible error handling for rejected link attempts.
- Reflecting successful linked state back into:
  - current transaction detail
  - ledger transfer indicators

## Out Of Scope
- Generic link types beyond transfer.
- Multi-transaction link groups.
- Unlinking existing transfers.
- Automatic inferred linking.
- Cross-page or cross-route selection flows.
- Full frontend reimplementation of backend transfer-pair validation.

## Dependencies
- Depends on `011` detail surface.
- Depends on `008` so transfer-linked state is visible in the ledger.
- Depends on `004` because the current backend only accepts pairwise `transfer` linking.
- Interacts with `015` because transfer-linked transactions are excluded from the current report aggregate base query.
- Reuses `003` overlay and return semantics for the nested selection flow.

## Important Changes To Public Interfaces And Types
This story should make these boundaries explicit:
- transfer-link launch state from detail
- in-flow target-selector state
- selected source and target transaction identities
- pairwise transfer-link submit payload using only:
  - `sourceTransactionId`
  - `targetTransactionId`
  - `linkType = transfer`

Do not introduce:
- a generic link-type picker
- multi-target selection
- a separate route for transfer linking
- frontend-only transfer relationship semantics that diverge from the backend contract

## Implementation Decisions
- Linking model:
  - active scope is pairwise transfer linking only
  - link type is fixed to backend `transfer`
- Launch model:
  - `014` owns the transfer-link affordance from detail
  - `011` remains read-only and only shows transfer-linked information once it exists
- Target selection model:
  - choosing the second transaction happens inside a finance overlay selector
  - the user does not type raw transaction ids
  - the flow does not bounce back to the ledger for selection
- Candidate filtering model:
  - apply only light prefiltering where frontend confidence is high
  - exclude the current source transaction
  - exclude already-linked transactions when current data makes that obvious
  - allow backend validation to reject subtler invalid pairings such as unsupported direction, account, or timing combinations
- Validation model:
  - backend validation remains authoritative
  - validation feedback should explain backend-driven failures such as already-linked target or invalid transfer pairing
- Success model:
  - after successful link, the detail surface reflects the linked relationship
  - ledger indicators also reflect linked state when the user returns to the ledger context
- Scope boundary:
  - this story owns the mutation flow and linked-state reflection
  - it does not own unlinking or broader link lifecycle management

## Acceptance Criteria
- A user can initiate transfer linking from transaction detail.
- The detail surface exposes the transfer-link launch affordance only through this story, not `011`.
- The transfer-link flow opens a dedicated in-flow selector for the target transaction.
- The selector does not offer the current source transaction as a target.
- The selector applies only light prefiltering and still relies on backend validation for final correctness.
- Selecting a target submits a pairwise transfer-link request using the current backend contract.
- If the backend rejects the link request, the UI shows recoverable validation feedback and keeps the user in a coherent flow.
- After a successful link, transaction detail shows the linked transfer state.
- After a successful link, ledger indicators show the transfer-linked state.
- The v1 UI does not expose unlinking or non-transfer link types.

## Tests
- Integration tests for selector behavior:
  - launch from detail opens the target-selector flow
  - source transaction is excluded from candidate targets
  - obviously already-linked targets are excluded when current data supports that
- Integration tests for request behavior:
  - submit uses `sourceTransactionId`, `targetTransactionId`, and fixed `linkType = transfer`
  - backend validation failures surface recoverable feedback
- Integration tests for success reflection:
  - successful link updates transfer-linked state in detail
  - successful link updates transfer-linked indicator state for ledger rendering
- E2E tests:
  - start from detail, choose a target through the selector, create a transfer link
  - attempt an invalid link and verify visible recoverable feedback
  - return to ledger and verify linked-state indicator is visible
