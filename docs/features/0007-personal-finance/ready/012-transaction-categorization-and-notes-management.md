# Transaction Categorization And Notes Management

## Goal
As a finance user, I want to refine a transaction’s interpretation and manage its notes from detail view, so I can keep the record useful while staying consistent with the backend capabilities that exist today.

## Outcome
Transaction detail exposes mutation affordances for single-category refinement and full note management, applies optimistic updates where safe, reconciles against the current backend behavior, and handles split transactions honestly without pretending they still have one editable effective category.

## In Scope
- Category-change affordance and interaction for non-split transactions.
- Visible but disabled category-change affordance for split transactions, with clear explanation.
- Showing the latest effective visible category after successful categorization.
- Note-management affordances from detail for:
  - add
  - update
  - delete
- Optimistic or immediate local UI updates for safe category and note mutations.
- Error handling that preserves user intent and restores coherence when optimistic work fails.

## Out Of Scope
- Append-only note policy.
- Bulk recategorization.
- Split editing.
- Transfer unlinking.
- Counterparty-based or suggestion-driven categorization.
- Rich note workflows such as threading, pinning, or attachments.

## Dependencies
- Depends on `011` transaction detail surface.
- Depends on `004` because active note management must match the backend’s mutable note API.
- Interacts with `013` because split transactions should point users toward split editing rather than misleading single-category mutation.
- Must remain coherent with `008` and `003` so ledger and detail context survives while these mutations occur.

## Important Changes To Public Interfaces And Types
This story should make these interaction boundaries explicit:
- mutation affordances inside detail for:
  - category change
  - note add, update, and delete
- local mutation state sufficient to represent:
  - pending category change
  - pending note add, update, or delete
  - recoverable mutation error
- optimistic reconciliation behavior against the existing read model:
  - category
  - notes

Do not introduce:
- note append-only frontend semantics
- category mutation for active split transactions
- a new backend detail endpoint
- note or category shapes beyond the adopted finance contract

## Implementation Decisions
- Categorization policy:
  - single-category change is available only when the transaction is not currently represented by an active split
  - categorization appends a new interpretation event; the latest successful categorization becomes the effective visible category
- Split-state policy:
  - if a transaction has an active split, the category-change affordance remains visible but disabled
  - the disabled state must explain that split transactions are edited through split management rather than single-category change
- Notes policy:
  - active frontend scope must match backend truth: notes are mutable, so add, update, and delete are all first-class
- Update model:
  - use optimistic updates where safe for category and note mutations
  - if backend confirmation fails, restore or reconcile detail state visibly and keep the user able to recover
- Error policy:
  - note-management failures preserve entered note intent when practical
  - categorization failures leave the prior effective category coherent and visible
- Scope boundary:
  - `012` owns mutation affordances and mutation-state behavior for category and notes
  - `011` remains inspection-only and should not claim these affordances

## Acceptance Criteria
- A user can change the category of a non-split transaction from transaction detail.
- After categorization succeeds, the detail surface reflects the latest effective category.
- If the transaction has an active split, the category-change affordance is visible but disabled with clear guidance toward split editing.
- A user can add, update, and delete notes from transaction detail.
- Category and note mutations update the detail UI immediately where safe, then reconcile with backend results.
- If a category or note mutation fails, the detail surface shows recoverable feedback and restores coherent visible state.
- The detail surface exposes category-change and note-management affordances only through this story, not `011`.
- The active story documentation does not claim notes are append-only when the backend allows update and delete.

## Tests
- Integration tests for categorization:
  - non-split transaction can open and apply category change
  - successful categorization updates latest effective visible category
  - split transaction shows visible disabled category-change affordance with explanation
  - failed categorization restores coherent prior category state
- Integration tests for notes:
  - add note optimistic path
  - update note optimistic path
  - delete note optimistic path
  - failed add, update, or delete restores coherent notes state and preserves recoverable user intent where applicable
- Integration tests for scope boundaries:
  - `011` inspection surface does not independently expose these affordances without `012`
  - split transactions do not allow misleading single-category mutation
- E2E tests:
  - categorize a non-split transaction from detail
  - add, edit, and delete notes from detail
  - verify split transaction shows disabled category-change affordance and guidance toward split editing
