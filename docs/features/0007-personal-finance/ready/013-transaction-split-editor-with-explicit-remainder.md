# Transaction Split Editor With Explicit Remainder

## Goal
As a finance user, I want to allocate one transaction across multiple categories while staying mathematically grounded, so the transaction’s interpretation matches reality without obscuring the original total.

## Outcome
Transaction detail can open a split editor that keeps the original transaction total explicit, lets the user construct or revise a category allocation, maintains a live remainder, enforces backend-valid split structure, and on successful save returns to detail with the updated split breakdown visible.

## In Scope
- Split-edit launch affordance from transaction detail.
- Split editor opened from detail for:
  - currently unsplit transactions
  - already split transactions
- Empty initial editor state for unsplit transactions, with explicit guidance to add split lines.
- First-line initialization for unsplit transactions when the user begins:
  - full transaction amount
  - current category or `Uncategorized`
- Loading and editing of existing split lines for already split transactions.
- Multiple split lines with amount and category.
- Explicit fixed transaction total and live remainder display.
- Add-line and remove-line behavior.
- Validation for backend-valid split structure:
  - at least two rows to save
  - full sum consistency
  - valid category selection
- Full-replacement save behavior.
- Successful save returns to detail inspection with updated split state visible.

## Out Of Scope
- Per-split notes.
- Percentage-based split entry.
- Split templates.
- Save-and-continue editing behavior.
- Returning directly to ledger on save.
- Zero-amount split lines as a valid saved result.

## Dependencies
- Depends on `011` detail surface for launch context.
- Interacts with `012` because split state replaces single-category interpretation.
- Depends on `004` because the backend split write shape is limited to rows of `amount` and `category`.
- Interacts with postponed story `019` because future richer report filters must remain coherent with proportional split semantics.
- Reuses `003` overlay-return semantics through the nested detail/edit flow, but owns the editor’s own save-result behavior.

## Important Changes To Public Interfaces And Types
This story should make these editor boundaries explicit:
- local split-editor state containing:
  - fixed transaction total
  - editable split rows
  - live derived remainder
  - save-enabled or invalid state
- split row shape aligned to backend contract only:
  - `amount`
  - `category`
- editor initialization modes:
  - empty unsplit-start state
  - existing split-loaded state

Do not introduce:
- row-level note fields
- percentage allocation fields
- any backend shape beyond `amount` and `category`
- a partially saved draft split separate from local editor state

## Implementation Decisions
- `013` owns the split-edit launch affordance from transaction detail; `011` only shows split state read-only.
- Unsplit initialization policy:
  - unsplit transactions open to an empty editor state with explicit add guidance
  - the first added line is initialized with full amount and current category or `Uncategorized`
  - save remains unavailable until the user has constructed a backend-valid split
- Existing-split policy:
  - already split transactions load their current rows directly into the editor
- Mathematical policy:
  - transaction total is fixed and always visible
  - remainder updates in real time as rows change
  - save is disabled whenever the row set is not backend-valid
- Validation policy:
  - a valid save requires at least two rows
  - row amounts must sum to the transaction total
  - categories must be valid selectable categories
  - zero-amount lines are not part of valid saved state
- Save policy:
  - save replaces the prior split interpretation entirely
  - successful save closes the editor and returns to detail
  - detail then shows the updated split breakdown
- Active split scope must not invent row-level notes that the backend does not support.
- Scope boundary:
  - this story owns split editing and save lifecycle
  - it does not own note editing, transfer linking, or generic category mutation

## Acceptance Criteria
- Opening the split editor for an unsplit transaction shows an empty state with guidance to add split lines.
- Adding the first line for an unsplit transaction initializes it with the full transaction amount and the current category, or `Uncategorized` if none exists.
- Opening the split editor for an already split transaction loads its existing rows.
- The detail surface exposes the split-edit launch affordance only through this story, not `011`.
- Editing line amounts updates the visible remainder immediately.
- A user can add and remove split lines while keeping the total explicit.
- Save is disabled until the split satisfies backend-valid structure, including at least two rows and full sum consistency.
- Saving a valid split replaces the prior interpretation and closes back to transaction detail.
- After save, transaction detail shows the updated split breakdown.

## Tests
- Integration tests for initialization:
  - unsplit transaction opens to empty editor state
  - first added line prefills full amount and current category or `Uncategorized`
  - already split transaction loads existing rows
- Integration tests for editing behavior:
  - live remainder updates as rows change
  - add-line and remove-line behavior
  - invalid state when fewer than two rows exist
  - invalid state when row amounts do not sum to total
- Integration tests for save behavior:
  - valid split save issues full-replacement payload
  - successful save closes back to detail
  - detail reflects updated split breakdown after save
- Integration tests for scope boundaries:
  - no per-row notes in editor
  - no percentage-entry controls
- E2E tests:
  - create a new split from an unsplit transaction
  - edit an existing split
  - save and verify updated split appears in detail and ledger indicators
