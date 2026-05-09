# Ledger Indicators And Navigation To Transaction Detail

## Goal
As a finance user, I want to spot structurally important transactions quickly and open their full detail without losing my ledger context, so I can investigate anomalies efficiently.

## Outcome
Ledger rows expose explicit structural indicators for split, transfer, notes, and adjustments, and the entire row acts as the detail-open target. Opening and closing detail preserves the current ledger filter, ordering, and scroll context.

## In Scope
- Structural row indicators for:
  - split state
  - transfer-linked state
  - note presence
  - adjustment state when present
- Whole-row interaction model for opening transaction detail.
- Clear interactive affordance for ledger rows while keeping indicators informational rather than action-specific.
- Preservation of current ledger context when detail opens and closes:
  - active account and date filters
  - current ordering
  - current scroll position
- Device-appropriate interaction presentation that still keeps the row as the primary target on both desktop and mobile.

## Out Of Scope
- Detail content structure or actions inside detail.
- Categorization, split editing, transfer linking, or note management behavior.
- Defining the full transaction detail layout.
- Secondary per-row actions, hover-only affordances, or inline quick actions.
- Alternate row actions such as multi-select or swipe actions.

## Dependencies
- Depends on `006` ledger workspace.
- Depends on `007` active-context persistence.
- Depends on `011` for the detail surface itself.
- Depends on `003` because detail-open and detail-close must preserve finance workspace context using the overlay contract established there.
- Interacts with `014` because transfer-linked state shown in the ledger must reflect the backend’s transfer model.

## Important Changes To Public Interfaces And Types
This story should not add new route state or a new detail route.

Behavior and interface changes to make explicit:
- the ledger row view model now includes derived indicator visibility for:
  - split present
  - transfer present
  - notes present
  - adjustment present
- the ledger interaction model includes one row-open action that targets a specific transaction id
- the ledger page state must retain enough context to restore the same scroll position after the detail overlay closes

Do not introduce:
- separate per-indicator click targets
- a new URL parameter for selected transaction in this story
- a different detail-opening mechanism on mobile versus desktop

## Implementation Decisions
- Interaction policy:
  - the whole row opens detail
  - indicators are informative and do not behave as independent buttons
- Visual affordance policy:
  - rows should look actionable once this story is implemented
  - the actionability cue should be consistent across supported layouts
- Indicator policy:
  - split indicator reflects current backend `splits`
  - transfer indicator reflects current backend `transfer`
  - note indicator reflects presence of one or more notes
  - adjustment transactions are visibly distinguishable from ordinary transactions
- Context-preservation policy:
  - opening detail must not discard active ledger account or date context
  - closing detail must return to the same ordering and same scroll position
  - scroll restoration is required, not optional, for the supported UI model
- Navigation policy:
  - detail opening should feel like drilling into the current workspace, not leaving it
  - row-open behavior becomes the first visible detail entrypoint that reuses the detail overlay contract already established by `003`
- Scope boundary:
  - this story owns navigation into detail from the ledger, but not what the detail surface contains once opened

## Acceptance Criteria
- A transaction with splits displays a split indicator in the ledger.
- A transfer-linked transaction displays a linked indicator in the ledger.
- A transaction with notes displays a note indicator in the ledger.
- An adjustment transaction is visibly distinguishable from a normal money movement.
- Ledger rows are visibly actionable and the whole row opens detail.
- Opening a row opens the detail surface for the correct transaction.
- Closing detail returns the user to the same ledger context, including active account and date filters and ordering.
- Closing detail returns the user to the same scroll position in the ledger.
- Indicators remain informational and do not require separate click targets.
- This story does not introduce inline editing or secondary per-row actions.

## Tests
- Integration tests for indicator visibility rules:
  - split present
  - transfer present
  - notes present
  - adjustment present
- Integration tests for row interaction:
  - whole-row action opens the correct transaction detail target
  - indicators do not behave as separate navigation targets
- Integration tests for context preservation:
  - active ledger filters survive detail open and close
  - ordering survives detail open and close
  - scroll position is restored after detail close
- E2E tests:
  - open a transaction from the ledger by clicking or tapping the row
  - close detail and observe preserved filter and order context
  - close detail and observe return to the same scroll position
  - inspect transactions with split, transfer, note, and adjustment states and verify their indicators

## Implementation Notes
- Ledger rows are now the primary detail-open target: clicking a row emits a detail-open output carrying the transaction id.
- The `facts` cell now renders informational badges (`split`, `transfer`, `note`, `adjustment`) with no separate click targets.
- Opened detail uses the existing finance overlay contract in `Pages.App` via `FinanceDetailOverlay`, with a placeholder modal until story `011`.
- Window scroll position is captured before opening detail and restored when detail closes (close button or browser back).
