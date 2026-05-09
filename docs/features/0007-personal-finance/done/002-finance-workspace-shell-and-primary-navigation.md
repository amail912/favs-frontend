# Finance Workspace Shell And Primary Navigation

## Goal
As a finance user, I want a coherent finance-local shell with clear primary navigation, so I can move between Transactions and Reports without reconstructing where those surfaces live.

## Outcome
The finance feature exposes a shared workspace shell with a dedicated finance-local sub-navigation row for `Transactions` and `Reports`, and one consistent capture-entry policy where the finance `+` action is visible on `Transactions` only.

## In Scope
- Shared finance shell rendered when the current route is inside finance.
- Dedicated finance-local sub-navigation row for:
  - `Transactions`
  - `Reports`
- Active-state rendering for the current finance primary surface.
- Local navigation behavior between the two finance primary surfaces.
- Shared visibility and placement policy for the finance `+` action.
- Behavior-level parity across mobile and desktop.

## Out Of Scope
- App-level `Finance` tab behavior.
- Nested create flow behavior.
- Transaction detail behavior.
- Browser-history semantics for nested flows.
- Account-context handoff specifics.
- Form fields or backend write behavior.

## Dependencies
- Depends on `001` finance route entry and guarded navigation.
- Interacts with `004` because the shell should only advertise backend-supported active finance surfaces.
- Precedes `006` and `015` because the ledger and reports surfaces need a shared finance host shell.
- Leaves nested-flow return semantics to `003`.

## Implementation Decisions
- Finance shell appears only when the active route is inside finance.
- The finance-local shell is separate from the global app tabs.
- The finance-local navigation is a dedicated local sub-navigation row, not only header links.
- The local sub-navigation has exactly two entries in this story:
  - `Transactions`
  - `Reports`
- Active-state policy:
  - finance transactions route activates `Transactions`
  - finance reports route activates `Reports`
- Finance `+` policy:
  - visible on `Transactions`
  - hidden on `Reports`
  - hidden rather than disabled when not available
- The shell must not introduce extra navigation entries for postponed finance features.
- Mobile and desktop may differ visually, but not in shell semantics, available destinations, or `+` visibility policy.

## Acceptance Criteria
- When the user is on a finance route, the finance-local shell is visible.
- The shell shows a dedicated local navigation row for `Transactions` and `Reports`.
- The currently active finance surface is visually clear in the local navigation.
- From `Transactions`, the user can navigate to `Reports`.
- From `Reports`, the user can navigate to `Transactions`.
- On the finance transactions surface, the finance `+` action is visible.
- On the finance reports surface, the finance `+` action is not visible.
- The shell behavior is consistent on mobile and desktop, even if presentation differs.
- Non-finance routes do not render the finance-local shell.

## Tests
- Integration:
  - finance routes render the finance-local shell
  - non-finance routes do not render the finance-local shell
  - finance transactions route activates `Transactions`
  - finance reports route activates `Reports`
  - local navigation from transactions goes to reports
  - local navigation from reports goes to transactions
  - finance `+` is visible on transactions
  - finance `+` is hidden on reports
- E2E:
  - authenticated user enters finance and sees local `Transactions` and `Reports` navigation
  - user switches between the two finance primary surfaces
  - `+` is visible on transactions and absent on reports
