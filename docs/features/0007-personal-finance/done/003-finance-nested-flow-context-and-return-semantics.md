# Finance Nested Flow Context And Return Semantics

## Goal
As a finance user, I want nested create and detail flows to preserve my finance workspace context, so I can inspect or act on a transaction and return to the same working place afterward.

## Outcome
Finance create and detail flows open as local modal or sheet overlays on top of the current finance route, preserve the active finance workspace context while open, and close with one consistent return and browser-history contract. In the first implementation pass, that contract is exercised through the finance create flow, while later detail stories adopt the same behavior without redefining it.

## In Scope
- Nested finance create flow opened from finance surfaces.
- Local overlay state model for those flows.
- Return behavior after close, cancel, and successful submit.
- Preservation of finance workspace context while overlays are open.
- Browser-history behavior for finance modal or sheet overlays using the repo’s shared modal-history pattern.
- Behavior-level parity across mobile and desktop.
- Internal overlay scaffolding that later detail stories reuse.

## Out Of Scope
- Create form fields and validation.
- Transaction detail content structure.
- Account-context entrypoint definition itself.
- Route-driven nested finance subroutes.
- Dedicated full-page create or detail screens.

## Dependencies
- Depends on `002` finance workspace shell and primary navigation.
- Interacts with `007` because account and date context must survive finance nested flows once the ledger establishes that context.
- Precedes `009`, `010`, `008`, and `011` because create and detail stories should reuse one return contract instead of inventing their own.
- Reuses the repo’s established modal-history behavior already documented in `0004-011`.

## Implementation Decisions
- Interaction model:
  - finance create and detail are local overlay state
  - they are not route-driven subpages
  - they are not separate full pages
- Underlying route policy:
  - opening create or detail does not change the current finance route
  - the underlying route remains the active finance primary surface such as transactions or reports
- Context-preservation policy:
  - at minimum, preserve the active finance primary surface while the overlay is open
  - once the ledger establishes account or date context, that context must also survive overlay open and close
- Return policy:
  - cancel closes the overlay and returns to the same finance workspace state
  - explicit close closes the overlay and returns to the same finance workspace state
  - successful submit closes the overlay and returns to the same finance workspace state unless a later story explicitly defines a different post-success behavior
- Browser-history policy:
  - if the overlay is modal or sheet based, opening it pushes a same-URL history entry
  - browser back closes the currently open finance overlay with cancel semantics
  - back-triggered close never validates or submits pending actions
  - manual overlay close consumes the modal back-navigation entry through the shared modal-history pattern
- Presentation policy:
  - desktop and mobile may render different overlay surfaces, but both follow the same open, close, and return contract
- Scope boundary:
  - this story defines the shared contract and validates it through the create flow first
  - later finance stories must reuse it instead of redefining return semantics locally

## Acceptance Criteria
- Opening a finance create flow from a finance surface preserves the active underlying finance route while the overlay is open.
- Canceling a finance overlay returns the user to the same finance workspace state they came from.
- Explicitly closing a finance overlay returns the user to the same finance workspace state they came from.
- Successful submit from a finance overlay returns the user coherently to the underlying finance workspace state.
- Browser back closes an open finance modal or sheet overlay instead of navigating away.
- Back-triggered close does not validate or submit pending finance actions.
- Manual overlay close keeps browser history coherent with no stale extra back step.
- Mobile and desktop preserve the same return semantics even if the overlay surfaces differ visually.
- The create flow is the first executable consumer of this contract; detail stories later adopt the same semantics without introducing route-driven overlays.

## Tests
- Integration:
  - create overlay open and close preserves underlying finance surface
  - cancel returns to prior finance context
  - explicit close returns to prior finance context
  - success returns coherently to prior finance context
  - opening finance overlay arms modal back navigation
  - browser back closes overlay with cancel semantics
  - manual close consumes modal back navigation
- E2E:
  - open finance create, press browser back, verify overlay closes and no action is submitted
  - reopen finance create, close it manually, and verify browser history has no stale modal step
  - simulate successful finance create close and verify the finance route remains stable
