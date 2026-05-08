# Finance Route Entry And Guarded Navigation

## Goal
As a finance user, I want finance routes to exist as first-class authenticated app routes, so I can enter the finance workspace directly and navigate to it coherently from the rest of the application.

## Outcome
The frontend app route model recognizes finance routes, prints and parses them consistently, exposes one authenticated `Finance` navigation entry, and resolves `/finance` to the canonical finance transactions landing route.

## In Scope
- Add finance routes in app routing at:
  - `/finance`
  - `/finance/transactions`
  - `/finance/reports`
- Add one authenticated app-level `Finance` navigation entry.
- Enforce authenticated-only access for finance routes through the existing guarded-route model.
- Canonicalize `/finance` to the finance transactions landing route.
- Keep route behavior coherent on reload and direct URL access.
- Add the minimal finance route outputs needed to host later stories `002` through `015`.

## Out Of Scope
- Finance-local shell chrome.
- Finance `+` action visibility policy.
- Nested create or detail flow return behavior.
- Backend contract definition.
- Account-context handoff.
- Separate app-level `Transactions` or `Reports` tabs.

## Dependencies
- Depends on the existing authenticated app-shell routing support in `Pages.App`.
- Must integrate with the current frontend auth session initialization, signin, and signout flow in `Pages.App`.
- Must remain coherent with the existing guarded-route pattern already used for `Admin`.

## Implementation Decisions
- Route shape:
  - `/finance/transactions` is the canonical transactions route
  - `/finance/reports` is the canonical reports route
  - `/finance` is a supported convenience entry URL only
- Canonicalization policy:
  - direct navigation to `/finance` resolves to the finance transactions route and normalizes the visible URL to `/finance/transactions`
- App navigation policy:
  - the authenticated app shell shows exactly one `Finance` tab
  - activating that tab navigates to `/finance/transactions`
  - `Transactions` and `Reports` are not separate app-level tabs in this story
- Guard policy:
  - authenticated users may access finance routes
  - unauthenticated direct navigation to finance routes resolves to `NotFound`, not redirect
  - while auth status is unknown, guarded finance routes remain unresolved until auth refresh completes
- Route-model policy:
  - finance routes should be added through the existing app route system rather than a separate routing mechanism
  - route parse and print behavior must remain symmetric for canonical finance routes
- Initial landing scope for this story:
  - finance routes only need minimal route ownership and reachability
  - full finance-local shell behavior belongs to `002`

## Acceptance Criteria
- `parseRouteString "/finance/transactions"` resolves to the finance transactions route.
- `parseRouteString "/finance/reports"` resolves to the finance reports route.
- `parseRouteString "/finance"` resolves to the finance transactions target used for canonical landing.
- Printing the finance transactions route yields `/finance/transactions`.
- Printing the finance reports route yields `/finance/reports`.
- Authenticated user sees exactly one `Finance` entry in the app-level navigation.
- Unauthenticated user never sees the `Finance` navigation entry.
- Direct unauthenticated navigation to `/finance`, `/finance/transactions`, or `/finance/reports` resolves to the existing `404` page behavior.
- Reloading a finance route with an active authenticated session keeps access stable after auth refresh completes.
- Reloading a finance route while auth status is still unknown does not briefly render finance content before auth refresh resolves.
- Entering finance from the app tab lands on `/finance/transactions`.
- Direct navigation to `/finance` normalizes to `/finance/transactions`.

## Tests
- Integration:
  - route parsing covers `/finance`, `/finance/transactions`, and `/finance/reports`
  - route printing covers canonical finance route paths
  - authenticated navigation renders `Finance` only for authenticated users
  - guarded finance routes remain unresolved while auth is unknown
  - guarded finance routes render `NotFound` for unauthenticated state
  - guarded finance routes render the requested finance route for authenticated state
  - direct `/finance` entry normalizes to `/finance/transactions`
- E2E:
  - authenticated user sees `Finance`, opens it, and lands on `/finance/transactions`
  - unauthenticated direct URL access to `/finance/transactions` shows `404`
  - authenticated reload on a finance route keeps access stable after auth refresh
