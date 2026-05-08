# Report/Ledger Exact Drill-Down And Richer Report Filters

## Goal
As a finance user, I want richer report controls and exact drill-down into the supporting transactions, so I can analyze beyond one date-scoped aggregate without the ledger view becoming approximate or misleading.

## Status
Postponed.

## Reason
The current backend report endpoint supports a richer query surface than the active ledger filter contract.

Today:
- reports can query by `direction`, account include/exclude, and category include/exclude
- the active ledger contract from `007` intentionally supports only:
  - `accountId`
  - `from`
  - `to`

Because of that mismatch, exact drill-down from richer report filters would require one of:
- extending the ledger contract and URL model beyond the scope chosen in `007`
- or introducing a separate exact transaction-set drill-down mode with its own persistence and restoration semantics

The first ready version of `015` therefore stayed date-range-only so drill-down could remain exact through the current ledger model.

This was deferred from `015` because exposing richer report controls before the ledger can represent the same scope would make drill-down diverge from the visible ledger context. The first pass favored exactness over broader filter coverage.

## Follow-Up Scope
- Richer active report controls such as:
  - direction
  - account include/exclude
  - category include/exclude
- Exact drill-down that remains faithful to those richer report filters.
- A documented decision on how exact drill-down state is represented:
  - extended ledger filter contract
  - or exact transaction-set navigation mode
- Explicit reconciliation with the current backend semantics for:
  - transfer-linked transaction exclusion
  - proportional split contribution
- Explicit handling of category-filtered reporting once category include and exclude controls return, including how split transactions contribute under those richer filters.

## Why This Was Not In `007`
`007` intentionally matched the current `/api/v1/finance/transactions` list contract and kept the ledger URL/query model limited to what the ledger endpoint can express directly.

That decision kept ledger context:
- explainable
- reload-safe
- browser-history-safe

But it also means richer report filters cannot yet drill down exactly through the same ledger model.
