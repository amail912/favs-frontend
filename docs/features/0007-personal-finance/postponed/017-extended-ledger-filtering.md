# Extended Ledger Filtering

## Goal
As a finance user, I want more powerful ledger filtering, so I can narrow the transaction workspace by financial meaning rather than only by account and date.

## Why This Is Postponed
The current backend transaction list contract supports only `accountId`, `from`, and `to`. Richer filtering should not be promised as active scope until the backend contract or deliberate client-side filtering strategy is defined.

## Deferred Scope
- Direction filter.
- Category filter on the ledger list path.
- Amount-range filter.
- Text search.
- Any filter behavior that relies on counterparty or description.

## Backend Evolution Needed
At minimum one of the following must be decided explicitly:
- extend the transaction list endpoint with richer query parameters
- or define a deliberate frontend-local filtering policy over a bounded loaded dataset
