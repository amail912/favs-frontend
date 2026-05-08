# Rich Analytical Reports

## Goal
As a finance user, I want analytical views such as category breakdowns, cashflow timelines, and balance-oriented report cards, so I can understand patterns over time instead of only consulting one aggregate total.

## Why This Is Postponed
The current backend report endpoint returns aggregate `total`, `count`, and `transactionIds` only. It does not provide chart-ready buckets, series, or balance summaries.

## Deferred Scope
- Category breakdown visualizations.
- Cashflow time series by day, week, or month.
- Account-balance cards inside the reports workspace.
- Parent/subcategory analytical groupings.
- Rich chart interactions that depend on backend bucketed results.

## Backend Evolution Needed
- Report endpoints that return category aggregations and/or time buckets.
- Explicit balance-report payloads if balance cards belong in the reports workspace.
- Contract decisions for drill-down from analytical buckets to ledger context.
