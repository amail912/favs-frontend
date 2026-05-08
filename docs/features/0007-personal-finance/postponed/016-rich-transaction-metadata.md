# Rich Transaction Metadata

## Goal
As a finance user, I want richer transaction context such as counterparty and description, so I can capture and review real-world meaning directly on each transaction.

## Why This Is Postponed
The current backend transaction contract does not expose or accept `counterparty` or `description`, and it does not expose historical counterparty data for suggestion workflows.

## Deferred Scope
- Counterparty on transaction create and read models.
- Description on transaction create and read models.
- Counterparty suggestions during capture.
- Counterparty-driven category suggestion UX.
- Ledger and detail rendering that relies on those fields.

## Backend Evolution Needed
- Transaction read model must expose counterparty and description.
- Transaction create or update paths must accept those fields.
- Suggestion endpoints or equivalent backend-derived data are needed for ranked counterparty assistance.
