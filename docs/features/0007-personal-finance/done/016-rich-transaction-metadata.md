# Rich Transaction Metadata

## Goal
As a finance user, I want richer transaction context such as counterparty and description, so I can capture and review real-world meaning directly on each transaction.

## How This Story Achieves The Goal
This story introduces metadata capture and editing across the finance workflow using the backend contract that now exists, and adds counterparty suggestions to speed up entry while keeping financial meaning explicit.

The user can:
- Capture `counterparty` and `description` at transaction creation time.
- See metadata in ledger/detail transaction inspection.
- Edit metadata after creation.
- Use backend-ranked counterparty suggestions during capture/edit.

## Technical Details
- Backend transaction read model now includes:
  - `counterparty`
  - `description`
- Create endpoints accept optional metadata fields:
  - `POST /api/v1/finance/transactions/sent`
  - `POST /api/v1/finance/transactions/received`
  - optional request fields: `counterparty`, `description`
- Metadata update endpoint is available:
  - `POST /api/v1/finance/transactions/:transactionId/metadata`
  - request supports partial updates:
    - `counterparty?: string | null`
    - `description?: string | null`
  - omitted field preserves previous value
  - unknown transaction returns `404`
- Metadata normalization and validation behavior must match backend:
  - `counterparty` is trimmed and lowercased
  - `description` is trimmed
  - blank strings normalize to `null`
  - overlength/invalid metadata payloads return `400`
- Counterparty suggestions endpoint:
  - `GET /api/v1/finance/counterparties/suggest?q=&limit=&direction=&accountId=`
  - response item fields:
    - `value`
    - `usageCount`
    - `lastUsedAt`
    - `suggestedCategory`
  - behavior:
    - missing or empty `q` returns empty `items`
    - default `limit` is `8`
    - `limit` is capped at `20`
    - `direction` supports `sent`, `received`, `all`
    - duplicated `accountId` query parameter returns `400`
    - unknown `accountId` returns empty `items`
- This story uses suggestion data as assistance only:
  - no silent auto-categorization
  - no automatic category mutation from `suggestedCategory`
- Out of scope:
  - richer ledger filter contract work (`017`)
  - analytical report datasets (`018`)
  - report/ledger exact rich drill-down work (`019`)

## Tests
- Integration tests for create payload support:
  - sent/received create with metadata succeeds
  - metadata appears on returned transaction payload and subsequent reads
- Integration tests for metadata update:
  - both-field update
  - partial update preserving omitted field
  - blank-to-null normalization
  - lowercase counterparty normalization
  - invalid payload and overlength validation failures (`400`)
  - unknown transaction (`404`)
- Integration tests for suggestion endpoint:
  - empty/missing `q` behavior
  - invalid `limit`/`direction` behavior
  - duplicate `accountId` behavior
  - direction/account filtering behavior
  - response shape includes `suggestedCategory`
- E2E tests for user workflow:
  - capture with metadata from finance create flow
  - metadata visible in ledger/detail
  - metadata editable after create
  - suggestion selection assists entry without silently changing category

## Implementation Notes (2026-05-12)
- Delivered metadata support in frontend finance contract and API helpers for create and metadata-update flows.
- Added counterparty and description capture in transaction create, metadata editing in transaction detail, and metadata visibility in ledger rows.
- Integrated backend counterparty suggestions into create flow as assistive input only, without automatic category mutation.
