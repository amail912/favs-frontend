# Personal Finance API Contract

## Goal
Document the finance backend contract that already exists in `foucl2` so frontend stories can align to real routes, payloads, validations, and current limitations.

## Route Prefix
- All finance routes are under `/api/v1/finance`.

## Accounts
### List
- `GET /api/v1/finance/accounts?status=active|closed|all`
- Default backend behavior when `status` is omitted: `active`

Response fields per account:
- `id`
- `name`
- `status`

### Create
- `POST /api/v1/finance/accounts`

Request body:
- `name`

Validation and errors:
- empty name -> `400`
- duplicate normalized name -> `409`

### Close
- `POST /api/v1/finance/accounts/:accountId/close`

Errors:
- missing account -> `404`

## Categories
### List
- `GET /api/v1/finance/categories`

Response fields per category:
- `id`
- `name`
- `parentId`
- `owner`
- `selectable`

Notes:
- categories already support hierarchy through `parentId`
- built-in categories are read-only

### Create
- `POST /api/v1/finance/categories`

### Update
- `POST /api/v1/finance/categories/:categoryId`

Request body for create and update:
- `name`
- `parentId` optional

Validation and errors:
- empty name -> `400`
- invalid parent or parent cycle -> `400`
- built-in category update -> `409`
- missing category -> `404`

### Delete
- `DELETE /api/v1/finance/categories/:categoryId`

Errors:
- missing category -> `404`
- category cannot be deleted -> `409`

## Transactions
### Read Model
Transaction responses currently expose:
- `id`
- `direction` as `sent` or `received`
- `accountId`
- `amount`
- `occurredAt`
- `recordedAt`
- `transfer` optional
- `category` optional
- `splits`
- `notes`
- `adjustment` optional

Not currently present:
- `counterparty`
- `description`

### List
- `GET /api/v1/finance/transactions?accountId=&from=&to=`

Supported filters:
- `accountId` optional
- `from` optional, inclusive
- `to` optional, exclusive

Validation and behavior:
- invalid ISO timestamps -> `400`
- `from > to` -> `400`
- `from == to` -> empty result
- sorted by newest `occurredAt` first, then transaction id
- balance snapshot adjustments are included in the returned list as adjustment transactions

### Create Sent
- `POST /api/v1/finance/transactions/sent`

### Create Received
- `POST /api/v1/finance/transactions/received`

Headers:
- `Idempotency-Key` required

Request body:
- `accountId`
- `amount`
- `occurredAt` optional

Validation and errors:
- missing `Idempotency-Key` -> `400`
- amount must be positive -> `400`
- invalid `occurredAt` -> `400`
- missing account -> `404`
- closed account -> `409`
- reused idempotency key with different request -> `409`
- when `occurredAt` is omitted, backend defaults to current time

### Categorize
- `POST /api/v1/finance/transactions/:transactionId/categorize`

Request body:
- `category`

Validation and errors:
- transaction or category missing -> `404`
- category must reference a selectable category -> `400`
- categorization is rejected when the transaction already has an active split -> `409`

### Split
- `POST /api/v1/finance/transactions/:transactionId/split`

Request body:
- `splits`: array of rows with:
  - `amount`
  - `category`

Validation and errors:
- transaction or category missing -> `404`
- split must contain at least two rows, sum to the transaction amount, and use selectable categories -> `400`

Notes:
- split rows do not support per-row notes
- split writes are full replacement

### Link Transfer
- `POST /api/v1/finance/transactions/link`

Request body:
- `sourceTransactionId`
- `targetTransactionId`
- `linkType`

Validation and errors:
- only `linkType = transfer` is accepted
- missing transaction -> `404`
- invalid transfer pairing -> `409`
- already linked transaction -> `409`

Notes:
- linking is pairwise, not generic multi-transaction linking

### Notes
#### Create
- `POST /api/v1/finance/transactions/:transactionId/notes`

Request body:
- `text`

#### Update
- `PUT /api/v1/finance/transactions/:transactionId/notes/:noteId`

Request body:
- `text`

#### Delete
- `DELETE /api/v1/finance/transactions/:transactionId/notes/:noteId`

Validation and errors:
- blank text or text longer than 2000 characters -> `400`
- missing transaction or note -> `404`

Notes:
- notes are mutable on the backend today; they are not append-only

## Reports
### Aggregate Report
- `GET /api/v1/finance/report?from=&to=&direction=&accountIn=&accountNotIn=&categoryIn=&categoryNotIn=`

Required query parameters:
- `from`
- `to`

Optional query parameters:
- `direction` values: `sent`, `received`, `all`
- repeated `accountIn`
- repeated `accountNotIn`
- repeated `categoryIn`
- repeated `categoryNotIn`

Validation and behavior:
- invalid ISO timestamps -> `400`
- `from >= to` -> `400`
- overlapping include and exclude lists -> `400`
- transfer-linked transactions are excluded from the report base query
- category filters treat uncategorized transactions as `uncategorized`
- split transactions contribute proportionally through their split rows
- account filters apply to both normal transactions and balance snapshot adjustments

Response fields:
- `total`
- `count`
- `transactionIds`

Current limitation:
- the endpoint does not return chart buckets, category aggregations, account-balance summaries, or time-series data

## Export
- `GET /api/v1/finance/export`

Response includes:
- `formatVersion`
- canonical finance events
- account, category, transaction, and snapshot views

## Frontend Planning Implications
The following planning ambitions are not currently backed by the existing backend contract and should stay postponed until backend support exists:
- counterparty and description on transactions
- counterparty suggestions and suggestion-driven categorization
- text-search transaction filtering
- direction/category/amount-range ledger filtering unless implemented client-side over a loaded set deliberately
- generic link types beyond transfer
- chart-ready reports such as category breakdowns, cashflow time series, or balance cards
