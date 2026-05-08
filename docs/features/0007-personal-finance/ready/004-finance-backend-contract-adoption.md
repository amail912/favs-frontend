# Finance Backend Contract Adoption

## Goal
As a finance implementer, I want one typed frontend finance contract layer aligned to the backend that already exists, so later finance UI stories can build against stable request helpers, payload decoders, and explicit backend constraints.

## Outcome
The frontend has an explicit finance API contract document plus initial `Api.Finance*` scaffolding and shared finance data types aligned to `/api/v1/finance`, and the active finance backlog is constrained to those supported backend shapes and capabilities.

## In Scope
- Keep the feature-level API contract doc as the source of truth for `/api/v1/finance`.
- Introduce initial frontend finance API modules and shared finance contract types for the backend surfaces that active stories depend on:
  - accounts list
  - categories list
  - transactions list
  - transaction create `sent` and `received`
  - transaction categorize
  - transaction split
  - transaction transfer link
  - transaction note create, update, and delete
  - aggregate report
- Define the initial shared read and write types and JSON encoders and decoders for those surfaces.
- Define frontend-visible backend constraints that later stories must respect:
  - no `counterparty`
  - no `description`
  - transaction list filters only `accountId`, `from`, `to`
  - create uses direction-specific endpoints plus required `Idempotency-Key`
  - notes are mutable
  - linking is transfer-only
  - reports return `total`, `count`, `transactionIds`
- Align downstream story expectations to the typed contract layer rather than to ad hoc payload assumptions.

## Out Of Scope
- Backend changes.
- Any finance page UI.
- Accounts creation or close UI.
- Categories create, update, or delete UI.
- Export UI.
- Rich client-side domain interpretation beyond what active UI stories require.
- Client-side caching, optimistic updates, or retry policy beyond what later stories define.

## Dependencies
- Depends on the current finance backend already present in `foucl2`.
- Follows `001` through `003` in delivery order because finance route and shell ownership are already established.
- Precedes `006` through `015` because those stories should consume shared finance request helpers and types instead of defining their own.
- Reuses existing repo conventions:
  - `Api.*` modules remain technical
  - JSON contract and types live near the finance API modules
  - page and domain modules own UI semantics and business decisions

## Important Changes To Public Interfaces And Types
The initial finance API surface should follow the repo’s current module style.

Recommended module boundary:
- `Api.Finance`
  - request helpers for active-scope endpoints
- `Api.FinanceContract`
  - route helpers and shared request and response types
  - JSON encode and decode instances near those types

Minimum shared types to define now:
- `FinanceAccount`
- `FinanceCategory`
- `FinanceTransaction`
- `FinanceTransactionDirection`
- `FinanceTransferLink`
- `FinanceTransactionCategory`
- `FinanceTransactionSplitRow`
- `FinanceTransactionNote`
- `FinanceTransactionAdjustment`
- `CreateFinanceTransaction`
- `CategorizeFinanceTransaction`
- `SplitFinanceTransaction`
- `LinkFinanceTransfer`
- `CreateFinanceTransactionNote`
- `UpdateFinanceTransactionNote`
- `FinanceReportQuery`
- `FinanceAggregateReport`

Minimum request helper coverage to define now:
- `getFinanceAccountsResponse`
- `getFinanceCategoriesResponse`
- `getFinanceTransactionsResponse`
- `createSentFinanceTransactionResponse`
- `createReceivedFinanceTransactionResponse`
- `categorizeFinanceTransactionResponse`
- `splitFinanceTransactionResponse`
- `linkFinanceTransferResponse`
- `createFinanceTransactionNoteResponse`
- `updateFinanceTransactionNoteResponse`
- `deleteFinanceTransactionNoteResponse`
- `getFinanceAggregateReportResponse`

Do not introduce helpers yet for unsupported or inactive surfaces such as:
- account creation or close UI flows
- category management UI flows
- export UI

## Implementation Decisions
- Active frontend scope must match the backend that exists today.
- `004` establishes only the contract and technical API layer required for active stories.
- Unsupported or postponed product ambitions stay out of the typed surface for now.
- `Api.Finance*` is technical only; page and domain modules decide UX semantics such as empty states, retries, or interpretation rules.
- Frontend types should mirror backend truth closely rather than normalize away important distinctions.
- Transaction create uses separate sent and received request helpers instead of one generic create helper with direction embedded in the payload.
- Only backend-supported transaction-list filters are modeled in the active query type:
  - `accountId`
  - `from`
  - `to`
- Finance create helpers must require an `Idempotency-Key` input at the API boundary.
- Later UI stories decide how to generate and persist that key per submit attempt.
- Report helpers model only aggregate report inputs and outputs already supported by the backend.
- Category buckets, time series, and balance cards are not represented in active types.
- Active contract reflects backend-mutability truth for notes: create, update, and delete are all available.
- Active contract reflects transfer-only linking; no generic link abstraction is introduced now.
- Downstream finance stories should depend on `004` for payload truth instead of restating route details unless needed for UX-specific error handling.

## Acceptance Criteria
- The finance feature contains an explicit API contract document for `/api/v1/finance`.
- The frontend codebase has initial `Api.Finance*` modules for the active finance backend surfaces used by planned stories.
- Shared finance request and response types exist for active-scope endpoints and match backend-supported fields.
- Transaction create is represented as separate sent and received requests and requires `Idempotency-Key` at the API helper boundary.
- Active transaction list query types support only `accountId`, `from`, and `to`.
- Active contract types do not include unsupported fields such as `counterparty` or `description`.
- Active contract types and helpers reflect mutable notes, transfer-only linking, full-replacement splits, and aggregate report responses of `total`, `count`, and `transactionIds`.
- Downstream finance stories can depend on `004` for backend truth without inventing additional unsupported payload capabilities.
- Unsupported planning ambitions remain represented only in postponed stories, not in active finance API types.

## Tests
- Unit and integration coverage for finance JSON decoding and encoding:
  - accounts list
  - categories list
  - transaction read model
  - create sent and received payloads
  - categorize payload
  - split payload and response decoding where applicable
  - transfer link payload
  - note create and update payloads
  - aggregate report query encoding and response decoding
- Contract-shape tests for important backend constraints:
  - transaction list query supports only `accountId`, `from`, `to`
  - create helpers require explicit idempotency-key input
  - active transaction type excludes `counterparty` and `description`
  - aggregate report type is limited to `total`, `count`, and `transactionIds`
- Documentation review:
  - `api-contract.md` and story `004` stay consistent
  - downstream active stories rely on the adopted contract, not unsupported assumptions
