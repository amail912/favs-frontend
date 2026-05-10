# Record-Transaction Form Validation And Submission UX

## Goal
As a finance user, I want transaction capture to be fast but safe, so I can record money events quickly without creating duplicate or invalid entries.

## Outcome
The create overlay presents a backend-compatible transaction form, validates the active fields locally, submits through the direction-specific backend create path with idempotency protection, preserves user input on failure, and applies explicit success behavior: mobile closes and shows a lightweight success toast, while desktop stays open for repeat entry with selective field preservation.

## In Scope
- Create form fields for the active backend-supported create payload:
  - amount
  - account
  - occurred-at date-time
  - direction shown as fixed by the chosen create intent from `009`
- Local validation and invalid-state blocking for:
  - required account
  - required amount
  - positive amount
  - parseable occurred-at when provided by the UI
- Submit loading and disabled behavior while request is in flight.
- Duplicate-submit protection using the required `Idempotency-Key`.
- Error behavior that keeps user input intact and recoverable.
- Post-success behavior on both device classes:
  - mobile close plus lightweight toast
  - desktop repeat-entry mode
- Minimal generic toast UI capability needed to show the mobile success confirmation.

## Out Of Scope
- Counterparty, description, or suggestion fields.
- Editing the chosen direction inside the form.
- Offline submission.
- Batch capture flows beyond repeated desktop entry.
- Generic warning or error toast adoption across the whole app beyond the minimal reusable toast primitive needed here.

## Dependencies
- Depends on `004` backend contract adoption.
- Depends on `009` entrypoint behavior.
- Depends on transaction create backend behavior, including the required `Idempotency-Key` header.
- Depends on `003` because successful submit and cancel paths must return coherently to finance context.
- Interacts with `007` because the form consumes supported ledger context defaults.
- May introduce a shared UI toast primitive in `Ui.*`, but the finance create flow remains the only story-owned consumer in this iteration.

## Important Changes To Public Interfaces And Types
This story should make these interfaces explicit:
- a create-form state carrying:
  - fixed direction from `009`
  - selected account
  - amount input
  - occurred-at input
  - validation error state
  - submit-in-flight state
  - last idempotency key or active submit token state as needed for duplicate-submit prevention
- a create-submit payload matching backend-supported fields only:
  - `accountId`
  - `amount`
  - optional `occurredAt`
- a reusable toast UI primitive sufficient for transient success confirmation, without requiring a full app-wide notification center

Do not introduce:
- extra create payload fields not supported by the backend
- a user-editable direction switch inside the form
- a full global toast system with persistence, stacking rules, or cross-page notification history

## Implementation Decisions
- Form-shape policy:
  - direction is fixed by the selected create intent and shown readably, not chosen again inside the form
  - active field set remains limited to amount, account, and occurred-at
- Validation policy:
  - amount and account are required
  - amount must be strictly greater than zero
  - occurred-at may be omitted if the chosen UI and input model allows omission; otherwise the defaulted value remains editable
  - malformed occurred-at input is blocked locally before submit
- Submission policy:
  - submit is disabled while a request is in flight
  - repeated rapid activation must not create duplicate requests
  - idempotency protection is part of the story-owned submit path, not deferred to later work
- Error policy:
  - request failure keeps the form open
  - user-entered values remain intact
  - prior validation or submission errors clear appropriately once the user edits the relevant field or retries
- Mobile success policy:
  - successful save closes the overlay
  - the underlying finance workspace shows a lightweight transient success toast
  - the toast is implemented through a generic reusable toast primitive introduced here
- Desktop success policy:
  - successful save keeps the overlay open for repeat entry
  - preserve direction, account, and contextual date defaults into the next entry
  - clear amount for the next entry
  - clear validation and submission feedback after success
  - do not preserve stale failure state into the next entry
- Context and default policy:
  - account and date defaults from `009` and `007` seed the form
  - repeat-entry preservation on desktop should not invent stronger defaults than those already established
- Scope boundary:
  - this story owns submit lifecycle and success/error UX
  - it does not own richer categorization or metadata capture after create

## Acceptance Criteria
- The create overlay shows only the backend-supported create fields plus the fixed chosen direction.
- The form cannot be submitted without a selected account and a positive amount.
- Invalid occurred-at input is blocked before submit.
- Submit becomes disabled while the request is in flight.
- Multiple rapid submit attempts do not create duplicate transaction writes.
- On submit failure, the form stays open, shows recoverable feedback, and preserves user input.
- On successful mobile submission, the overlay closes and the finance workspace shows a lightweight success toast.
- On successful desktop submission, the overlay stays open for repeat entry.
- After successful desktop submission, direction, account, and contextual date defaults remain available for the next entry.
- After successful desktop submission, amount is cleared and prior validation or submission feedback is reset.
- This story does not expose unsupported create fields or an in-form direction switch.

## Tests
- Integration tests for local validation:
  - missing account blocked
  - zero or negative amount blocked
  - malformed occurred-at blocked
- Integration tests for submit lifecycle:
  - submit disables while in flight
  - duplicate activation does not issue duplicate create requests
  - failure preserves user-entered values and shows recoverable feedback
- Integration tests for success behavior:
  - mobile success closes overlay and triggers success toast
  - desktop success keeps overlay open
  - desktop repeat-entry preserves direction, account, and date and clears amount
  - desktop success resets prior validation and submission feedback
- Integration tests for context and default consumption:
  - account context from ledger preselects account
  - eligible single-day context seeds occurred-at date
- Integration tests for the reusable toast primitive only as needed by this story:
  - toast renders transient success feedback
  - toast can be dismissed or expires according to the chosen minimal implementation
- E2E tests:
  - successful `Expense` create on desktop, followed by immediate second entry
  - successful `Income` create on mobile with close plus toast confirmation
  - failed create preserves entered values and supports retry

## Implementation Notes
- Implemented the create form flow in `Pages.App` with a dedicated `FinanceCreateState` that tracks account/amount/occurred-at inputs, validation errors, in-flight submit state, and a nonce-backed idempotency key.
- Connected submit to `Api.Finance.createSentTransaction` / `createReceivedTransaction` and account loading to `Api.Finance.getAccounts` with `AccountsActive` filter.
- Added local validation for required account, required positive numeric amount, and valid `datetime-local` occurred-at input format.
- Desktop success behavior keeps the overlay open, clears amount, and resets errors while preserving direction/account/date defaults.
- Mobile success behavior closes the overlay and displays a transient success toast on the finance workspace.
- Added a minimal reusable toast primitive in `src/Ui/Toast.purs` and finance-scoped toast styling in `static/app.css`.
- Updated E2E navigation tests to cover create submission behavior and mobile toast confirmation.
