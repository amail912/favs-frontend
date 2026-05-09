# Contextual Record-Transaction Entrypoints

## Goal
As a finance user, I want to start recording a sent or received transaction directly from my current finance context, so I can capture a money movement quickly without re-entering information the workspace already knows.

## Outcome
The `Transactions` workspace exposes a finance create action that opens a device-adapted chooser for `New Expense` and `New Income`, then opens the create overlay with direction fixed by the chosen action and supported defaults derived from current ledger context when eligible.

## In Scope
- The visible finance `+` action on `Transactions`, as already established by `002`.
- A two-action chooser surfaced from `+`:
  - `New Expense` mapped to backend `sent`
  - `New Income` mapped to backend `received`
- Device-adapted chooser presentation:
  - desktop: anchored dropdown or menu
  - mobile: bottom sheet
- Opening the create overlay after action selection.
- Forwarding supported ledger context into the create flow:
  - active `accountId`
  - eligible single-day date context
- Direction default derived only from the chosen quick action.
- Consistent availability of the same two create actions across supported devices.

## Out Of Scope
- Form fields, validation, submit behavior, or idempotency handling.
- `+` visibility rules outside `Transactions`.
- Counterparty, description, or suggestion behavior.
- Additional create shortcuts or presets.
- Always-visible separate `Expense` and `Income` buttons.
- Create access from `Reports`, where `+` is already hidden by `002`.

## Dependencies
- Depends on `002` for `+` visibility and shell placement policy.
- Depends on `003` because create opens as a finance overlay with shared return semantics.
- Depends on `007` because ledger URL context is the source of supported defaults.
- Consumes the finance backend direction model already adopted in `004`.
- Interacts with `010`, which later provides the create form consumed by this entrypoint.

## Important Changes To Public Interfaces And Types
This story should not add new routes or new backend contract shapes.

Behavior and interface changes to make explicit:
- a create-intent type or equivalent UI state representing:
  - `Expense` or backend `sent`
  - `Income` or backend `received`
- a chooser-open state for the finance `+` interaction
- a create-launch payload or equivalent form-initialization state carrying:
  - chosen direction
  - optional `accountId`
  - optional contextual occurred-at date seed

Do not introduce:
- a generic free-form direction chooser inside `009`
- extra create modes beyond sent and received
- create defaults sourced from anything outside the supported ledger context model

## Implementation Decisions
- Interaction policy:
  - `+` does not open the create form directly
  - it first opens a chooser with exactly two actions
- Device policy:
  - desktop uses an anchored dropdown or menu
  - mobile uses a bottom sheet
  - both surfaces expose the same action set and semantics
- Direction policy:
  - `New Expense` means backend `sent`
  - `New Income` means backend `received`
  - the chosen action fixes initial direction before the form opens
- Context forwarding policy:
  - if ledger `accountId` is active, create preselects that account
  - if the active ledger date context represents a single day, create inherits that day
  - otherwise create does not invent a stronger date default than what `010` already defines
- Scope policy:
  - only the supported ledger context from `007` may prefill create
  - no hidden inference from recent actions, reports state, or unrelated page history
- Navigation policy:
  - after selecting `Expense` or `Income`, the create overlay opens through the existing nested-flow contract from `003`
  - the chooser itself is a transient entry surface, not a separate route or lasting workspace state

## Acceptance Criteria
- On `Transactions`, the finance `+` opens a chooser instead of opening create directly.
- On desktop, the chooser appears as an anchored dropdown or menu.
- On mobile, the chooser appears as a bottom sheet.
- The chooser offers exactly `New Expense` and `New Income`.
- Choosing `New Expense` opens create with initial direction `sent`.
- Choosing `New Income` opens create with initial direction `received`.
- If ledger `accountId` is active, the create flow opens with that account preselected.
- If the current ledger date context is an eligible single-day context, the create flow inherits that day.
- If no eligible ledger context exists, the create flow does not invent unsupported defaults.
- The chooser behavior is consistent across desktop and mobile apart from presentation.
- This story does not expose create entry from `Reports`.

## Tests
- Integration tests for chooser availability:
  - `+` on `Transactions` opens the chooser
  - chooser exposes exactly `New Expense` and `New Income`
- Integration tests for device-adapted presentation:
  - desktop renders anchored chooser or menu
  - mobile renders bottom sheet chooser
- Integration tests for action mapping:
  - `Expense` launches create with initial direction `sent`
  - `Income` launches create with initial direction `received`
- Integration tests for context forwarding:
  - active `accountId` is forwarded into create initialization
  - eligible single-day date context is forwarded into create initialization
  - absent or ineligible context does not produce unsupported defaults
- E2E tests:
  - open create from `Transactions` on desktop and choose `Expense`
  - open create from `Transactions` on mobile and choose `Income`
  - verify account-prefill behavior when ledger account filter is active
  - verify contextual date forwarding when ledger is scoped to a single day

## Implementation Notes
- `+` on `Transactions` now opens a two-action chooser instead of opening create directly.
- Desktop chooser is rendered as an anchored inline menu near the finance `+`; mobile chooser uses a bottom sheet.
- Choosing `New Expense` maps to `sent`; `New Income` maps to `received`.
- Create overlay now receives and displays a launch payload with direction, optional `accountId`, and optional date seed.
- Date seed policy in this implementation follows from-only fallback: when `from` exists, its date part is forwarded as the occurred-at day seed.
