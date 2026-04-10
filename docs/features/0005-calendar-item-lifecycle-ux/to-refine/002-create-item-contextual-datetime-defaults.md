# Create Item Contextual Date-Time Defaults

## Goal
As a calendar user, I want item creation fields to default from the consulted day and start time so I can create items faster with less manual typing.

## How This Story Achieves The Goal
It applies contextual defaults directly in create flows: start date inherits the consulted day, and end date-time gets a one-hour default only when still empty. This reduces manual entry while keeping user overrides respected.

## Technical Details
- Surface: Task and Trip create forms in Calendar.
- Start default: initialize create start date from consulted day (`focusDate` context).
- End default: when start date-time is filled and end is empty, auto-fill end to `start + 1h`.
- Override safety: once end is manually set by user, automatic prefill must not overwrite it.
- Platform behavior: same defaulting logic on desktop and mobile create paths.
- Scope boundary: applies to create flows only, not edit flows.

## Tests
- Unit tests for `start + 1h` computation, including day boundary cases.
- Unit tests for "do not overwrite end when user already edited it".
- Integration tests for create draft initialization from consulted day.
- Integration tests for end auto-prefill only when end is empty, on Task and Trip create flows.
- E2E test opening create flow from non-today consulted day and verifying start prefill.
- E2E test setting start with empty end and verifying auto-filled end.
- E2E test editing end manually, then changing start, verifying end remains user-defined.
