# Direct Item Deletion Across Calendar Surfaces

## Goal
As a calendar user, I want to delete an item directly from where I see it so I do not need to open edit flows for simple removal.

## Outcome
Users can delete Task and Trip items directly on day and range surfaces with device-adapted entrypoints and explicit confirmation safeguards.

## In Scope
- Direct deletion on:
  - Day timeline cards
  - Week/Month range list cards
- Device-adapted entrypoints:
  - desktop: visible delete action on item surface
  - mobile: delete action exposed through item action sheet
- Explicit confirmation before delete request is sent.
- Functional coverage for both Task and Trip items.

## Out Of Scope
- Bulk deletion.
- Undo snackbar pattern.
- Soft-delete policy.

## Dependencies
- Depends on current calendar item listing and day timeline rendering.
- Depends on backend delete capability for calendar items owned by authenticated user.

## Implementation Decisions
- Safety policy:
  - deletion requires explicit confirmation on all devices
- Interaction policy:
  - desktop prioritizes fast direct action access
  - mobile prioritizes tap safety via action sheet
- Scope policy:
  - same deletion capability in Day, Week, and Month surfaces

## Acceptance Criteria
- Desktop user can trigger delete directly on Task and Trip cards.
- Mobile user can trigger delete from action sheet for Task and Trip cards.
- Confirmation is required before deletion; cancel keeps item unchanged.
- Successful deletion removes item from visible surface without manual reload.
- API errors are visible and recoverable.

## Tests
- Integration:
  - delete action availability matrix by surface and device mode
  - confirmation gating and cancel path
  - success/error row state transitions
- E2E desktop:
  - delete Task and Trip from Day and range surfaces
- E2E mobile:
  - open action sheet, delete Task and Trip with confirmation
  - cancel confirmation keeps item visible
