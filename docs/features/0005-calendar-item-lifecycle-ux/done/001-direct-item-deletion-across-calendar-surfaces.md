# Direct Item Deletion Across Calendar Surfaces

## Goal
As a calendar user, I want to delete an item directly from where I see it so I do not need to open edit flows for simple removal.

## Outcome
Users can delete server-backed Task and Trip items directly from the calendar surfaces where they are displayed, with device-adapted entrypoints and explicit confirmation before the backend delete request is sent.

## In Scope
- Direct deletion on:
  - Day timeline cards
  - Week range list cards
  - Month range list cards
- Device-adapted entrypoints:
  - desktop: visible `Supprimer` action directly on the item surface
  - mobile: `Supprimer` exposed from an item-specific bottom sheet
- Explicit confirmation before the delete request is sent.
- Functional coverage for both Task and Trip items.
- Only `ServerCalendarItem` instances are deletable in this story.

## Out Of Scope
- Bulk deletion.
- Undo snackbar pattern.
- Soft-delete policy.
- Deletion of local-only unsaved items.
- General redesign of calendar cards or edit flows.

## Dependencies
- Depends on the current `Calendar` surfaces already present in `Pages.Calendar`:
  - desktop Day timeline cards
  - Week/Month range list cards
- Depends on the existing modal and bottom sheet primitives already used in the calendar page.
- Depends on backend delete capability for authenticated users on owned calendar items.

## Backend Contract
- Delete endpoint:
  - `DELETE /api/v1/calendar-items/:id`
- Story expectation:
  - frontend calls delete only for `ServerCalendarItem { id }`
  - successful delete is followed by the same item refresh path already used elsewhere in the page
- Error handling to cover explicitly:
  - `401` / `403`: auth or permission failure
  - `404`: item no longer exists
  - `500`: technical failure
- This story does not require any backend payload body for delete.

## Implementation Decisions
- Safety policy:
  - deletion requires explicit confirmation on all devices
- Interaction policy:
  - desktop prioritizes fast direct action access
  - mobile prioritizes tap safety via an item action sheet
- Scope policy:
  - same deletion capability in Day, Week, and Month surfaces
- Desktop surface policy:
  - Day timeline cards get a visible `Supprimer` action alongside the existing desktop item actions
  - Week/Month list cards also expose a visible `Supprimer` action on desktop
- Mobile surface policy:
  - tapping an item opens a dedicated item actions bottom sheet
  - the bottom sheet exposes at minimum:
    - `Editer` when the item is editable
    - `Supprimer`
- Confirmation policy:
  - `Supprimer` never sends the request immediately
  - desktop and mobile converge to the same explicit confirmation UI before network call
  - confirmation does not require typing the title or checking a checkbox
- State policy:
  - one deletion in flight at a time
  - while deletion is running, the destructive submit path is disabled
  - success is reflected by disappearance of the item after refresh
  - failure stays on page with visible, recoverable feedback
- UI selector policy:
  - stable selectors/classes must be introduced for:
    - desktop delete button on timeline
    - desktop delete button on range list
    - mobile item actions sheet
    - delete confirmation surface

## Acceptance Criteria
- Desktop user can trigger delete directly on Task and Trip cards in Day, Week, and Month surfaces.
- Mobile user can open an item action sheet and trigger delete for Task and Trip items from there.
- Confirmation is required before deletion; cancel keeps the item unchanged.
- Successful deletion removes the item from the visible surface without manual reload.
- Delete failure is visible and recoverable without leaving the current page.
- Local-only unsaved items are not exposed as deletable through this story.

## Tests
- Integration:
  - delete action availability matrix by surface and device mode
  - confirmation gating and cancel path
  - success/error state transitions
  - only server items are deletable
- E2E desktop:
  - delete Task and Trip from Day timeline
  - delete an item from Week/Month range list
  - cancel confirmation keeps the item visible
- E2E mobile:
  - open item action sheet
  - delete Task and Trip with confirmation
  - cancel confirmation keeps the item visible
  - action sheet exposes `Editer` and `Supprimer` for a server item
