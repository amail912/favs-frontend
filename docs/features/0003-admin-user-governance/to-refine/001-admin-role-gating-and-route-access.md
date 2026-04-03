# Admin Role Gating And Route Access

## Goal
As a non-admin user, I must not access admin UI. As an admin user, I need a clear entry point to user governance.

## Outcome
Frontend exposes an admin navigation entry only when the authenticated profile includes the admin role, and non-admin direct navigation resolves to not-found behavior.

## In Scope
- Add admin route in app routing.
- Add role-aware admin navigation entry.
- Enforce non-admin route-inaccessible behavior (`404` page policy).
- Consume authenticated profile role data from session/profile contract.

## Out Of Scope
- Approval and deletion actions.
- Backend role assignment policy.

## Dependencies
- Depends on backend profile payload availability at signin and session refresh.
- Depends on frontend auth session refresh path.

## Implementation Decisions
- Access policy:
  - admin menu entry visible only when role includes admin
  - non-admin direct URL access lands on not-found behavior
- Source of truth:
  - role comes from authenticated profile model, not from ad hoc probes

## Acceptance Criteria
- Admin user sees an admin entry in navigation and can open admin page.
- Non-admin user never sees admin navigation entry.
- Non-admin user navigating to admin URL receives not-found behavior.
- Route behavior remains stable after page reload with active session.

## Tests
- Integration:
  - route + nav rendering matrix by profile role
- E2E:
  - admin sees and opens admin page
  - non-admin cannot discover/admin route and receives not-found on direct URL
