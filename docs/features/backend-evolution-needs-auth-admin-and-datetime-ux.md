# Backend Evolution Needs: Auth Profile, Admin Governance, and Date-Time UX

This document lists backend capabilities required by upcoming frontend stories. It is not an implementation spec for backend internals.

## Related Feature Backlogs
- [Admin User Governance](0003-admin-user-governance/admin-user-governance.md)
- [Global UI UX](0004-global-ui-ux/global-ui-ux.md)
- [Calendar Item Lifecycle UX](0005-calendar-item-lifecycle-ux/calendar-item-lifecycle-ux.md)

## 1) Authenticated Profile Contract

### Goal
Provide a stable frontend source of truth for authenticated identity and roles.

### Required Capabilities
- `POST /api/signin` returns authenticated profile payload in addition to session cookie issuance.
- A dedicated authenticated profile endpoint (cookie-based, no credentials body), for example:
  - `GET /api/auth/profile`
- Both responses should expose the same profile model:
  - `username :: String`
  - `roles :: Array String`
  - `approved :: Boolean` (or equivalent status field)

### Frontend Usage Expectations
- Use signin response for immediate role-aware rendering.
- Use profile endpoint for reload/session refresh and route gating.

## 2) Admin Governance Endpoints

### Goal
Support admin UI for approvals and user deletion.

### Required Capabilities
- Pending approvals:
  - list pending users
  - approve one pending user
  - delete one pending user
- User management:
  - list existing users for admin scope
  - delete one user

### Frontend Behavior Expectations
- Per-row actions (approve/delete) with deterministic refresh.
- Clear error signaling for unauthorized and failed operations.

## 3) Authorization/Error Semantics Needed By Frontend

### Minimum Status Semantics
- `401` unauthenticated
- `403` authenticated but not admin
- `404` unknown resource
- `409` conflict (when relevant)
- `5xx` server failure

### Frontend Expectation
- Stable, machine-distinguishable status handling for UX messaging and route behavior.

## 4) Date-Time Contract Compatibility

### Goal
Keep frontend UX improvements compatible with existing backend payload expectations.

### Required Constraint
- Backend continues accepting current local ISO-like date-time payload format already used by frontend writes.
- No breaking change in field names for existing date-time write payloads during this iteration.

## 5) Calendar Item Deletion Contract Compatibility

### Goal
Support direct item deletion UX for both Task and Trip items across calendar surfaces.

### Required Capabilities
- Stable endpoint contract for deleting a calendar item by id.
- Same authorization semantics for Task and Trip deletion (owner-only expected).

### Frontend Behavior Expectations
- Deletion API status codes are stable and distinguishable for:
  - unauthenticated / unauthorized
  - not found
  - conflict or validation failure when relevant
  - server error
