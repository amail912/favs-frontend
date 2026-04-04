# Admin Role Gating And Route Access

## Goal
As a non-admin user, I must not access admin UI. As an admin user, I need a clear entry point to user governance.

## Outcome
Frontend exposes an `Admin` navigation entry only when the authenticated profile includes the admin role, and non-admin direct navigation to `/admin` resolves to the existing not-found behavior.

## In Scope
- Add `Admin` route in app routing at `/admin`.
- Add role-aware `Admin` navigation entry in the authenticated app shell.
- Enforce non-admin route-inaccessible behavior through the existing `404` page policy.
- Replace boolean-only auth gating with profile-aware auth state where route access is derived from the authenticated profile.
- Use the authenticated profile returned by backend auth endpoints as the single source of truth for username and roles.
- Provide a minimal admin landing page that is visible only to admins and is ready to host stories `002` and `003`.

## Out Of Scope
- Pending approval actions.
- Approved-user deletion actions.
- Backend role assignment policy.
- Role editing or role-management UX.

## Dependencies
- Depends on the existing backend auth contract already available in `../foucl2`.
- Must integrate with the current frontend auth session initialization, signin, and signout flow in `Pages.App`.

## Backend Contract
- `POST /api/signin`
  - returns authenticated profile JSON on success
- `GET /api/auth/profile`
  - returns authenticated profile JSON for the active session
  - returns `401` when no valid authenticated session exists
- Authenticated profile payload shape:
  - `username :: String`
  - `roles :: Array String`
  - `approved :: Boolean`
- Role values currently exposed by backend:
  - `"admin"`
  - `"member"`

## Implementation Decisions
- Auth source of truth:
  - app initialization refreshes auth state via `GET /api/auth/profile`
  - signin stores the profile returned by `POST /api/signin`
  - signout clears the stored authenticated profile
- Auth state shape:
  - frontend auth state must become profile-aware rather than boolean-only
  - authenticated vs unauthenticated state is derived from presence of the authenticated profile
- Admin access policy:
  - admin access is granted when `roles` contains `"admin"`
  - non-admin and unauthenticated users never see the `Admin` navigation entry
  - non-admin and unauthenticated direct navigation to `/admin` resolves to `NotFound`, not redirect
- Reload and initialization policy:
  - guarded admin UI must not render before profile refresh resolves
  - after refresh, `/admin` resolves deterministically to admin page or `NotFound`
- Admin landing page scope for this story:
  - title: `Administration utilisateurs`
  - short introduction explaining that this page is the entry point for approvals and user management
  - two non-interactive placeholder sections:
    - `Comptes en attente`
    - `Utilisateurs existants`
  - no approve/delete actions in this story

## Acceptance Criteria
- Admin user sees an `Admin` entry in the authenticated navigation and can open `/admin`.
- Approved non-admin user never sees the `Admin` navigation entry.
- Unauthenticated user never sees the `Admin` navigation entry.
- Non-admin user navigating directly to `/admin` receives the existing `404` page behavior.
- Unauthenticated user navigating directly to `/admin` receives the existing `404` page behavior.
- Reloading `/admin` with an active admin session keeps access stable after auth refresh completes.
- Reloading `/admin` with a non-admin or unauthenticated session resolves to `404` after auth refresh completes.
- The app does not briefly expose privileged admin content before auth profile resolution completes.

## Tests
- Integration:
  - auth refresh stores authenticated profile on `200 /api/auth/profile`
  - auth refresh clears authenticated profile on `401 /api/auth/profile`
  - signin success stores the returned authenticated profile
  - authenticated navigation renders `Admin` only for profiles whose `roles` contain `"admin"`
  - `/admin` route renders admin landing page for admin profile
  - `/admin` route renders `NotFound` for member profile
  - `/admin` route renders `NotFound` for unauthenticated state
  - initial unresolved auth state does not leak admin content before refresh completion
- E2E:
  - admin user signs in, sees `Admin`, opens `/admin`, and sees `Administration utilisateurs`
  - approved non-admin user does not see `Admin`
  - approved non-admin direct URL access to `/admin` shows `404`
  - admin reload on `/admin` keeps access and shows admin landing page after refresh
