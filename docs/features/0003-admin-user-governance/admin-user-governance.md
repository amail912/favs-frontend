# Admin User Governance

This backlog captures admin-only user governance flows for account approvals and user deletion.

## Goal
Allow authorized admins to manage user lifecycle safely from the frontend: approve pending accounts and remove accounts when required.

## Scope For This Iteration
- Admin-only page with explicit role gating.
- Pending account approvals list.
- Account deletion flows with explicit confirmation.
- Strict non-admin behavior aligned with route-inaccessible policy.

## Backend Contract Input
- [Backend Evolution Needs: Auth Profile, Admin Governance, and Date-Time UX](../backend-evolution-needs-auth-admin-and-datetime-ux.md)

## Recommended Delivery Order
1. `001` Admin role gating and route access.
2. `002` Pending approvals management.
3. `003` User deletion management.

## Stories

### Done
- [001 Admin Role Gating And Route Access](done/001-admin-role-gating-and-route-access.md) - Expose admin page entry only to admins, gate `/admin` from the authenticated profile, and keep unauthorized access aligned with not-found behavior.
- [002 Pending Approvals Management](done/002-pending-approvals-management.md) - Let admins load pending accounts, approve or delete them per row, and recover cleanly from load or action errors.
- [003 User Deletion Management With Explicit Confirmation](done/003-user-deletion-management-with-explicit-confirmation.md) - Let admins load approved users, confirm destructive deletion explicitly, and recover cleanly from delete conflicts or technical errors.

### Ready

### To Refine

### Canceled

### Postponed
