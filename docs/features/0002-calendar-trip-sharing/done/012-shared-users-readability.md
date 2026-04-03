# Shared Users Readability

## Goal
As a calendar owner, I want shared presence to remain readable when several users are visible so the Day view stays useful instead of becoming a color puzzle.

## Outcome
The Day view rail now supports up to three directly visible shared users and exposes an explicit `+N` overflow affordance beyond that limit. Hidden users remain inspectable through a dedicated bottom sheet, so readability is preserved without silently degrading lane width.

## In Scope
- Freeze a visible-lane readability limit of three shared users in Day view.
- Keep backend ordering as the priority source for which users stay visible versus overflow.
- Render an explicit overflow affordance when more than three users are present.
- Expose hidden users and derived-state details in a bottom sheet on desktop and mobile.

## Out Of Scope
- Personalization rules for colors.
- Presence derivation itself.
- Week and Month view presentation.

## Dependencies
- Depends on `008 Day View Side Rail`.
- Depends on `009 Day View Location Inspection` for consistent derived-state copy and interaction language.

## Acceptance Criteria
- Day view shows at most three shared users in the rail.
- With four or more users, only the first three backend-ordered users stay in the rail and the UI shows `+N`.
- Selecting `+N` opens a sheet listing hidden users in backend order with readable derived-state details.
- Desktop and mobile both use the same readability rule and overflow behavior.

## Tests
- Integration tests validate:
  - no overflow for one to three users
  - overflow partitioning for four users while preserving backend order
- E2E tests validate:
  - desktop overflow behavior (`3 lanes + +N + sheet`)
  - mobile overflow behavior through bottom sheet
  - no overflow when exactly three users are shared
