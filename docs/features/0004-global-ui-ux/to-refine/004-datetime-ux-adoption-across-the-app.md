# Date-Time UX Adoption Across The App

## Goal
As a user, I want date-time behavior to feel consistent across all domains, not only calendar.

## Outcome
Existing pages and components adopt shared date-time display and desktop input components, reducing divergence and duplicated date-time logic.

## In Scope
- Migrate existing date-time display usages to shared French policy.
- Migrate relevant desktop input flows to reusable picker.
- Keep domain-specific behavior intact while removing duplicate technical formatting logic.

## Out Of Scope
- New domain feature behavior unrelated to date-time.
- Backend API redesign.

## Dependencies
- Depends on `002 Shared French Date-Time Display Policy`.
- Depends on `003 Desktop Custom Date-Time Picker`.

## Implementation Decisions
- Adoption strategy:
  - prioritize high-impact existing forms and displays first
  - remove redundant per-domain formatting logic where replaced by shared components
- Compatibility:
  - no change to functional business decisions in domain modules

## Acceptance Criteria
- Covered forms and labels use shared date-time components/helpers.
- No regression in existing domain workflows after migration.
- Remaining non-migrated surfaces are explicitly listed for follow-up.

## Tests
- Integration:
  - migrated screens use shared components/helpers
- E2E:
  - representative workflows continue to pass with new shared UX primitives
