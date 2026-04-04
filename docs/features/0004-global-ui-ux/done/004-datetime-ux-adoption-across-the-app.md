# Date-Time UX Adoption Across The App

## Goal
As a user, I want the new shared date-time display and desktop input behavior to feel coherent across the app, not only where the primitives were introduced.

## Outcome
Existing frontend surfaces adopt the shared French display helpers from `002` and the shared desktop split date-time picker from `003`, while preserving existing business behavior and ISO-local write contracts.

## In Scope
- Adopt the shared display helpers on all current visible frontend surfaces that render date/time text.
- Adopt the shared desktop date-time picker on all current editable frontend surfaces that use date-time entry.
- Remove redundant local formatting/input wiring where safely replaced by shared primitives.
- Produce an explicit inventory of any remaining non-migrated surfaces discovered during implementation.

## Out Of Scope
- New domain features unrelated to date-time UX.
- Backend API redesign.
- New route/history semantics covered by `005` and `006`.

## Dependencies
- Depends on `002 Shared French Date-Time Display Policy`.
- Depends on `003 Desktop Custom Date-Time Picker`.
- Must remain compatible with existing ISO-local parser, validation, and payload contracts.

## Implementation Decisions
- Adoption policy:
  - the story is cross-app at backlog level, but implementation must start from the actual surfaces present in the repo at implementation time
  - all currently discovered surfaces must be migrated, and any not migrated must be listed explicitly
- Current repo baseline:
  - date-time display and input behavior currently lives primarily in `Pages.Calendar`
  - implementation must still inspect the full repo for other visible date/time surfaces before closing the story
- Compatibility policy:
  - display helpers replace only user-visible formatting
  - shared picker replaces only UI input behavior on desktop
  - domain validation rules and submit payloads remain unchanged
- Cleanup policy:
  - remove duplicated technical formatting logic when it becomes unused
  - keep domain-specific business messaging and labels intact

## Acceptance Criteria
- All currently existing date/time display surfaces in the repo use the shared French display policy.
- All currently existing desktop date-time entry surfaces in the repo use the shared desktop picker.
- Mobile date-time entry surfaces keep native behavior.
- Existing domain workflows remain functionally equivalent after migration.
- Any remaining non-migrated date/time surfaces are explicitly listed for follow-up instead of being left implicit.

## Tests
- Integration:
  - migrated screens use shared display helpers instead of local raw formatting
  - migrated desktop forms use the shared picker instead of direct native desktop `datetime-local`
  - existing validation and submit behavior remain unchanged
- E2E:
  - representative create/edit/display workflows continue to pass using shared date-time primitives
  - desktop and mobile behavior diverge only where intended
