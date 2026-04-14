# Late-Items Refresh On Route Changes

## Goal
As a user, I want reminder information to stay current while I navigate so I can trust the late-item count and list.

## Outcome
Late-item reminder data refreshes on every address-bar-affecting route change and remains coherent with sheet state and quick actions.

## In Scope
- Refresh trigger policy tied to navigation changes.
- Refresh after successful quick complete action.
- Coherence rules between chip count and sheet content.
- Handling of concurrent/overlapping refresh responses.

## Out Of Scope
- Timer-based polling refresh.
- Manual refresh-only UX.
- Background synchronization beyond route transitions.

## Technical Details
- Refresh is triggered whenever application navigation changes the route/URL.
- Refresh is also triggered immediately after successful quick complete action.
- Chip count and sheet rows update from a single canonical refreshed data source.
- If overlapping requests occur, stale responses must not overwrite newer state.

## Acceptance Criteria
- Navigating between authenticated tabs recomputes reminder data.
- Chip count reflects latest recomputed state after each route change.
- If sheet is open during refresh, its visible list updates without forcing sheet close.
- After quick complete success, list/count update without waiting for a later navigation.
- In case of concurrent refreshes, final rendered state matches latest completed refresh intent.

## Tests
- Integration test for route-change-triggered refresh dispatch.
- Integration test ensuring chip and sheet consume same refreshed dataset.
- Integration test for stale-response protection when refreshes overlap.
- E2E test navigating across tabs with changing late dataset and verifying updated count.
- E2E test with sheet open during navigation-triggered refresh.
