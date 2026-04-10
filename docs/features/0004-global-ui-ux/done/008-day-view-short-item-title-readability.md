# Day View Short Item Title Readability

## Goal
As a calendar user, I want short-duration Day cards to keep the item title readable so I can identify items even when their visual height is small.

## How This Story Achieves The Goal
It defines a deterministic compact fallback for short Day cards. The default is a two-line text layout. When card size cannot safely render that layout, the title moves to the first line next to the time tag with adaptive shrink-to-fit while preserving readability and avoiding invisible titles.

## Technical Details
- Surface: Day timeline cards on desktop and mobile.
- Primary rule: render timeline time on first line and title on second line when card height allows it.
- Compact trigger rule: enter compact mode when the two-line layout cannot be rendered safely in the available card height.
- Compact layout rule: render time and title on the same first row; hide the category badge in compact mode.
- Typography rule: adaptive shrink-to-fit is allowed in compact mode, with a minimum title size floor of `11px`.
- Overflow rule: once `11px` is reached, title may clip at the end but must not become fully invisible.
- Safety rule: no rendered Day card should hide title completely due to insufficient height.
- Selector stability: implementation should expose stable compact-mode hooks (for example `calendar-calendar-card--compact` and/or `calendar-calendar-item-title--compact`) for integration/E2E assertions.
- Scope boundary: no redesign of Week/Month card typography in this story.

## Tests
- Integration tests for two-line mode selection when card height is sufficient.
- Integration tests for compact mode selection when two-line layout is not renderable.
- Integration tests verifying category badge is hidden in compact mode.
- Integration tests verifying compact title font never goes below `11px`.
- Integration tests ensuring title remains visible in both normal and compact modes.
- E2E desktop scenario with very short-duration item validating compact fallback readability.
- E2E mobile scenario with very short-duration item validating compact fallback readability.
