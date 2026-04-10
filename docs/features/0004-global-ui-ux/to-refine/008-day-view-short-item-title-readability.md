# Day View Short Item Title Readability

## Goal
As a calendar user, I want short-duration Day cards to keep the item title readable so I can identify items even when their visual height is small.

## How This Story Achieves The Goal
It defines a deterministic title rendering fallback for short Day cards. The default is a second-line title. When card size cannot display that second line, the title moves to the first line next to the time tag and shrinks to fit within a readable minimum, avoiding invisible titles.

## Technical Details
- Surface: Day timeline cards on desktop and mobile.
- Primary rule: render title on second line when card height allows it.
- Fallback rule: if second line is not renderable, place title on first line with time tag.
- Typography rule: adaptive shrink-to-fit is allowed, but not below a minimum readable size.
- Safety rule: no rendered Day card should hide title completely due to insufficient height.
- Scope boundary: no redesign of Week/Month card typography in this story.

## Tests
- Integration tests for two-line mode selection when card height is sufficient.
- Integration tests for first-line compact fallback when second line is not renderable.
- Integration tests ensuring title remains present in both modes.
- E2E test on desktop with very short-duration item validating fallback readability.
- E2E test on mobile layout with very short-duration item validating fallback readability.
