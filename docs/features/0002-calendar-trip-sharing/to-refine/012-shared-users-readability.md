# Shared Users Readability

## Goal
As a calendar owner, I want shared presence to remain readable when several users are visible so the Day view stays useful instead of becoming a color puzzle.

## How This Story Achieves The Goal
The presence cue design should stay understandable for a small number of simultaneous users. This story defines the readability target and the fallback behavior if the number of visible users grows beyond it.

## Technical Details
Design the side rail and inspection affordances for roughly three simultaneously visible users. The story should define how the UI degrades or limits visibility beyond that threshold instead of leaving the behavior implicit.

## Tests
- Integration tests for the supported small-multi-user layout.
- Integration tests for fallback behavior beyond the readability target.
- E2E tests verifying the Day view remains understandable with multiple visible shared users.
