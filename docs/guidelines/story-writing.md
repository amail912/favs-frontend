# Story Writing Guideline

This guideline defines how to write high-quality feature backlogs and stories under `docs/features`.

## Purpose
- Keep stories consistent across features.
- Make stories implementation-ready without turning them into low-level specs.
- Keep backend/API contracts outside frontend stories.

## Scope
- This guideline applies to:
  - feature index files under `docs/features/<feature>/`
  - story files under status folders such as `to-refine`, `done`, `canceled`, or `postponed`
- This guideline does not replace backend or API contract documentation. When backend details matter, write them in a separate contract or spec document and link to it.

## Language
- Write feature backlogs and stories in English.
- Keep wording direct, simple, and specific.
- Prefer concrete user outcomes over vague statements such as "improve UX" or "make it better".

## Frontend-Only Rule
- Stories are frontend only.
- A story may mention consumed data, dependencies, or assumptions coming from backend behavior, but it must not prescribe backend endpoints, payload design, persistence rules, or backend acceptance tests.
- If a feature needs backend contracts, document them in a separate file and link to that document from the feature index, not from the story body.

## Feature Index Format
A feature index should:
- start with a short title and a one-paragraph summary
- state a clear feature goal
- list active stories with short one-line descriptions
- organize stories by status when useful, for example `To Refine`, `Done`, `Canceled`, or `Postponed`
- link to separate supporting documents such as screenshots or API contracts when they exist

The feature index should describe the backlog, not duplicate full story content.

## Story Template
Each active story should use this structure:

```md
# Story Title

## Goal
As a ..., I want ..., so ...

## How This Story Achieves The Goal
...

## Technical Details
...

## Tests
- ...
```

Relevant screenshots may be added between `Goal` and `How This Story Achieves The Goal` when they materially help understanding.

## Section Quality Bar
### Goal
- Write the goal from the user's point of view.
- Keep it outcome-oriented.
- Use the story to explain why the change matters, not just what UI element changes.

### How This Story Achieves The Goal
- Explain the intended product or UX effect.
- Describe the behavior change in a way that helps prioritization and review.
- Avoid implementation dumps.

### Technical Details
- Give enough frontend direction to implement safely.
- Mention the main surface, rendering area, interaction model, or state involved.
- Do not over-specify internals unless the detail prevents a real implementation mistake.
- Do not put backend contract design here.

### Tests
- List only tests that are justified by the story.
- Prefer integration and E2E coverage for UI behavior.
- Mention unit tests only when a pure helper or isolated computation is introduced.
- Keep tests scenario-based rather than generic.

## Naming And Granularity
- Use short, action-oriented titles.
- Split stories when a single document starts mixing several different user outcomes.
- Keep each story focused enough that one engineer can implement it without reinterpreting the goal.
- Avoid umbrella stories that combine UI, data contracts, migration work, and follow-up polish in one file.

## Do And Don't
### Do
- Write "Make the exact location readable on hover, tap, and focus."
- Write "Use the Day view timeline structure as the main rendering surface."
- Write tests like "Integration tests for rail rendering from derived presence segments."

### Don't
- Write "Improve the calendar UX."
- Mix backend API design into the story body.
- List generic tests like "Add tests" without saying what behavior must be covered.
- Freeze unnecessary implementation details when the behavior can be specified more simply.

## Consistency Checklist
Before finalizing a story set, verify that:
- titles are concise and parallel in tone
- every story has the same section structure
- stories are frontend only
- backend details are moved to a separate contract document when needed
- tests are specific to the story behavior
- the feature index and the story files do not contradict each other
