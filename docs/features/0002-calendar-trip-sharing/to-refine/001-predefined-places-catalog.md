# Predefined Places Catalog

## Goal
As a user creating trips, I want to pick from a fixed list of valid places so trip data stays consistent and comparable across users.

## How This Story Achieves The Goal
The trip workflow should rely on a predefined places list instead of free-form place entry. This keeps departure and arrival values stable for sharing, validation, and presence derivation.

## Technical Details
Load the predefined places list into the trip creation and editing flows and keep the UI constrained to those choices. Each place only needs a stable key and a display name in this iteration.

## Tests
- Integration tests ensuring trip forms load and render the predefined places.
- E2E tests covering trip creation from the allowed place list.
