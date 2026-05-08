# Account Context Surface And Handoff

## Status
Canceled.

## Reason
This story assumed account context would originate from a dedicated finance account-entry surface or handoff mechanism.

That assumption was removed. In this iteration, account context is determined by the account filter on the `Transactions` page.

Because of that decision, `005` no longer owns a distinct prerequisite surface or flow.

## Redistribution
The useful behavior from this story was redistributed to the stories that actually own the context source and its consumers:
- `007` now owns the account filter as the canonical source of account context in finance.
- `009` now owns create preselection from the active ledger account context.
- `015` now owns report drill-down restoring ledger context through the supported ledger filters.
- `003` continues to own preserving that context while nested create and detail overlays are open.

## Notes
- No separate finance account launcher is part of this iteration.
- No external account page or account-management surface is required for active finance scope.
- If a dedicated account-specific entry surface is introduced later, it should be planned as a new story rather than reviving this one unchanged.
