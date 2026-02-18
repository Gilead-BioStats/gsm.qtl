# ARCHITECTURE

## Package purpose
(TODO: replace with repo-specific purpose. Keep it to ~5–10 lines.)

## Required fields
Fill and keep current:
- Package owner/steward
- Primary exported entry points
- Known downstream gsm.* consumers
- Contract stability level for each produced artifact

## Position in the GSM DAG
Upstream gsm.* packages (Imports):
- (TODO) e.g., gsm.core

Downstream gsm.* packages (suite reverse deps):
- (TODO) list which gsm.* packages must be checked if this changes

## Cross-package contracts
This package participates in suite-level contracts defined in ECOSYSTEM.md.

### Inputs (consumes)
- (TODO) What artifacts/data structures does this package expect from upstream?

### Outputs (produces)
- (TODO) What artifacts/data structures does it guarantee to downstream?

### Stability guarantees
- (TODO) Which outputs are stable interfaces vs “internal details”?

## Change impact matrix
- Change type: docs-only | mechanical | behavior/API/contract
- Requires downstream verification: yes/no
- Downstream packages to validate:
- Required checks/commands:

## Public API surface
- (TODO) Top exported functions/classes users should start with.

## Directory map
- R/            Implementation
- man/          Generated docs (roxygen)
- tests/        Unit tests
- vignettes/    Long-form docs (if present)
- inst/         Templates/workflows/assets

## How to run locally
- devtools::document()
- devtools::test()
- devtools::check()

## Orchestration checklist (when making changes)
- Does this change affect a contract in ECOSYSTEM.md?
- Which downstream gsm.* packages need verification?
- Are changes mechanical vs behavioral split into separate PRs?
