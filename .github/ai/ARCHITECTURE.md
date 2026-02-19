# ARCHITECTURE

This document is repo-local and should be maintained in each package repo.
Unlike `ECOSYSTEM.md` (suite-canonical), `ARCHITECTURE.md` captures package-specific contracts, entry points, and downstream impact details.

## Package purpose
`gsm.qtl` is the QTL-focused rendering/reporting package in the GSM suite.
It consumes metric/group/result artifacts produced by upstream GSM workflows
(primarily `gsm.core`, typically orchestrated with `gsm.mapping`) and turns
them into QTL-specific visual outputs and HTML reporting content.
The package provides chart/table builders, post-processing helpers, and an
opinionated report template (`inst/report/Report_QTL.Rmd`) used by `Report_QTL()`.
It also ships workflow YAML assets under `inst/workflow/` to support QTL use cases.
Primary value: consistent, reproducible QTL communication from standardized GSM data.

## Required fields
Fill and keep current:
- Package owner/steward
- Primary exported entry points
- Known downstream gsm.* consumers
- Contract stability level for each produced artifact

## Position in the GSM DAG
Upstream gsm.* packages (Imports):
- `gsm.core` (hard dependency; imported)

Upstream gsm.* packages (Suggests/integration):
- `gsm.datasim` (optional; examples/tests/integration support)

Downstream gsm.* packages (suite reverse deps):
- None explicitly declared in suite docs at this time (`none` known).
- Practical note: QTL report consumers (pipelines/apps) outside gsm.* may still need smoke checks.

## Cross-package contracts
This package participates in suite-level contracts defined in ECOSYSTEM.md.

### Inputs (consumes)
- `dfResults` from GSM reporting/analysis pipelines (study/group snapshot metrics and thresholds).
- `dfMetrics` metadata used to derive/report threshold context.
- `dfGroups` grouping metadata used in report layouts.
- Participant-level mapped data frames for eligibility/discontinuation visualizations/listings.
- Workflow configuration assets (YAML) for reason mappings and QTL metric workflows.

### Outputs (produces)
- `plotly` objects for QTL/eligibility/discontinuation trend and bar charts.
- `gt` tables and HTML-wrapped listing/table fragments for report embedding.
- Processed result `data.frame` artifacts from helpers like `Analyze_OneSideProp()` and `ResultsProcessor()`.
- Rendered HTML report output path via `Report_QTL()`.

### Stability guarantees
- Stable interface: exported function signatures in `NAMESPACE` and documented roxygen contracts.
- Stable artifact classes: return types documented as `data.frame`, `plotly`, `gt`, or rendered report path.
- Internal details: chart styling choices, non-exported widget internals, and `inst/workflow` implementation specifics.
- Any behavior/API contract change to exported functions requires downstream impact declaration and verification.

## Change impact matrix
- Change type: docs-only | mechanical | behavior/API/contract
- Requires downstream verification: yes/no
- Downstream packages to validate:
- Required checks/commands:

## Public API surface
- Report orchestration: `Report_QTL()`
- Result post-processing: `ResultsProcessor()`, `Analyze_OneSideProp()`
- Overview tables: `QTL_Overview()`, `Eligibility_Overview()`, `eligibility_listing()`
- Trend and distribution visuals: `QTL_lineplot()`, `eligibility_groupBar()`, `discontinuation_groupBar()`
- Reason/source drilldowns: `criteria_groupBar()`, `reasons_groupBar()`, `discontinuation_reasonBar()`, `discontinuation_map_reasons()`, `eligibility_sourceBar()`
- Display helpers: `scrollable_gt()`, `calc_fig_size()`

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
