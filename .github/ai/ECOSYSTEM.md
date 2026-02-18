# GSM Ecosystem

This document defines the **suite-level architecture** across gsm.* packages.
It is intended to be stable and shared across repos.

Machine-readable metadata for AI tooling is tracked in `ai_manifest.json`.

## Dependency DAG (suite)
Interpretation:
- **Imports** = hard dependency edge (must orchestrate + test downstream)
- **Suggests** = optional edge (run docs/examples/qualification checks when relevant)

Canonical DAG (high level):
- gsm.core is the root analytical/workflow foundation.
- gsm.mapping depends on gsm.core.
- gsm.datasim depends on gsm.mapping + gsm.core.
- gsm.reporting depends on gsm.core.
- gsm.kri depends on gsm.core (and commonly integrates with reporting/mapping/qtl).
- gsm.qtl depends on gsm.core (and may integrate with datasim).

## Cross-package contracts (what each package produces/consumes)

### gsm.core (Foundation)
Produces:
- Workflow execution utilities
- Metric construction/evaluation primitives
Consumes:
- Domain-ish inputs (from mapping or simulated data)
Stability:
- High: avoid breaking exported API; require deprecation path

### gsm.mapping (Transform layer)
Produces:
- Mapped/domain datasets ready for analytics
Consumes:
- Raw/source datasets, mapping specifications/workflows
Contract:
- Output schema must remain compatible with gsm.core workflows and reporting model inputs

### gsm.reporting (Reporting model)
Produces:
- Reporting data model objects (e.g., dfGroups/dfMetrics/dfResults)
Consumes:
- Outputs from gsm.core + inputs prepared by gsm.mapping
Contract:
- These data frames are the stable interface consumed by visualization/report packages

### gsm.kri (Rendering: KRI)
Produces:
- Widgets/visuals + HTML KRI reports
Consumes:
- Reporting model objects (primarily from gsm.reporting) and/or gsm.core outputs
Contract:
- Rendering layer: keep compute logic out; depend on reporting model stability

### gsm.qtl (Rendering: QTL)
Produces:
- QTL report template + related visuals/tables
Consumes:
- gsm.core outputs; optionally datasim for examples/testing
Contract:
- Rendering layer specialized for QTL workflows

### gsm.datasim (Synthetic data)
Produces:
- Synthetic study datasets for testing/examples
Consumes:
- gsm.core + gsm.mapping (to align generated data with expected pipeline schema)
Contract:
- Generated data should support realistic pipeline runs without real clinical data

### gsm.utils (Scaffolding)
Produces:
- Templates + repo standardization tooling
Consumes:
- None (runtime); used by maintainers during development
Contract:
- Single source of truth for shared templates (SKILLS/AGENTS/ECOSYSTEM/etc.)

## Orchestration rules (dependency-aware)
1) If you change an upstream package, you must validate all downstream packages in the suite.
2) Prefer PR order: upstream → downstream → integration checks.
3) Keep “mechanical/doc sync” PRs separate from behavior changes.
