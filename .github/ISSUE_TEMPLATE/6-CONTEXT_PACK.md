---
name: "Context Pack Issue Template"
about: "Define a scoped AI-ready ticket envelope before implementation"
title: "(fill in)"
type: Technical
Project: Gilead-BioStats/41
---

## Goal
<!-- Single primary outcome this ticket should achieve. -->

## Non-goals
<!-- Explicitly list what this ticket should NOT do. -->

## Target Repo + Branch
<!-- Example: gsm.qtl / dev -->

## Allowed-to-touch Files
<!-- Explicit list of file paths the implementer/agent may edit. -->

## Core-doc Edit Permission (default: do not edit)
<!-- If any core docs are allowed to be edited, list exact paths here. Otherwise write "none". -->
<!-- Typical core docs: .github/ai/AGENTS.md, .github/ai/ECOSYSTEM.md, .github/ai/SKILLS.md, .github/ai/SECURITY.md, .github/ai/CONTRIBUTING.md (legacy root paths may still exist) -->

## Entry Points
<!-- Exported functions/modules/classes involved in this change. -->

## Tests to Run (Exact Commands)
<!-- Baseline protocol already runs full-suite `devtools::test()`. -->
<!-- List ADDITIONAL exact commands only (e.g., `devtools::test(filter = 'foo')`, integration checks, downstream verification commands). -->
<!-- If no additional checks are required, write: `None (full-suite only)`. -->
<!-- If API/signature/roxygen changes are in scope, also include `devtools::document()`. -->

## Definition of Done
<!-- Objective pass/fail criteria for this ticket. -->

## DAG Impact
<!-- List downstream gsm.* packages to verify, or write "none" if no impact is expected. -->

## Change Classification
<!-- Select one -->
- [ ] Docs-only
- [ ] Mechanical refactor (no behavior change)
- [ ] Behavior/API/contract change

## Risks / Rollback
<!-- Key risks and how to safely roll back if needed. -->
