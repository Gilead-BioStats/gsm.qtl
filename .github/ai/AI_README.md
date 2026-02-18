# AI README (Copilot / Codex 5.3)

This repository is the canonical source for AI-ready templates used across the GSM suite.

## Purpose
- Keep shared template docs consistent across gsm.* repositories.
- Make dependency-aware orchestration deterministic for humans and agents.
- Provide one sync utility to distribute template updates.

## Canonical files
Templates live in `inst/ai_templates/` and are copied into target repos under `.github/ai/` by default (or repo root when explicitly configured).

- `AGENTS.md` (canonical orchestration + Context Pack policy)
- `ECOSYSTEM.md` (suite DAG + cross-package contracts)
- `ARCHITECTURE.md` (repo-local contract details and DAG position)
- `SKILLS.md` (daily execution workflow)
- `PR_SUMMARY_GUIDE.md` (standards for PR/review summaries)
- `CONTRIBUTING.md`, `SECURITY.md`, `.github/pull_request_template.md`
- `ai_manifest.json` (machine-readable template manifest + version)

## Source of truth rules
- Orchestration policy lives in `AGENTS.md`.
- Suite dependency and contracts live in `ECOSYSTEM.md`.
- Other files should reference these sources rather than restating full policy text.

## Syncing templates
Use:

```r
gsm.utils::update_gsm_ai_docs(strPackageDir = ".")

# One-command standards sync (AI docs + issue templates + workflows)
gsm.utils::sync_gsm_standards(strPackageDir = ".")

# Check drift only (no writes)
gsm.utils::update_gsm_ai_docs(strPackageDir = ".", mode = "check")

# Check full standards drift (AI docs + workflows)
gsm.utils::sync_gsm_standards(strPackageDir = ".", mode = "check")

# Preview writes only
gsm.utils::update_gsm_ai_docs(strPackageDir = ".", dry_run = TRUE, overwrite = TRUE)

# Sync selected files
gsm.utils::update_gsm_ai_docs(strPackageDir = ".", include = c("AGENTS.md", "ai_manifest.json"))

# Build a ready-to-paste agent prompt from issue + Context Pack
prompt <- gsm.utils::build_agent_prompt(
	issue = "gsm.qtl#123",
	context_pack = "<paste full context pack text>"
)

cat(prompt)
```

## Maintainer workflow
1) Update templates in `gsm.utils` first.
2) Sync templates into target repos.
3) Run package tests/checks in each touched repo.
4) For behavior/API changes in any suite package, verify downstream packages per `ECOSYSTEM.md`.

## CI drift gate (recommended)
If target repos use gsm.utils GitHub Actions templates, include `ai-template-drift-check.yaml`.
This workflow fails PRs when synced templates drift from gsm.utils canonical templates.

When it fails, run:

```r
gsm.utils::sync_gsm_standards(strPackageDir = ".")
```

Then commit the updated template files.

## Agent guardrails
- Do not run from whole-repo context when a scoped Context Pack is available.
- If Context Pack fields are missing, request them before making behavioral changes.
- Keep mechanical/doc synchronization separate from behavior changes.
