# AGENTS

This file is the canonical orchestration policy for humans + agents in gsm.* repos.

## Non-negotiable: Context Packs
No agent works from “the whole repo”.
Every ticket MUST include:

1) Goal + non-goals
2) Target repo + branch (usually dev)
3) Allowed-to-touch file list (explicit paths)
4) Entry points (exports/functions involved)
5) Tests to run (additional targeted/downstream checks beyond baseline full-suite run)
6) Definition of done
7) DAG impact (which downstream packages are affected)

If any item is missing, STOP and request it.

Required Context Pack fields must never be left empty. Use explicit placeholders:
- `TBD` = not decided yet
- `Unknown` = cannot determine yet
- `None` = explicitly not applicable

## Protected core docs (default no-edit)
Agents should treat these as read-only unless they are explicitly included in
Allowed-to-touch Files:

- `AGENTS.md`
- `ECOSYSTEM.md`
- `SKILLS.md`
- `SECURITY.md`
- `CONTRIBUTING.md`

Repo-local `ARCHITECTURE.md` is editable when required by the ticket scope.

Ownership guardrail:
- `AGENTS.md`, `ECOSYSTEM.md`, `SKILLS.md`, `PR_SUMMARY_GUIDE.md`, `CONTRIBUTING.md`, and `SECURITY.md` are suite-canonical standards from `gsm.utils`.
- Prefer updating these in `gsm.utils` and re-syncing target repos, rather than making ad-hoc local edits.
- `ARCHITECTURE.md` is intentionally repo-local and should be updated in the target repo when package-specific contracts/entry points change.

Quick routing table:

| Change type | Where to edit first |
|---|---|
| Suite policy/standards (`AGENTS`, `ECOSYSTEM`, `SKILLS`, `PR_SUMMARY_GUIDE`, `CONTRIBUTING`, `SECURITY`) | `gsm.utils` (then re-sync target repos) |
| Package-specific contracts/entry points/downstream notes | Target repo `ARCHITECTURE.md` + Context Pack |
| Ticket scope/tests/allowed files | Target repo Context Pack issue |

### Context Pack template (copy/paste)
```
Goal:
Non-goals:
Target repo + branch:
Allowed-to-touch files:
Entry points:
Tests to run (additional exact commands beyond full-suite baseline):
Definition of done:
DAG impact:
```

For R packages, baseline protocol always includes full-suite `devtools::test()`.
Use `Tests to run` to list additional exact commands (targeted tests, integration checks, downstream verifications).
If no additional checks are needed, write `None (full-suite only)`.
If function signatures, exported APIs, or roxygen docs change, include `devtools::document()` as well.

Do not leave any field blank; use `TBD` / `Unknown` / `None` when needed.

## Dependency-aware orchestration (how it “knows what to do”)
The suite DAG is defined in ECOSYSTEM.md and (mechanically) in DESCRIPTION Imports.

Algorithm:
1) Identify changed package(s).
2) If change touches exported API or shared contracts:
   - enumerate downstream gsm.* packages (reverse deps in suite)
3) Sequence work:
   - upstream PR(s) first
   - downstream compatibility PR(s) second
   - integration/docs checks last
4) Verification:
   - at minimum run tests for changed package
   - plus tests for all downstream packages in the suite DAG

### Impact declaration (required for behavior/API changes)
When behavior or contracts change, include this artifact in the ticket/PR:

```
Changed package(s):
Change type: behavior | exported API | contract
Downstream packages to verify:
Verification commands:
Breaking changes: yes/no
Deprecation path (if applicable):
```

## Roles
- Coordinator: slices work into tickets + assembles Context Packs (including DAG impact).
- Package Steward: implements one ticket in one repo.
- Docs Curator: README/roxygen/vignettes/pkgdown consistency.
- Test Sheriff: tests + flakiness + CI parity (esp. downstream after upstream changes).
- Release Wrangler: DESCRIPTION/version/NEWS + sequencing.

## Routing rules
- Exported API or contract change → Release Wrangler + downstream verification required
- DESCRIPTION/version change → Release Wrangler review
- Tests changed → Test Sheriff review
- Docs/vignettes/pkgdown changed → Docs Curator review
- Shared templates/docs change → update gsm.utils first, then re-sync

## Required response format (agents)
- Follow `PR_SUMMARY_GUIDE.md` for section quality and style.
- Summary:
- Files changed:
- Patch/diff:
- Tests run:
- Downstream verification (what was checked):
- Risks/rollback:
- Follow-ups:
