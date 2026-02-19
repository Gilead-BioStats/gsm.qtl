# SKILLS

This file defines how we make changes in the GSM package ecosystem (gsm.*).
It is intentionally procedural and stable.

Canonical policy references:
- Orchestration + Context Packs: `AGENTS.md`
- Suite DAG + contracts: `ECOSYSTEM.md`
- PR summaries/review notes: `PR_SUMMARY_GUIDE.md`

Before coding:
1) Decide whether the change is: docs-only, mechanical refactor, or behavior/API change.
2) If behavior/API/contract changes:
   - list downstream gsm.* packages affected (suite DAG)
   - plan verification steps for each downstream package
3) Keep upstream-first sequencing: gsm.core → others.

## Golden rules
- Keep PRs small: one package, one theme.
- Separate mechanical refactors from behavioral changes.
- Do not break exported APIs without a deprecation path.
- Always state: what changed, why, tests run, risk/rollback.

## Default workflow (every PR)
1) Define scope (which files + why)  
2) Make change (minimal diff)  
3) Update docs/tests if needed  
4) Run checks  
5) Write a PR summary using the Output Contract below

## Output contract (required in PR description)
- Summary (3–6 bullets)
- Files changed (paths)
- Tests run (exact commands)
- Risks / rollback plan
- Follow-ups / out-of-scope items

Use `PR_SUMMARY_GUIDE.md` for formatting expectations.

## Safe refactor (no behavior change)
- Confirm public surface is unchanged.
- If behavior must change, split into a separate PR.
- Touch the smallest number of call sites possible.

## Exported API change (requires deprecation)
- Keep old entry point as wrapper.
- Add a deprecation notice and timeline.
- Update docs + NEWS if present.

## Documentation change
- README must explain: what the package does, how to install, minimal example.
- Keep examples deterministic and fast.
- Prefer short “how to” snippets over long essays.

## Cross-package change
- Change upstream package first.
- Update downstream packages afterward.
- If shared templates/docs change: update gsm.utils first, then re-sync other repos.
