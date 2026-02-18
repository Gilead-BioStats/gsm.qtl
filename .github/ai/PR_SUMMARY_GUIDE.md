# PR Summary Guide

Use this guide whenever preparing PR summaries or review handoff notes.

## Required sections
- Summary (3–6 bullets)
- Why
- Files changed
- Tests run (exact commands)
- Risk / rollback
- Follow-ups

## Style rules
- Use clear, direct bullets.
- Prefer behavior + impact language over implementation trivia.
- List exact test commands, not vague statements.
- Call out any downstream package verification explicitly.
- Keep out-of-scope items separate from completed work.

## Summary checklist
- What changed at user or package behavior level?
- Why was this needed?
- Which files or modules were touched?
- Which checks/tests were run and passed?
- Any risks, caveats, or rollback plan?

## Examples

### Good summary bullets
- Fixes axis label overlap in qtl summary plot for long category names.
- Preserves existing exported function signatures.
- Adds focused test coverage for long-label rendering path.
- Verifies no impact to downstream contracts.

### Avoid
- "Updated some code"
- "Ran tests" (without commands)
- Mixing completed work with future ideas in one bullet
