# Agent Instructions

Follow the parent `../AGENTS.md` worklog policy strictly.

For this repository:

- Do not recreate `docs/worklog/` for agent worklogs.
- Do not add `.org`, `.md`, or `.txt` worklog handoff files to the repo.
- Record nelisp work through `anvil-worklog` only, and verify searchability before deleting any migrated file-based log.
- When MCP worklog tools are not available, use the local `nelisp` command
  (`./target/nelisp` from this repo, or an explicit `NELISP` path) for
  worklog add/search operations.  Do not use `emacsclient` as the fallback
  command path for nelisp work.
