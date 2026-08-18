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

## Query the definition index before the third repeat

`nelisp-emacs-lib/CLAUDE.md` already says to use the anvil definition index
and call graph before large edits.  That rule went unused through a whole
session because it names no moment to apply it, so state the trigger here:

**When a replay stops twice for the same class of reason, query the index
before attempting a third individual fix.**

One rebuild+replay cycle on the real bootstrap costs about 12 minutes, so
discovering a set of missing names one failure at a time is the most expensive
way to find them.  Measured 2026-07-26..28: `cl-defstruct`, then a prelude
helper, then `fboundp`, then `backquote` were fixed in four separate cycles;
querying `nelisp--macros` from replayed code then listed **54** further prelude
macros absent from the sandbox in a single run.  Continuing one at a time would
have taken another eleven hours.

    emacsclient -e '(anvil-defs-index-status)'          ; files / defs / refs
    emacsclient -e '(anvil-defs-search "NAME")'         ; where is it defined
    emacsclient -e '(anvil-defs-references "NAME")'     ; who calls it
    emacsclient -e '(anvil-defs-who-requires "FEATURE")'

Do not count a capture list by grepping the build script: the list is emitted
as string fragments and line wrapping makes the count wrong (measured: 14
entries read as 3).  Ask the runtime instead — see the
`nelisp-sandbox-visibility-audit` skill for the procedure and for which table
each kind of name belongs in.

## One behaviour, one owner: check the other route before adding to a list

The standalone artifact commands run through two different bootstraps.  The
full-source route is `nelisp-standalone--artifact-command-runtime-src`; the
cache route is `nelisp-standalone--artifact-command-cache-src`, and
`compile-runtime-image --flat-artifact-cache` takes the latter.  They were
maintained separately, so the cache route ran neither
`(nelisp--install-core-macros)` nor the prelude macro capture.

The drift was invisible from the capture list itself.
`nelisp-standalone-build-artifact-runtime-cache` writes only a slice of the
generated runtime source — measured 2026-07-28, bytes 518,579..956,497 of
957,084 — and both the core-macro install (offset 336,487) and the capture
`dolist` (offset 508,277) fall outside it.  A `--flat-artifact-cache` replay
then died on the first `cl-defstruct` module item with
`(nelisp-void-function nelisp-ec-buffer)`, because the struct's option list was
evaluated as a call once `cl-defstruct` was void in `nelisp--macros`.

So: **before extending a capture list or a polyfill block, check whether the
other route has its own copy.  If it does, extract the block into one function
and call it from both** rather than editing the copy you happened to open.  A
one-line static check catches this class:

    grep -c "cl-defstruct" target/nelisp-artifact-runtime.el   # 0 == not captured

Note also that module replay batches
`nelisp-artifact-module-replay-chunk-size` (default 64) items into a single
source string.  One failing item takes up to 63 others with it, and the chunk
fallback re-runs the batch from the start, so `:eval` items before the failure
execute twice.  When a symbol is missing, check whether its chunk aborted
before concluding that its own definition is at fault.
