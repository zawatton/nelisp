# Agent Instructions

Read [`AI.md`](AI.md) first: it is the orientation page for this
repository — how to see the current state, the inner loop, and the rules
that exist because each one was broken here.  This file covers worklog
policy only.

Follow the parent `../AGENTS.md` worklog policy strictly.

For this repository:

- Do not recreate `docs/worklog/` for agent worklogs.
- Do not add NEW `.org`, `.md`, or `.txt` worklog handoff files to the repo.
  Grandfathered exceptions are `FINDINGS.md`, `build-notes-p1.md`,
  `build-notes-p2.md`, `build-notes-p3ab.md`, `build-notes-p3c.md`,
  `build-notes-p4b.md`, and `build-notes-p4c.md`.
- Record nelisp work through `anvil-worklog` only, and verify searchability before deleting any migrated file-based log.
- When MCP worklog tools are not available, use `emacs --batch` with
  `anvil-worklog.el` and **the database path pinned explicitly**:

  ```
  emacs --batch -L <anvil.el dir> \
    --eval '(progn (require (quote anvil-worklog))
                   (setq anvil-worklog-db-path "<Notes>/.anvil-worklog/anvil-worklog-index.db")
                   (anvil-worklog-add "TITLE" "BODY"))'
  ```

  Do not use `emacsclient` — that is the daemon path this rule has always
  been about, and it is not the same thing as `emacs --batch`.

  Pinning is not optional, and it is the whole trap.  Left unpinned on this
  Windows host, `anvil-worklog--db` resolves to
  `~/.emacs.d/anvil-worklog-index.db`, which is a DIFFERENT database — 59
  entries, last written 2026-08-16 — while the canonical
  `Notes/.anvil-worklog/anvil-worklog-index.db` holds 2013.  Linux avoids
  this by symlinking the former to the latter; Windows has no such link, so
  an unpinned write lands somewhere nothing else reads.  Measured 2026-09-02.

  Writing the DB by hand with `sqlite3` also works and was used for a whole
  session before the above was found, but it means inserting into **both**
  `worklog_entry` and `worklog_fts` yourself, because nothing keeps them in
  sync outside `anvil-worklog.el`.  Prefer the elisp path.

  Two things about verifying the write, both learned by getting them wrong:

  - **Quote a hyphenated search term.**  `(anvil-worklog-search "split-brain")`
    answers 0 and looks like the write failed; the FTS query parser reads the
    hyphen as syntax.  `"\"split-brain\""` finds it.  A failed search is not
    evidence of a failed write.
  - **The elisp path and a hand-written row disagree about the machine
    name.**  `anvil-worklog.el` derives `windows-THINKPAD-E14-GE` where this
    session's `sqlite3` rows used `windows-ThinkPad-E14-Gen5`, so the two land
    under different synthetic `file` keys and a per-machine listing shows only
    one of them.  This predates any of it -- the canonical DB already holds
    five spellings of this one Windows host (898 / 580 / 24 / 23 / 22 rows).
    Search finds entries regardless; `worklog-list` filtered by machine does
    not.

  This bullet used to name `bin/anvil standalone-db` (via the local `nelisp`
  command) as the fallback.  That path has not been able to work since
  2026-06-02 and the claim is withdrawn rather than left standing.  Measured
  2026-09-02, on windows-x86_64, three separate faults stacked:

  1. `bin/anvil` loads `nelisp-coding.el` from the sibling `nelisp-emacs`
     tree, not this repository's own.  The sibling copy still has a hard
     `(require 'subr-x)`; the canonical one here was changed to
     `(require 'subr-x nil t)` in `cd64c0bd7` precisely because the
     standalone prelude already provides the functions and only the feature
     is missing.  The stale copy is what aborts, at the third `load`.
  2. Past that, the SQLite backend calls `nl_sqlite_*` inside
     `nelisp_runtime.dll` — a Rust runtime deleted in `b76cc5ea` ("delete all
     Rust (9 crates, ~18.7K LOC, 116 files) — pure-elisp self-host",
     2026-06-02).  `standalone-db` was added later, in `fb8bc5bb`, against a
     backend that was already gone.  `target/nelisp.exe` imports only
     `KERNEL32.dll`, `SHELL32.dll` and `WS2_32.dll` and carries no
     `nl_sqlite` symbol, so this is a contract failure on every OS, not a
     Windows one.
  3. With `ANVIL_NOTES_DIR` unset, `lisp/anvil-standalone-db.el` treats `PWD`
     as the Notes root, so running it from this repository would target
     `./.anvil-worklog/` rather than the real `../../.anvil-worklog/`.

  Restoring the path means replacing the deleted backend, not fixing a
  loader.  Until someone does, this bullet says what actually works.
