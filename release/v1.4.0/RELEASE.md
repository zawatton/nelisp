# NeLisp v1.4.0

Release implementation and qualification notes, written 2026-09-19.

A minor release: new public surface (a syntax-parsing layer, a declarative
FFI with a pure-Elisp loader, a project CLI and test runner, a language
server and VS Code extension, three official packages, bignum bitwise
arithmetic) and new required gates.  No existing contract is intentionally
changed.  v1.3.1 was prepared but never tagged; everything its notes describe
-- the macOS ARM64 qualification on real hardware and the nine repairs it
found, plus the three Windows repairs measured on real MSYS2 hardware -- is
included here, and [`release/v1.3.1/RELEASE.md`](../v1.3.1/RELEASE.md) remains
the record of how those were measured.

Sixty-seven commits since `v1.3.0` (`71ce45b9a`, 2026-09-12).  Every number
below names the run or commit it was taken from; a row without one is a
claim about code, not a measurement.

## Changes since v1.3.0

### The syntax-parsing layer (Doc 204, P1-P5)

The standalone had no way to answer a character's syntax class, to move
through a buffer, or to walk an s-expression; `docs/design/204-*.org`'s
`fboundp` census found the names absent, not merely unexposed.  Five phases
added them:

- P1 (`7e8531aa4`): the standard-name buffer forms see the same current
  buffer as the bridge layer -- two current-buffer variables, one character
  apart, had left `nelisp-char-after` erroring inside a working
  `with-temp-buffer`.
- P2 (`90cab0590`): eleven motion primitives (`forward-char`,
  `forward-line`, `bolp`, `eolp`, `line-beginning-position`,
  `line-end-position`, `current-column`, `following-char`,
  `preceding-char`, `skip-chars-forward`/`-backward`).
- P3 (`a14a0ba7f`): `make-syntax-table`, `standard-syntax-table`,
  `syntax-table`, `set-syntax-table`, `modify-syntax-entry`, `char-syntax`
  and `emacs-lisp-mode-syntax-table`.
- P4 (`fd568a7ce`): `parse-partial-sexp`, `scan-lists`, `scan-sexps`,
  `forward-sexp`, `forward-comment`.
- P5 (`2864b3678`): `emacs-lisp-mode`, `check-parens` (ported from Emacs
  30.1's lisp.el, minus `push-mark`), `save-excursion` as a macro,
  `line-number-at-pos`, and the names the real consumer turned out to need
  beyond the phase's own acceptance script (`back-to-indentation`,
  `search-forward`, and more -- see the commit).

### Foreign function interface

- `ffi:library` / `ffi:defun` (`ca0e3ea67`, `packages/nl-ffi`): a
  declarative surface over the existing `nl-ffi-call`, with typed argument
  conversion and six named error conditions instead of a silent nil.
- Symbols outside the build-time table resolve at run time through
  `dlopen`/`dlsym` on the dynamic reader (`45dade37c`); `:float`/`:double`
  through a `dlsym`-resolved address is now marshalled rather than refused
  (`91c8dd711`).
- The default static reader loads a shared object itself with a pure-Elisp
  ELF loader built on `syscall-direct`/`ptr-read`/`ptr-write`/`alloc-bytes`/
  `ptr-call` (`e3d9fd5fc`), follows `DT_NEEDED` graphs, runs initializers and
  applies IFUNC relocations (`909a5d34d`), and establishes a thread pointer
  for Initial-Exec TLS (`b7f2c103b`).  General-Dynamic TLS, `DT_RELR`
  decoding, unloading and symbol versioning remain refused by name; see
  `packages/nl-ffi/README.org`.
- aarch64 builds no longer receive the x86-64 `xmm0` arms of
  `ptr-call-typed` (`f494ef8e7`; every aarch64 build had failed to assemble).

### Project CLI, toolchain and editor

- `nelisp new/run/build/test/fmt/repl/check/clean`, build profiles
  (dev/release/debug/profile), application arguments, source documentation,
  process benchmarks, locked dependency resolution against explicit HTTPS
  registries with verified offline caches, and a relocatable read-only Linux
  installation with a per-user native build cache (`834cf6366`).  The
  frontend still orchestrates through Python and builds through a host
  Emacs; `docs/strategy-implementation.md` states the boundary.
- `nelisp test --jobs N` runs project tests in isolated processes
  (`304347986`).
- A stdio language server and a VS Code extension (`7236081ab`): syntax
  diagnostics, symbols, formatting, completion, definition/hover/references,
  local rename, signature help, workspace symbols, a Testing view, CLI tasks
  and a project REPL terminal.  The extension's CI step is split into six
  named steps (`824d56375`) and its per-task timeout raised from 90 s to
  300 s after the slow lane was measured against the old ceiling
  (`1d6baba32`).

### Runtime

- Uninterned symbols carry plain names and are not `intern-soft`-visible;
  declaration-aware `let`/`defvar`/`defconst` binding; UTF-8 frame hashes
  (`f25ec9af1`).
- Bignum bitwise arithmetic (Doc 190 Phase C, `00be3b502`): `ash`, `logand`,
  `logior`, `logxor`, `lognot` accept bignums through two's-complement
  windows; `/`, `%`, `mod` accept a bignum dividend with a fixnum divisor of
  magnitude <= 2^31 and signal `nelisp-bignum-division-unsupported`
  otherwise; a fixnum `ash` that leaves the fixnum range now promotes instead
  of wrapping (`(ash 1 61)` is 2305843009213693952, not
  -2305843009213693952).  The standalone bignum smoke grew from 54 to 120
  cases; 44 expressions were compared against host Emacs 31.1 with a
  positive control.  `lsh` had relied on the old wrap in its mask and is
  now written as Emacs 30's subr.el writes it (`dc6bacbd3`).
  **Known gap, measured 2026-09-19 against host Emacs 31.1 (9 of 41 rows
  differ):** `1+`, `1-` and `zerop` reject a bignum with
  `number-or-marker-p`; `format "%d"`, `float` and `truncate` answer 0 for a
  bignum; `expt` signals `overflow-error` where the answer is a bignum;
  `(1+ most-positive-fixnum)` still wraps.  This is the next Doc 190 phase.
- The eight higher-level `bool-vector-*` operations in the prelude no longer
  shadow the natives (`13b6bf39d`; `emacs-compat` 276 -> 268 without moving
  the baseline).
- `alloc-sites` attributes allocations to call sites (`1c76c85c1`).
- The x86-64 assembler's `resolve-fixups` patches the materialised buffer in
  place instead of round-tripping every byte through the interpreted
  evaluator (`cd927153f`, `2cc9f43b7`): on the standalone, 1,535 ms ->
  127 ms for the measured unit, ~8.3% of its previous cost, three runs each,
  collections accounted for.

### Self-hosting (Doc 205)

P1 puts `scripts/` on the load path and adds eight bridges (`711daebcf`);
P2 repairs the three real defects and a fourth the document had denied
(`152342bff`); P3 routes `secure-hash` through an external helper
(`22f210cdd`).  The document's §6.6-§6.7 (`5d6163bed`, `5176634c9`,
`476b0e129`) record why a build without Emacs is still 450-630x slower than
one with it -- the evaluator's floor of about 5 us per form, with the
allocator the largest single cost -- and where the exit criterion could not
be met.  Doc 205's exit criterion is **not** met in this release; the
measurements are the deliverable.

### Packages

- `nelisp-toml`, `nelisp-uuid` and `nelisp-log` (`5aff7bb7f`), and the
  pattern by which an official package is qualified.
- The artifact contract is frozen behind a named service seam and
  `fetch`/`run`/`test`/`build` read through it (`dab0676d5`, `2bb6ed6a1`;
  Doc 203 §8 steps 1 and 3).
- `nl-ns`'s host baseline is pinned to Emacs 30.2 and its README says the
  file is curated, not a dump (`c66fda05b`, `790a64cfa`).

### Platforms

- macOS ARM64 on real hardware: the Darwin path/stat layer and eight
  further repairs (`3233e5be4`), previously deferred since v1.0.  The run
  sheet and measurements are in `release/v1.3.0/MACOS-QUALIFICATION.md` and
  `release/v1.3.1/RELEASE.md`.
- Windows on real MSYS2 hardware: a blind `PATH` search and two of v1.3.1's
  cross-platform repairs that had not worked there (`f8dc4385b`); a replay
  timeout verdict that was computed correctly and then discarded
  (`d31978df8`).

### CI and gates

- The Emacs 30 lanes moved from 30.1 to 30.2 (`84fd31f1c`), the last 30.x
  release.
- The `gates` job now runs the ERT suite before the matrix: the same failure
  is reported at 4.1 minutes instead of 11.4, and a green `gates` job takes
  6.1 minutes instead of 2.4 (`7122a6a34`).
- A failing preflight gate prints what it found (`382f8d1cc`); the reader
  smokes run under `preflight --full` (`d04000d21`); the size ratchet
  refuses to judge a binary it cannot attribute to the checked-out source
  and its pin is CI's own number (`786bb7569`, `8c308d12d`); gates nothing
  ran are registered and a mutation row that proved nothing is retired
  (`25dd1eaee`, `0a5131357`, `057bea143`); the checked soak fails on
  accumulated retention rather than one round (`ffc862a66`).
- The ratchets the new code moved were raised with reasons rather than
  regenerated: unsafe calls (`952eb411f`), the size pin (`18367284e`), the
  three Doc 204 ratchets (`825243412`), the bignum smoke's probe handlers and
  the presence corpus (`ce88e33da`), `lsh`'s divergent-body pin
  (`b932b14c2`).

## Qualification

| Check | Result |
|---|---|
| Branch CI, every lane, code tree `373adde65` | PENDING — run 35431470405 in progress; 13 of 15 jobs green at 24 minutes (gates, tier perf/smokes/extras, four gate-mutation shards, windows 29.4/30.2, macOS 29.4/30.2, ubuntu 30.2) |
| Full ERT (`gates` job, run 35431470405) | 6,105 tests, 5,873 as expected, **0 unexpected**, 232 skipped; preflight 10/10 |
| Check tier (ubuntu 30.2 lane, run 35431470405) | `VERDICT: PASS (23 gate(s))`. Run 35429395922 on `ce88e33da` was red on two source-level ratchets, `ns-gate` and `doc200-census`, settled in `373adde65`; run 35427127362 before it was red on `fallback-inventory` and `substrate-presence-corpus-check`, settled in `ce88e33da` |
| `emacs-compat` shared-shadowing (run 35431470405, Emacs 30.2) | 268 against a baseline of 268; 410 files, 8,830 defined names; PASS |
| Binary-size ratchet (run 35431470405) | 8,334,384 bytes against a ceiling of 8,391,025 (baseline 8,226,496, 2% slack); PASS |
| Standalone shadow differential and Emacs 30.2 parity (run 35431470405) | shadow-smoke PASS (native and prelude agree); emacs-parity PASS, 22,395 bytes identical to stock Emacs 30.2 |
| Version consistency | 9/9 sites say v1.4.0 (`tools/nelisp-version-consistency.sh`, this tree) |
| stage-d-v3.0 standalone parity: linux-x86_64, macos-x86_64, macos-aarch64, windows-x86_64 | PASS — run 35429395865 on `ce88e33da`, all four lanes, tarballs built and verified |
| Semver release pipeline (`workflow_dispatch`, linux-x86_64 blocker with the 1-hour soak; macOS / linux-aarch64 non-blockers) | PENDING |
| Standalone agent-worker consumer (`../nelisp-agent`, 9 shared test files + 2 stdio smokes) | PASS on the `dc6bacbd3` binary, 2026-09-19, 18 s |

## Release qualification

The tag is cut only after the rows above are filled from real runs.  As in
v1.3.0, the semver release pipeline is driven by `workflow_dispatch` against
this tree before any tag exists, so the result is known first.

Windows is a release target of this version: the `stage-d-v3.0 standalone
parity` workflow builds and verifies the `windows-x86_64` tarball on every
push, and the branch CI runs the Windows 29.4 and 30.2 smoke lanes.  This is
the Windows CI result `release/v1.3.1/RELEASE.md` was waiting for.
