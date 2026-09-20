# NeLisp v1.5.0

Release implementation and qualification notes, written 2026-09-20.

A minor release aimed at one consumer: the `nelisp-agent` host, whose 80
host-only ERT test files now all load and run to their summary line on the
standalone binary (segments 1-3 below).  New public surface: a compat layer
for `ert`, `json`, `subr-x`, `seq`, `simple` and `url-parse`; Emacs-style
command-line flags; subprocess `process-send-*`; the batch-startup
variables; a set of file, buffer, mode and minibuffer functions the agent
uses.  Emacs 29.4 is no longer a supported host; the per-push CI runs on
Emacs 30.2 only and finishes in about half the time.  No existing contract
is intentionally changed.

Sixteen commits since `v1.4.0` (`30036d52b`, 2026-09-19).  Every number
below names the run or commit it was taken from; a row without one is a
claim about code, not a measurement.  The host Emacs on the development
machine is 31.1; where a local comparison is quoted it is labelled so.

## Changes since v1.4.0

### Release blocker (`bd5f0dd12`, `0a72762cc`)

The linux-x86_64 blocker in the semver release pipeline was a one-hour soak
(65.0 min on the v1.4.0 dispatch).  It is now the deterministic
`standalone-reader-checked-soak` gate (five rounds of armed frees with
violation counting, about 5 s) plus a 180 s RSS sanity run under
`tools/nelisp-nothp.c` with a 4 KiB/s growth ceiling: 5.0 min measured on run
35438851189.  The one-hour soak moved to `.github/workflows/weekly-soak.yml`
(Mondays 04:17 UTC).  `tools/gate-mutations.txt` has a row that retains one
block and must turn the new blocker red.

### Segment 1: the agent host's test files load (`24e7a17ba`, `0069ecfef`, `e2e39c017`)

- `standalone-compat/` at the repository root is appended last to the
  standalone's default `load-path` (never to the host `-L`).  It provides
  `ert` (registration, `should`/`should-not`/`should-error`, `ert-skip`,
  `ert-fail`, an Emacs-format batch report), `json` (wrappers over
  `nelisp-json` plus the `json-false`/`json-null`/`json-object-type`/
  `json-array-type`/`json-key-type` variables), and `subr-x`.
- `--load` binds `load-file-name` for the duration of the load and restores
  the outer value afterwards; the host-parity smoke checks the restored value
  against Emacs's, not against nil.
- `generate-new-buffer-name`.

### Segment 2: command line and startup names (`77a95e19b`, `35873692c`)

- `-L DIR` / `--directory DIR` prepend to `load-path`, in argument order.
- `noninteractive` is `t`; `invocation-name` and `invocation-directory` are
  set from `argv[0]`.
- Compat `seq`, `simple`, `url-parse`.
- `file-symlink-p`; `set-process-query-on-exit-flag` /
  `process-query-on-exit-flag` (stored on the process plist); a minimal
  keymap surface (`make-sparse-keymap`, `keymapp`, `define-key`);
  `file-locked-p`.

### Segment 3: the remaining nine files reach their summary (`47d34204a` .. `5d4b6caef`)

- `define-derived-mode`, `run-mode-hooks`, `delay-mode-hooks`,
  `derived-mode-p`, `kill-all-local-variables`, `set-keymap-parent`,
  `lookup-key` (`47d34204a`).
- `process-send-string` on a native subprocess writes to it through
  `nelisp-process-write`; `process-send-eof` and `process-send-region`
  are new (`00c7d349c`; `nelisp-process-adapter`).
- `insert-file-contents` leaves point before the inserted text, as Emacs
  does, and returns `(FILENAME LENGTH)`; `file-modes`,
  `file-in-directory-p`, `file-equal-p` (`9fb86eef3`).
- Emacs-style flags: `-Q`, `-q`, `--batch`, `--no-site-file`,
  `--no-init-file`, `--no-splash` are accepted and ignored; `-l FILE` and
  `-f FUNCTION` run in argument order with `--load`/`--eval`;
  `command-line-args` and `command-line-args-left` (`59353fcc7`).
- `read-string` and `read-from-minibuffer` read a line from stdin in batch,
  honouring the default value, as Emacs does (`5d4b6caef`).
- The mirror `lisp/nelisp-stdlib-misc.el` keeps its byte-compile diagnostic
  ceiling: helpers defined inside `unless` guards are declared before their
  callers (`e32dc661e`).

### CI (`f1455aa9b`)

Emacs 29.4 is dropped from the matrix: its bytecode execution of the
runtime's pure-Elisp hot paths is about 500 times slower than 30.x (measured
on run 32956362867; the native-comp hypothesis was refuted on run
35446949456, where the 30.2 lane reports `native-comp-available-p` nil), and
it was the critical path of every run.  The ERT suite now runs as three
parallel jobs (`plain`, `jit`, `nojit`) on each of ubuntu, macOS and Windows;
`verify` requires all of them.  Wall clock: 26 min on run 35479401233
against 46 min before.  The supported host Emacs is 30.2 (30.x); `README.org`
and `docs/repl-development.md` say so.

### Gates and ratchets moved (each with its reason in the commit)

| Ratchet | v1.4.0 | v1.5.0 | Why |
|---|---|---|---|
| unsafe-inventory `unsafe-call` / `pinned-kernel-call scripts/nelisp-standalone-build.el` | 953 / 831 | 997 / 875 | `--load`'s `load-file-name` (+3), `-L` and the startup names (+17), the Emacs-style flags and action loop (+24) |
| `emacs-compat` shared-shadowing | 268 | 270 | `process-send-eof`, `process-send-region` |
| ns-inventory ns-collision / ns-prefix-violation | 269 / 318 | 274 / 320 | collision +5 from `read-string`'s names (`5d4b6caef`); prefix-violation +2 from the adapter's two names (`00c7d349c`) |
| Binary-size baseline | 8,226,496 (measured 8,334,384) | 8,614,864 (ceiling 8,787,161; measured 8,643,592 on run 35479401233) | segments 1-3 in the prelude and CLI; itemised in `c1acd3468` |
| substrate-presence corpus | 935 names | 956 names | the 21 new names above (regenerated in `24e7a17ba`, `77a95e19b`, `47d34204a`, `9fb86eef3`, `5d4b6caef`; NAME-set diff checked each time) |
| `scripts/nl-ns-accepted.el` (ns-gate accepted divergences) | 149 keys | 145 keys | regenerated in `24e7a17ba`: the four `ns-collision-divergent` keys for `ert-deftest`, `should`, `should-error`, `should-not` (inline ERT shims in the package standalone smokes) ceased to exist once the compat `ert` provided them; nothing added |

## Agent-host census (the release's purpose)

Harness: every host-only test file of `../nelisp-agent` (main `3d85d56`,
with `../nelisp-llm` `7064135` and `../nelisp-photon` `215fd64` on the
`-L` path, exactly the agent Makefile's set) run on the standalone with a
600 s timeout; a file "reaches" when it prints ERT's `Ran N tests` line or
its own `NL-AGENT-<NAME> ALL-PASS/HAS-FAILURES` line.

| Tree | Reached | Tests passed | Tests failed |
|---|---|---|---|
| v1.4.0 (`6d77d0752`), bare `--load` | 30 / 80 | — | — |
| segment 2 (`35873692c`) | 66 / 80 | — | — |
| segment 3 (`5d4b6caef`), 2026-09-20 | **80 / 80** | 222 | 265 |

The 265 failures inside reached files are the next segment's work.  Their
causes, from a census run with the failure condition printed (2026-09-20,
same binary): `void-function` 162 (`lock-file` 47, `markerp` 34,
`file-attribute-file-identifier` 29, `file-remote-p` 25, `special-mode` 8,
`match-string-no-properties` 8, `set-buffer-file-coding-system` 7),
`wrong-type-argument` 36 (25 of them `stringp nil` in one file),
`setf` on a `plist-get` place 16, `void-variable` 11 (`process-environment`
6), `ert-test-failed` 18, agent-domain errors 30.  Every one of the top
thirteen names is absent from the standalone (`fboundp`/`boundp` nil); 33 of
the 64 files with failures fail only on absent names.

## Qualification

| Check | Result |
|---|---|
| Branch CI, every lane, code tree `e32dc661e` | PASS — run 35479401233, 21/21: gates, tier perf/smokes/extras, four gate-mutation shards, smoke ubuntu/macOS/windows 30.2, ert 3 OS × plain/jit/nojit, verify; 26 min |
| Full ERT (`ert ubuntu-latest / plain`, run 35479401233) | 6,105 tests, 5,932 as expected, **0 unexpected**, 173 skipped |
| Check tier (ubuntu 30.2 lane, run 35479401233) | `VERDICT: PASS (23 gate(s))`; checked-soak live blocks per round 337,405 / 337,411 / 337,412 / 337,412 / 337,410, violations 0 |
| `emacs-compat` shared-shadowing (run 35479401233, Emacs 30.2) | 270 against a baseline of 270; PASS |
| Binary-size ratchet (run 35479401233) | 8,643,592 bytes against a ceiling of 8,787,161 (baseline 8,614,864, 2% slack); PASS |
| Standalone shadow differential and Emacs 30.2 parity (run 35479401233) | shadow-smoke PASS; emacs-parity PASS, 22,395 checks |
| Version consistency | 9/9 sites say v1.5.0 (`tools/nelisp-version-consistency.sh`, tree `2a650ee59`) |
| stage-d-v3.0 standalone parity: linux-x86_64, macos-x86_64, macos-aarch64, windows-x86_64 | PASS — run 35479401265 on `e32dc661e`, all four lanes |
| Semver release pipeline (`stage-d-v2.0-release.yml`, `workflow_dispatch` with `release_version=v1.5.0` on `2a650ee59`, run 35486093104) | PASS — blocker linux-x86_64 5 min (checked-soak PASS, violations 0; 180 s RSS sanity under `nelisp-nothp`: 103 batches, start 66,472 KiB, sampled peak 66,472 KiB, growth 0 against a 4 KiB/s ceiling), blocker linux-arm64 5 min; artifacts `v1.5.0-linux-x86_64` (4,847,514 bytes), `v1.5.0-linux-arm64` (6,793,308 bytes), `v1.5.0-manifest`; macos-arm64 deferred (unverified), as in v1.4.0 |
| Standalone agent-worker consumer (`../nelisp-agent`, 9 shared test files) on Windows | PASS 9/9 on the v1.4.0 `nelisp.exe` (13,224,960 bytes, MSYS2, 2026-09-19, 61.6 s); the two stdio smokes fail on the harness's POSIX assumptions (`sleep 1` shorter than the 4.9 s Windows start-up; MSYS FIFO as stdin), not on the worker — handed to the agent repository |
| Agent host-only census | 80/80 reached on `5d4b6caef` (table above); the binary is `5d4b6caef`'s tree, which differs from `e32dc661e` by the mirror's `declare-function` lines only |

## Release qualification

Every row above is filled from a named run.  The code tree is `e32dc661e`;
`2a650ee59` changes the version sites only and the notes commit after it
changes documentation only.  As in v1.4.0, the semver release pipeline was
driven by `workflow_dispatch` against `2a650ee59` before any tag existed
(run 35486093104, PASS in 5 min per blocker), so the result was known first;
the tag push runs the same pipeline once more.

## Known gaps carried into the next segment

- The 265 in-file failures listed under "Agent-host census"; the criterion
  for segment 4 is void-function + void-variable failures 173 → 10 or fewer.
- Windows worker start-up of 4.9 s (`--eval "(+ 40 2)"` wall clock on the
  v1.4.0 build); not decomposed yet.
- Bignum: `1+`/`1-`/`zerop`, `format %d`/`float`/`truncate` on bignums,
  `expt` (Doc 190 Phase D), unchanged from v1.4.0.
