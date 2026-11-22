# nl-dev-session: resident NeLisp image for build-free probes

One long-lived `exec-runtime-image` process; probes are sent as files and cost milliseconds.
No binary rebuild, no image re-bake. The base binary/image are used read-only.

    export NLDS_BASE=<dir with target/nelisp and build/nemacs-runtime.nlri>   # read-only base
    export NLDS_CPUS=9,25 NLDS_TIMEOUT=120          # taskset list (SMT pair), per-eval timeout (s)
    tools/dev/nl-dev-session.sh start               # ~65 s once (image replay of the 8 MB bootstrap)
    tools/dev/nl-dev-session.sh eval-string '(+ 1 2)'
    tools/dev/nl-dev-session.sh eval FORM-FILE      # forms are read+eval'd one by one; last value printed
    tools/dev/nl-dev-session.sh load-el FILE.el [real]   # (re)load a changed .el; `real` = image's own load
    tools/dev/nl-dev-session.sh load-neln FILE.neln SYM...   # needs (require 'nelisp-native-load) first
    tools/dev/nl-dev-session.sh status | log | stop

Properties
- Process runs under `ulimit -v 8388608`, stdin /dev/null, optional taskset. State dir NLDS_DIR.
- Per-eval timeout: on expiry the client kills the session (own pid only) and says so; restart costs 65 s.
- Fast loader (default, NLDS_FAST=0 to disable): `load`/`require` of .el files under the lane's `lisp/`
  (plus NLDS_LOAD_PATH dirs) read+eval each form instead of the image's `load`. 0.14 s vs 50 s for
  lisp/nelisp-native-load.el; `(require 'nelisp-native-load)` 0.24 s vs 36-97 s. Differences: no reader
  rewrites, no eager macroexpansion, `load-in-progress` stays nil. Vendor/baked files keep the real loader.
  Use `load-el FILE real` when you need the real loader semantics.
- In-load trap (state-perf F95): requests are evaluated by the loop, not inside a `(load ...)`; the client
  refuses a request containing both `(load ` and timing code (NLDS_FORCE=1 overrides). Send the load and the
  timed form as separate requests. `(nl-dev-time-call FN)` errors if `load-in-progress` is set.
- Output (princ) and `NLDS=> VALUE` / `NLDS-ERR ...` come back on the client's stdout.

A truncated or unbalanced final form in a loaded file signals an error (the reader's end-of-file is the same for a clean end and for an unfinished form, so the rest after the last form must be whitespace/comments). A warning (NLDS-WARN) is printed on first fast load: fast-loaded code can run on a different tier than under the real loader (F95), so there is no timing claim and no tier/semantic equivalence claim; time in cold runs only.

Verdicts from the session license "correct and worth batching" only, never perf numbers or "integrated"
(see review-fable-buildfree-strategy.md section 2). Pin probes to CPUs that no benchmark uses.

# Overlay manifest and differential gate

Build-free lane verification (see review-fable-buildfree-strategy.md, investment 2). No NeLisp rebuild.
Verdicts license "correct and worth batching" only, never "integrated" or any perf number.

## overlay-manifest

    tools/dev/overlay-manifest --repo DIR --base SHA --lane NAME=REF|WORKTREE [--lane ...] [--format tsv|summary]

Per lane: every file changed vs the base (three-dot diff), its tier, and the changed/added/removed top-level
definitions (column-0 `(defun|defvar|defconst|defmacro|...`, compared by form hash). Tiers:

| Tier | Files | Meaning |
|---|---|---|
| T0 host-only | `lisp/nelisp-aot-compiler.el`, `lisp/nelisp-artifact.el`, `test/`, `tools/`, `docs/`, other | verify on host Emacs |
| T1 neln hot-load | (none by path) | .neln leaves are derived from T0 compiler output; no source file is T1 |
| T2 Lisp overlay | other `lisp/*.el`, `packages/**/*.el` | load into the base image |
| T3 needs rebuild | `src/`, `mk/`, `Makefile`, `scripts/nelisp-standalone-build.el`, `lisp/nelisp-cc-fx-call-primitives.el`, anything named `*preload*`/`*bootstrap*` | batch build only |

The lane's overall tier is the maximum. Duplicate-symbol check: a definition changed in two lanes with different
text prints `DUPLICATE` and the exit status is 3 (merge conflict or silent override). Warns when a lane is not
based on the given base.

## overlay-diff.sh

    tools/dev/overlay-diff.sh --lane LANE --base-home DIR --base-dump FILE \
        [--overlay F]... [--new-home DIR (--new-dump FILE | --new-nlri)] [--corpus syn,gg,aot] [--out DIR] [--inject-row]

Runs a fixed corpus on (a) the base binary booted from a flat dump with the overlay loaded, and (b) a new binary
(without overlay), normalises, diffs. Exit 0 identical, 1 different, 2 run failure (a side producing no rows is a
failure, never "clean"). Without `--new-home`, (b) is (a) again (self-diff). Default overlay: the lane's T2 files.

Corpus (`tools/dev/overlay-corpus/`): `syntax-parity.el` R rows, `gg-driver.el` checksums (timings dropped),
and the AOT lisp-arith differential cases compiled by the lane's compiler (`aot-run.el`) and run natively.

Flat dump (about 44 s, once per base binary; 0.03 s boot, digest-checked against that binary, re-bake after any rebuild):
see mk/emacs-lib.mk `bake-runtime-dump`. All runs use `ulimit -v 8388608`, timeout, stdin closed, `taskset` (default
CPUs 6,22).

Negative control: `--inject-row` adds one row to (b); the gate must exit 1. A real check: run (a) with and without
a mutated overlay file.

Not covered: speed/tier, GC-root and stack behaviour, load order of baked files (the overlay is loaded after
the bake), functions called by direct linked symbol. Those need the final batch build.
