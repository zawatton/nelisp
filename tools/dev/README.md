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

# nl-prof: function profiler for code in the emacs-lib runtime image

Build-free (Lisp only). One run gives, per wrapped function: calls, inclusive and exclusive ns, ns/call and the
top caller->callee edges, for any form run in the image booted from a flat dump (0.03 s boot).

    tools/dev/nl-prof.sh FORM-FILE [--prefix P]... [--functions F...] [--runs N] [--warmup N] [--top K]
                         [--setup FILE] [--log-slots N] [--bin PATH] [--dump FILE] [--cpus LIST] [--timeout S]

Env: `NL_PROF_BIN` (default `target/nelisp`), `NL_PROF_DUMP` (default `$NEMACS_RUNTIME_DUMP` or
`build/nemacs-runtime.dump`; bake: mk/emacs-lib.mk `bake-runtime-dump`), `NL_PROF_CPUS` (default 9,25).
Runs under `ulimit -v 8388608`, timeout, stdin closed, `taskset`.

* FORM-FILE: forms evaluated N times (after `--warmup` unprofiled runs so the fx tier has compiled them).
  `--setup FILE` is evaluated once first (define functions, create the buffer). Forms run from a top-level
  `eval`, never inside a `load`, so the in-load trap (F95, ~5x slower) does not apply. Do not `load` timed code.
* Selection: `--prefix P` wraps every fboundp symbol starting with P (via `nelisp--intern-table-symbols`;
  `mapatoms` is unsupported in the image); `--functions` names them. Macros, special forms, autoloads and the
  profiler's own dependencies are skipped. Wrapping many functions is fine (589 in the forward-word example).
* Method: each function's symbol cell gets a wrapper `(lambda (&rest args) (nl-prof--call ID 'orig args))`
  built with `eval` so it has no captured variables (a closure over variables runs in the slow interpreter).
  The hot path only stores (id, `nelisp--monotonic-ns`) at enter and exit into a preallocated vector; exit is in an
  `unwind-protect` cleanup so throw/signal stay nested; all arithmetic is offline. Recursion: inclusive time counts
  the outermost activation only. Everything is restored afterwards, also when the form signals.
* Overhead: an empty wrapped leaf is calibrated each run (about 0.9 us inside + 1.4 us outside the window per call)
  and subtracted. Because nested wrappers cost more in real code, the same forms are also timed unwrapped and the
  overhead is scaled so the predicted total equals measured instrumented minus unprofiled wall time (scale printed,
  only trusted when the overhead is >30% of the run; CAUTION if scale <=0.5 or >=2.5). Percentages are of the
  unprofiled run. Rows with <1 us/call are at the noise floor of the correction.
* Blind spots: calls through a captured function object or a function-valued variable (`files--native-insert`
  is applied that way, so the native part of `insert` shows as insert's exclusive time), and primitives the VM
  calls directly. Exclusive ns/call of tiny leaves carries a few hundred ns of error.
* Validation (state-perf F172): counts exact through recursion/throw/signal (`nl-prof-selftest.sh`, with a negative
  control), insert/erase-buffer within 20% of a hand probe, forward-word inclusive 2.78 ms vs hand 2.74-2.80 ms.
* GNU `elp.el` is not usable here: `elp-results` signals `unsupported-feature`, and elp has no exclusive time.

Examples (`nl-prof-examples/`):

    tools/dev/nl-prof.sh nl-prof-examples/insert-erase-form.el --setup nl-prof-examples/insert-erase-setup.el \
        --functions insert erase-buffer --runs 3000
    tools/dev/nl-prof.sh nl-prof-examples/forward-word-form.el --setup nl-prof-examples/forward-word-setup.el \
        --prefix emacs-syntax- --prefix emacs-edit --prefix char- --prefix syntax- --functions forward-word

## gen-bundle.py (bootstrap bundle generator and audit)

    tools/dev/gen-bundle.py gen   --tree DIR [--rev REV] --out OUT --name vN [--overlay DIR --overlay-file REL ...] [--ref BUNDLE] [--cpus 9,25]
    tools/dev/gen-bundle.py check --bundle B --tree DIR [--rev REV] [--overlay DIR --overlay-file REL ...] [--cpus 9,25]

The gnu562-style image bundle (`process-runtime-chartab-repair-v1.el`) is the unmodified output of
`scripts/build-nelisp-bootstrap.el` (`make build-nelisp-bootstrap`, about 12 s). Members sit between
`;;; >>> LOGICAL` and `;;; <<< LOGICAL` lines (`src/X.el` maps to `packages/*/src/X.el`); the text outside them
(prologue, load-path with the seed root, canonical-provider seam) is generator output too. Measured on
bundle-v67 and bundle-v9: no hand-written glue exists, so nothing needs keeping verbatim; hand patches
(mkbundle.py hunks) show up only as member differences.

`gen` snapshots TREE (working tree, or `--rev`) into `OUT/seed-NAME` (dirs packages scripts lisp src vendor mk plus
Makefile), runs that tree's own generator there (host emacs, `ulimit -v 8388608`, timeout, stdin closed) and writes
`OUT/bundle-NAME/<bundle>.el` and `OUT/gen-NAME.json` (commit, dirty files, sha256). Both output paths must be new.
The bundle embeds the seed path, so two runs are byte-identical after path normalisation. Use `--overlay DIR
--overlay-file F` to take single files from another checkout. `--ref BUNDLE` compares the result as `check` does.

`check` regenerates into a temp dir and reports, between what runs (bundle) and what you read (tree): glue
differences, members only in either side, member order, and per divergent member the changed (`~`), bundle-only
(`B`) and tree-only (`T`) top-level forms (a form is a column-0 `(`; defs key on name, other forms on their first
line). Exit 0 identical, 1 divergent, 2 failure. It never writes inside the tree or next to the bundle.

Measured: bundle-v9 vs `4983f92c7` 353/356 members identical (3 explained hand patches); bundle-v67 vs its seed 352/356
(buffer-builtins, process-buffer, cc-aggregate-arity-1, cc-print-1: patch on one side only); bundle-v67 vs
`origin/feat/emacs-lib-import` 324/354 with `emacs-buffer-builtins--change-edit` (no PUBLIC arg, no `--run-edit`) listed.
An image built from a regenerated bundle ran the casebench public batch with matching results on every case.
