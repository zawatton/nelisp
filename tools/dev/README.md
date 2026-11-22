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
