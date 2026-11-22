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
