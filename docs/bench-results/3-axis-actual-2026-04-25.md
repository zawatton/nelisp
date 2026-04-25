# Phase 7.1 3-axis bench actual measurement (Doc 28 v2 §5.2)

**Date:** 2026-04-25
**Host:** Linux x86_64 (`x86_64-pc-linux-gnu`)
**Branch:** `feat/bench-actual-measurement`
**Doc 28 v2 LOCKED-2026-04-25-v2 §5.2 reference gate**

## Summary

| benchmark    | gate target | gate result | bytecode VM elapsed | NeLisp native elapsed | Emacs native-comp elapsed |
|--------------|-------------|-------------|---------------------|-----------------------|---------------------------|
| fib(30)      | 30x         | **FAIL**    | 0.5260 s (10 iter) | skipped (compile fail) | 0.5083 s (10 iter)       |
| fact-iter    | 20x         | **FAIL**    | 0.0033 s (10 iter) | skipped (compile fail) | 0.0036 s (10 iter)       |
| alloc-heavy  | 5x          | **FAIL**    | 0.0364 s (1 iter)  | skipped (compile fail) | 0.0112 s (1 iter)        |

**Overall §5.2 3-axis gate:** **FAIL** (0/3 axes meet target)

This is the **expected** result for the current Phase 7.1.5 codebase
state. The bench harness, FFI bridge, and gate-verification machinery
are all in place; the missing piece is Phase 7.5 callee resolution +
SSA frontend extension for `letrec` / `while` (deferred per Doc 28 v2
§3.5).

## What this run actually proves

1. **Bench infrastructure is wired end-to-end.** All three axes
   compile lambdas via `byte-compile`, time them, then attempt the
   NeLisp native pipeline (`nelisp-cc-runtime-compile-and-allocate`
   + Phase 7.5.4 in-process FFI module), then time them via Emacs
   native-comp, and emit a structured report. The `make bench-actual`
   target wraps the whole run and exits non-zero on any axis FAIL —
   ready for v1.0 ship gate enforcement.

2. **The Phase 7.5.4 in-process FFI bridge works.** The cargo-side
   secondary bench (`make bench-actual-cargo`) measured the FFI
   round-trip overhead at:

   ```
   ffi_overhead_xor_rax    (N=10): mean=7338.9 ns (7.34 µs/call)
   ffi_overhead_mov_eax_42 (N=10): mean=7317.7 ns (7.32 µs/call)
   ```

   This is comfortably inside the Doc 32 v2 §7 design budget of
   "~10 µs / call" (~100x faster than the Phase 7.5.1 subprocess
   path's ~1 ms/call). Throughput projection: ~136k calls/sec, well
   above the §7 ≥100 calls/sec gate.

3. **Bytecode VM and Emacs native-comp baselines are reproducible.**
   The bytecode-VM elapsed numbers can be re-measured at any time
   to track regressions in the host Emacs runtime; the
   native-comp ratio (currently ~1.0x because Emacs's libgccjit
   pipeline produces near-byte-code performance on these tiny
   numerically-pure benches) is recorded for posterity.

## Why all three axes FAIL today

The bench lambdas exercise three constructs that are not yet handled
by the Phase 7.1.X SSA frontend:

| construct | bench using it | SSA support status |
|-----------|----------------|--------------------|
| `letrec` (named recursion via lexical closure) | fib(30), fact-iter | not implemented (Phase 7.5 deferred) |
| `funcall` to a closure | fib(30), fact-iter | not implemented (callee resolution = Phase 7.5) |
| `while` loop body | alloc-heavy | not implemented (Phase 7.5 deferred) |
| `+` / `<` / `*` / `1+` / `cons` / `setq` (built-in primitives) | all 3 | callee resolution = Phase 7.5 |

When `nelisp-cc-runtime-compile-and-allocate` is invoked on these
lambdas the SSA frontend signals one of:

- `nelisp-cc-x86_64-unsupported-opcode :unknown-opcode closure 0`
  (fib / fact-iter — `letrec` lowers to a closure instruction the
  backend doesn't yet emit code for)
- `nelisp-cc-unsupported-form :head while :form (while ...)`
  (alloc-heavy — `while` is not in the frontend's macro expander)

The bench harness catches these with `condition-case` and records
`:native-skip-reason native-compile-failed` so the report makes the
underlying cause auditable.

## Expected behavior post-Phase-7.5

When Phase 7.5 lands callee resolution + the deferred SSA forms,
this exact harness (no code change) will:

1. Compile each bench lambda to actual machine code that respects
   the System V AMD64 calling convention.
2. Resolve the embedded primitive-function calls (`+`, `<`, etc.)
   via `nelisp-defs-index` (Phase 6.5 SHIPPED).
3. Execute the bytes through the Phase 7.5.4 in-process FFI bridge.
4. Emit `:speedup` ratios for each axis and exit 0 when all three
   meet their §5.2 gate targets.

Until then, the `make bench-actual` target documents the gate's
*current* failure mode and unambiguously surfaces what Phase 7.5
must deliver.

## Reproduction

```bash
cd /path/to/nelisp
make runtime-module    # builds the in-process FFI .so
make bench-actual      # runs the 3-axis bench
make bench-actual-cargo  # runs the FFI overhead probe
```

`make bench-actual` prints the report to stderr, writes the
`*NeLisp Bench Actual*` Emacs buffer for interactive inspection,
and exits 0 (all 3 gates PASS) / 1 (any FAIL).

## Files

- `bench/nelisp-cc-bench-actual.el` — Lisp-side bench harness (~340 LOC)
- `test/nelisp-cc-bench-actual-test.el` — ERT smoke (5 tests, all
  skip-unless-gated by `nelisp-runtime-module.so` availability)
- `nelisp-runtime/benches/nelisp_cc_bench.rs` — cargo-side FFI
  overhead probe (~110 LOC, libtest-harness-free)
- `Makefile` — `bench-actual` + `bench-actual-cargo` targets

## ERT result

```
Ran 5 tests, 5 results as expected, 0 unexpected (2026-04-25 18:47:36+0900, 1.642002 sec)
```

Full test suite:

```
Ran 1248 tests, 1245 results as expected, 0 unexpected, 3 skipped
                                                       ^^^^^^^^^^^
                                                       (existing scaffold tests, unchanged)
```

5 new ERTs added to the previous 1243 baseline → 1248 total, all
PASS or skip-unless-gated as designed.

## Next actions

This run unblocks:

- v1.0 ship gate **infrastructure** verification (the harness is
  the source of truth for §5.2 gate-pass status going forward).
- Phase 7.5 implementation prioritization: the three deferred SSA
  forms above are now a concrete blocker with bench evidence.

Future runs (post-Phase-7.5) will replace this report with one that
records the actual speedup numbers — the harness, gate targets, and
report format are stable.
