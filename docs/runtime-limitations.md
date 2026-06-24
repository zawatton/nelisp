# nelisp runtime — known limitations

Living reference (snapshot: 2026-06-24).  These are the known constraints,
gaps, and approximations of the **nelisp runtime** — the native code emitted
by the AOT backend (and the standalone NeLisp interpreter) — that make
compiled C *not always behave identically to the same C built with a native
toolchain*.  This is the basis for the "honest scope" of downstream showcases
(e.g. C-to-nelisp demos): a function only behaves like its C original when it
avoids the items below.

Companion: [`design/142-native-exec-findings.md`](design/142-native-exec-findings.md)
(object-mode loader / hidden-boundary ABI).

Grouped A–E by impact on C-equivalence.  Each item cites a primary source.

---

## A. Host-environment dependence (C using these is not reproducible)

- **Time / random are unimplemented.**  No AOT grammar for `current-time`,
  `get-internal-run-time`, `random`, etc.; C depending on wall-clock or RNG
  does not match native.  → `src/nelisp-eval.el:765`.
- **`getenv` is Linux x86_64 only.**  Environment access is gated to that
  target; other platforms cannot read the environment.
  → `lisp/nelisp-cc-bi-getenv.el:44, 151-153`.
- **No nested `extern-call` as a call argument.**  A nested external call must
  be hoisted to its own defun (to keep eval order well-defined).
  → `lisp/nelisp-cc-bi-getenv.el:104-105`.

## B. extern / libc calls are link-time dependencies (`.el` is not self-contained)

- AOT objects reference external symbols (`nl_alloc_symbol`, `strlen`, `sin`,
  …) and emit PLT32 relocations the linker MUST resolve; unresolved symbols
  fault at run time.  The emitted `.el`/`.o` alone does not run libc-dependent
  code — it must be linked.
  → `docs/design/142-native-exec-findings.md:16-29`,
    `lisp/nelisp-aot-compiler.el:185-190`,
    `lisp/nelisp-cc-extern-call-f64.el:20-26`.

## C. Floating point (f64 / double) gaps

- **`double` arguments/returns through `extern-call` are unsupported.**  cfront
  carries a `double` as i64 bits, which does not fit the AOT f64-call leaf
  shapes (`:f64-leaf-shape-unsupported`); this is a front-end/AOT-bridge gap.
  **`va_arg` of a `double` (the fp_offset path) is likewise unsupported**
  (loud error).
- **`extern-call-f64` caps at 8 f64 args**; mixing f64 and integer args makes
  register scheduling complex and is staged.
  → `lisp/nelisp-aot-compiler.el:136-140, 549-557`.
- **f64 arithmetic is x86_64 only** today; aarch64 is being migrated.  NaN
  comparisons follow IEEE-754 (always false / unordered).
  → `lisp/nelisp-cc-jit-float.el:28-30, 68-74`.

## D. syscall / memory / GC

- **syscall 6th argument (a5) is fixed to 0** (no 7-arg defun stack-slot
  support yet) → `mmap` with a non-zero `offset` is not expressible.
  **Raw syscalls are Linux x86_64 only.**
  → `lisp/nelisp-cc-jit-syscall-call.el:42-50, 56-57, 96`.
- **mmap allocator is 4096-byte granular**: small objects still consume a full
  page.  Mixing Rust-heap boxes with the mmap allocator risks `free`/`munmap`
  mismatch.
  → `lisp/nelisp-cc-alloc-mem.el:18-29, 51-58`.
- **GC root stack must be initialised** (`nl_rootstack_init`); before that the
  base pointer is 0 and `nl_gc_mark_rootstack` is silently skipped, which can
  produce incomplete marking.
  → `lisp/nelisp-cc-rootstack.el:19-23`.

## E. AOT grammar / ABI — staged support

- **Argument registers: 6 GP (rdi, rsi, rdx, rcx, r8, r9) + 8 FP (xmm0–7).**
  Functions beyond the register file use stack-passed slots with caps, and
  some argument-class shapes (mixed GP/FP on the stack) are only partially
  supported.  → `lisp/nelisp-aot-compiler.el:136-140, 544-557`.
- **object-mode defuns require an 18-slot hidden-boundary block** after the
  parameters (`out`, `mirror`, `frames`, `scratch`, `name_slot`, callback
  slots 0–11); a loader that does not populate them gets undefined behaviour,
  and defun `:arity` / `:rt-slot-count` / `:body-offset` metadata must be
  accurate.  → `docs/design/142-native-exec-findings.md:36-58, 136-140`.
- **Dynamic binding of special variables in a runtime `let` is unsupported**
  (only statically-analysable bindings).  → `lisp/nelisp-aot-compiler.el:175`.
- **Odd-arity stack-alignment double-correction — FIXED (2026-06-24).**  The
  prologue always rounds post-prologue rsp to 0 mod 16, so a call site's
  alignment depends only on the words that call itself pushes, NOT on the
  enclosing defun's arity.  Several `needs-align` formulas still added the
  arity, so an odd-arity caller reached a call site at rsp ≡ 8 mod 16 — latent
  because most libc callees tolerate it, but a SIGSEGV in SSE-heavy callees
  (e.g. `vsnprintf`).  All SysV paths were brought in line with the win64
  branch (which never added arity): `--emit-extern-call` (`needs-align` +
  `spill-needs-align`), `--emit-call` (both the ≤6-arg and 7+-arg stack
  paths), and `--emit-runtime-call-args`.  The now-unused
  `--current-defun-arity` readers were removed.
  → `lisp/nelisp-aot-compiler.el` (`--emit-call`, `--emit-extern-call`,
    `--emit-runtime-call-args`).  Regression test:
    `nelisp-cfront-odd-arity-stack-arg-extern-align-e2e`.

---

## Out of scope here (packaging / platform reach, not C-equivalence)

These appear in `RELEASE_NOTES.md` and concern build/platform reach rather
than runtime semantics of compiled C: Linux x86_64 is the CI blocker, arm64 is
best-effort, 32-bit ARM and Windows native (`--no-emacs`) are out of scope;
macOS notarization is a placeholder; the Japanese coding tables are partial
(~885 entries).  → `RELEASE_NOTES.md:36-50, 103, 105-106, 109-110`.

## Practical implication

For a compiled-C function to behave identically to native C it must: take no
time/RNG/env dependency (A), be linked against the libc/runtime symbols it
calls (B), avoid `double` across `extern`/`va_arg` (C), avoid the syscall/mmap
edges (D), and stay within the supported argument/ABI shapes (E).  Curated
showcases pick functions that satisfy all of the above and then verify their
output byte-for-byte against a native build.
