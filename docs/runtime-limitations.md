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

## A. Host-environment dependence — two layers, don't conflate them

- **Compiled C reaches the host environment through ordinary extern libc
  calls, and gets REAL values.**  `time(0)`, `clock_gettime`, `getenv`,
  `rand`/`srand`, etc. are just `extern-call`s the linker resolves against
  libc, so a cfront-compiled function returns the real wall clock (NOT a 1970
  stub), reads the real environment, and runs the real RNG.  Verified:
  `nelisp-cfront-host-env-via-extern-libc-e2e` (in nelisp-cfront).  The only
  caveat is the universal one (§B): the symbols must be linked.
- **The nelisp ELISP interpreter's own host builtins are a separate,
  AOT-unimplemented layer** — this is what "no clock / 1970" refers to, and it
  does NOT constrain compiled C.  When the AOT'd *interpreter* evaluates elisp,
  there is no AOT grammar for `current-time` / `get-internal-run-time` /
  `random` (→ `src/nelisp-eval.el:765`), and the `getenv` *builtin* wiring is
  gated to Linux x86_64 (→ `lisp/nelisp-cc-bi-getenv.el:44, 151-153`).
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

- **`double` arguments/returns through `extern-call` — SUPPORTED (2026-06-24).**
  cfront carries a `double` as i64 bits in a gp slot; an extern `double`
  argument is now bridged into its xmm register via `bits-to-f64` (MOVQ) and a
  `double` return is bridged back via `f64-bits` (`--emit-f64-bits` accepts an
  `extern-call` IR node with `:ret-class f64`).  This lets cfront call the
  standard-ABI libm (`sqrt`/`sin`/`pow`/`ldexp`, incl. mixed f64+int args).
  Regression test: `nelisp-cfront-float-extern-libm-e2e`.  (Note: cfront's own
  defined `double` functions still use the gp-bits ABI for their *own* params /
  return, so a C caller bit-casts across them — see that test's `B`/`U`.)
  **Still unsupported:** `va_arg` of a `double` (the fp_offset walk) — loud
  error.
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

## F. Standalone Lisp raw-byte characters

- **The historical raw-byte string refusal is being removed through verified
  C-core slices.**  The batch18 reader preserves GNU byte8 characters
  `#x3FFF00 + B` in multibyte strings and passes 23 storage/printer comparisons.
  Batch19 implements explicit conversions and mixed concat/format, with
  append/vconcat preserving numeric input characters.  The final native reader
  passes 100/103 conversion cases; three error-position cases remain red.
  The package's Lisp diagnostic provider corrects those positions and passes
  103/103 with the same reader.  Explicit ASCII-multibyte state, cloning and
  GC are covered by these focused tests.  Mixed raw-byte/Unicode
  reader literals such as `"\310あ"` retain the reader-specific refusal.
  Do not infer complete string compatibility from these focused cases.
  Primary sources: `scripts/nelisp-standalone-build.el`,
  `scripts/nelisp-stdlib-prelude.el`,
  `test/nelisp-character-storage-regression.py`, and
  `docs/design/200-unibyte-string-representation.org` §8.7.
- **String `aset` follows the stricter Emacs 31.1 fixed-width rule and
  deliberately differs from the Emacs 30.1 parity host in two cases.**
  Unibyte strings accept only values 0–255.  Multibyte strings mutate only
  when both the replaced and replacement characters are ASCII, keeping the
  exact Sexp tag and `string-bytes` unchanged.  Emacs 30.1 instead turns a
  copied `"あ"` into `"a"` and a copied `"ab"` into `"あb"`; NeLisp signals
  and leaves each string unchanged, matching the 31.1 rule quoted in Doc 200
  §2.  Primary implementation and executable assertions:
  `scripts/nelisp-standalone-build.el` (`bf_aset_unibyte_string`,
  `bf_aset_multibyte_string`),
  `lisp/nelisp-cc-evalport-nonenv-mut-str-set-cp.el`, and
  `test/nelisp-doc200-unibyte-repr-test.el`.

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

### Shared active catches (U8r)

The evaluator and GNU bytecode VM share a thread-local active-catch registry.
A throw searches it before publishing an exit. An unmatched tag or a nil tag
signals `(no-catch TAG VALUE)` at the throw site, so a local `condition-case`
can handle it. Cleanup keeps enclosing catches active and retires catches
when their saved unwind tail is crossed. A cleanup exit restarts handler
selection before any enclosing cleanup runs. VM registration root pairs are
reused after popping, so root use follows nesting depth rather than loop count.

U8b native registration uses the raw entry pair
`nl_ct_active_push(env, tag_slot, node)` / `nl_ct_active_pop(env, node)`.
The caller supplies a stable, registered 32-byte tag root and a separate
registered 32-byte integer metadata root. Push returns `node`; pop returns
zero. Pop in strict LIFO order before releasing either root, on normal and
non-local exits. Neither entry allocates nor alters the pending-exit stash.
`node+8` holds the preceding link, and `node+16` holds the tag-slot address.
Main evaluation uses `nl_catch_head` in driver BSS; a registered worker uses
`env+160`. These are evaluator-internal entries, with no new public Lisp
builtin. Registry entries do not perform native landing themselves.

U8n lowers GNU bytecode opcodes 48–50 through `nl_native_frame_v2`.
After a callback reports an exit, the active native handler chain selects
the landing block, retires crossed catches, runs U7 cleanup and restores
the U8a operand bank. A cleanup replacement exit repeats selection with
the remaining handlers. Unmatched exits use the existing raw-v2 exit triple.
Handler constants are initialized from the live bytecode constant vector,
preserving object identity across private cache serialization and GC.

Native cache recipes refuse unreadable buffer and marker constants, including
objects nested in constant data. They cannot be relocated from a serialized
artifact, matching GNU native-comp's refusal to spill such objects into .eln
files. Compilation reports `Cache relocation refused`; the original function
remains byte code and can still run. U10 checks this refusal on the original
fixtures, then tests their opcode lowering with source-owned live variables.

The standalone bytecode VM does not decode 16-bit `stack-set` (179). U10's
single straight-line offset-1 fixture uses the equivalent 8-bit encoding only
for the VM comparison; its original GNU oracle and both native compilations
retain opcode 179. This does not qualify the VM's 16-bit instruction support.

`nl-signal` is an opt-in Lisp frontend for explicit `signal` calls: it invokes
`signal-hook-function` and reuses the existing .eln debugger decision. U10
loads it for its four exceptional-ordering cases. Errors raised directly by
runtime primitives do not acquire this frontend's hook behavior. Loading the
feature again preserves the installed function. The supplemental handler and
cleanup use named Lisp callbacks because nested executable bytecode constants
are outside the data-only cache recipe; all four original GNU observations
remain unchanged.

The standalone reader's `buffer-string` currently returns the whole buffer
when narrowed. U10 captures accessible text with `buffer-substring` between
`point-min` and `point-max`, preserving the GNU observation and separately
checking both restriction bounds.

U10 retains its first GNU-matching VM observation across compile/load and
rejects changes to the original function, code, constant-vector identity,
nested data and metadata before invoking native code. The former duplicate
VM invocation after mapping (and its incidental pre-entry GC stress) is
removed. GC inside every native invocation remains mandatory. Worker fixtures
are exact projections of the complete GNU master, authenticated by host
readback and generator-issued byte hashes. Every selected fixture still runs
in both compile and fresh-load phases; the loader's actual mapped header is
used for ABI checks instead of rereading the artifact in the harness.
