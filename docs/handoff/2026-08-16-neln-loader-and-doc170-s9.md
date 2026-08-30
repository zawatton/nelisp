# Handoff: in-process `.neln` loader, and the Doc 170 §9 measurement

Branch `feat/aot-dynamic-user-calls`, 11 commits ahead of `31f75537`.
Everything below is on that branch. Nothing is merged to `main`.

The goal that is still open: **measure the Doc 170 §9 borrow budget on the
AOT native path.** Everything else here exists because it was in the way.

---

## 1. State: what works

```
make test                      5001 tests, 4648 as expected, 0 unexpected, 353 skipped
make compile                   clean (includes the nl-check gate)
make neln-loader-test          10 cases, 0 failures
make standalone-reader-realrt-smoke   exit 42
```

`lisp/nelisp-native-load.el` reads a `.neln` at run time and executes its
native code inside the standalone reader — no linker, no `cc`, no
subprocess. Ten shapes pass: both calling conventions, arity 0 through 6,
a string in and a string out, `t` and `nil`, and a `calln`-plus-literal
body at 427 bytes.

Commits, oldest first:

| commit | what |
|---|---|
| `7e690a48` | `--dynamic-user-calls`: unresolvable user calls lower through the calln dispatcher, closing a unit's extern set over the runtime symbols |
| `8bd3f268` | native `nelisp_aot_builtin_calln` provider for the reader (the symbol did not exist) |
| `ddf192ee` | **fix**: a nested delegated call dispatched on the inner call's name; plus the loader generalisation (`data-addr` stubs, sizing from the artifact) |
| `e3a702a7` | **fix**: literal arguments to a delegated call were passed as raw untagged words |
| `e30ff0ab` | `nelisp--native-symbol-addr` — runtime symbol addresses for interpreted elisp |
| `a892332e` | the loader itself, plus tests in two halves |
| `46160af2` | Doc 142 §6.4 written up |
| `e9821ed0` | **fix**: three loader defects around vectors (see §4) |
| `f5278cc0` | §9 bench: helpers moved into the artifact's own unit |

---

## 2. The open problem

`make nl-safe-native-bench` compiles both sides of the §9 pair, loads
them, and segfaults on the first call.

Reduced as far as it goes, and the reduction is the interesting part:

```elisp
(defun vget      (n) (aref (vector 7 8 9) 0))   ; -> 7        works
(defun plainref  (n) (aref (vector 7 8 9) 1))   ; -> segfault
```

Those two artifacts are **identical in every measurable way**:

```
vget      rt=17  size=550  externs=(calln nl_alloc_symbol nl_alloc_vector
                                    nl_vector_set_slot nl_vector_slot_ptr)
plainref  rt=17  size=550  externs=(same)
```

Same slot count, same text size, same extern set. The only difference in
the source is the constant `0` versus `1`.

Reading element 0 works; reading element 1 faults. That is where to
start. Note the vector itself is built correctly — see §4.3 for how to
check that directly.

Also failing, all with the same smell:

```elisp
(defun letvec   (n) (let ((c (vector 7 8 9))) (aref c 1)))
(defun symvec   (n) (aref (vector 'nl--cell 7 0) 1))
(defun nestvec  (n) (aref (vector 1 (vector 7 8 9) 3) 0))
(defun vsetget  (n) (let ((v (vector 0 0 0))) (aset v 1 n) (aref v 1)))  ; -> nil, not a crash
```

`vsetget` returns `nil` rather than faulting, so `aset` lands somewhere
harmless. It may or may not be the same defect.

The §9 cell is `(vector 'nl--cell (vector 7 8 9) 0)`, so every one of
these has to work before the budget can be measured.

---

## 3. How to run things

Everything native runs on Linux x86_64. On this machine that is WSL
Debian; the repo is visible there at
`/mnt/c/Users/kuroz/Cowork/Notes/dev/nelisp-nl-ns` (a junction to
`D:\Cowork\Notes\dev`).

```sh
make standalone-reader          # ~3-4 min, rebuilds target/nelisp
make neln-loader-test           # the ten passing cases
make nl-safe-native-bench       # the §9 pair (currently faults)
make test-one FILE=test/nelisp-native-load-test.el
```

A reader rebuild is needed after touching anything under `scripts/` or
`lisp/nelisp-cc-*`. It is **not** needed after touching
`lisp/nelisp-native-load.el` — that is interpreted, loaded from disk each
run, so the edit-test loop there is seconds.

To run a one-off against the loader:

```sh
cat > /tmp/probe.el <<'EOF'
(load "/mnt/c/.../nelisp-nl-ns/lisp/nelisp-native-load.el")
(princ (format "%S\n" (nelisp-native-load-exec "/path/to/f.neln" "f" (list 1))))
EOF
./target/nelisp --load /tmp/probe.el
```

Compile a fixture with the host Emacs, not the reader:

```sh
emacs --batch -Q -L lisp -L src --eval '(progn
  (setq load-prefer-newer t)
  (require (quote nelisp-artifact))
  (require (quote nelisp-aot-compiler))
  (let ((nelisp-aot-compiler--dynamic-user-calls t))
    (nelisp-artifact-compile-file "/tmp/f.el" "/tmp/f.neln" nil nil nil nil nil (quote neln))))'
```

`--dynamic-user-calls` is what closes the extern set. Without it the unit
carries relocations naming elisp functions and the loader refuses it.

---

## 4. Techniques that worked

These are not obvious and each saved a lot of time.

### 4.1 Get the faulting address from `dmesg`

The reader dies with a bare "Segmentation fault". The kernel knows more:

```sh
./target/nelisp --load /tmp/probe.el; sleep 1
dmesg | grep 'nelisp\[.*segfault' | tail -1
```

gives `segfault at 8 ip 00000000007f06af`. Then

```sh
readelf -sW target/nelisp | grep -E ' nl_vector_slot_ptr$'
```

places `0x7f06af` inside `nl_vector_slot_ptr`, and `at 8` says it
dereferenced offset 8 of a null pointer. That turned "it crashes" into
"this function received a Sexp whose payload is null" in two commands.

### 4.2 Verify the loading before suspecting it

The handle exposes `:codepage`, `:stubs` and `:body-entry` for this. Read
the patched bytes back and follow each call:

- each stub's imm64 (at `codepage + stub_offset + 2`) should equal
  `nelisp--native-symbol-addr` for its name;
- each relocation site should hold opcode `0xE8` and a disp32 such that
  `site + 4 + disp == its stub`.

Doing this ruled the loader out for the vector case in one run, which is
what pointed at the semantics instead.

### 4.3 Read the built object out of memory

The loader leaves intermediates behind, so you can inspect what compiled
code actually built:

```elisp
(defvar H (nelisp-native-load-artifact "/tmp/vmake.neln" "vmake"))
(nelisp-native-load-box (plist-get H :arg-slots) 5)
(ptr-call (plist-get H :entry) (plist-get H :arg-slots) 0 0 0 0 0)
(defvar OUT (plist-get H :out))
(defvar BOX (ptr-read-u64 OUT 8))          ; Sexp::Vector payload
(defvar DATA (ptr-read-u64 BOX 8))         ; NlVector data pointer
(ptr-read-u64 DATA 0)                       ; element 0's word
```

A NeLisp integer immediate is `(value * 4) + 1`, so 7 reads as 29. This
is how "the vector is three Nils" was established — the word was 3, the
Nil immediate.

### 4.4 Disassemble the artifact text

```sh
emacs --batch -Q -L lisp -L src --eval '(progn
  (require (quote nelisp-artifact))
  (let* ((nat (plist-get (nelisp-artifact--read-payload "/tmp/f.neln") :native))
         (txt (base64-decode-string (plist-get nat :text-base64)))
         (coding-system-for-write (quote binary)))
    (write-region txt nil "/tmp/f.bin" nil (quote quiet))))'
objdump -b binary -m i386:x86-64 -M intel -D /tmp/f.bin
```

Frame slot index `i` sits at `[rbp - 8*(i+1)]`. For arity `a`, slots
`0..a-1` are the arguments and `a..a+16` are the boundary, in the order
`out mirror frames scratch name_slot callback-0 .. callback-11`. So
`[rbp-0x28]` in an arity-1 defun is `scratch`.

---

## 5. Traps

**Read the whole output.** Three wrong conclusions this session came from
truncating: `tail -20` on a test failure list hid the top of it, reading
only the first `sub rsp` of a prologue (there are three) produced a bogus
"the frame is too small" theory, and a `grep` filter over `make test`
returned nothing and looked like success.

**Do not batch reader probes in a shell loop.** Quoting through
`wsl -- bash -lc '... for e in "..."; do ./target/nelisp --eval "$e"; done'`
silently mangles the expressions and every case comes back `nil`. It
looked like a broken runtime twice. Write the probe to a file and
`--load` it.

**The reader has no `getenv`.** Pass paths by generating a prelude file
that `defvar`s them, which is what the make targets do.

**`base64-decode-string` returns a string whose bytes over 127 are UTF-8
encoded** in this runtime. Use `aref` (character at index), never
`string-byte` (which walks the encoding). This corrupted the text on the
code page with 137 arriving as 194 137.

**The reader's `nelisp_apply_function` does not look up user functions.**
It dispatches a fixed if-chain over `nelisp-standalone--reader-builtins`
and writes anything else to stderr. The general symbol lookup is in the
*host* elisp bridge, `nelisp-cc-runtime--aot-default-builtin-dispatchn`,
which is a different thing. This is why the §9 helpers had to move into
the artifact's own unit.

**`nelisp_aot_signal` does not exist in the reader**, so a unit
containing a compiled `signal` cannot be loaded at all.

**`emacs -Q --batch` without `load-prefer-newer`** picks up stale `.elc`.
Prefer `make test-one FILE=...`.

---

## 6. Facts about the loader worth knowing before editing it

**Two calling conventions, not recorded in the artifact.** A defun either
takes Sexp pointers and returns through rax-as-a-pointer, or takes raw
i64 and returns raw in rax. `param-class` is `gp` for both. The CLI
decides by trying the integer call and falling back; in-process a wrong
guess corrupts rather than errors, so `nelisp-native-load-abi` derives
it: no externs means integer, any extern means boxed.

**The result is in rax, not in `out`.** For a body ending in a delegated
call the two are the same pointer, which hid this until a body ended in
something else.

**`scratch` is a vector, and its elements must hold pointers.**
`nl_vector_slot_ptr` returns the stored word when it is a pointer and a
*fresh temporary* when it is an immediate. Compiled code fills an element
by calling it once for somewhere to write and again to hand that storage
to `nl_vector_set_slot`, which only works when both calls agree. Since
`nl_val_clone_into` folds nil, `t` and integers back to immediates, the
scratch elements are seeded with a string.

**Symbol addresses are indexed, not named.** `data-addr` is a
compile-time form, so `nelisp--native-symbol-addr` selects from a chain
fixed when the reader is built.
`nelisp-native-load-bridgeable-symbols` must stay identical, in order, to
`nelisp-standalone--reader-neln-bridgeable-symbols`; a test asserts it.

**`nl_alloc_consbox` and `nl_val_clone_into` exist in the reader but are
not in the bridgeable set.** Adding them (both lists, then rebuild) would
let cons-using artifacts load. Not needed for §9; noted because the
pre-flight check refuses them today and the refusal reads like a missing
feature rather than a decision.

---

## 7. What to do next

1. **Find why element index 1 faults and index 0 does not.** Start from
   the `vget` / `plainref` pair in §2 — identical artifacts, one
   constant apart. Disassemble both (§4.4) and diff. The answer is in
   the few instructions that differ.
2. Then `letvec`, `symvec`, `nestvec` — likely the same cause.
3. Then `aset` (`vsetget` returns `nil`), which the borrow needs for its
   state counter.
4. Then `make nl-safe-native-bench` should run, and §9 has its number.

The bench harness is already written and does not need changing:
`scripts/nl-safe-native-bench-fixtures.el` compiles the pair,
`bench/nl-safe-native-bench.el` times them and compares against the
1.15x budget, and both sides refuse to report if they disagree on the
value they compute.

One caveat about the fixture, already recorded in its header: the
helpers are nl-safe's fast path transcribed, and the branch that differs
is the one a passing run never takes — nl-safe signals there, the
fixture returns 0, because of the `nelisp_aot_signal` limit above.

---

## 8. Why this kept finding bugs

The loader that existed before was a demo: `(defun inc1 (x) (1+ x))`,
one call, no literal arguments, no cons, no vector, 122 bytes, with its
bytes and extern addresses baked into the reader at build time. Five
defects in the AOT and the loader had never been reached because nothing
had ever run anything else natively in-process.

That is worth keeping in mind while working through the list above: a
fault in this area is more likely to be a first visit than a regression.

---

## 9. 2026-08-16 completion record

The AOT value-representation boundary is now explicit:

- `:repr` flows through local `setq`, runtime `let`, arithmetic and
  comparisons; Sexp dispatcher results unbox at raw arithmetic consumers.
- `calln` boxes raw integer expressions (not only raw frame references).
- Local `setq` clones Sexp dispatcher results into a private slot, so a
  subsequent delegated call cannot overwrite a prior value through shared
  `out`.
- Native artifact defun metadata now records `:return-repr`; the loader
  decodes rax as raw or boxed accordingly instead of guessing from externs.

Regression coverage in `neln-loader-test` includes `rawloop`,
`dispatchint` (dispatcher result -> `setq` -> arithmetic), `vsetget`, and
`cell-acquire` (state read/update plus nested vector read).  The loader run
completed with 17 cases and zero failures.

Doc 170 §9 now runs on the in-process native loader with the fast-path
fixture and fixed integer cell tag.  The former `N=20`, one-run result was
dominated by setup and is not a valid budget measurement.  With `N=2000`,
best of seven runs, the result is checked `1625.1 ns`, plain `1621.6 ns`,
ratio `1.00x`: the `<= 1.15x` gate **PASSES**.

The fixture enables the vector-only AOT path, so `aref`/`aset` lower to
refcount-safe native vector IR rather than the dynamic dispatcher.  Its
checked side uses ordinary `nl-with-borrow`; the opt-in preprocessor recognizes
only a syntactic fresh `(vector ...)` binding whose cell name never occurs in
the body, then lowers it to internal `aot-with-fresh-shared-borrow`.  It
therefore eliminates state read/increment/decrement/write while retaining the
value-slot read.  The checked and plain artifacts consequently have the same
text size (1254 bytes), runtime-slot count (26), and extern set.

The source-form analysis now reports a reason rather than silently declining:
`:cell-escape`, `:state-observed`, `:multiple-path`, `:exceptional-control`,
`:nested-borrow`, and `:exclusive-borrow` all retain checking.  Fat-pointer
elision now recognizes a fresh `nl-ptr-make (alloc-bytes ...)` with literal
length, generation and in-range `nl-ptr-ref-u8` offset, and lowers that narrow
case to native `ptr-read-u8`.  Dynamic offsets, out-of-bounds offsets and
pointer escape retain checks.

### Follow-up: fat-pointer loop analysis and native gate

The fat-pointer analysis now accepts a body containing any number of literal,
in-range `nl-ptr-ref-u8` reads, including reads inside a loop.  It rejects an
alias, escape, rebinding, dynamic offset, or out-of-range offset; eligible
reads alone are rewritten to `ptr-read-u8`.  The compiler regression covers
both the loop rewrite and an alias rejection.

The raw byte address now has a distinct `raw-ptr` representation.  It keeps
the GP-register calling convention but is not implicitly boxed for the Sexp
dispatcher.  The actual loader fault was an independent representation gap:
`let-rt-n` did not forward the representation of its final body, so a raw
integer return was recorded as `unknown` and unboxed as if it were a Sexp
address.  Propagating its `:repr` fixes the loader boundary without changing
vector Sexp-slot handling.

The native pair is now part of `make nl-safe-native-bench`: both sides run
the same 2000-read loop, validate equal output, and enforce the Doc 170 fat
pointer budget of `<= 1.20x`.  Latest run: checked `1437.5 ns`, plain
`1405.0 ns`, ratio `1.02x` — **PASS**.  The borrow gate in the same run is
`0.99x <= 1.15x`.

### Follow-up: derived-slice provenance and narrowed loop bounds

`s = nl-ptr-slice(p, off, len)` is now tracked as a non-escaping narrowed
range of `p`, rather than as an alias.  A nested `q = nl-ptr-slice(s, ...)`
composes the parent offset and length; eligible u8 reads/writes are rewritten
against `p` only after the composed range and literal byte value are proven.
For a canonical monotone loop, an index is accepted only when its literal
upper bound fits the *derived* slice, then rewritten as the corresponding
root offset expression.  Returning, storing, rebinding, passing to an
unknown operation, an out-of-range slice/index, or a non-byte write value
retains checked semantics.  Any unknown `if` branch in a candidate body also
retains checked semantics rather than joining provenance across paths.

The analysis canonicalizes both the public pointer primitives and nl-safe's
compiler-macro-expanded `nl-safe--ptr-{ref,set}-u8` spellings.  This avoids a
load-order-dependent escape classification when nl-safe has already loaded.
The compiler regression now has 211 passing tests, including nested-slice
read/write, escape, derived-loop proof, and derived-loop overrun rejection.

`make neln-loader-test` now executes 20 fixtures with zero failures; the new
`fat-derived` and `fat-derived-loop` cases cover nested derived read/write and
the raw loop-index path.  `make nl-safe-native-bench` adds the derived-slice
checked/plain pair.  On the final run (N=2000, best of seven): borrow
`1479.0/1516.0 ns = 0.98x <= 1.15x`, direct fat pointer
`1318.1/1353.5 ns = 0.97x <= 1.20x`, and derived fat-pointer loop
`1357.0/1329.1 ns = 1.02x <= 1.20x`; all gates passed.

---

## 9. Intra-unit calls do not establish the callee's boundary (2026-08-17)

A `.neln` with more than one defun runs until the callee touches a
boundary slot, then faults. Reduced:

```elisp
(defun bp-probe (n) (aref (vector 41 42 43) n))
(defun bp-outer (n) (bp-probe n))
```

`bp-probe` loaded and called directly answers 42. `bp-outer`, which calls
it, faults. `pair` (a callee that touches no boundary slot) runs fine, so
intra-unit calls themselves are not the problem.

The caller passes one argument and nothing else:

```asm
bp-outer:
   4: push rdi                ; spill n
   c: sub  rsp,0x90
  21: mov  rdi,[rbp-0x8]      ; rdi = n
  25: call -637               ; -> bp-probe + 0  (its prologue)
```

and the callee reads a boundary slot nobody filled:

```asm
bp-probe:
   4: push rdi                ; spills n only
  2c: mov  rax,[rbp-0x30]     ; slot 5 = name_slot -- never initialised
  4c: call nl_alloc_symbol    ; writes through it
```

So the boundary is established **only by the loader's trampoline**, which
fills slots `arity..arity+16` and enters at `body-offset`. A normal call
enters at offset 0 and gets uninitialised stack in those slots. Nothing
in the unit establishes them.

### What the fix has to be

A callee **cannot** establish its whole boundary by itself. `out`,
`scratch`, `name_slot` and the callbacks are storage, and it could
allocate those. `mirror` and `frames` are *context* — the environment the
dispatcher resolves in — and a defun has no way to synthesize them. Any
callee that reaches a builtin through `calln` needs `frames`, and
`bp-probe` does, for its `aref`.

So the boundary has to be threaded, which means hidden parameters. Note
there is no existing mechanism to lean on: the `&rest` arm of the
same-unit call lowering appends `scratch` to the argument list, but that
lands in the *rest parameter's* slot, not an extra hidden one.

Four places have to move together:

1. `--object-hidden-boundary-fenv` (lisp/nelisp-aot-compiler.el:1069) —
   give `mirror` / `frames` a `:reg` so the prologue spills them, instead
   of the frame-only `:slot` they get now.
2. `--defun-signature` (:8650) — arity has to count the hidden params so
   call sites agree with callees.
3. The same-unit call arm of `--parse-value` (:10698) — append the
   caller's `mirror` / `frames`; today it passes source args padded to
   arity and nothing else.
4. **The loader's trampoline** (`nelisp-native-load--trampoline`) — it
   currently writes all 17 boundary values into frame slots and enters at
   `body-offset`. Whatever the new ABI is, the trampoline has to match it.

Point 4 is the risk: that ABI is what the 20 passing loader cases depend
on. A half-finished change here breaks everything that currently works,
so it wants doing in one go with `make neln-loader-test` after each step.

An earlier draft of this section proposed "each defun's prologue
establishes its own boundary" as the cheap option. That does not work,
for the `mirror` / `frames` reason above; it is recorded here so the idea
is not re-derived and re-tried.

Note for whoever picks this up: two earlier readings of this same fault
were wrong, both from disassembling without pinning down which function
the bytes belonged to. Slice by the manifest's `:offset` / `:size`, and
check a call's displacement arithmetic against the manifest rather than
eyeballing it. `§4.4` has the recipe.

Until this is fixed, a benchmark or fixture that needs helper functions
has to inline them into one defun -- which is what
`scripts/nl-safe-native-bench-fixtures.el` does, and says so.

---

## 10. Cache quoted symbol literals (next perf lever, 2026-08-17)

The Doc 170 §9 checked borrow is 4.99x against a 1.15x budget after tag
predicates were lowered natively in test position (`7b90a98f`). The
remaining cost is attributed, one predicate per rung:

```
state bookkeeping only   1.36x
+ vectorp                1.77x   (+0.41)
+ length                 2.44x   (+0.67)
+ eq                     4.90x   (+2.46)
```

`eq` dominates, and **not as a comparison**. `(eq (aref c 0) 'nl--cell)`
materialises a symbol on every iteration -- twice, in fact: `eq` is a
delegated builtin so the dispatcher needs a symbol for the name `"eq"`,
and the quoted literal `'nl--cell` needs another. Each goes through
`nl_alloc_symbol`, which heap-allocates the name buffer. The
disassembly shows the pattern inline:

```asm
movabs rax, <name bytes>
push   rax
call   nl_alloc_symbol
```

Nothing frees them. Adding two more pairs to `make nl-safe-native-bench`
made the reader OOM (`Killed`, rc=137, on a 3.9 GB box) for this reason;
the attribution ladder had to be run as a standalone probe instead
(kept at `scratchpad/ladder.sh` in the session scratch, trivial to
rewrite).

### Design

Give each distinct symbol literal in a unit one zeroed 32-byte `bss`
blob, initialise it on first use, and copy from it afterwards. The
runtime's own hand-written elisp already uses exactly this idiom -- see
`nl_logic_build_scratch` in `lisp/nelisp-cc-evalport-env-leaves-logic.el`:

```elisp
(if (= (ptr-read-u64 <cache-slot> 0) 0)
    (let* ((s (alloc-bytes 32 8)))
      (seq (nl_logic_write_symentry s) (ptr-write-u64 <cache-slot> 0 s)))
  0)
```

Compiled user code has no equivalent.

The pieces exist: `(data-blob NAME BYTES bss)` declares a same-unit
static blob (`lisp/nelisp-aot-compiler.el:10990`) and `(data-addr NAME)`
takes its address. A symbol Sexp is immutable and name buffers are
already shared by the rebox path, so copying from a cache is not a new
aliasing risk.

Do the rewrite where `sexp-write-symbol-lit` is **parsed**, not at its
ten-plus construction sites and not in the per-architecture emitters:
one place, and it stays ordinary IR so x86_64 / aarch64 / wasm all get
it.

Three parts have to come together:

1. a per-unit registry of symbol name -> blob name, filled during parse;
2. a hook to append the collected `data-blob` declarations to the unit's
   top-level node list before layout -- **this is the piece not yet
   located**; `data-blob` is a statement-only top-level form and parsing
   happens per defun;
3. the parse-time rewrite of `sexp-write-symbol-lit`.

Worth doing: it is not a benchmark artifact. Every delegated builtin
call in every compiled unit allocates a symbol for the callee's name on
every execution, so this is a whole-program cost, and fixing it also
removes the per-iteration leak that made the bench OOM.

After this, `length` (+0.67x) is the next rung.

---

## 11. The arena has no reclamation, and every operation costs ~560 bytes (2026-08-17)

Loading an artifact repeatedly OOMs: 5 and 20 load/call/unload cycles
complete, 60 is killed on a 3.9 GB host. `nelisp-native-load-unload`
munmaps all three regions and that does not help, because the mappings
were never the cost.

Measured with `nelisp--arena-stats` across one load of a 2.9 KB `.neln`:

```
baseline                      37,247,568
after read-file (2949 chars)  38,093,816    +846 KB
after manifest parse         104,402,128    +66 MB
after text decode (144 B)    110,760,984     +6 MB
after object decode (1152 B) 154,750,736    +44 MB
after full artifact load     268,435,448   +114 MB
```

~230 MB per load. But the decoder is not at fault -- **every interpreted
operation allocates, and nothing is ever reclaimed**:

```
char-to-string   1911 bytes/op      aref         1991 bytes/op
cons             2744 bytes/op      arithmetic   2753 bytes/op
concat           2151 bytes/op      funcall      6031 bytes/op
```

Isolating the per-operation cost from the per-iteration cost:

```
empty while (condition only)   1502 bytes/iteration
+ one extra setq               2062   (+560)
+ two extra setq               2622   (+560)
+ nested arithmetic            3894   (+1832 for three more ops)
```

**~560 bytes per interpreted operation.** Nothing observed reclaims it:

- `garbage-collect` is fbound and frees none of it;
- neither does crossing a top-level form boundary. Three identical pure
  loops, each its own top-level form in a `--load`ed file, add 54.7 MB
  apiece and none is given back:

```
baseline                 23,628,152
after pure loop #1       78,312,320   (+54.7 MB)
after pure loop #2      132,996,496   (+54.7 MB)
after pure loop #3      187,680,680   (+54.7 MB)
```

The arena caps at 256 MB (268,435,456), which is where a run dies.

**On the mechanism, be careful.** The sources describe reclamation that
this probing could not observe: a comment on the native buffer-scan
helpers says GC runs "only at form boundaries", and `wf_dirty` maintains
a mutation-epoch counter at 268435544 described as a NO-ESCAPE gate so
that "the per-eval arena reset never frees a still-reachable escapee".
So a per-form reset exists in the design. Three successive models built
from outside -- "no reclamation at all", "reset at form boundaries",
"reset suppressed by escapes" -- were each contradicted by the next
measurement. Whoever continues should read the reset logic in the
evaluator rather than probe from elisp; the numbers above are reliable,
the explanation for them is not yet.

Either way it is the real ceiling on anything long-running in the
reader, including this loader, `make nl-safe-native-bench`, and the §9
attribution ladder (which had to be split into separate processes for
exactly this reason).

### Two separable problems

1. **Reclamation does not fire on this path.** A per-form reset exists in
   the design (see above) but was not observed releasing anything.
   Finding out why is a read of the evaluator's reset logic, not more
   black-box probing.
2. **~560 bytes per operation.** A `Sexp` is 32 bytes, so this is ~17
   Sexps' worth per operation. Even without reclamation, cutting this
   moves the ceiling by whatever factor it recovers.

(2) is worth attacking first: it is bounded, and it does not depend on
(1). A hypothesis worth checking, **not verified**: 17 × 32 = 544, close
to the measured 560, and 17 is exactly the boundary slot count
(`out mirror frames scratch name_slot` + 12 callbacks). If the evaluator
allocates a fresh boundary block per operation, pooling it per frame
would collapse the cost. A grep for a matching `alloc-bytes` in the eval
path did not find one, so treat the numeric coincidence as a lead, not a
finding.

Anyone measuring here: `(nth 2 (nelisp--arena-stats))` is the used-bytes
field, and the difference across a `while` loop divided by the iteration
count gives a clean per-operation figure.

---

## 12. Symbol-literal cache: compiler side done, two layers left (2026-08-17)

`nelisp-aot-compiler--symbol-literal-cache` (default nil) gives each
distinct symbol literal in a unit one zeroed 32-byte `data` slot,
materialises it on first execution and copies from it afterwards. Tag 0
is the sentinel: a zeroed slot reads as tag 0 and a symbol Sexp never
does.

Verified at the link-unit level:

```
(defun sc (n) (if (eq n 'foo) 1 0))   with the cache on

:data len = 64
sym nl_symcache_1  offset 0   size 32  section data
sym nl_symcache_0  offset 32  size 32  section data
```

Two slots for one `eq` — the dispatcher's name `"eq"` and the quoted
`'foo`. That matches the measurement that motivated this: two symbol
allocations per iteration.

Design notes worth keeping:

- The rewrite emits `let`, not `let*`. Preprocessing lowers `let*` to
  nested `let` before `--parse-value`, which rejects the starred form as
  `:not-value-expr`.
- The slot is `data`, not `bss`. The ELF writer wants a `:bss-size` this
  path does not supply, and the slot must be writable so `rodata` is out.
- Applied only when the destination is a plain variable: the rewrite
  mentions it five times.
- `--symbol-literal-inhibit` guards the rewrite's own initialiser, which
  is itself a `sexp-write-symbol-lit`.
- The registry is bound per unit in `nelisp-aot-compile-to-link-unit`; a
  global would carry one unit's slots into the next, naming blobs that
  are not there.

### What is left

The cache needs writable static data to survive into execution, and two
layers do not carry it yet:

1. **The artifact does not store `.data`.** `:native` has
   `:text-base64` / `:text-size` but no data section; the bytes exist
   only inside `:object-base64`. The link unit already returns `:data`,
   so this is storing what is already computed.
2. **The loader maps only the code page.** Every relocation it handles
   today is extern-to-stub. A `data-addr` against a local data symbol is
   a new kind: the address has to come from a mapped data page. The
   symbol table entry (`:name "nl_symcache_0" :value 0 :section data`)
   gives the offset, so the loader needs a data mapping plus a reloc arm
   that resolves against it.

Until both exist, an artifact compiled with the cache on loads with
`relocation for nl_symcache_0 has no stub`. The flag is off by default,
so nothing changes until they do.

### Fixed on the way

`nelisp-artifact--write-elf-rel-object` forwarded `:text` / `:rodata` /
`:symbols` / `:relocs` / `:machine` and **not** `:data` / `:bss-size`, so
a unit carrying a writable blob had its symbol written without its bytes
and the writer rejected its own output: "symbol references data but
:data is empty". No artifact had used a data blob before, so nothing had
exercised it.

---

## 13. Symbol-literal cache works end to end (2026-08-17)

All three layers are in place and the flag still defaults off.

```
(defun symname (x) (symbol-name 'abc))

cache=nil  text=234  data=0   syms=nil
cache=t    text=996  data=64  syms=("nl_symcache_1" "nl_symcache_0")

symname[off] -> "abc"
symname[on]  -> "abc"
```

Two slots for one call: the dispatcher's name and the quoted literal.

### The data section had to reach the loader

- `nelisp-artifact--native-section-plist` now stores `:data-size`,
  `:data-base64` and `:data-symbols` (the `data`-section entries of the
  unit's symbol table). Omitted when empty, so an artifact without a data
  section is unchanged.
- The loader places that data **inside the code page**, after the stubs,
  rather than in a mapping of its own. Compiled code takes a data
  symbol's address with a pc32, and two independent `mmap(NULL)` results
  can sit further apart than a signed 32-bit displacement reaches — the
  same range problem the stubs solve for calls, which an address load
  cannot solve the same way. The page is already RWX.
- The relocation arm now resolves a symbol to either a stub or a data
  offset, and says so when it is neither.

### Not measured

The saving is an allocation per execution turning into a 32-byte copy,
but there is no number here: every loop-shaped benchmark fixture written
for it crashes in the loader, with the cache on or off. Do not quote a
figure until one runs.

### A sharper bisect of the loader's remaining fragility

```
l0  pure loop                                    -> 20      works
l1  loop + (integerp i)      [delegated call]    -> crash
l2  loop + (symbol-name 'abc)[delegated call]    -> crash
l3  single (symbol-name 'abc), no loop           -> "abc"   works
```

A delegated call inside a loop crashes even when it allocates almost
nothing, and the same call outside a loop is fine. **But this is not the
whole rule**: the §9 checked fixture has a delegated `eq` inside its loop
and runs (that is where 4.78x comes from). So "delegated call in a loop"
is necessary to reproduce and not sufficient to explain. Two earlier
attempts to benchmark the cache were wasted comparing against fixtures
that crashed with the cache off too — check a baseline runs before
drawing anything from a comparison against it.

---

## 14. A minimal reproducer for the loader's remaining crash — NOT fixed (2026-08-17)

Five lines:

```elisp
(defun q1 (n)
  (let ((v (vector 7 8 9)) (i 0) (a 0))
    (while (< i n)
      (setq a (length v))
      (setq i (+ i 1)))
    a))
```

Compiled with `--dynamic-user-calls` and loaded, `(q1 20)` faults at
`nl_vci_store_slot_imm+0x2b` with `segfault at 0` — a null store into a
vector slot.

### What runs and what does not

```
runs                                    crashes
l0  pure loop                           l1  loop + (integerp i)
l3  one delegated call, no loop          l2  loop + (symbol-name 'abc)
n1  single-form while body              m1  result stored in a let slot
n2  seq while body, no delegation       m2  result discarded
n3  seq while body, accumulation        m3  arity 0, literal bound
dispatchint (driver: delegated, no loop) m4  predicate in an `if' test
nl-safe-native-bench-checked            p1  delegated on a raw integer
  (delegated calls inside a loop!)      p2  delegated on a vector
                                        p3  delegated on a literal
                                        p4  (length v) in a loop
                                        q1  written in the section 9 shape
                                        q2  the same, seq-wrapped
```

### Six hypotheses, all refuted

1. *scratch vector too small* — enlarging it to 64 made things worse, not
   better.
2. *`(seq A B)` as a while body* — n2/n3 do that and run.
3. *arity* — m3 is arity 0 and crashes.
4. *whether the result is stored* — m2 discards it and crashes.
5. *argument type* — p1 (raw integer), p2 (vector), p3 (literal) all crash.
6. *delegated call in a loop* — the closest rule, and
   `nl-safe-native-bench-checked` refutes it: it has `length` and `eq`
   inside its loop and returns 7 under the current loader (re-checked, not
   a stale result).

q1 is written in that fixture's own shape -- several `while` body forms,
`length` applied to a vector -- and still crashes, so the difference is
not structural in any way this could find from outside.

### Two traps that cost time here

- **Stale artifacts.** After the manifest grew `:data-*`, previously
  built fixtures crashed until recompiled, which briefly looked like the
  loader had broken. `make neln-loader-test` rebuilds its own fixtures;
  hand-built ones do not.
- **Concurrent builds.** Two `make test` runs and these probes were live
  at once, and `make neln-loader-test` relinks `target/nelisp` — so
  probes were executing a binary being rewritten under them. Every
  measurement in that window was noise. Check `pgrep make` before
  believing a loader probe.

The authoritative check is `make neln-loader-test`: 20 cases, and it
passed throughout, so the loader is not broken in general.

---

## 15. The loader crash, correctly characterised at last (2026-08-17)

Section 14's framing was wrong on every count. It is not about loops, not
about delegated calls, and several cases were not even crashes.

### The real reproducer

```elisp
(defun c1 (n) (let ((v (vector 7 8 9)) (i 0)) (if (< i n) 111 222)))
```

`(c1 0)` faults. No loop. The controls that isolate it:

```
c2  (let ((v (vector 7 8 9)) (i 0)) n)                      -> 0    works
c3  (let ((v (vector 7 8 9)) (i 0)) i)                      -> 0    works
c4  (let ((v (vector 7 8 9)) (i 0)) (while (< i 0) ...) i)  -> 0    works
c5  (let ((v (vector 7 8 9)) (i 0)) (while (< i 3) ...) i)  -> 3    works
x5  (let ((i 0)) (while (< i n) ...) i)                     -> 0    works
```

So `while` is fine, a bound vector is fine, and reading the parameter is
fine. What breaks is **comparing a local against the PARAMETER while a
vector literal is bound**; comparing against a literal (c4, c5) works.

### What the fault says

```
segfault at 6f  ip=0x7e5753  = nelisp_apply_function + 0x1293e
```

`0x6f` is **111** — the literal in c1's then-branch. A raw integer is
being dereferenced as a Sexp pointer inside the dispatcher. That is the
same class as the bug fixed in `e3a702a7` (literal arguments passed as
raw untagged words), surviving on another path.

### Symptoms that were not what they looked like

- `w2` and `x4` were reported as crashes; they are **timeouts** (rc 124),
  and `w3` is an **OOM kill** (rc 137). Different failure, possibly
  different cause.
- Part of that hang was the loader's own doing: `--payload-string`
  decoded a result Sexp's claimed length byte by byte, so a wrong length
  became an unbounded loop rather than a bad value. Now capped at 1 MB
  (`nelisp-native-load-max-payload-bytes`), which turns that hang into a
  message naming the address. c1 then reports its fault in seconds
  instead of appearing to hang.

### Three measurement mistakes to avoid repeating

Each produced a wrong report in this section's investigation:

1. **Concurrent builds.** Two `make test` runs plus probes, with
   `make neln-loader-test` relinking `target/nelisp` underneath. Check
   `pgrep make` first. ("scratch=64 makes everything worse" came from
   this; on an idle machine 16/32/64/128 are identical and the driver
   passes at all of them.)
2. **Stale artifacts.** After the manifest grew `:data-*`, hand-built
   fixtures crashed until recompiled.
3. **Inline shell.** `bash -lc '... $n ...'` loses the loop variable to
   an outer expansion, and a missing `--load` file prints usage, which a
   grep for `->` reports as a crash. Write the probe to a file.

---

## 16. Two bugs were tangled; one is fixed (2026-08-17)

**Sections 13-15 characterise the crash wrongly.** Every comparison in
them was made before the ABI derivation was fixed, so the set of
"crashing cases" they were fitted to was mostly crashing for a reason
that has since gone away. Read them for the fixtures, not the rules.

### Bug A -- fixed: a raw result dereferenced as a Sexp pointer

Two shapes, neither settled by the signals available:

```elisp
(defun c1 (n) (let ((v (vector 7 8 9)) (i 0)) (if (< i n) 111 222)))
(defun u3 (n) (let ((m 3) (i 0)) (integerp n) (if (< i m) 111 222)))
```

`c1` allocates a vector without ever delegating, so "any extern means
boxed" misread it; the derivation now looks for the dispatcher externs
(`nelisp_aot_builtin_call1` / `_calln`) and gets it right.

`u3` delegates once and *still* answers in a register. Its extern set
says boxed and its `:return-repr` is `unknown`, so neither signal helps.
The loader now refuses to dereference a result below the first page --
nothing there is a Sexp, so 111 is the value.

Both faulted at their own literal's address, inside the caller's
`ptr-read-u64` rather than anywhere near the defun. Of seventeen
fixtures accumulated while chasing this, thirteen now run.

### Bug B -- open: a delegated call in a loop with a variable bound

```
t1  arity 1, `(while (< i 3) ...)'    -> 7      runs
t2  arity 0, `(while (< i m) ...)'    crash     m is a local
t3  arity 1, `(while (< i n) ...)'    crash     n is the parameter
```

Arity is irrelevant; a literal bound runs and a variable bound does not.
The remaining failures (w3, q1, l1, l2) are all this shape. `l1` faults
in `nl_vci_store_slot_imm` with a null store -- a different site from
bug A, which is why fixing A did not move them.

Note the section 9 checked fixture is this shape too and runs, because
its bound is the literal 2000. Reducing it (s1 -> s2 -> s4) is what
isolated the bound as the variable.

Also seen while stripping that fixture: `s3` returns a pointer instead of
7, from `acc` being assigned a boxed read in one `if` arm and a raw `0`
in the other. That is a third defect, in representation agreement across
arms -- and the same one worked around when the section 9 fixture was
written.

## 17. Bug C: the last-parsed arm decides how a slot is read (2026-08-17)

A frame slot holds one machine word. Whether that word is a raw integer
or a pointer to a Sexp is not in the word; the compiler remembers it in
the slot's FENV cell. `setq` overwrites that record, and parsing runs in
evaluation order -- so after an `if`, the record describes whichever arm
was parsed **last**. Reading the slot on the other path reinterprets the
word, silently, because both are just words.

Swapping the arms changes the answer, which is the whole proof:

```
a1  then boxed / else raw   (raw parsed last)     123289510590208   wrong
a2  the same with a progn                         129410895953248   wrong
a3  then raw / else boxed   (boxed parsed last)   7                 right
a4  both arms boxed -- no disagreement            7                 right
```

`a3` is right by luck: the arm that actually ran happened to match the
record the other arm left. Nothing about it is more correct than `a1`.

### The rule now enforced

Both arms parse from the frame state the `if` was entered with -- at run
time the `else` arm never observes the `then` arm's assignments, so it
must not observe them while parsing either -- and the arms must leave
every slot in agreement. Disagreement signals, the unit is refused, and
the caller falls back to byte code. The report names the slot:

```
:aot-if-arm-repr-mismatch :var acc :then sexp-ptr :else raw-i64
:form (if (= (length c) 3) (setq acc (aref c 0)) (setq acc 0))
```

Agreement is on **boxedness**, not on the exact representation symbol.
The vocabulary is `raw-i64`, `raw-ptr`, `sexp-ptr`, `unknown`, and only
`sexp-ptr` decides whether reading the slot dereferences. Demanding
exact equality was tried first and the reader corpus failed to build on
the first `if` it reached -- `(if (< bt 8) (setq hdr end) (nl_seq2
... (setq hdr (+ hdr bt))))`, whose arms record `unknown` against
`raw-i64` while both are raw words.

Cost: `a1`/`a2`/`a3` and `s3` stop compiling natively. `a4`, and the
section 9 fixtures `s1`/`s2`, still do. Loader driver 20/20, `make
compile` clean.

### The same hazard is in three more places

`cond`, `and`/`or` and `while` are parsed directly, not lowered to `if`,
and each has a path that does not run: the unchosen clause, the
short-circuited arm, and **the zero-iteration loop**. The last one is
live in this document's own benchmark: section 9's fixture starts `acc`
at raw `0` and makes it boxed in the loop body, so it is correct only
because the bound is the literal 2000. `(while (< i 0) ...)` would read
raw `0` as a pointer.

One rule covers all four: **`setq` may not change a slot's boxedness**,
since that is where every instance starts. Alone it rejects too much --
the reader corpus and the section 9 fixture included. Its partner is
promotion: if a slot is boxed anywhere in the body, allocate it boxed
from the start. The materials exist; `--aot-dispatcher-arg-form` already
boxes a raw value with `(let ((s (alloc-bytes 32 8))) (seq (sexp-int-make
s FORM) s))`. Since the lattice is monotone (`raw` -> `sexp-ptr` only),
adding to a promotion set and re-parsing converges in finitely many
rounds. That pairing, not the refusal, is the real fix.

## 18. Both the refusal and the promotion were withdrawn (2026-08-18)

Neither survived contact with the build that actually consumes the
compiler. Recorded here because the mechanism above is real and the
reason these particular answers fail is the useful part.

**Promotion** broke `make standalone-reader` and 24 tests, one of them a
`Segmentation fault (exit 139)` in `nat-ng-tail-sum`. Two causes, both
structural rather than incidental:

- It boxed what `--aot-dispatcher-arg-form` can box and left the rest, so
  a variable it promoted could still receive an extern-call result it
  could not box. The mismatch then survived its own promotion and the
  retry gave up -- on `nl_runtime_image_copy_argv_forms`, where `off` is
  a byte offset, `:then sexp-ptr :else unknown`.
- GC roots are collected from `ref` nodes carrying `:root-p`
  (`--gc-root-slots-for-defun`), and `:root-p` is decided at binding time
  from the source form. A promoted slot holds a fresh Sexp and is not
  marked, so nothing scans it.

**The refusal**, kept on its own, then failed the same build as soon as
anything else got more precise. Declaring parameters `sexp-ptr` -- which
is correct, and fixes `(defun p7 (n) (integerp n) (+ n 1))` answering
134637738934817 instead of 4 -- turned the very form that had justified
comparing boxedness rather than exact symbols,

```
(if (< bt 8) (setq hdr end) (nl_seq2 (ptr-write-u64 hdr 0 bt)
                                     (setq hdr (+ hdr bt))))
```

from `unknown` vs `raw-i64` (no mismatch) into `sexp-ptr` vs `raw-i64`
(mismatch), and the reader stopped building. 25 tests failed.

### What that says about where to look

These are not three bugs. A frame slot's representation is not a
property the compiler computes; it is a side effect of the order a
single-pass parser walked the tree, written into a mutable annotation
that 27 sites read and 13 branch on -- over a vocabulary that includes
`unknown`, meaning "nobody classified this yet". Precision added at any
one site changes what every other site sees. Patching per construct
therefore yields one new failure per construct, which is what happened
twice.

The target is one analysis that gives each frame slot a single
representation for its whole lifetime, removes `unknown` from the
vocabulary, and derives GC rooting from the same result. `if`, `while`,
`cond`, `and`/`or` and parameters are then consequences rather than
cases.

The parameter change is parked in `param-repr-wip.patch` -- correct on
its own terms, and not landable until the analysis exists.

Method note: every fixture in this document was hand-built to test a
shape guessed in advance. That found real defects but one at a time, and
each "verified" claim rested on those fixtures rather than on `make
standalone-reader`, which is the compiler's actual consumer. The next
step is a differential harness -- the same source through the
interpreter and through AOT, answers compared -- so the population is
enumerated rather than guessed.

## 19. The defect, measured rather than guessed (2026-08-18)

`make aot-differential` crosses the value producers that differ in
representation against the control shapes that have a path which does
not run, compiles each program, and runs it twice inside the reader --
interpreted and native -- comparing the answers. Nothing is written
down as expected; the interpreter is the oracle.

Baseline on this commit, 90 programs, full coverage:

```
75 summaries, 15 reader deaths, 35 wrong comparisons
```

### Wrong answers fall out exactly along representation classes

Group the producers by the representation the compiler gives them:

```
A   0            (+ i 1)                 raw machine integer
B   (aref v 0)   (aref (aref w 1) 0)     pointer to a Sexp
C   (length v)                           neither -- `unknown'
```

Every two-armed program is wrong **iff its arms come from different
classes**, and never otherwise. The 16 wrong `if2' programs are exactly
the 16 ordered cross-class pairs; `cond2' repeats it. Same-class pairs
(`zero'/`arith', `aref'/`deep') and the diagonal are all correct.

`(length v)` forming a class of its own is the part hand analysis had
wrong: the working model was two classes, raw against boxed. `unknown'
is not an absence of information the compiler ignores, it is a third
representation that disagrees with both of the other two. That is the
measured argument for removing it from the vocabulary rather than
teaching more sites to tolerate it.

### Reader deaths are the same defect where one path is empty

```
if1 / cond1 (no t clause) / and   x  {aref, len, deep}     9
while bound 0 / bound n           x  {aref, len, deep}     6
```

Bounds 1 and 3 -- loops that always run -- never die, and neither do the
class A producers, which agree with the `(acc 0)` binding. The slot is
bound raw, one path makes it boxed, and the path that does not run
leaves the raw word to be dereferenced.

### What this asks the fix to do

Drive `35 wrong / 15 dead` to zero without refusing programs, and keep
`make standalone-reader` building. The staging is:

- **B.** Give branch nodes (`if', `cond', `and'/`or') a joined
  representation and emit the coercion in the arm that needs it, so a
  branch is a value join rather than a slot patched in evaluation order.
- **A.** Classify the roots -- defun parameters and `extern-call' return
  values -- so the rest resolves through them.
- **C.** Derive each slot's representation, and its GC rooting, from the
  analysis -- which is what the withdrawn promotion got wrong -- and
  delete the mutable `:repr` overwriting in `setq'.

## 20. Where `unknown' actually comes from (2026-08-18)

`nelisp-aot-compiler--repr-audit' walks each parsed defun and reports
every node whose representation is `unknown'.  It changes no decision,
and the reader it builds is byte-identical to the one built without it.

Over the reader corpus, 73644 such nodes:

```
71204  ref                                 96.7%
  684  if
  495  call
  371  let-rt
  311  extern-call
  283  value-seq
  142  setq-local
   28  syscall-direct
   21  logic
  <20  vector-ref-ptr, sexp-write-str-lit, record-*, sexp-tag,
       str-byte-at, atomic-fetch-add, shift, vector-make
```

`let-rt', `value-seq' and `setq-local' are already classified -- they
delegate, and report `unknown' only because what they delegate to does.
They are not separate work.

The finding is that this is not a long tail of exotic IR kinds. It is
variable references, by two orders of magnitude. A `ref' copies `:repr'
from its FENV cell, a `let' copies it from its initialiser, and an
initialiser is usually another `ref' -- so the `unknown' is circular and
has to be broken at the roots, which are defun parameters and the
values externs return.

That reorders the staging above. Parameters were changed once already,
in isolation, and broke the build; the reason is now visible rather than
inferred. Precision at a root flows into joins, and the joins cannot yet
represent two arms disagreeing. Joins first, roots second.

Expected effect, stated before the work so it can be wrong: **B** should
take the 35 wrong comparisons to zero and 15 deaths to 6, since the 9
deaths in `if1' / `cond1' / `and' are branch shapes. The remaining 6 are
`while' with a bound that can be zero, where the disagreement is between
the loop's entry state and its body -- that needs **C**, because coercing
the entry value is exactly the promotion that failed for want of rooting.

## 21. Branch joins, and what they are waiting on (2026-08-18)

A frame slot holds one machine word and the compiler tracks, per slot,
whether that word is a raw integer or a Sexp pointer.  Branches now agree
with themselves about that: where two paths leave a slot differently, the
raw one is boxed, and the slot becomes a GC root at the same moment --
the pairing the withdrawn promotion missed.

Two shapes, because the placement differs:

- **Selection** (`if`, `cond`): the conversion goes in the arm.  A path
  that assigned the slot converts after; a path that did not converts
  before, or the conversion becomes that path's value and
  `(if c (setq acc BOXED) nil)' stops answering nil.
- **Skip** (`and`, `or`, `while`): the skipped path is the absence of
  code, so the conversion goes before the whole form and the skip
  inherits it.  Appending to an `and' arm would corrupt the test it is.
  Arms are then parsed once more, since they were parsed against the old
  representation; once, not to a fixed point, because a slot only moves
  raw -> boxed and it has already moved.

Measured on `make aot-differential`, 90 programs:

```
baseline                     35 wrong, 15 deaths
joins alone                  44 wrong,  3 deaths
joins + parameter convention  0 wrong,  0 deaths
```

The middle row is the honest one. Alone, the joins remove the deaths --
a raw word is no longer dereferenced -- but the answers stay wrong,
because the branch is still choosing the wrong arm for an unrelated
reason. Trading a crash for a silent wrong answer is the wrong
direction, and the joins only pay once section 20's parameter question
is settled. They are landed rather than held because that fix needs
them: an entry wrapper makes the arm choice right, and the joins make
what the arms leave behind readable.

Also landed here, from the suite: `nl_sexp_clone_into` now has a
definition in the `native-exec-general` C harness (a unit that assigns a
delegated call to a local references it, and the link failed without
one), and the compiler test's list of bridgeable runtime symbols is
taken from the loader's constant instead of copied -- the copy had gone
stale by two symbols and failed on units the loader loads and runs.

State: reader builds and runs, `make neln-loader-test` 20/20, `make
compile' clean, suite at one known load-dependent failure
(`nelisp-aot-tco-bench-tco-keeps-up-with-nl-loop').
