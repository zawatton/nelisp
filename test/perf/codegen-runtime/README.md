# Pinned runtime code quality fixtures

`runtime.nl.el` contains the exact ten S5 function bodies and five small
same-unit helpers from callpath revision `bf8b39924`. `pins.json` records the
full revision and source-file SHA256s. `pin.el` reads Git objects at that
revision, so concurrent working-tree edits cannot move the benchmark.
The sole source normalization is `heap-read-uN` to `ptr-read-uN` on both
compiler inputs. To deliberately refresh the pins:

```sh
CODEGEN_RUNTIME_SOURCE=../callpath-perf-20261021 emacs -Q --batch \
  -l test/perf/codegen-runtime/pin.el
```

Run from the checkout root, with an 8 GiB virtual-memory limit and a writable
`TMPDIR`/`XDG_CACHE_HOME`. Outputs go to `target/codegen/s5/runtime`. Generated C and lowering metadata
live under `generated/TARGET/`; static census products have separate paths.
Different target builds therefore do not overwrite one shared `runtime.c`.

```sh
python3 tools/ai/codegen-runtime.py study
python3 tools/ai/codegen-runtime.py build wf_bytecode_w_fast_run
python3 tools/ai/codegen-runtime.py ir wf_bytecode_w_fast_run
python3 tools/ai/codegen-runtime.py wall wf_bytecode_w_fast_run
python3 tools/ai/codegen-runtime.py selfcheck
python3 tools/ai/codegen-runtime.py endtoend
```

`study` compiles all ten bodies through the standalone unit pipeline and
gcc `-O2`. Each C target is compiled in its own translation unit: that target
has `noipa`, and the other pinned helpers are `static`, so GCC can inline
small callees. Counts exclude padding NOPs. The mechanical C lowering keeps
left-to-right argument evaluation, parallel `let`, sequential `let*`, wrapped
64-bit arithmetic, x86 shift counts, and raw-store sentinel returns.
Intrinsic layouts follow this compiler's emitted operations. This is a
comparison using NeLisp's representation, not a GNU object-layout port.

The ten fixture drivers cover byte access, FNV including all tail lengths,
symbol identity/name equality, main-thread root frames with zero through
seven slots, the VM fast loop for `add1`/`goto` with 256 backedges, allocator
run carving and free-list popping, marked/live sweep spans, actual interned
symbol-cell hits, nested evaluator entry on immediate forms, and a full
bytecode frame running constant42/return135. The corresponding paths use
genuine pinned helpers from `dependencies.nl.el`; `dependencies.json` records
their providers and source hashes. Cold external boundaries trap with distinct
exit codes recorded in each target's `*.cold-map.json`. These are path fixtures,
not a claim of coverage for every evaluator, GC, mirror or VM behavior.

Every executable must match seven independent GNU Lisp checksums across the
complete 64-bit result. `selfcheck` runs an actual executable whose checksum
is mutated by one bit and requires the same checker to reject it. Repeated
allocator/GC cases include deterministic state reset and checksum inspection
in both compiler inputs. `ir` labels this cost scope, and additionally reports
the target's exclusive Ir and observed out-of-line calls. An inlined target
can have zero observed entries while its semantic invocation cost is positive.

The full-frame driver constructs true compact vectors: header kind8 plus
length shifted left8, slots at+8, and vectorlike WORD tag6. It must compile
with the explicit `compact-word` vector convention. Legacy compilation is an
intentional architectural failure: it reads a length at payload+16. No fake
legacy fields are planted in the fixture. Set
`CODEGEN_RUNTIME_VECTOR_CONVENTION=legacy-view` to reproduce that control.
The fixture fails clearly if the compiler has no compact convention setting. Raw fixture compilation follows the production
standalone policy: TCO is on unless `NELISP_TCO=0`. The general compiler/API
default is unchanged; wall receipts record the effective raw setting.

The two semantic heap-field operations are the exact mappings from pinned
callpath `nelisp-heap-accessors.el`, implemented by small Lisp wrappers to the
genuine `nl_word_tag64` and `nl_word_payload` providers; `heap-fields.json`
records the original module hash. This is the same frontend adaptation in
both compiler inputs. The full-frame executable uses the real standalone
unit compiler and linker, with the genuine arena-base-slot provider and boot
initialization. Its cold `standard-output` literal/runtime boundary remains
opaque and traps if exercised. All cold branches remain in the compiled body.
The study stays failing until all ten independent fixtures and measured
runtime attribution exist. No speed criterion passes on static counts or Ir.

`wall` calibrates GCC to at least 0.5 seconds, warms both variants, and runs
seven pairs with alternating order on CPU 9. Each pair subtracts a
same-shape zero-iteration process, records CPU frequency, and computes
NeLisp/GCC within that pair. The median ratio must be at most 1.5. All
observations, full-width results, and executable hashes are saved.
`CODEGEN_CPU` overrides the CPU. Coordinate ownership before measuring.

`ir` runs N and zero through Callgrind on CPUs 0–7, with one `heavy-run.sh`
unit per profile. It reports diagnostic Ir per invocation and never changes
a speed verdict. `CODEGEN_RUNTIME_IR_N` overrides the iteration count.

Existing real-runtime profiles can supply shares without another campaign:

```sh
python3 tools/ai/codegen-runtime.py profile \
  --callgrind WORKLOAD.cg.3 --control-callgrind EMPTY.cg.3 \
  --profile-binary SNAPSHOT/nelisp
```

Address-only function records are resolved against the supplied binary's
text symbols. Raw self Ir/call and call counts remain available; independent
control subtraction produces incremental self Ir and shares. Raw calls and
incremental calls are distinct: a long and empty bytecode loop can each call
the same VM function once. `attribution.json` ranks investigation by runtime
share times static-count reduction; this is an estimate, not a wall forecast.
Explicit zero/inlined/unobserved profile records are valid attribution
observations and do not require fabricated out-of-line calls.

The end-to-end driver runs identical frozen GNU 31.1 bytecode for while and
funcall on real candidate, baseline and GNU binaries, includes GC during the
measured workload, records per-run frequencies, and requires seven-pair
candidate/base medians at most 1.03. It reports GNU ratios and runs the six
existing artifact differential stages. The pins' source revision and the real
runtime binary hashes are recorded separately. Defaults are checkout-relative:

- `CODEGEN_RUNTIME_BIN`: `target/codegen/lv-src/target/final/nelisp-align`
- `CODEGEN_RUNTIME_BASE`: `target/codegen/alignment/e9/nelisp-dyn`
- `CODEGEN_RUNTIME_GNU` / `CODEGEN_EMACS`: `emacs`
- `CODEGEN_CC`: `gcc-14`, falling back to `gcc`
- `CODEGEN_RUNTIME_OUT`: `target/codegen/s5/runtime`
- `CODEGEN_RUNTIME_VECTOR_CONVENTION`: normally from the per-fixture specification
- `CODEGEN_HEAVY_RUN`: otherwise located from Git's canonical checkout

`bytecode-gen.el` reproduces `bytecode.eld` using GNU's byte compiler.
GNU mechanisms consulted before fixture design are bytecode.c:510–525
(contiguous frames), bytecode.c:567–597 (threaded dispatch),
bytecode.c:854–870 (backedge GC/quit budget), and alloc.c:1668–1700
(typed-block free lists). They describe the architectural comparison;
this fixture change does not claim to implement those runtime mechanisms.
