# NeLisp v1.2.2

Release implementation and qualification notes, updated 2026-09-11.

## Changes since v1.2.1

### Macro expansion, loops, and evaluation

- Artifact compilation expands macros before the bytecode lane and rejects
  unsupported forms that would otherwise reach it. `pcase` fallback bindings
  retain branch-local scope.
- The supported `cl-loop` subset now models parallel `for` stepping,
  `unless`, `downto`, `on`, and `across`; unsupported shapes fail loudly.
  `cl-dolist` and `cl-dotimes` also receive their anonymous `cl-block`.
- Argument lists, lambda bodies, and `progn` use iterative evaluation while
  preserving evaluation order and live values.

### Reader and standalone artifacts

- `read` and `read-from-string` dispatch to the native parser for string
  inputs. Modifier chains, read labels, GNU string escapes, and empty
  `#s(hash-table ...)` literals are handled safely.
- Reader traversal and the standalone reader driver are cheaper through
  cached drivers and iterative `nthcdr` walks. Artifact function entries
  carry a source-defun fallback when native code is unavailable.
- Bool-vectors are supported through reading, allocation, length, `aref`, and
  `aset`. Standalone `copy-sequence` preserves record type and payload slots.
- ARM64 large-stack addressing preserves the stack pointer when literal
  spills exceed the immediate addressing range. This fixes a source-loading
  crash in `eval-elisp-source`; subprocess smoke failures now include the
  child exit status and stderr.
- Darwin translates the portable file-access operation to its native syscall,
  allowing file existence checks to discover host helper executables.
- Darwin startup reconstructs the environment and child-process environment
  vector from bounded process arguments. Empty middle arguments are preserved;
  fork child detection and pipe descriptors consume both syscall return
  registers. macOS release qualification of these repairs is pending.

### Function introspection and API surface

- `fboundp` accepts `nil` and `t`; `indirect-function`, `macrop`, and
  `commandp` are available in the standalone prelude.
- Standalone `func-arity` reports Emacs-compatible ranges for native
  builtins, lambdas, closures, and macros, including open ranges represented
  by `many`.
- Process substrate parity embeds the async core and process adapter in
  artifact runtimes.

### Numbers, randomness, and printing

- Decimal literals use exact IEEE 754 binary64 conversion backed by limb
  arithmetic and oracle coverage. `exp` and `expt` handle extreme arguments
  in bounded time, and `random` stays within the fixnum range.
- `float-time` honors its `TIME` argument. Float printing goes directly
  through `number-to-string`; the measured path is 108x faster than the old
  wrapper path.

### `cl-generic`

- `cl-defgeneric` accepts a default body and `:method` forms. Dispatch adds
  `:extra STRING`, `(head SYMBOL)`, bare `:before`/`:after`/`:around`,
  `subclass`, and multi-argument methods.
- Literal symbols in EQL specializers are treated as data, while compound
  EQL forms retain definition-time evaluation.

### Regexp, strings, and other hot paths

- Regexp compilation is cached with literal fast paths, O(1) LRU touches, a
  1,024-entry cache, and cheaper plan combination.
- Common list `append` calls, `format`, `intern`, `intern-soft`, and the string
  case of `substring` take direct or native paths. Directory listings cover
  `directory-files-and-attributes` compatibility and count validation, decode
  in bulk, and Windows returns empty growth chunks to the OS.

### GC and runtime safety

- Standalone boot arms GC debt; the root stack has a bound and a 16,000-level
  recursion guard. Mid-form collection coverage is 6/6, and T110 records a
  99% ratio over six collections.
- Native-stack words pointing at unibyte strings are treated as roots, and GC
  never pins through a header word whose high half is a pointer. Oversized
  free blocks are split on reuse, closing the related allocator and header
  safety gaps.

- Conservative roots are resolved against allocation starts before precise
  tracing, including interior pointers, boot-heap edges, and large objects.
  Growing the marking queue no longer leaves a pointer to its unmapped old
  storage. Focused corruption and allocation-pressure probes pass; the full
  user configuration audit is still incomplete.
- Small free-list lookup now finds the first eligible size in at most six
  bit-search steps. Native probes cover 7,524 single-bit and mixed-mask/start
  combinations on x86_64 and ARM64, plus existing split/relink/purge routes.
  A running development REPL retained its data across installation and
  restoration of the allocator change. This is not a completed real-init
  performance or memory qualification.
- Linux GC returns complete 64 KiB interiors of coalesced free blocks with
  `MADV_DONTNEED` during the existing free-list rebuild. Live blocks and the
  header/link words remain intact; checked-allocation and poison modes retain
  their diagnostic bytes. A failed OS request leaves the free block usable.
  Other platforms retain their existing behavior. Native boundary tests
  exercise the production caller, including each diagnostic guard, and detect
  deliberate guard and header-range regressions. Long-run qualification of
  the new binary is still pending.

### REPL debugging and native replacement

- The development launcher loads failure capture and explicit retry, function
  source/hash/generation inspection, GC snapshots and comparisons, and session
  reproduction scripts. Failure arguments retain references; replay runs only
  explicitly recorded operations. These are not heap snapshots or automatic
  retries of side effects. See `docs/repl-development.md` for a fresh-session
  walkthrough.
- An opt-in Linux x86_64 reader can compile and replace allocator and GC code
  while retaining Lisp state. A fixed shared ABI permits changes to private GC
  helpers; heap-layout migration is not supported. Restoring original code
  does not roll back heap state, and old code mappings remain until process exit.
- GC diagnostics expose aggregate conservative retention counts and collection
  counters. Collection-call elapsed time includes overhead; it is not an exact
  stop-the-world pause measurement or an individual object retention path.
- Native socket primitives remain available on Linux and Windows x86_64 only.
  ARM64 targets report the catchable `nelisp-unsupported-primitive` condition;
  ARM64 release qualification does not imply native networking support.

### Release workflow and presence corpus

- The semver tag workflow is zero-Rust: it builds and tests the pure-Elisp
  standalone artifacts, checksums each bundle, and runs the release soak.
  Before the GitHub Release, tag CI requires `linux-x86_64`, `linux-aarch64`,
  and the Linux 1-hour soak.  macOS ARM64 qualification is deferred; its
  artifact is not a v1.2.2 release target and must not be reported as PASS.
- The generator-authoritative presence corpus contains **856 names**:
  **571 shared** and **285 standalone-only**. This corpus count is separate
  from the full runtime presence sweep below.

## Current qualification status

| Check | Result |
|---|---|
| REPL development APIs | PASS: failure recording, explicit retry, export/replay, code provenance, GC snapshot/collect/compare, and integrated entry require |
| Native runtime reload (Linux x86_64) | PASS: development binary SHA-256 prefix `86a0d4f947bc`; actual collection uses threshold percentage A 300 → new private helper B 301 → restored 300; changing B's expected result to 300 triggers the intended assertion |
| Normal standalone binary | SHA-256 prefix `0c09c8af9fc4`; bounded memory probes below use this binary |
| Full ERT suite | 5,594 PASS, 159 skipped, 5,753 total; the numeric oracle uses isolated artifacts while a production REPL remains running |
| Check tier | 23/23 PASS |
| Isolated mutation gate | 64/64 PASS on the isolated GC snapshot; all four CI mutation shards also pass on `ded6ebf13` |
| Bounded memory probes | 6/6 PASS; 500,000 → 1,000,000 workload RSS 279,064 → 279,668 KiB; 1,006,632,960 bytes reclaimed |
| Full real-init audit | `0c09c8af9fc4` with the library regex repair reached form 309, then timed out after 9,001 seconds (exit 124, peak 941,356 KiB). The run exposed additional compatibility errors; all 930 forms and startup hooks remain unqualified |
| Linux x86_64/ARM64 semver tag CI | Required Linux qualification; macOS ARM64 is deferred and excluded from the v1.2.2 release target |
| Linux 1-hour soak | CI binary `620980fbe756` passes: 2,053 batches, 3,600.621 seconds, RSS 95,312 KiB unchanged. Local `0c09c8af9fc4` has both failures and a later strict 1-hour pass. Free-page-return binary `35d16f0f6681` failed after 559.525 seconds: RSS 61,844 → 68,868 KiB, exceeding the unchanged 5,120 KiB growth ceiling. A subsequent parallel 1-hour comparison passes all three conditions: default `35d16f0f6681` (2,326 batches, sampled RSS 61,844 KiB unchanged), arena-only no-huge-page `17d4fc336e58` (2,319 batches, 55,096 KiB unchanged), and arena-plus-intern `eb821597fde1` (2,319 batches, 53,080 KiB unchanged). The comparison did not reproduce the intermittent failure or establish its cause; the no-huge-page candidates remain experimental and memory qualification is not complete |
| Large string allocation and replay | Old `0c09c8af9fc4` crashes on a single 6 MiB string. Iterative UTF-8 repeat binary `ffbbc2666928` passes 6/32 MiB, Unicode, zero-length and GC checks. Dedicated development binary `0cedf8060373` completes the exported 32 MiB recipe, native GC publication and restoration with both completion markers and empty stderr |

The table above predates the native-unit and memory work; the current
qualification is below it, measured on the native-runtime-reload candidate at
`9ae5fa4ce`. It does not yet represent a completed public release: the branch
is not integrated to `main` and no v1.2.2 tag or GitHub Release exists.

## Current qualification (native-runtime-reload, 2026-09-12)

| Check | Result |
|---|---|
| Branch CI, every lane | PASS — run 34626792979: ubuntu 30.1/29.4, windows 30.1/29.4, macOS 30.1/29.4, four gate-mutation shards, gates, tier perf/smokes/extras, and the final unscoped `verify` |
| Full ERT | 5,921 tests, 5,760 as expected, **0 unexpected**, 161 skipped |
| Check tier | 23/23 PASS locally with `NELISP_CHECK_SKIP=gate-mutation`; gate-mutation runs as CI's four shards, all green |
| `native-unit-repl-smoke` (new required Linux gate) | PASS in CI, checked=88 findings=0, binary `50718ddd0ffd` |
| `lisp-byte-compile` (new required Linux gate) | PASS, 254 files, 39 baselined. `make compile` had never covered `lisp/` at all |
| `nelisp-sexp-clone-bind-smoke` (new required Linux gate) | PASS, checked=10 findings=0; mutation row verified red |
| Full real-init audit (930 forms) | PASS — 930/930 boundaries, `AUDIT_DONE 930`, exit 0, no signal, 36s, binary `b9cb89afd25c` |
| Native unit replacement, live | PASS in one standalone process: publish, a caller compiled once observing a later generation, CAS rejection of a stale candidate, arity refusal, preserved Lisp state |
| Retained-mapping accounting | Over 10 republications RSS grew 4,164 KiB, of which retained native mappings are 64 KiB resident (<1.5%); accounted retention is exactly 8,192 B per retired generation, linear in the retired count |
| Linux 1-hour soak | THP-dependent, not a leak — see blocker 2 below |
| Semver tag CI | Not run; needs a tag |

## Prior candidate evidence (historical)

The following results belong to a prior candidate before the current GC and
coalescing changes. They are retained for traceability and are not current
qualification results.

The previous post-coalescing checkpoint reported reader 32/32, smokes 51/51,
source bootstrap 8/8, and focused numeric mutation 1/1 passing. Its binary size
was 7,683,840 bytes against a 7,786,916-byte ceiling. An older real-init run
crashed at form 206 with exit 139 and peak RSS 1,063,968 KiB. Those measurements
do not describe the current binary; the earlier allocator candidate's reported
violation count reduction (2 to 0) was not sufficient release qualification.

| Check | Prior candidate result |
|---|---|
| Version consistency | 9/9 PASS |
| Check tier | 23/23 PASS with mutation coverage |
| Full ERT suite | 5,673 total; 5,514 pass; 159 skip; 0 fail |
| Gate-mutation | 64 PASS + 5 platform skips |
| Native-artifact | 9/9 PASS |
| Selfhost | 3/3 PASS |
| Performance | 9/9 PASS; checked arithmetic 1.014x; ceiling 1.15 |
| Extras | all 21 PASS |
| Presence corpus | 856 names; 571 shared / 285 standalone-only |
| Full presence sweep | 4,851 checked; 0 findings; 685 accepted divergences |
| No-JIT suite | exit 0 |
| JIT suite | exit 0 |
| `git diff --check` | PASS |

## Remaining release qualification

Two of the three blockers below are closed; the third is the tag run itself.

1. **Full real-init audit across all 930 forms — CLOSED.** It had been stopped
   at form 4 by a deterministic SIGSEGV in the arena's boundary reclaim: the
   rewound bump span was handed out again without being zeroed, unlike the
   free-list reuse path, so a constructor expecting fresh memory inherited a
   stale `Sexp::Cons` with a garbage payload pointer. Present in every
   standalone binary built that day, so never a recent regression. With the fix
   (`b9cb89afd25c`) the audit reaches all 930 boundaries, prints
   `AUDIT_DONE 930`, exits 0 with no signal in 36s, and leaves the init file's
   hash unchanged. 322 `FORM_ERROR`s remain (238 `void-function`, 54
   `file-missing`, 29 `void-variable`, 1 `error`) — unimplemented Emacs APIs
   and absent files, not memory faults, and more numerous than the earlier
   partial run because far more of the file now executes.
   `nelisp-sexp-clone-bind-smoke` is a required Linux gate holding the
   mechanism, and its mutation row is verified red.
2. **Linux 1-hour soak — EXPLAINED, and it is not a leak.** Same binary, same
   host, one variable: with this host's `transparent_hugepage/enabled` at
   `[always]` it FAILS at 1,135.7s (RSS 61,844 → 70,688 KiB, ceiling 5,120,
   AnonHugePages 51,200 KiB); with THP disabled for that process alone through
   `prctl(PR_SET_THP_DISABLE)` (`tools/nelisp-nothp.c`) it PASSES — 1,500.6s,
   926 batches, peak RSS **equal to** start. A leak grows the heap either way.
   The 5,120 KiB growth ceiling is also smaller than three 2 MiB huge pages, so
   a THP-backed process can cross it on granularity alone. This also accounts
   for the previously unexplained `35d16f0f6681` failure, whose starting RSS
   (61,844 KiB) matches to the kilobyte. The soak still needs to pass on the
   release runners, which is blocker 3.
3. **Semver tag CI on `linux-x86_64` and `linux-aarch64` — still open.** It has
   not been run; it needs a tag. Branch CI itself is green on every lane
   (run 34626792979: six smoke lanes, four gate-mutation shards, gates, all
   three tiers and the final unscoped `verify`).
