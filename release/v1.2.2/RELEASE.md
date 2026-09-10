# NeLisp v1.2.2

Release implementation and qualification notes, prepared 2026-09-10.

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

### Release workflow and presence corpus

- The semver tag workflow is zero-Rust: it builds and tests the pure-Elisp
  standalone artifacts, checksums each bundle, and runs the release soak.
  Before the GitHub Release, tag CI gates `linux-x86_64`, `macos-aarch64`,
  `linux-aarch64`, and the Linux 1-hour soak.
- The generator-authoritative presence corpus contains **856 names**:
  **571 shared** and **285 standalone-only**. This corpus count is separate
  from the full runtime presence sweep below.

## Current qualification status

| Check | Result |
|---|---|
| Standalone reader | 32/32 PASS |
| Smokes | 51/51 PASS |
| Main-path binary size | `target/nelisp`: 7,683,840 bytes; ratchet ceiling 7,786,916; PASS |
| Latest source bootstrap | 8/8 PASS |
| Focused `nl-num` mutation | 1/1 PASS |
| Full real-init audit | NOT COMPLETE; prior run stopped at form 206 with exit 139 and peak RSS 1,063,968 KiB; scope is 930 init forms plus startup hooks |
| Luna memory-fix candidate | Checked allocator violations 2 → 0 under pressure; not integrated and not qualified |
| 3-architecture semver tag CI | Pending |
| Linux 1-hour soak | Pending |

The current results above are from the post-coalescing tree. They do not
represent a completed release qualification while the real-init audit and
release gates remain pending.

## Prior candidate evidence (historical)

The following results belong to a prior candidate before the current GC and
coalescing changes. They are retained for traceability and are not current
qualification results.

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

The release is not qualification complete. Remaining blockers are:

1. Integrate and requalify the memory fix, then complete the full real-init
   audit across all 930 init forms and startup hooks.
2. Pass semver tag CI on `linux-x86_64`, `macos-aarch64`, and `linux-aarch64`.
3. Pass the Linux 1-hour soak.
