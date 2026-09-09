# NeLisp v1.2.2

Release implementation and qualification notes, prepared 2026-09-10.

## Changes since v1.2.1

- **Exact binary64 decimal literals.** Decimal literals use the exact IEEE
  754 conversion path with limb arithmetic and oracle coverage.
- **Bool-vectors.** Reader, allocation, length, `aref`, and `aset` paths carry
  Emacs-compatible bool-vector behavior.
- **GC safety and measurement.** The rootstack bound and 16,000-level guard
  are covered, mid-form collection coverage is 6/6, and T110 records a 99%
  ratio over six collections.
- **Iterative evaluation.** Argument lists, lambda bodies, and `progn` use
  iterative evaluation paths while preserving order and live values.
- **Bootstrap and fallback bindings.** `load-file` is available before nested
  `require` calls, and pcase fallback bindings preserve branch-local scope.
- **`cl-defmethod` dispatch.** `subclass` and multi-argument methods are
  supported, as are bare `:before`, `:after`, and `:around` qualifiers.
  Literal symbols in EQL specializers are treated as data; compound EQL
  forms retain definition-time evaluation.
- **Process substrate parity.** The async core and process adapter are
  embedded in artifact runtimes.
- **Standalone record copying.** `copy-sequence` copies record type and
  payload slots in the standalone runtime, preserving record sequence
  behavior.

## Qualification evidence

| Check | Result |
|---|---|
| Presence corpus | 852 names; generator corpus-check PASS |
| Standalone reader | 31/31 PASS |
| No-JIT suite | exit 0 |
| JIT suite | exit 0 |
| Native-artifact | 9/9 PASS |
| Selfhost | 3/3 PASS |
| Performance | 9/9 PASS; checked arithmetic 1.014x, ceiling 1.15 |
| Smokes | 51/51 PASS |
| Extras | all 21 gates PASS |
| Gate-mutation | 64 PASS; 5 platform skips |
| Full ERT suite | 5,673 total; 5,514 pass; 159 skip; 0 fail |
| Binary-size ratchet | PASS; final main-path build pending; ceiling 7,786,916 bytes |
| Check tier | 23/23 PASS |
| `git diff --check` | PASS |

## Remaining release qualification

| Check | Status |
|---|---|
| Full presence sweep | Pending; currently running, with no release PASS result yet |
| macOS CI | Release CI is tracked by GitHub Actions; macOS hardware validation remains pending |

The macOS NaN printer assertion compares the passthrough result with the
host `number-to-string` result, so platform-specific NaN spelling does not
decide the assertion. The release CI result is tracked by GitHub Actions, and
the macOS hardware result remains pending.
