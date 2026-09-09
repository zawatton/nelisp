# NeLisp v1.2.2 — Draft

Draft prepared on 2026-09-09 for the changes currently ahead of
`origin/main`.  The release qualification results and final integration
status are TBD until the remaining implementation and release gates finish.

## Changes since v1.2.1

- **Decimal literals are exact binary64.**  The compiler's decimal-to-float
  path now converts literals to IEEE 754 binary64 values exactly, with limb
  arithmetic and an oracle test covering the conversion boundary.
- **Bool-vectors are integrated across the standalone path.**  Reader,
  allocation, length, `aref`, and `aset` handling follow Emacs-compatible
  bool-vector semantics, including the standalone helper and cache paths.
- **GC debt and fragmentation instrumentation are present.**  Standalone
  boot arms GC debt, and the pause-growth tooling records fragmentation policy
  measurements.  Final release qualification for these measurements is TBD.
- **Argument, lambda-body, and `progn` evaluation uses iterative paths.**
  Argument-list evaluation and the relevant lambda/progn loops avoid the
  previous recursive shape while preserving evaluation order and live-value
  handling.
- **Standalone bootstrap supplies `load-file` before nested requires.**
  Bootstrap sources can therefore load files during the early nested-require
  phase; the regression coverage is included in the standalone bootstrap
  tests.
- **The pcase fallback preserves branch-local bindings.**  Macroexpansion
  fallback handling now accepts the binding shapes used by the affected
  patterns, with a standalone regression smoke.

## Planned release-candidate additions

The following are scheduled for this release preparation and remain
unverified in this draft:

- **Multi-argument and `subclass` `cl-defmethod` support.**  Implementation
  and focused host/standalone verification: TBD.
- **macOS NaN printer portability.**  The float-printer regression should
  compare the passthrough result with the host `number-to-string` result,
  because the NaN sign spelling differs between Linux and macOS.  macOS
  validation: TBD.

## Qualification

| Check | Result |
|---|---|
| `make version-consistency` | TBD |
| `git diff --check` | TBD |
| Full ERT suite | TBD |
| Standalone reader and release artifact gates | TBD |
| macOS qualification | TBD |
