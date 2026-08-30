# NeLisp Release Notes

## v1.1.1 — 2026-08-30

Full notes: [`release/v1.1.1/RELEASE.md`](release/v1.1.1/RELEASE.md).

Fourteen precise-root defects in the standalone runtime, all of two shapes,
plus the missing bound that had to be fixed first -- and four in the AOT
compiler, where a raw machine word crossed a defun boundary that expects a
Sexp pointer.

- **The layout-dependent crash is gone.** anvil's standalone MCP server produced
  correct output and then died -- SIGSEGV or `form aborted without signal`,
  decided by memory layout.  Cause: `nl_gc_mark_recorded_pool` walked each
  recorded frame's parse pool with a global capacity word naming a different
  load.  Amplifier: `nl_gc_mark_char_table_slots` believed the length it found
  there and walked 1.26 GB past the arena.
- **Eleven walkers stopped carrying a materialised cdr across an eval.**
  `setq`, `let` bindings and bodies, `let*`, `if`'s else branch, `while`,
  `progn`, lambda bodies, `condition-case` handlers, `catch` and `throw`.
  Position decides: `(progn (f) 1)` was always correct, `(progn 1 (f))`
  answered 47826824.
- **Four live values stopped living in unrooted scratch** -- `let`'s value slot,
  the builtin argument list, and the `catch` / `throw` tags.  A blanked tag
  makes a `throw` walk past its own `catch` and report `no-catch`.
- **`nl_root_reserve_slot` has a bound.** It bumped through a fixed
  131072-entry region with none, writing past it under deep recursion.
- **New gate:** `make precise-root-coverage`, 51 configurations run with the
  conservative native-stack scan off, asserting values -- most of these defects
  answered wrongly rather than crashing.
- **AOT: raw words stopped crossing a value-word boundary.** In the lane where
  user `.el` modules compile to native code every parameter arrives as a Sexp
  pointer, but nothing converted the values going the other way: `(g 0)` had
  the callee load `[0+8]`.  Call arguments, the string grammar's index and
  count operands, defun returns, and the `call` node's own declared
  representation were all missing their conversion.  `(defun f (x) (g 0 x))`
  in nine lines reproduced it; `nelisp-nelix-native-hot-gate` goes from failing
  its fourth case to 6/6.

With that scan disabled, the whole standalone tier and anvil's own module load
now pass; before, the load segfaulted in under a second.  The scan stays on.

## v1.1.0 — 2026-08-27

Full notes: [`release/v1.1.0/RELEASE.md`](release/v1.1.0/RELEASE.md).

Closes v1.0.1's own known issue. Unibyte strings are now a distinct
representation -- Sexp tags 14 and 15 -- so a raw byte and a character are told
apart everywhere it matters.

- **`(append (unibyte-string 200 201 202) nil)` answers `(200 201 202)`.** It
  answered `(521 640 0)`. `equal`, `length`, `aref`, `concat`, `substring`,
  `upcase`, `multibyte-string-p` and the printer all agree with stock Emacs
  30.1 now; the measured before/after table is in the full notes.
- **`equal` compares characters, then bytes, then content** -- as Emacs does,
  never the multibyte flag. `(equal "abc" (unibyte-string 97 98 99))` stays
  `t`; a tag-identity rule would have broken it.
- **`aset` follows Emacs 31.1's fixed-width rules.** The Sexp tag and
  `string-bytes` are asserted invariant under mutation.
- **The reader implements `\NNN` and `\xNN`.** A separate, older defect:
  `"\310"` had been the three-character string `"310"` and `"\x1b["` had been
  `"x1b["`, silently. Zero files in the tree used them, which is why it
  survived.
- **Two tests had been passing for the wrong reason**, and implementing the
  reader escapes is what exposed them. One compared received bytes against a
  literal that both sides had been misreading identically; fixing the literal
  made the comparison real, and it failed because `nelisp-socket-recv` was
  labelling bytes off a wire as UTF-8. It answers a unibyte string now.
- **A countable audit, not an asserted one.** `make doc200-census` enumerates
  every site that tests or writes the string tag and fails when one appears or
  vanishes. v1.0.1's notes estimated 59 such lines by grep; the structural
  census finds 119, in a 174-row ledger. Six mutation rows, one per consumer
  arm including the GC marker's, all go red.

Not done, and recorded as not done: raw-byte characters (`#x3FFF00 + B`) have
no representation here, so mixing a non-ASCII unibyte string into a multibyte
context signals rather than inventing a character; `\N{U+XXXX}` is
unimplemented; and `aset` diverges from the 30.1 parity host, which is laxer
than 31.1.

## v1.0.1 — 2026-08-26

Full notes: [`release/v1.0.1/RELEASE.md`](release/v1.0.1/RELEASE.md).

A same-day follow-up to v1.0.0, carrying the in-process `.neln` loader and
three defects that v1.0.0's own change -- the collector running by default --
brought into the open.

- **The loader runs any artifact, read at run time.** v1.0.0's version was a
  demo: bytes and extern addresses baked in at build time, one function only.
- **`pcase` now supports `(cl-type TYPE)`.** It signalled where Emacs answers.
  `emacs-parity` compares 19,995 behaviours and this was not among them.
- **`base64-decode-string` returns bytes, not characters.** The pair did not
  round-trip for any input with a high byte; only the encoder had been fixed.
- **The loader's digest buffer was not a GC root.** The same intact artifact
  digested differently on every run -- the signature of reading reclaimed
  storage. Impossible before the collector ran by default.
- **The native harness's slot registry grows** instead of stopping at 64,
  which every tight arithmetic loop exhausted in tens of iterations.
- **`stage-d` parity ran for the first time in 12 attempts** -- it had been
  demanding a `main` branch from a sibling that is on `master`.

Known issues, both stated in the full notes: macOS aarch64's `boxed` parity
case segfaults (not a regression -- previously unreachable), and unibyte
strings are still not a distinct representation, which is what makes `append`
answer `(521 640 0)` for three raw bytes. `docs/design/200-unibyte-string-
representation.org` records that one rather than half-fixing it.

---

## v1.0.0 — 2026-08-26

Full notes: [`release/v1.0.0/RELEASE.md`](release/v1.0.0/RELEASE.md).

Two things make this 1.0 rather than another point release.

**The runtime collects garbage by default.** The precise-root collector
existed before and was correct, but only ran behind a debug switch, so a
default build grew without bound. On a 200k allocating loop peak RSS falls
**669,936 KiB → 339,920 KiB (-49.3%)**, RSS is flat from 500k to 1M
iterations, and 256 MiB goes back to the OS. It is also faster — 5,204 ms
collected vs 8,125 ms uncollected — so this was not a memory-for-speed trade.

**Ordinary allocating Elisp runs on real OS threads.** Doc 199 Tiers 1–3b:
cooperative futures, GC-free `clone(2)` workers, bounded allocating tasks, and
finally unrestricted allocating Elisp with per-thread precise roots and a park
barrier. The barrier stops every mutator before marking, which is why Doc 152's
planned write barrier (Stage 6) was retired rather than implemented.

Also in 1.0:

- **Standard Emacs names are the API.** `current-buffer`, `insert`, `point`,
  `make-process`, `set-process-filter`, `make-network-process`, `run-at-time`,
  `add-hook` and 15 more are `fboundp` in a default `target/nelisp` with no
  `--load`. The `nelisp-`-prefixed functions are the implementation beneath.
- **Opt-in language extensions**, all sharing one rule — loading them changes
  nothing about plain Elisp semantics: `nl-safe`/`nl-static`/`nl-check`/
  `nl-contract` (borrow cells, fat pointers, an `nl-unsafe` boundary under a CI
  ratchet, expansion-time totality and types, contracts with blame), `nl-ns`
  (namespace crossings reported, nothing rewritten, nothing at run time),
  `nl-clj` (persistent vector/map/set, atom, eager and lazy seqs). 39 packages.
- **Buffers, bignums and full backquote** — all three were listed as deferred
  in the v0.6.0 README and all three had in fact shipped.
- **Networking** — processes, event loop, `make-network-process`,
  `open-network-stream`, `/etc/hosts`, DNS over TCP, nonblocking sockets, IPv6.
- **Zero Rust.** No `.rs` files remain.

Verification: 5,466 tests / 0 unexpected; `emacs-parity` **19,961 checks, 0
findings** against a real stock Emacs; `verify` PASS (66 gates); six CI lanes
plus a fast Linux `gates` job.

Known limits are listed in the full notes — no windows or frames, no markers
or overlays, Linux-only sockets, worker heaps reclaimed at process exit.

---

# NeLisp v0.6.0 Release Notes — Pure-Elisp Standalone Runtime

v0.6.0 (2026-06-26) is the current stable SemVer tag.  It collects the
large post-v0.5.1 development line into one user-facing release and
switches the default standalone tarball names from the historical
`stage-d-v3.0` label to `v0.6.0`.

## Highlights

- **SemVer release line** — stable users should pin `v0.6.0`; `main`
  remains active development.  Historical `stage-d-v3.0` installer and
  runbook paths remain for compatibility, but generated standalone
  artifacts now default to names like
  `anvil-v0.6.0-linux-x86_64.tar.gz`.
- **Zero-Rust standalone runtime** — `target/nelisp` remains the
  no-Emacs runtime for REPL, `--eval`, `--load`, file execution,
  runtime-image replay, artifact loading, and self-hosting AOT compile.
- **AOT / OS surface expansion** — adds data blobs, `.rela.data`
  handling, `frame-alloc`, `shr`, `f64-bits`, defined SysV varargs,
  libm/f64 return bridging, and extra raw syscall helpers such as
  `unshare`, `mount`, `pivot-root`, `chdir`, `rmdir`, and `mkdtemp`.
- **Float and printer correctness** — includes Eisel-Lemire float
  parsing, correctly-rounded power tables, shortest-round-trip
  printing, C99 hex-float, negative-zero preservation, inf/nan handling,
  and `%f` / `%e` / `%g` precision support.
- **Stdlib and package-load breadth** — closes many bare-reader gaps:
  `%` and `/=`, broader CL/Elisp helpers, rx / cl-generic support,
  setf places, `cl-defstruct` breadth, hash-table introspection,
  bucketed hash-table storage, list-search hot paths, and incremental
  `load` to avoid retaining an entire source AST.
- **Low-memory load path** — flat-arena cold-load plus compacting GC and
  8-byte container slots keep the full vendor-load path around 118MB
  peak RSS instead of the older 1.80GB path.

## Verification

Recommended local release gates:

```bash
make test-parallel
make standalone-reader-test
make standalone-reader-prelude-test
make standalone-selfhost-test
make standalone-selfhost-mt-test
make standalone-parallel-compile-test
make standalone-tarball PLATFORM=linux-x86_64
make standalone-tarball-verify PLATFORM=linux-x86_64
```

The first-class release blocker remains `linux-x86_64`.  macOS arm64
and Windows x86_64 are CI-gated where runners are available; Linux
aarch64 and macOS x86_64 remain supported by the pure-Elisp object
writers but are not hard blockers without dedicated runners.

---

# NeLisp v3.0 Release Notes — Pure-Elisp (0 Rust, archived stage name)

v3.0 (2026-06-02) completes the pure-elisp migration: all `.rs` files
and Cargo.toml have been removed.  The standalone interpreter/compiler
is now built entirely by `emacs --batch` (`make standalone-reader`),
with zero Rust or Cargo involved.  This document describes the artifact,
the per-platform tier matrix, and how to verify a download.

Prior release notes for the v2.0 Stage D bundled-Emacs tarball are
preserved below for reference.

---

# NeLisp Stage D v2.0 Release Notes (archived)

Phase 7.5 (Doc 32 v2 LOCKED) shipped `stage-d-v2.0`, the first NeLisp
distribution that ran without a host Emacs install on the target
machine.  As of v3.0 the Rust runtime substrate has been deleted
entirely; these notes are preserved for historical reference.

## Highlights (v2.0, archived)

- *Phase 7+ NeLisp purity max path 完遂* — syscall surface trimmed to
  ~819 LOC of Rust (Phase 7.0 SHIPPED) + the remaining 3-core
  (allocator / GC inner / coding) ported into NeLisp itself.
  (v3.0: the remaining Rust is also gone — 0 LOC total.)
- *`bin/anvil --strict-no-emacs` mode* gives a truly standalone binary
  path; the default `--no-emacs` mode falls back to the host Emacs path
  on cold-init failure (Doc 32 v2 §2.6).
- *4-stage cold-init bootstrap protocol* (Doc 28 §3.5) — stage0 embed
  → stage1 native compile → stage2 semantic diff → stage3 self-recompile.
- *MCP server compatibility* — `bin/anvil mcp serve` exposes the
  headless profile (~28 tools) without any change to the existing
  `claude-code-ide` / Claude Code MCP client integration.

## Tier matrix (Doc 32 v2 §7 4-tier gate)

| Tier                          | Platform        | v1.0 status              |
|-------------------------------|-----------------|--------------------------|
| blocker                       | linux-x86_64    | CI gate (must pass)      |
| non-blocker (v1.0 時限)       | macos-arm64     | best-effort 95%+         |
| non-blocker (v1.0 時限)       | linux-arm64     | best-effort 95%+         |
| post-ship audit               | weekly 24h soak | release-audit            |
| release artifact ready        | all of the above | tarball + sha256 + sig   |

`v1.0 時限` is the explicit time-boxed exception ratified in Doc 32 v2
§11 LOCKED: arm64 ships as best-effort for v1.0 only.  v1.1+ is
expected to promote arm64 to *blocker* status (6 month target, anvil
leverage included), at which point this RELEASE_NOTES.md tier matrix
must be updated and the audit grep guard in
`test/nelisp-release-test.el` retired.

## Soak gate (Doc 32 v2 §2.7)

| Tier             | Duration | RSS growth ceiling |
|------------------|----------|--------------------|
| blocker (CI)     | 1h       | < 5 MB             |
| post-ship audit  | 24h      | < 10 MB / 24h      |

Implemented by `tools/soak-test.sh` — `SOAK_DURATION_HOURS=1` for
blocker, `SOAK_DURATION_HOURS=24` for post-ship audit.

## Verifying a downloaded artifact

```bash
# linux-x86_64 example — adjust platform suffix as needed
sha256sum --check stage-d-v2.0-linux-x86_64.tar.gz.sha256

# Inspect the ad-hoc signature placeholder.  Real GPG signing lands
# in v2.1+ (Doc 32 v2 §2.5 + §8); v2.0 ships an ad-hoc tag only.
cat stage-d-v2.0-linux-x86_64.tar.gz.sig
```

## Building from source (v3.0)

Requires: Emacs 29+.  No Rust/Cargo needed.

```bash
# Build the standalone interpreter (emacs --batch, zero cargo)
make standalone-reader

# Self-host verification
make standalone-selfhost-test        # (fact 5) → native ELF → exit 120
make standalone-selfhost-mt-test     # clone+atomics → exit 42
make standalone-parallel-compile-test  # 4 fork workers → 11,22,33,44

# Full test suite
make test

# Release artifact (Stage D tarball, archival)
make release-artifact PLATFORM=linux-x86_64 RELEASE_VERSION=stage-d-v2.0
make release-checksum PLATFORM=linux-x86_64 RELEASE_VERSION=stage-d-v2.0

# 1h blocker soak
make soak-blocker

# 24h post-ship soak (run only when you have a day to spare)
make soak-post-ship
```

## Known limitations

- *macOS notarization* — out of v2.0 scope (Doc 32 v2 §8 v2.1+).  The
  artifact ships an ad-hoc signature placeholder only.
- *Windows native build* — Stage A path (Doc 18) handles Windows via
  msys2 mingw64 today; a true `--no-emacs` Windows binary is v2.0+ scope.
- *ARM 32-bit* — out of v2.0 scope (Doc 32 v2 §8).  Only x86_64 + arm64
  qualify.
- *14000-entry full Japanese coding table* — Phase 7.5 ships ~885
  curated entries; the full table is Phase 7.5 follow-up scope.
- *Self-update* — no `bin/anvil --self-update` in v2.0 (Doc 32 v2 §8).

## Doc references

- `docs/design/32-phase7.5-integration.org` (v2 LOCKED) — primary
  authority for Phase 7.5 / `stage-d-v2.0`.
- `docs/design/27-phase7-rust-syscall-stub.org` — Phase 7.0 syscall
  surface (LOCKED).
- `docs/design/28-phase7.1-cold-init.org` — 4-stage bootstrap protocol.
- `docs/design/18-stage-d-anvil-launcher.org` — Stage D / `bin/anvil`
  launcher (LOCKED).
