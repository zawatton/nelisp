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
