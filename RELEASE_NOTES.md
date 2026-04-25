# NeLisp Stage D v2.0 Release Notes

Phase 7.5 (Doc 32 v2 LOCKED) ships `stage-d-v2.0`, the first NeLisp
distribution that runs without a host Emacs install on the target
machine.  This document describes the artifact, the per-platform tier
matrix, and how to verify a download.

## Highlights

- *Phase 7+ NeLisp purity max path 完遂* — syscall surface trimmed to
  ~819 LOC of Rust (Phase 7.0 SHIPPED) + the remaining 3-core
  (allocator / GC inner / coding) ported into NeLisp itself.
- *`bin/anvil --strict-no-emacs` mode* gives a truly standalone binary
  path; the default `--no-emacs` mode falls back to the host Emacs path
  on cold-init failure (Doc 32 v2 §2.6).
- *4-stage cold-init bootstrap protocol* (Doc 28 §3.5) — stage0 embed
  → stage1 native compile → stage2 semantic diff → stage3 self-recompile.
- *Staticlib static-link binary* (Doc 32 v2 §2.1) — `libnelisp_runtime.a`
  is linked statically; cdylib remains as an opt-in sidecar for
  embedders who want a shared object instead.
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

## Building from source

```bash
# Single platform
make release-artifact PLATFORM=linux-x86_64 RELEASE_VERSION=stage-d-v2.0

# Verify the freshly built tarball
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
