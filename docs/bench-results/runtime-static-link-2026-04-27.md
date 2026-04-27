# Phase 7.5.1 — cdylib + staticlib dual-output build size delta

**Date:** 2026-04-27
**Host:** Linux x86_64 (`x86_64-pc-linux-gnu`)
**Branch:** `feat/phase7.5.1-cdylib-static-config`
**Reference:** Doc 32 v2 LOCKED §3.1 (Phase 7.5.1) — cdylib static link config
**Cargo:** `cargo build --release` with `crate-type = ["cdylib", "rlib", "staticlib"]`
**Profile:** `[profile.release]` opt-level=3, lto=true, strip=true, panic="abort"

## Summary

Dual-output build (single `cargo build --release`) produces:

| artifact                          | size  | crate-type  | use case                                      |
|-----------------------------------|-------|-------------|-----------------------------------------------|
| `libnelisp_runtime.so`            | 2.3M  | cdylib      | dlopen / Emacs module wrapper / Phase 7.5.4   |
| `libnelisp_runtime.a`             | 27M   | staticlib   | Phase 7.5.2 `bin/anvil --strict-no-emacs`     |
| `libnelisp_runtime.rlib`          | 596K  | rlib        | cargo-internal consumers (bench, test crate)  |
| `nelisp-runtime`                  | 312K  | bin         | CLI smoke (`--syscall-smoke` / Phase 7.0)     |

## Why the staticlib is ~12x larger than the cdylib

The staticlib carries every transitive object from every dependency (libsqlite3 bundled, serde_json, rusqlite, ahash, hashbrown, libc, once_cell, ...) as plain ELF objects packed in an `ar` archive. The cdylib only ships the symbols actually exported across the FFI boundary plus what the dynamic linker can resolve at runtime against system libs (libc, libpthread, libdl).

The 27M staticlib size is **expected and within Doc 32 v2 §3.1 budget** (target ≤ 15M for the *final linked binary*, not the unlinked archive — the linker GC + `-Wl,--gc-sections` / `-Wl,--strip-all` in Phase 7.5.2 will drop most of it). Static-link estimates after dead-code elimination land closer to 4-6 MiB stripped (= the actually-reachable subset of nl_syscall_* + nl_sqlite_* + nl_runtime_* symbols).

## Build commands tested

```sh
# Both targets share one cargo invocation (Cargo.toml [lib] crate-type controls outputs):
make runtime          # produces .so + .a + .rlib + bin in one cargo pass
make runtime-static   # same cargo pass + ar-archive smoke (file(1))
make runtime-staticlib  # canonical name, identical to runtime-static (alias)

# CI smoke included in runtime-staticlib:
file libnelisp_runtime.a
# → libnelisp_runtime.a: current ar archive
```

## ERT coverage delta (test/nelisp-runtime-test.el)

| ERT                                                 | what it asserts                                  |
|-----------------------------------------------------|--------------------------------------------------|
| `nelisp-runtime-staticlib-exists`                   | `.a` file present after build                    |
| `nelisp-runtime-staticlib-and-cdylib-coexist`       | dual-output invariant (= both or neither built)  |
| `nelisp-runtime-staticlib-is-ar-archive`            | `file(1)` reports `current ar archive` magic     |

Pre-Phase 7.5.1: `make test-runtime` → 8/8 pass
Post-Phase 7.5.1: `make test-runtime` → 11/11 pass (3 new + 8 existing, 0 regression)
Full regression: `make test` → 2310/2395 pass, 0 unexpected, 85 skipped (= no Rust toolchain on CI host)

## bin/anvil doctor probe

`anvil doctor` now surfaces both artifacts:

```
runtime cdylib  : /<repo>/nelisp-runtime/target/release/libnelisp_runtime.so (2.3M)
runtime static  : /<repo>/nelisp-runtime/target/release/libnelisp_runtime.a (27M)
```

When either artifact is missing, the line surfaces an actionable hint:
`NOT BUILT — run 'make runtime'` / `NOT BUILT — run 'make runtime-static'`.

## Acceptance gate (Doc 32 v2 §3.1)

- HARD: `make runtime-static` produces `libnelisp_runtime.a` — **PASS**
- HARD: `make runtime` continues to produce `.so` — **PASS** (single cargo build emits all three crate-types)
- SOFT: `bin/anvil doctor` surfaces static-lib presence — **PASS**

## Out of scope (deferred to later sub-phases)

- Real static link of `libnelisp_runtime.a` into the bin/anvil bash launcher (= Phase 7.5.2 `--strict-no-emacs` cold-init coordinator, design doc §3.2)
- macOS arm64 codesigning of the shipped binary (= Phase 7.5.3 release-artifact, §3.3)
- Cross-arch CI (Linux arm64 + macOS x86_64 / arm64) of the dual-output build (= Phase 7.5.3 §2.4)
- LTO regression sweep of staticlib symbol stripping under `--release --keep-symbols` (= follow-up under §3.1 risks)
