# macOS ARM64 qualification — run sheet (ships as v1.3.1)

macOS ARM64 has been deferred since v1.0 and has never been qualified on real
hardware. CI's macOS smoke lanes are green, and that is a **different and much
weaker claim** than this run: CI does not build the release artifact for macOS,
does not verify the tarball, and does not run the native replacement work this
release is mostly about. Until this sheet has been worked through on a real
Mac, the macOS artifact is not a v1.3.0 release target and must not be
reported as PASS.

This is written to be followed by someone who was not in the session that
produced v1.3.0. Every step says what to run, what a pass looks like, and what
to do when it is not that.

## Before you start

- **Hardware.** A real Apple-silicon Mac. Not a VM, not a CI runner, not
  Rosetta. `uname -m` must say `arm64`.
- **Emacs.** 30.1 or 29.4, on `PATH`. `emacs --version` — record which.
  A newer build works but is a deviation worth recording: the 2026-09-12 run
  used a Homebrew 30.2 development build (`636f166cfc86`). If that build was
  configured `--with-native-compilation`, `nelisp-ai.sh compile` can fail on
  ONE file with `ld: library 'emutls_w' not found` while native-compiling a
  subr trampoline. That is the host toolchain, not this repository: libgccjit
  invokes the gcc driver without Homebrew's gcc lib directory on the search
  path. Export it and re-run —
  `LIBRARY_PATH=/opt/homebrew/lib/gcc/current/gcc/$(uname -m)-apple-darwin*/*`
  — and `compile` reports pass, 117 files, 0 failed.
- **Toolchain.** `cc`, `codesign`, `shasum`. `xcode-select -p` must succeed.
- **The checkout.** `git clone` this repository and `git checkout v1.3.0`
  (or the tag's commit). Do NOT reuse a working tree someone has been
  developing in: a stale `.elc` under `lisp/` or `scripts/` makes
  `make standalone-reader` fail with `Invalid function:
  nelisp-elf--build-rel-sym`, which looks like a code defect and is not.
  If you hit it: `find lisp src scripts packages -name '*.elc' -delete`.

**Record every command's exit code and the sha256 of every binary you
measure.** A result that cannot name the artifact it came from is not a
result. `tools/nelisp-pin-binary.sh BINARY -- CMD` does this for you: it runs
`CMD` against a read-only copy with `{}` substituted, and re-checks the hash
afterwards.

## 1. Build and the host-side suite

```sh
tools/ai/nelisp-ai.sh doctor          # record the branch and which binary is in target/
tools/ai/nelisp-ai.sh compile         # expect: pass, 117 files, 0 failed
tools/ai/nelisp-ai.sh test            # the full ERT suite
```

Expected, from Linux at the same commit: **5,921 tests, 0 unexpected, 161
skipped**. On macOS the skip count will be higher — targets and primitives
this host does not have skip explicitly. That is fine. What is not fine is any
`unexpected` result, or a lower TOTAL, which would mean a file failed to load
and ran zero cases rather than skipping honestly.

Observed on macOS 26.6.2 arm64 (M1, Emacs 30.2), 2026-09-12: **5,921 tests,
5,372 expected, 0 unexpected, 549 skipped**, 202.8s. Same total as Linux, so
nothing failed to load; the extra 388 skips are `nelisp-sys-*`,
`nelisp-text-buffer-*` and `nelisp-worker-*` cases. Neither known flake fired.

Two known macOS-only flakes, both timing races rather than defects, both
recorded in project memory. If you see either, re-run that one file before
reporting it:

- `nelisp-process-actor-receives-process-state-event` — a 2.0s sentinel
  timeout racing macOS's callback delivery under load. Seen on macos 30.1 only.
- Any offload-timeout test with a budget under ~2s — macOS IPC round-trip
  alone can reach 400ms.

## 2. Mach-O acceptance

```sh
make macho-acceptance-test
```

This is the one macOS-specific gate CI already runs, so it is a cheap early
signal that the toolchain is wired up. It emits arm64 `MH_EXECUTE` and
`MH_OBJECT` and checks them against `codesign`, `clang` and `ld64`. Expect
`GATE-COUNT ... findings=0`.

## 3. The standalone binary

```sh
make standalone-reader
shasum -a 256 target/nelisp        # RECORD THIS
make standalone-reader-test        # expect GATE-COUNT checked=32 findings=0
```

`file target/nelisp` must say **`Mach-O 64-bit executable arm64`**. Check it,
because this step used to produce the wrong thing here without saying so: the
Makefile's native-host default fell through to `linux-x86_64` on every
non-Windows host, so a bare `make standalone-reader` on a Mac cross-built an
x86-64 ELF and exited 0, and the hash you recorded next belonged to a binary
that cannot run on the machine under test. Fixed 2026-09-12 — Darwin on
Apple silicon now resolves to the builder's canonical `macos-aarch64`. The
spelling `macos-arm64` that §4 and `tools/build-release-artifact.sh` use is
accepted as an alias; before the fix it aborted with
`standalone: unsupported target macos-arm64` and exit 255.

`standalone-reader-test` ends with `PASS: "(+ 40 2)" -> exit 42`. If the build
itself fails, capture the full output: a macOS-only build failure in the
linker or the Mach-O writer is exactly the class of thing this qualification
exists to find, and it is a real finding, not a setup problem.

## 4. The release artifact

This is the part CI does not do for macOS at all.

```sh
./tools/build-release-artifact.sh macos-arm64 v1.3.0
./tools/verify-standalone-tarball.sh v1.3.0 macos-arm64 --release-artifact
cd dist && shasum -a 256 --check v1.3.0-macos-arm64.tar.gz.sha256 && cd ..
```

Then unpack the tarball somewhere clean and run the binary it contains — not
the one in `target/`. An artifact that builds and checksums but does not run
after extraction is the failure mode this step is for. Record the extracted
binary's sha256 and confirm it matches what the tarball claims.

## 5. Memory: the soak, and the THP question does NOT apply here

```sh
tools/nelisp-pin-binary.sh target/nelisp -- \
    python3 tools/nelisp-standalone-soak.py --binary {} --duration 3600
```

Expect PASS: RSS growth under the 5,120 KiB ceiling for the full hour.

The harness read RSS only from `/proc/<pid>/status`, so on macOS it used to
fail before the timing loop even started — `batches=0 elapsed_seconds=0.000`
for a requested `--duration 3600`, which is not a failing soak but no soak.
Fixed 2026-09-12: Darwin reads `pti_resident_size` via libproc, and
`--diagnostic-dir` captures `vmmap -summary` in place of `smaps`. The
pass/fail metric is still resident-set growth against the same 5,120 KiB
ceiling, so the number means what it means on Linux. Read the `vmmap`
`Physical footprint` and `SWAPPED` figures too: macOS compresses memory, so a
leak can grow the compressor while RSS stays flat.

**Important context so you do not misread a failure.** On Linux this soak fails
on hosts whose `transparent_hugepage/enabled` is `[always]`, and that was
settled as huge-page granularity rather than a leak (same binary passes with
THP off, peak RSS equal to start). **macOS has no THP and no such knob**, so
that explanation is not available here. If the soak fails on macOS, treat it
as a genuine finding and report the numbers — do not reach for the THP
explanation.

## 6. The real-user-init audit

```sh
tools/nelisp-pin-binary.sh target/nelisp -- \
    bash tools/nelisp-real-init-audit.sh --binary {} --out /tmp/nelisp-audit-macos
```

The tool needs an Emacs init file to drive; without one of your own, point
`--init` at any substantial `.el` file. It never modifies the file it reads and
checks its hash before and after.

Its per-second memory columns were also `/proc`-only, so every one of them
read `NA` on macOS. Fixed 2026-09-12: `vmrss_kb` and `vmsize_kb` come from
`ps -o rss=,vsz=` (already KiB) and `vmhwm_kb` is a high-water mark the script
accumulates. The `smaps_*` columns stay `NA` — macOS has no `smaps_rollup`,
and no transparent huge pages to account for.

Expect `AUDIT_DONE`, exit 0, **no signal**. On Linux at this commit the
equivalent run reaches all 930 form boundaries in 36s with 322 `FORM_ERROR`s
(unimplemented Emacs APIs and absent files — not memory faults). Your
`FORM_ERROR` count will differ because your init differs; the number is not
the pass criterion. **The pass criterion is that it completes without a
signal.** A SIGSEGV here is the single most valuable thing this whole sheet
could find: it is precisely the failure that was open on Linux until
v1.3.0 fixed the arena's boundary reclaim, and a macOS-only variant of it
would not have been caught by anything else.

## 7. What is explicitly NOT expected to work

Do not report these as failures.

- **Native unit replacement** (`nelisp-native-unit-*`,
  `nelisp-native-callsite-*`, `reload.plan`/`reload.apply`) is **Linux x86_64
  only** in v1.3.0 and refuses elsewhere with
  `NELISP-DEV-NATIVE-UNAVAILABLE`. A clean refusal IS the correct macOS
  behaviour. Worth one check that it refuses cleanly rather than crashing:
  `tools/ai/nelisp-ai.sh repl`, then
  `(require 'nelisp-dev-reload)` and `(nelisp-dev-reload-context default-directory)`
  — expect an error naming that code, not a crash.
- Native sockets on ARM64 report the catchable `nelisp-unsupported-primitive`.
- The GUI binary hardcodes X display `:0` and is not a macOS target at all.

## 8. Reporting

Record, in the worklog (`anvil-worklog-add`), one entry containing:

- macOS version, chip, `uname -m`, Emacs version.
- Every command above with its exit code.
- The sha256 of `target/nelisp` and of the extracted release binary.
- Full ERT: total / expected / unexpected / skipped.
- Soak: pass or fail with the actual RSS figures and batch count.
- Audit: whether `AUDIT_DONE` was reached, exit code, whether any signal.
- Anything in §7 that did NOT behave as described there.

**This run ships as v1.3.1.** v1.3.0 was tagged with macOS ARM64 explicitly
deferred rather than held back for it, so nothing here blocks a release that
already went out — which also means there is no pressure to report a pass.
If every step passes, macOS ARM64 becomes a qualified target in the v1.3.1
notes, citing this run. If any step fails, report it with its numbers and
leave the deferral standing: a deferred platform is an honest release, a
platform claimed on CI smoke alone is not.
