# NeLisp v1.3.1

Release implementation and qualification notes, updated 2026-09-12.

A patch release whose whole content is the macOS ARM64 qualification that
v1.3.0 deferred, and the nine repairs that qualification found. No existing
target's behaviour is intentionally changed; three of the repairs do reach
Linux and Windows and are marked as such below.

v1.3.0's own deferral is left exactly as v1.3.0 states it. That release
already records macOS ARM64 as **Deferred to v1.3.1 — not a v1.3.0 release
target** (`release/v1.3.0/RELEASE.md`), and its run sheet already says
**this run ships as v1.3.1** — both decided upstream in `2fb0fd8f`,
independently of and concurrently with the hardware run below. Nothing here
rewrites that; this release is where the run sheet's result lands, which is
where it was always going to land.

`release/v1.3.0/MACOS-QUALIFICATION.md` remains the run sheet and is edited
here only to add what the hardware measured — observed ERT figures, the
`file target/nelisp` check §3 now needs, the two repaired harnesses, and the
host-toolchain trap below. Its framing is upstream's and is untouched.

**What was measured, exactly.** The run sheet was first worked through on
`2e67ca4cb4f7e4ae27df651a69dc47b3e5cfa63e`, the commit the task pinned, which
is five commits before the `v1.3.0` tag (`71ce45b9`). Everything below was
then re-measured on the tag itself, because a figure that cannot name its
artifact is not a figure and a different base is normally a different binary.

Here it is not. Rebuilt on the tag, `target/nelisp` hashes to
`a2c5d0e2f2256e2eb0ed1c7e4e7eea5473d89d3d78130691b14c61df0ecef8a4` — **byte
for byte what the earlier base produced.** Of the five commits, only two touch
code, and neither reaches the standalone binary: `eaae749b` changes
`test/nelisp-standalone-gc-test.el`, which is never linked in, and `e2451ac5`
changes `lisp/nelisp-repl-development.el`, a host-side library that is not one
of the reader's units. So every binary-level measurement here names one
artifact on both bases rather than two artifacts that happen to agree.

Host-side results were re-run rather than reasoned about, since ERT does load
the changed files: same 5,922 / 5,432 / **0 unexpected** / 490 skipped, and
`standalone-reader-test` again `checked=32 findings=0`. The release tarball's
own hash DID move (`81a57a08…` → `c24e1a0a…`), correctly — it packages `lisp/`
and `test/`, which changed — while the `bin/nelisp` inside it still hashes to
`a2c5d0e2…`.

## Changes since v1.3.0

### macOS ARM64: the standalone path/stat/directory layer

- `scripts/nelisp-standalone-build.el` implements the Darwin half of the
  portable fileio layer. `stat`, `lstat`, `rename`, `symlink`, `unlink`,
  `mkdir`, `rmdir`, `chmod`, `readlink`, `opendir`/`getdents`, `utimes` and
  `nanosleep` were deliberate `-ENOSYS` stubs — only `access` and `exit` were
  wired — so `file-attribute-size` returned a negative errno **as a size** and
  `rename-file` reported "No such file or directory" about a file that was on
  disk. The syscall ABI itself was already correct: Darwin's carry-flag error
  convention is normalised by a `csneg` in
  `nelisp-aot-compiler--emit-syscall-direct-arm64`, so only the numbers and
  the struct translation were missing.
- Syscall numbers come from the macOS SDK's `sys/syscall.h` and each was
  exercised from C against the kernel before being written: `stat64` 338,
  `lstat64` 340, `rename` 128, `readlink` 58, `unlink` 10, `symlink` 57,
  `mkdir` 136, `rmdir` 137, `chmod` 15, `utimes` 138, `getdirentries64` 344,
  `select` 93. Darwin has no `nanosleep` syscall — libc builds it on
  `__semwait_signal` — so `select(0,0,0,0,&tv)` provides it, measured at
  252.9ms for a 250ms request.
- `struct stat` is translated into the Linux x86-64 layout the layer above
  addresses, the way `nl_win_stat_from_data` already synthesises it from Win32
  data and `nl_os_stat_fixup` rewrites it on linux-aarch64. This is the part
  that fails silently if it is wrong: Darwin's `struct stat` is **also** 144
  bytes, so no size check can catch a layout slip, and Linux's `st_size`
  offset 48 is Darwin's `st_mtimespec.tv_sec` — keeping the Linux offsets
  makes a 365-byte file report its size as 1654566969. Offsets are `offsetof`
  output, not read off a header by eye. Every Darwin field is read into a
  local before the buffer is rewritten, because the two layouts overlap in
  both directions.
- `getdirentries64` needs an `off_t *position` the three-argument
  `nl_os_getdents64` contract cannot carry, so `nl_os_open_dir` owns a state
  block (fd, position, record scratch) and returns its address, exactly as the
  Windows branch does for `FindFirstFileW`. Darwin records are translated into
  Linux `dirent64` records; a translated record is always shorter than its
  source, so bounding the kernel's write by CAP bounds the output too.
- `statx` has no Darwin equivalent and remains `-ENOSYS`, which callers
  already handle by falling back to `stat`. The errno space stays Linux
  vocabulary, matching the syscall numbers the layer is addressed by.

### macOS ARM64: build and gate wiring

- `Makefile` resolves the native-host standalone target on Darwin/arm64 to the
  builder's canonical `macos-aarch64`. Every non-Windows host used to fall
  through to `linux-x86_64`, so a bare `make standalone-reader` on a Mac
  silently cross-built an **x86-64 ELF** into `target/nelisp` and exited 0 —
  and §3 of the run sheet says to run exactly that and then hash
  `target/nelisp`. Other hosts keep their previous default.
- `scripts/nelisp-standalone-build.el` accepts `macos-arm64`, `linux-arm64`
  and `windows-arm64` as aliases for its canonical `aarch64` spellings. The
  repository spelled the Apple-silicon target two ways that never met:
  `tools/build-release-artifact.sh`, the run sheet's §4 and
  `nelisp-integration-release-artifact-platforms` all say `macos-arm64`, while
  every `pcase` arm says `macos-aarch64`, so passing the release script's
  spelling aborted the build with `unsupported target macos-arm64` and exit
  255 — a failure that reads like an unported platform rather than a typo.
- **`standalone-reader-test` now exports `NELISP_STANDALONE_TARGET` like every
  other standalone recipe.** It did not, which was invisible on Linux because
  the builder's default is `linux-x86_64` anyway. On macOS it asked for
  `linux-x86_64`, found it unrunnable, printed `GATE-SKIP` and **exited 0**: a
  gate that executed zero checks reporting success, which is AI.md rule 1 in
  the one place a macOS qualification would have trusted it. Read the
  `GATE-COUNT` line, not the exit code.
- The Stage-3 rootstack smoke no longer asserts a heuristic's schedule. It
  required the mid-form collector to have fired, which depends on allocation
  debt crossing a threshold, which depends on arena size: macOS reserves
  512 MiB where the Linux bootstrap first chunk is 256 MiB, and at 200k
  iterations it never fires there. Measured: `mid-form-fired-count` 0 at 200k,
  76 at 2M, 347 at 8M, with the poison counter scaling linearly throughout
  (6.8M / 122M / 489M objects) — the collector was working and simply never
  asked to run. Lowering the allocation-debt floor at 200k did not help. Only
  `macos-aarch64` moves, to 2,000,000; every other target keeps 200000.

### Repairs that also reach Linux and Windows

- **`secure-hash` in the standalone prelude.** Its helper `cond` ended in a
  bare `"sha256sum"`, which could never work: this runtime's `call-process`
  does not search `PATH`, so the arm turned "helper not found" into the
  misleading `sha256sum exited 127`. On Linux the first probe always hit and
  hid both problems. macOS has no `sha256sum` in `/usr/bin` or `/bin` — 26.6.2
  ships it in `/sbin`, Homebrew coreutils in `/opt/homebrew/bin`, a stock
  install has only `shasum` — so every artifact command that hashes failed
  there, `compile-elisp-artifact` first. Absolute paths are now probed
  explicitly and `shasum -a 256`, which prints the same line shape, closes the
  stock-macOS case. **Windows has neither tool at any of those paths**, so
  this repair alone left `secure-hash` unconditionally erroring there too —
  found during the Windows regression check and closed by a further commit
  (`6dad1d6f`, after this tag's base) that probes `certutil.exe` (shipped at
  a fixed path since Vista/Server 2008) and parses its differently-shaped,
  locale-dependent output by finding the 64-hex-character line rather than
  by position. A second, independent bug surfaced getting there: this
  runtime's own `make-temp-file` returns a path only its internal I/O
  understands, which `certutil.exe` — a real Win32 process reading its input
  path as a command-line argument — cannot open; the input path is now
  rooted under `%TEMP%` instead. Verified on windows-x86_64: `secure-hash`
  now returns the correct digest for both a string and a file's content,
  matching `sha256sum` independently for each.
- **`nelisp-native-load--sha256-file-external` had never worked under the host
  Emacs.** It passed a filename *string* as `call-process`'s DESTINATION. The
  standalone runtime treats that as the file to write stdout to; GNU Emacs
  does not accept a string there at all, so it discarded the output and still
  returned the child's exit status — the helper "succeeded" and left a
  zero-byte file, and this fast path silently returned nil on every host while
  callers fell back to slower in-process hashing. Emacs spells it
  `(:file PATH)`. Detected by runtime capability, not `system-type`. **This
  changes behaviour on Linux and Windows.** Verified on windows-x86_64 (host
  GNU Emacs 31.1): the fast path now returns the correct SHA-256 digest for a
  real file, matching `sha256sum` independently; no zero-byte file is left
  behind.
- **`tools/nelisp-real-init-audit.sh` was sampling the wrong process.** `$!`
  is the `timeout` wrapper's pid, not the binary under audit, so every memory
  column reported the wrapper's footprint — a constant ~1.5 MiB. Measured
  2026-09-12: wrapper 1472 KiB while its child, the audited binary, was
  206704 KiB at the same instant. It now resolves the child via
  `pgrep -P "$CHILD_PID"` and prints which pid it sampled. **The Linux
  audit's memory figures change with this**, so the numbers in
  `release/v1.3.0/RELEASE.md`'s audit row are not comparable with anything
  measured after it. **On Windows, `pgrep -P` alone was not enough**: a
  stock MSYS2 install has no `pgrep` (it ships in the separate `procps-ng`
  package), so the call failed silently (`2>/dev/null`, `|| true`) and
  `MEM_PID` fell back to the wrapper's own pid — reproducing the exact bug
  this fix targets, via a different missing-dependency path, with `VERDICT`
  still reading `ok`. Closed by the same further commit (`6dad1d6f`), which
  falls back to a `ps -ef`-based lookup only when `pgrep -P` returns nothing,
  so a platform where `pgrep` already works keeps its exact prior behavior.
  Verified on windows-x86_64: `memory samples target pid` and
  `timeout wrapper pid` are now distinct, and the sampled `vmrss_kb` on a
  4-form smoke init rose from 6,016 (the wrapper's own footprint) to 23,168
  (the actual audited process).

### Memory measurement on Darwin

- `tools/nelisp-standalone-soak.py` reads RSS from libproc's
  `PROC_PIDTASKINFO` on Darwin. It read only `/proc/<pid>/status`, so on macOS
  the first sample — taken *before* the timing loop starts — raised and the run
  reported `batches=0 elapsed_seconds=0.000` for a requested
  `--duration 3600`. That is not a failing soak; it is no soak. Cross-checked
  against `ps -o rss=`: both reported 65184 KiB for the same pid at the same
  moment, so the `ps` fallback is a second source rather than a second metric.
  `--diagnostic-dir` captures `vmmap -summary` in place of `smaps`; read its
  `Physical footprint` and `SWAPPED` figures, because macOS compresses memory
  and a leak can grow the compressor while RSS stays flat. The pass/fail
  metric is still resident-set growth against the same 5,120 KiB ceiling.
- `tools/nelisp-real-init-audit.sh` fills `vmrss_kb` and `vmsize_kb` from
  `ps -o rss=,vsz=` (already KiB) and accumulates `vmhwm_kb` itself, since
  Darwin has no `VmHWM`. `vmdata_kb` and the `smaps_*` columns stay `NA`
  rather than being filled with numbers that would mean something else:
  macOS has no `smaps_rollup`, and no transparent huge pages to account for.

### Tests

- `nelisp-standalone-target-macos-access-translates-portable-number` asserted
  the whole `nl_os_syscall_path_int` form, so it pinned the *shape* of a
  two-arm `if` rather than the mapping, and adding a translation broke it with
  nothing wrong. It now names each (portable, Darwin) pair it requires across
  all three translator functions: seven pairs instead of one.
- `nelisp-standalone-target-macos-stat-uses-darwin-offsets` is new, and pins
  both halves of the `struct stat` translation — the Darwin reads and the
  Linux writes — plus `stat64` 338 against `lstat64` 340. It is the
  against-the-bug test for the silent-layout class described above.
- `test/nelisp-shadow-differential-cases.el` gains a filesystem round trip:
  write ten bytes, read the size, rename, read it again, list the directory.
  Every value is derived and none is a path, so it is stable across runs and
  runtimes. Answers `(10 10 nil t ("beta"))` in stock Emacs 30.2 and in the
  standalone alike.

## Qualification

macOS ARM64, real hardware: MacBook Air M1, macOS 26.6.2 (Darwin 25.6.0),
`uname -m` = `arm64`, 8 GiB, Emacs 30.2. Binary under measurement
`target/nelisp` sha256
`a2c5d0e2f2256e2eb0ed1c7e4e7eea5473d89d3d78130691b14c61df0ecef8a4`, Mach-O
64-bit arm64, ad-hoc signed. Every measured run went through
`tools/nelisp-pin-binary.sh`, whose before/after hash check matched in all of
them.

| Check | Result |
|---|---|
| macOS `compile` | PASS — `pass, ran 117, failed 0`, exit 0 |
| macOS full ERT | PASS — 5,922 tests, 5,432 as expected, **0 unexpected**, 490 skipped, 351s. Same total as Linux; 59 cases that had always skipped on macOS now run, all passing |
| macOS `macho-acceptance-test` | PASS — `GATE-COUNT checked=8 findings=0`, exit 0 |
| macOS `standalone-reader` | PASS — bare `make standalone-reader` produces `Mach-O 64-bit executable arm64`, exit 0 |
| macOS `standalone-reader-test` | PASS — `GATE-COUNT checked=32 findings=0`, `PASS: "(+ 40 2)" -> exit 42`, exit 0 |
| macOS release artifact | PASS — build, `verify-standalone-tarball.sh --release-artifact` (all OK), `shasum --check` OK, tarball sha256 `c24e1a0afbfce04f`; extracted `bin/nelisp` hashes to the same `a2c5d0e2f2256e2e`, `codesign -v` clean, and answers `(42 365 76)` from a clean extraction |
| macOS 1-hour soak | PASS (measured on the pre-rebase base, and the binary is byte-identical on both — same artifact, not a carried-over claim) — 1,948 batches over 3601.6s, start RSS 94,848 KiB, sampled peak RSS **equal to start** (0 KiB growth, ceiling 5,120), host at load average 2.6–5.0. `vmmap` confirms the compressor did not absorb growth either: `Physical footprint` 88.3M and peak 89.0M at both ends, while RESIDENT fell 116.6M → 78.7M and SWAPPED rose 16.4M → 44.5M |
| macOS real-init audit | PASS — 920/920 boundaries, `AUDIT_DONE 920`, exit 0, **no signal**, 54s, init hash unchanged before and after. Peak RSS 542,144 KiB (527,024 on the earlier base; the spread is run-to-run variation in an out-of-process sampler against the same binary), falling to ~145,000 KiB after collection. 322 `FORM_ERROR`s (236 `void-function`, 56 `file-missing`, 29 `void-variable`, 1 `error`) — unimplemented Emacs APIs and absent files, not memory faults |
| macOS §7 boundaries | As documented: native unit replacement refuses with `NELISP-DEV-NATIVE-UNAVAILABLE`, catchable, no crash; `nelisp-socket-listen`/`-connect`/`-send` all signal the catchable `nelisp-unsupported-primitive` |
| Version consistency | 9/9 sites say v1.3.1 |
| Linux `nelisp-ai.sh check` | PASS — VERDICT: PASS (23 gates), on this branch. Two ratcheted inventories moved and were raised with reasons: `unsafe-inventory` 879 → 946 (the Darwin `struct stat`/dirent translation is raw-memory work by construction) and `fallback-inventory` (the `sha256-file-external` probe splits into one arm per calling convention) |
| Linux `compile` / `standalone-reader-test` | PASS — `pass, ran 117, failed 0`; `GATE-COUNT checked=32 findings=0`, `PASS: "(+ 40 2)" -> exit 42` |
| Linux full ERT | PASS — 5,922 tests, 5,761 as expected, **0 unexpected**, 161 skipped, 241s. Same total as the macOS run, so the new parity case is running on both |
| Linux `secure-hash`, after the prelude repair | PASS — correct SHA-256 for both a string and a file's contents, through the documented `nelisp-ai.sh repl` path. Checked explicitly because that repair reaches Linux |
| Linux real-init audit | PASS — 930/930 boundaries, `AUDIT_DONE 930`, exit 0, **no signal**, init hash unchanged. The pid fix is visibly working: the run reports `memory samples target pid: 3880044 (timeout wrapper pid: 3880041)`. **Peak RSS 278,708 KiB, final 190,744 KiB** — against the 2,104 KiB the pre-fix harness reported for the same audit, which was the wrapper. A ~90x error, and the correction is this release's, not a re-measurement of the same thing |
| Linux 1-hour soak | THP-dependent on this desktop, not a leak — FAIL with THP `[always]`, PASS with it disabled per-process and peak RSS **equal to** start. See "The Linux soak" below. The release-runner result is blocker 2 |
| Linux binary identity | `target/nelisp` hashes to `c00d5ee5e7904645` here, versus `6c9ab049f1996446` at the v1.3.0 tag. Expected: the `secure-hash` prelude repair is compiled into the standalone binary |
| Windows `compile` | PASS — `pass, ran 117, failed 0`, exit 0. Unchanged by this section's two further repairs |
| Windows full ERT | PASS-with-a-known-exception — 5,922 tests, 5,477 as expected, **1 unexpected**, 444 skipped, 227s. The one unexpected result is `nelisp-dev-replay/timeout-kills-worker`, a pre-existing Windows-only process-reaping race already documented in that test file's neighbor (CI run 34608788600); reproduced on both a short and a long clone path, unrelated to either repair below, and present before this section's commit too |
| Windows `secure-hash`, after the further certutil/`%TEMP%` repair (`6dad1d6f`) | PASS — correct SHA-256 for both a string and a file's contents, matching `sha256sum` independently for each. Reached through a manual replay of `nelisp-ai.sh repl`'s own runtime-generation steps over a plain pipe, not that script itself — see "Windows x86_64 environment notes" below |
| Windows real-init audit, after the further `ps -ef` fallback (`6dad1d6f`) | PASS on a 4-form synthetic smoke (`--limit`-style; not a real ~900-form user init) — `AUDIT_DONE 4`, exit 0, 0 `FORM_ERROR`. The pid fix is visibly working: `memory samples target pid: 31948 (timeout wrapper pid: 31945)` — distinct pids, and `vmrss_kb` rose from 6,016 (the wrapper alone, the pre-fix reading) to 23,168 (the actual audited process) on the same smoke. **A full-scale run against a real, large init file — the shape that gave Linux 930/930 and macOS 920/920 — was not done and is still open** |
| Windows `nelisp-ai.sh check`, 1-hour soak, `standalone-reader-test`, release artifact, §7 boundaries, version consistency | **Not run.** Out of scope for the regression check that found the two repairs above; still open |
| Windows binary identity | First Windows measurement on record — no prior tag to diff against. `target/nelisp.exe` (windows-x86_64) hashes to `1ce281ec03a78d49` after both repairs in this section, `91f0ce3732969a1e` before them |
| Semver tag CI | **Not yet run** — it needs this tag |

The v1.3.0 arena boundary-reclaim SIGSEGV has **no macOS variant**: the audit
completes with no signal, and it does so with the filesystem syscalls actually
executing, which the pre-v1.3.1 binary never did — it returned `-ENOSYS`
before reaching them.

Deviations from the run sheet, recorded because they are conditions of these
numbers:

- **Emacs 30.2**, not the 30.1 or 29.4 the sheet asks for — a Homebrew
  development build, `636f166cfc86aa90d63f592fd99f3fdd9ef95ebd`,
  `aarch64-apple-darwin25.3.0`, configured `--with-native-compilation=aot`.
- **`LIBRARY_PATH` was exported** for every step. That build's native-comp
  cannot find `libemutls_w.a` when libgccjit invokes the gcc driver, so
  `compile` failed on one file with `ld: library 'emutls_w' not found` until
  Homebrew's gcc 16 lib directory was on the search path. Host toolchain, not
  this repository; no source change was involved.
- The two known macOS flakes the sheet names did not fire, so neither was
  re-run.

### The Linux soak

It fails on this desktop and that is not a v1.3.1 finding. Same host, same
workload, one variable, measured the way v1.3.0 settled the identical
question:

  THP as the host has it (`/sys/.../enabled` = `[always]`):
      FAIL at 1,554.2s, RSS 61,896 → 72,180 KiB (+10,284, ceiling 5,120),
      AnonHugePages 53,248 KiB
  THP disabled for the soak child only (`tools/nelisp-nothp.c`):
      PASS, 1,800.0s, 1,163 batches, RSS 53,260 KiB with peak **equal to**
      start — no growth at all

A leak grows the heap either way; peak == start is not a smaller leak, it is
none. The 5,120 KiB ceiling is also smaller than three 2 MiB huge pages, so a
THP-backed process crosses it on allocation granularity alone.

The v1.3.0 figures on this same desktop were FAIL at 1,135.7s, RSS
61,844 → 70,688 KiB, AnonHugePages 51,200 — the same shape, from a starting
RSS 52 KiB apart. On the release runner, which is not a `[always]` host, the
v1.3.0 tag's own pipeline passed the hour at 2,572 batches with +1,508 KiB.
The release-runner result for THIS release is blocker 2's job, not this
desktop's.

### Windows x86_64 environment notes

Real hardware, not CI: Windows 11 Pro (`10.0.26200`), MSYS2/git-bash, GNU
Emacs 31.1, GNU Make 4.4.1. `target/nelisp.exe` built with
`NELISP_STANDALONE_TARGET=windows-x86_64` explicitly set — see the first
finding below for why that is not optional on this toolchain. Three
environment-level findings, none of them regressions in this release, and
none fixed by this section's commit (out of scope for the regression check
that found them):

- **Bare `make standalone-reader` silently links the wrong binary.** The
  Makefile's target autodetection reads GNU Make's `$(OS)` variable
  (`NELISP_NATIVE_STANDALONE_TARGET ?= $(if $(filter Windows_NT,$(OS)),
  windows-x86_64,linux-x86_64)`), but MSYS2's `/usr/bin/make` — the `make`
  first on `PATH` after a stock MSYS2 install, and what `nelisp-ai.sh
  doctor` reports — does not import the `OS` environment variable at all:
  confirmed with a one-line test makefile, `$(OS)` reads empty there even
  though the same shell's `echo $OS` reads `Windows_NT`. `mingw32-make.exe`,
  also part of this same MSYS2 install, does see it correctly. The result
  is silent: no error, no warning, and a `target/nelisp` (not `.exe`) that
  `file` identifies as `ELF 64-bit LSB executable, x86-64` — a Linux binary
  that cannot run on Windows at all. AI.md documents the bare command as
  the way to build; on this toolchain it needs
  `NELISP_STANDALONE_TARGET=windows-x86_64` said explicitly, every time.
- **`tools/ai/nelisp-ai.sh repl`'s `mkfifo`-based input relay does not
  deliver input to a native (non-MSYS) Windows PE binary.** Piped input
  through the documented `repl` command evaluates nothing: `target/nelisp
  .exe --repl` prints its first prompt and exits, as if it had received
  EOF immediately. A plain OS pipe directly into `--repl` (bypassing the
  script's fifo relay) evaluates forms correctly, which is how both
  `secure-hash` checks in this section were actually run — by replaying
  `cmd_repl`'s own runtime-generation steps and piping the bootstrap plus
  the check forms straight into `--repl`. AI.md already flags this
  script's source-reload workflow as "verified on Linux; other launcher
  platforms need their own verification."
- **A clone path near Windows's ~260-character `MAX_PATH` breaks one T91
  test, and it is not a code defect.** `nelisp-t91-independent-python-
  oracle-corpus` failed with `Opening output file: No such file or
  directory` against a `target/<tmp>/units/windows-x86_64-arena-.../*.unit`
  path roughly 260 characters long, on a deeply-nested session-scratchpad
  clone. Re-run from a short clone path (`C:\Users\<user>\nl131w`) with no
  other change, the same test passes in 57s. Recorded so a future run
  against a long path does not get mistaken for a regression.

## Remaining release qualification

1. **Linux and Windows regression — CLOSED for Linux, partially closed for
   Windows.**
   Everything blocker 1 named has now been run on Linux and is in the table
   above: `nelisp-ai.sh check` (23 gates), the full ERT suite (5,922 tests, 0
   unexpected), the real-init audit (930/930, no signal), the 1-hour soak
   (THP-dependent on the measuring desktop, and shown so by a one-variable
   control), and `secure-hash` after the prelude repair. The two ratcheted
   inventories the Darwin work moves were raised with reasons rather than
   regenerated.

   **Windows is now measured, not unmeasured, but not yet closed.** A
   regression check on real Windows hardware (see "Windows x86_64
   environment notes" and the Windows rows in the table above) found that
   two of the three repairs blocker 1 named as reaching Windows did not, in
   fact, work there: `secure-hash` errored unconditionally (no Windows
   probe path existed at all) and the real-init-audit pid fix silently
   no-op'd (its `pgrep` dependency is absent from a stock MSYS2 install).
   Both are now fixed (`6dad1d6f`) and verified — `secure-hash` against an
   independent `sha256sum` digest, the pid fix against distinct
   wrapper/target pids and a plausible `vmrss_kb`. The third repair,
   `nelisp-native-load--sha256-file-external`'s `(:file PATH)` fix, was
   confirmed correct on Windows as originally shipped, no further change
   needed. What full Linux/macOS parity would still need on Windows and has
   not been run: `nelisp-ai.sh check`, the 1-hour soak, and a full-scale
   real-init audit against a real, large init file rather than the 4-form
   smoke used to verify the pid fix. Also open: `make standalone-reader`
   silently building the wrong target unless `NELISP_STANDALONE_TARGET` is
   set explicitly (a toolchain gap, not fixed here), and
   `nelisp-ai.sh repl`'s fifo relay not working against a native Windows
   binary. Windows is also still covered by the tag CI in blocker 2, which
   runs both Windows lanes. The original text of this blocker follows,
   unedited, because it is what was true when the hardware run was written
   and it named the work correctly:

   > None of this was run on Linux: the qualification host has no Linux. v1.3.0's
   Linux blockers were closed upstream by run 34662576736, which drove the
   semver release pipeline through `workflow_dispatch` — the same jobs a tag
   push runs, without creating a tag — and that is the mechanism to reuse
   here. It does not carry over on its own: that run predates every change
   in this release. Three of them reach beyond macOS and must be re-measured
   there before this tag ships —
   the `(:file PATH)` repair in `lisp/nelisp-native-load.el`, which makes a
   fast path run that had silently returned nil on every host; the audit's pid
   fix, which changes what the Linux audit's memory columns report; and the
   new parity corpus case, which runs everywhere. `nelisp-ai.sh check`, the
   full ERT suite, the 1-hour soak and the real-init audit all need a Linux
   result. Everything else was scoped to Darwin and verified not to move other
   hosts (`make -n` on the Makefile default, and the Stage-3 iteration count
   is target-conditional), but "scoped by construction" is not a measurement.
2. **Semver tag CI on `linux-x86_64` and `linux-aarch64` — still open.** It
   needs a tag.

macOS ARM64 becomes a qualified target of **this** release when blocker 1
closes. Every step of the run sheet passes on real Apple-silicon hardware, and
the numbers are in the table above; what is missing is not a macOS result but
the proof that getting it did not cost anything on Linux. CI's macOS smoke
lanes being green is a different and much weaker claim, and this release does
not lean on it.

The soak deserves one note for whoever runs it next. RSS alone would not have
settled it: resident memory FELL over the hour while swapped-out memory rose by
about the same amount, which read on its own is either reassuring or alarming
depending on which column you look at. `Physical footprint`, identical at both
ends, is what makes it a flat hour. Read that column.
