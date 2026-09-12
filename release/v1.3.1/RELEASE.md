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
  stock-macOS case.
- **`nelisp-native-load--sha256-file-external` had never worked under the host
  Emacs.** It passed a filename *string* as `call-process`'s DESTINATION. The
  standalone runtime treats that as the file to write stdout to; GNU Emacs
  does not accept a string there at all, so it discarded the output and still
  returned the child's exit status — the helper "succeeded" and left a
  zero-byte file, and this fast path silently returned nil on every host while
  callers fell back to slower in-process hashing. Emacs spells it
  `(:file PATH)`. Detected by runtime capability, not `system-type`. **This
  changes behaviour on Linux and Windows**, in the direction of the path
  finally running, and has not been observed there.
- **`tools/nelisp-real-init-audit.sh` was sampling the wrong process.** `$!`
  is the `timeout` wrapper's pid, not the binary under audit, so every memory
  column reported the wrapper's footprint — a constant ~1.5 MiB. Measured
  2026-09-12: wrapper 1472 KiB while its child, the audited binary, was
  206704 KiB at the same instant. It now resolves the child and prints which
  pid it sampled. **The Linux audit's memory figures change with this**, so
  the numbers in `release/v1.3.0/RELEASE.md`'s audit row are not comparable
  with anything measured after it.

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
| Windows `(:file PATH)` repair | PASS on real MSYS2 hardware — the helper returns the correct SHA-256 and leaves no zero-byte file. This is the repair that had silently returned nil on every host |
| Windows `secure-hash` after the prelude repair | **FAIL on real MSYS2 hardware, now repaired** — `no sha256 helper: looked for sha256sum in /usr/bin /bin /sbin /opt/homebrew/bin /usr/local/bin ...` while `sha256sum.exe` sat on PATH. The v1.3.1 repair had added macOS paths to an absolute-path list; Windows has none of those directories. The real defect was next to it: the prelude's `executable-find`, which does search PATH, never tried an executable suffix, so it was blind on windows-nt for every caller. See "The Windows repairs" below |
| Windows real-init audit pid fix | **Ineffective on real MSYS2 hardware, now repaired** — the script ran to completion but reported the wrapper's footprint again, because stock MSYS2 ships no procps-ng and the `pgrep -P` the fix uses failed silently. Now falls back to `ps -ef`, whose PID/PPID columns are the same on GNU/Linux, macOS and MSYS2 |
| Windows bare `make standalone-reader` | **Built a Linux ELF on a Windows host, now repaired** — MSYS2's make does not carry `OS=Windows_NT` through, so the `$(OS)` test chose `linux-x86_64`. Detected from `uname -s` now, the same way the Darwin case already was. CI never saw this: `.github/workflows/ci.yml` passes `NELISP_STANDALONE_TARGET=windows-x86_64` explicitly |
| Windows full ERT | 5,922 tests, 1 unexpected — `nelisp-dev-replay/timeout-kills-worker`, since fixed on `main` (`d31978df8`) and green on both Windows CI lanes in run 34687396917. Not a v1.3.1 repair; a cleanup that could not survive a platform refusing to remove the worker's working directory |
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

### The Windows repairs

Windows was verified on real MSYS2 hardware after this release's macOS work
landed, because three of the nine repairs reach beyond Darwin. Two of the
three did not work there, and the run also found a build defect that predates
this release. None of this was visible from CI, and two of the three could not
have been: they are properties of a developer's own machine, not of a runner.

**`secure-hash` could not find a hasher.** The v1.3.1 repair replaced a bare
`"sha256sum"` -- which could never work, since the runtime's `call-process`
hands the name straight to execve -- with a list of absolute paths. That list
was drawn from the macOS host it was written on. Windows has no `/usr/bin`,
so the repair moved the failure rather than fixing it.

The fix is not more paths. The prelude already has an `executable-find` that
splits PATH itself, and `secure-hash` simply was not using it. That function
turned out to have its own windows-nt defect: it probed exactly
`DIR/COMMAND`, never `DIR/COMMAND.exe`, so it answered nil for every program
that was installed and on PATH. Every caller was affected; `secure-hash` is
only where it surfaced. `executable-find` now sweeps
`nelisp--exec-suffixes' -- `(".exe" ".com" "")` on windows-nt, `("")`
everywhere else, so the POSIX probe stays one `file-exists-p` per PATH entry
-- and `secure-hash` asks it first, keeping the absolute list as the fallback
for a run with no usable PATH.

Verified on GNU/Linux, where the same probe can be driven both ways:
`test/nelisp-prelude-executable-find-test.el` plants a file that carries the
suffix and nothing that does not, then runs the real prelude function with
`system-type` bound each way. Red before the change, green after, with the
POSIX case passing both ways -- which is the part that shows the sweep did
not change behaviour where it already worked. In the rebuilt standalone
binary, a hasher reachable only through PATH (a shim in a temporary directory
that appears in no absolute-path entry) is the one `secure-hash` uses, which
is the Windows situation exactly.

**The audit's pid fix was inert.** It resolves the child of the `timeout`
wrapper with `pgrep -P`, and stock MSYS2 ships no procps-ng. The substitution
failed silently and every memory column went back to reporting the wrapper --
on the one platform where nobody would catch it from the numbers alone. It
now falls back to `ps -ef`, whose PID and PPID sit in columns 2 and 3 on
GNU/Linux, macOS and MSYS2 alike. Both paths were measured against the same
wrapper and agree; with `pgrep` forced to fail, the fallback still resolves
the child.

**A bare `make standalone-reader` built a Linux ELF on Windows.** The host
target came from `$(OS)`, and MSYS2's make, started from git-bash, does not
carry `OS=Windows_NT` through -- so `$(OS)` is empty and the fallback chose
`linux-x86_64`. The Darwin case directly above it already used `uname -s`;
Windows now does too. Four host shapes were checked with a stub `uname`:
MSYS/MINGW answer `windows-x86_64`, Linux and Darwin/arm64 are unchanged.
CI never saw this because `.github/workflows/ci.yml` passes
`NELISP_STANDALONE_TARGET=windows-x86_64` explicitly -- the defect only
existed for a person typing the command.

Three findings the run made that are not defects in this tree, recorded so
the next Windows run does not spend time on them: a clone under a deep path
fails `nelisp-t91-independent-python-oracle-corpus` on Windows' 260-character
path limit (it passes from a short path); `nelisp-ai.sh repl`'s mkfifo input
relay does not reach a native PE binary, so a plain pipe has to stand in; and
the THP explanation for a failing soak is a property of a Linux `[always]`
host and does not transfer.

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

## Remaining release qualification

1. **Linux and Windows regression — CLOSED for Linux; Windows measured,
   three defects found and repaired, awaiting its own CI.** Windows is no
   longer unmeasured: the run happened on real MSYS2 hardware and found that
   two of this release's own three cross-platform repairs did not work there,
   plus a pre-existing build-target misdetection. All three are repaired and
   the repairs are in the table above; what is still missing is a Windows CI
   result for the repairs themselves. The measurement was worth more than the
   release note it corrects — "scoped by construction" was exactly the phrase
   the original blocker text warned about, and it was wrong twice.
   Everything blocker 1 named has now been run on Linux and is in the table
   above: `nelisp-ai.sh check` (23 gates), the full ERT suite (5,922 tests, 0
   unexpected), the real-init audit (930/930, no signal), the 1-hour soak
   (THP-dependent on the measuring desktop, and shown so by a one-variable
   control), and `secure-hash` after the prelude repair. The two ratcheted
   inventories the Darwin work moves were raised with reasons rather than
   regenerated. **Windows has since been measured on hardware** — see the
   Windows rows in the table above and "The Windows repairs" below. The original text of this blocker
   follows, unedited, because it is what was true when the hardware run was
   written and it named the work correctly:

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
