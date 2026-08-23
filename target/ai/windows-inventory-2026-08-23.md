# Windows gate-battery inventory — 2026-08-23

## Scope

- Qualified commit: `eb4eb81f3cb555ef05935098fc1b8b2501e3972b`
- Branch: `feat/windows-gate-inventory`
- Exercised shell: `git-bash` (`MINGW64_NT-10.0-26200`, Git for Windows bash 5.2.26)
- Windows-side tools inherited from MSYS2: GNU Make 4.4.1 and GNU Emacs 30.2
- WSL2 Debian was present but was not used for gate execution. The first orientation probe resolved the Windows `bash.exe` WSL shim; it was discarded before `doctor` and all gate runs.
- Commands were run in this order: `doctor`, `check`, `standalone`, `smokes`, `native-artifact`, `perf`, `selfhost`, `extras`, `test`, `bench-aot-tco`, `macho-acceptance-test`. `verify` was run after every tier and after both uncovered commands.
- The separate Stage D v3.0 PowerShell path was not touched.

## Environment inventory

```text
uname: MINGW64_NT-10.0-26200 ThinkPad-E14-Gen5 3.4.10-87d57229.x86_64 2024-02-14 20:17 UTC x86_64 Msys
bash: GNU bash 5.2.26
emacs: GNU Emacs 30.2
make: GNU Make 4.4.1
timeout: GNU coreutils 8.32
sha256sum: /usr/bin/sha256sum
shasum: /usr/bin/core_perl/shasum
pgrep: missing
mkfifo: /usr/bin/mkfifo
cc: /c/msys64/mingw64/bin/cc
objcopy: /c/msys64/usr/bin/objcopy
ulimit -v: unlimited
```

`pgrep` was not reached: `nelisp-nelix-native-hot-gate` failed its earlier `target/nelisp` executable check. The three `ulimit -v` reader smokes did not reach their rc=88 assertions because the generated Linux ELF was not runnable on Windows. GNU `timeout`, `sha256sum`, and `shasum` resolved successfully. No failure was attributed to a missing or incompatible POSIX utility.

## Doctor output (verbatim)

```text
repo:        /c/Users/kuroz/Cowork/Notes/dev/.worktrees/nelisp-windows-gate-inventory
branch:      feat/windows-gate-inventory
head:        eb4eb81f
worktrees:   21
emacs:       GNU Emacs 30.2
make:        GNU Make 4.4.1
nelisp:      (none in target/)
gate dir:    target/gates (0 report(s))
```

## Result summary

The executed inventory produced 46 result rows: 23 PASS, 16 FAIL, and 7 SKIP. Thirteen optional `gates.expected` entries were outside the runbook's exact command battery and were not attempted; they are included below as explicit SKIP rows so every expected gate has a row. With those optional rows included, the table totals 59 rows: 23 PASS, 16 FAIL, and 20 SKIP.

Failure classification: 7 `logic-gap`, 9 `expected-skip` (the gate failed but should have reasoned-skipped on Windows), and 0 `posix-tool`. The seven gates that already emitted a reasoned SKIP are also categorized `expected-skip`.

The category `n/a` is used for PASS rows because §6 defines only failure/skip categories; it is deliberately non-blank. Optional gates not invoked by the exact battery are categorized `expected-skip` and explicitly say they were not attempted.

## Findings

| gate | tier | shell | result | category | evidence | suspect tool |
|---|---|---|---|---|---|---|
| compile | fast-lane | git-bash | PASS | n/a | `target/gates/compile.json`: ran 101, failed 0 | — |
| ert-full | binary-tier (`test`) | git-bash | PASS | n/a | `target/gates/ert-full.json`: ran 4785, failed 0, skipped 357; elapsed 193s | — |
| parens-check | fast-lane | git-bash | PASS | n/a | ran 431, failed 0 | — |
| generated-source-parse | fast-lane | git-bash | PASS | n/a | ran 5, failed 0 | — |
| unsafe-inventory | fast-lane | git-bash | PASS | n/a | ran 254, failed 0 | — |
| ns-inventory | fast-lane | git-bash | FAIL | logic-gap | 19 `ns-collision-divergent` entries were absent from `pinned-collision`; `windows-inventory-check.log` | — |
| reader-surface-audit | fast-lane | git-bash | PASS | n/a | ran 190, failed 0 | — |
| pkg-graph | fast-lane | git-bash | PASS | n/a | ran 35, failed 0 | — |
| pkg-load-lists | fast-lane | git-bash | PASS | n/a | ran 41, failed 0 | — |
| fallback-inventory | fast-lane | git-bash | PASS | n/a | ran 280, failed 0 | — |
| bootstrap-contract | fast-lane | git-bash | PASS | n/a | ran 8, failed 0 | — |
| doc-claims | fast-lane | git-bash | PASS | n/a | ran 20, failed 0 | — |
| parity-coverage | fast-lane | git-bash | PASS | n/a | ran 424, failed 0 | — |
| substrate-presence-corpus-check | fast-lane | git-bash | PASS | n/a | ran 1, failed 0 | — |
| ert-focus | optional/not attempted | git-bash | SKIP | expected-skip | Optional gate; no file selection was part of the exact battery | — |
| ns | optional/not attempted | git-bash | SKIP | expected-skip | Optional ad-hoc namespace gate; `check` exercised `ns-recipes` instead | — |
| ns-recipes | fast-lane | git-bash | PASS | n/a | ran 5, failed 0 | — |
| recipe-stdio-service | optional/not attempted | git-bash | SKIP | expected-skip | Optional recipe tier was outside the exact battery | — |
| recipe-batch-data | optional/not attempted | git-bash | SKIP | expected-skip | Optional recipe tier was outside the exact battery | — |
| recipe-native-hotpath | optional/not attempted | git-bash | SKIP | expected-skip | Optional recipe tier was outside the exact battery | — |
| recipe-native-hotpath-exec | optional/not attempted | git-bash | SKIP | expected-skip | Optional recipe tier was outside the exact battery | — |
| recipe-checked-resources | optional/not attempted | git-bash | SKIP | expected-skip | Optional recipe tier was outside the exact battery | — |
| bench-borrow-check | optional/not attempted | git-bash | SKIP | expected-skip | Optional measurement was outside the exact battery | — |
| bench-service-borrow | optional/not attempted | git-bash | SKIP | expected-skip | Optional measurement was outside the exact battery | — |
| runtime-probe | optional/not attempted | git-bash | SKIP | expected-skip | Optional probe was outside the exact battery | — |
| wasm-smoke | optional/not attempted | git-bash | SKIP | expected-skip | Optional node-dependent gate was outside the exact battery | — |
| standalone-reader-test | binary-tier (`standalone`) | git-bash | SKIP | expected-skip | `GATE-SKIP target linux-x86_64 cannot run on host "x86_64-w64-mingw32"` | — |
| nelisp-native-artifact-gate | binary-tier | git-bash | SKIP | expected-skip | `GATE-SKIP target linux-x86_64 cannot run on host MINGW64_NT...` | — |
| nelisp-performance-gate | binary-tier | git-bash | SKIP | expected-skip | `GATE-SKIP target linux-x86_64 cannot run on host MINGW64_NT...` | — |
| standalone-selfhost-test | binary-tier | git-bash | SKIP | expected-skip | Linux x86_64 ELF self-host smoke detected the unrunnable target | — |
| binary-size-ratchet | binary-tier (`standalone`) | git-bash | PASS | n/a | 7,256,768 bytes; ceiling 7,385,191 | — |
| emacs-parity | binary-tier (`standalone`) | git-bash | FAIL | logic-gap | Generated `target/emacs-parity.el` lacked its opening wrapper and ended with unmatched `)))`; Emacs reported `invalid-read-syntax (")" 1295 1)` | — |
| prelude-toplevel-check | fast-lane | git-bash | PASS | n/a | ran 749, failed 0 | — |
| partial-inventory | fast-lane | git-bash | PASS | n/a | ran 50, failed 0 | — |
| gate-selfcheck | fast-lane | git-bash | FAIL | logic-gap | `emacs-parity checked=652` was below the pinned lower bound 1000 | — |
| ns-gate | fast-lane | git-bash | FAIL | logic-gap | ran 2051; 40 findings, including 20 changed divergent-collision fingerprints and 20 stale accepted entries | — |
| nl-check-gate | fast-lane | git-bash | PASS | n/a | ran 3, failed 0 | — |
| fallback-inventory-selftest | fast-lane | git-bash | PASS | n/a | ran 4, failed 0 | — |
| wasm-dtw-skeleton-smoke | fast-lane | git-bash | PASS | n/a | ran 1, failed 0 | — |
| nelisp-runtime-image-cache-gate | binary-tier (`extras`) | git-bash | FAIL | logic-gap | Build produced `target/nelisp`, then shell `-x` handling reported `missing-nelisp`; ran 0 | — |
| nelisp-source-command-substrate-gate | binary-tier (`extras`) | git-bash | FAIL | logic-gap | Git Bash `mktemp` returned `/tmp/...`; Windows Emacs mapped it to nonexistent `c:/tmp/...` and raised `file-missing` | — |
| standalone-chunk-growth-test | binary-tier (`extras`) | git-bash | FAIL | expected-skip | Attempted to execute generated Linux ELF and got `Exec format error`; should use the target/host reasoned-skip predicate | — |
| standalone-parallel-compile-test | binary-tier (`extras`) | git-bash | FAIL | expected-skip | Linux-only fork(2), mmap, ELF execution path exited 127 while building the reader; should emit a reasoned skip on Windows | — |
| standalone-selfhost-mt-test | binary-tier (`extras`) | git-bash | FAIL | expected-skip | Linux clone(2)/mmap/ELF self-host path exited 127 while building the reader; should emit a reasoned skip on Windows | — |
| standalone-eval-test | binary-tier (`extras`) | git-bash | SKIP | expected-skip | `GATE-SKIP target linux-x86_64 cannot run on host "x86_64-w64-mingw32"` | — |
| wasm-runtime-image-smoke | binary-tier (`extras`) | git-bash | PASS | n/a | ran 2, failed 0; wasm image validated and returned 3 | — |
| standalone-reader-smokes | binary-tier (`smokes`) | git-bash | FAIL | logic-gap | Aggregate: ran 34, findings 32. MSYS2 `make` wrote logs to `C:\msys64\tmp`, while Git Bash `tail /tmp/...` looked elsewhere. Underlying logs show 32 Linux-ELF `Exec format error`/permission failures and 2 reasoned skips; see `windows-inventory-smokes-msys-tmp/` | — |
| nelisp-nelix-native-hot-gate | binary-tier (`extras`) | git-bash | FAIL | expected-skip | Failed early at `missing-nelisp` (`-x`/Linux target) before the absent-`../nelix` or `pgrep` path; should emit a reasoned skip for an unrunnable target | — |
| substrate-parity-smoke | binary-tier (`extras`) | git-bash | SKIP | expected-skip | Host/target predicate emitted `GATE-SKIP` | — |
| nl-condition-standalone-smoke | binary-tier (`extras`) | git-bash | FAIL | expected-skip | Generated Linux ELF returned `Exec format error` (126); should emit a reasoned skip | — |
| nl-safe-standalone-smoke | binary-tier (`extras`) | git-bash | FAIL | expected-skip | Generated Linux ELF returned `Exec format error` (126); should emit a reasoned skip | — |
| nl-resource-standalone-smoke | binary-tier (`extras`) | git-bash | FAIL | expected-skip | Generated Linux ELF returned `Exec format error` (126); should emit a reasoned skip | — |
| standalone-reader-buffer-smoke | binary-tier (`extras`) | git-bash | FAIL | expected-skip | Generated Linux ELF returned `Exec format error` (126); should emit a reasoned skip | — |
| nl-actor-standalone-smoke | binary-tier (`extras`) | git-bash | FAIL | expected-skip | Generated Linux ELF returned `Exec format error` (126); should emit a reasoned skip | — |
| nl-ns-reader-standalone-smoke | optional/not attempted | git-bash | SKIP | expected-skip | Optional gate was outside the exact battery | — |
| substrate-presence-sweep | optional/not attempted | git-bash | SKIP | expected-skip | Optional 15–17 minute presence tier was outside the exact battery | — |
| gate-mutation | fast-lane | git-bash | PASS | n/a | ran 22, failed 0; two injected `standalone-reader-test` cases reasoned-skipped for the unrunnable Linux target | — |
| bench-aot-tco | uncovered | git-bash | PASS | n/a | Host informational lane ran: input 2,000,000, repeats 3, ratio 0.965x; native lane correctly not claimed | — |
| macho-acceptance-test | uncovered | git-bash | SKIP | expected-skip | `GATE-SKIP macho acceptance requires macOS (got MINGW64_NT...)` | — |

## Classified failure details

### Logic gaps

1. `ns-inventory` and `ns-gate`: current source fingerprints and pinned namespace baselines disagree. The failures do not involve a POSIX utility and reproduce inside Emacs batch scanning.
2. `gate-selfcheck`: the live `emacs-parity` corpus count is 652 but the lower bound is 1000. This is a stale or incorrect ratchet, not a shell-tool failure.
3. `emacs-parity`: the Make recipe's generated file lost the prepended opening wrapper in this mixed Git Bash/MSYS2/Windows Emacs environment, leaving a trailing unmatched `)))`.
4. `nelisp-runtime-image-cache-gate`: a generated Linux target exists but the script treats shell `-x` as binary identity/runnability and reports it missing on Windows.
5. `nelisp-source-command-substrate-gate`: a Git Bash `/tmp` path is passed unconverted to native Windows Emacs, which interprets it as `C:/tmp`.
6. `standalone-reader-smokes`: the aggregate recipe mixes the MSYS2 `make` temp namespace (`C:\msys64\tmp`) with Git Bash `/tmp`; it therefore cannot read or remove its own per-smoke logs. Independently, the individual smokes mostly lack host/target skip guards.

### Gates that should emit a reasoned skip on Windows

The following failed by trying to build or execute a Linux-only ELF/syscall path: `standalone-chunk-growth-test`, `standalone-parallel-compile-test`, `standalone-selfhost-mt-test`, `nelisp-nelix-native-hot-gate`, `nl-condition-standalone-smoke`, `nl-safe-standalone-smoke`, `nl-resource-standalone-smoke`, `standalone-reader-buffer-smoke`, and `nl-actor-standalone-smoke`.

The following already emitted correct reasoned skips: `standalone-reader-test`, `nelisp-native-artifact-gate`, `nelisp-performance-gate`, `standalone-selfhost-test`, `standalone-eval-test`, `substrate-parity-smoke`, and `macho-acceptance-test`.

### POSIX-tool assessment

No gate was classified `posix-tool`:

- GNU `timeout` 8.32 resolved.
- Both `sha256sum` and `shasum` resolved; no hash invocation failed.
- `pgrep` was absent, but the only relevant gate failed before reaching that branch.
- `mkfifo` resolved and no checked gate called it.
- `ulimit -v` was accepted by Git Bash but reported `unlimited`; its enforcement could not be measured because the Linux target failed earlier. This remains unmeasured, not a cause assigned to a red gate.

## Completeness check

- `gates.expected` entries: 57
- Required entries with no report: 0
- Expected-gate rows in this file: 57 (including 13 explicit optional/not-attempted rows)
- Uncovered-command rows: 2
- Rows with a blank category: 0

Full machine-local logs are under `target/ai/windows-inventory-*.log`; recovered per-smoke logs are under `target/ai/windows-inventory-smokes-msys-tmp/`.
