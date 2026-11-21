# Debian handoff: macroexpansion speed and language-level development APIs

This is the requested execution brief for continuing on Debian. It is a
checkpoint, not a claim that the full suite or the 5-10x target passes.
Read the checkout's `AGENTS.md` and `AI.md`, plus the parent workspace rules.
The Windows session's missing-skill waiver does not apply automatically to
a new Debian session. Discover the installed Anvil operations; do not assume
the Windows connection's restricted tool list is the Debian tool list.

## 1. Fetch this checkpoint without disturbing another checkout

Remote: `https://github.com/zawatton/nelisp.git`.
Checkpoint branch: `perf/load-source-speed`.
The parent before this checkpoint is `527594130`; its main base is
`1d2d08a8a4c1d9ae8673807e67459f9c6e7b4a4e`.

From an existing NeLisp repository:

```sh
git fetch origin main perf/load-source-speed
checkpoint=$(git rev-parse origin/perf/load-source-speed)
repo=$(git rev-parse --show-toplevel)
parent=$(dirname "$repo")
git worktree add --detach "$parent/nelisp-macroexpand-base" "$checkpoint"
git worktree add -b perf/macroexpand-debian "$parent/nelisp-macroexpand-debian" "$checkpoint"
```

Inspect existing paths/branches before reusing them. Do not reset, stash,
force-push, or overwrite someone else's work. Keep the baseline worktree and
its complete runtime bundle unchanged; implement only in the candidate.
Record the fetched checkpoint hash in the worklog before modifying code.

The new nelisp-macrocluster-wt-20261116 Windows worktree was only a
24-file hash-checked source seed. No batch-query or Win64 proof implementation
was completed there; there is no additional patch to recover from it.

## 2. Build locally, with the actual required toolchain

The split builder supports **Linux x86_64** and this standalone build pins
the verified **GNU Emacs 31.1 bytecode dialect**. Record `uname -a`, the host
Emacs version, Git hash and binary SHA256. Do not silently substitute an
older host Emacs or use Windows `.eln`, cold images or executables.

Set `NOTES_ROOT` to the existing Notes workspace, and `EMACS` to the pinned
GNU executable. These are environment inputs, not paths to bake into code.
Before building, unset cross-target/output overrides and `NELISP_FX_DISABLE`.
The split builder rejects target/output overrides and writes `target/nelisp`
inside each isolated worktree, so this does not overwrite the other baseline.

```sh
: "${NOTES_ROOT:?Set the existing Notes workspace root}"
: "${EMACS:?Set the GNU Emacs 31.1 executable}"
test -f "$NOTES_ROOT/bin/heavy-run.sh"
unset NELISP_STANDALONE_TARGET NELISP_NATIVE_STANDALONE_TARGET
unset STANDALONE_GATE_TARGET NELISP_STANDALONE_READER_OUTPUT NELISP_FX_DISABLE

# Run once in EACH fresh baseline/candidate worktree.
(ulimit -v 8388608
 bash "$NOTES_ROOT/bin/heavy-run.sh" bash tools/ai/nelisp-standalone-build-split.sh)
sha256sum target/nelisp target/nelisp.cold target/nelisp.native-startup.el
```

Every REPL/test has the 8 GiB address-space limit. Heavy builds/tests use the
workspace's six-slot queue and 12 GiB available-memory gate; do not weaken it.
Choose only real CPUs on this server and reserve benchmark cores/siblings.
Preserve all runtime companions, including any enabled artifact-runtime
cache. The builder may emit that optional cache at the target root. Never
benchmark a copied executable without its required adjacent bundle.

Split-build success proves creation of the image, not every cold-boot path.
Run actual fresh-process restore checks before claiming cold-boot validity.
Linux native FXIR rows are normally enabled; the Windows row-disable guard
is a separate platform rule and must not be globally removed.

## 3. Verify the new language contract first

Implementation and usage: `docs/function-analysis.md`.

- `lisp/nelisp-function-info.el`: versioned representation-neutral metadata;
  private validated source parts retain body/environment identity.
- `test/nelisp-fxir-eval.el`: `nelisp-fxir-analyze` returns accepted code/IR or
  the real rejection reason/condition. Unknown source nodes stay nil.
- `tools/ai/compall-gen.el`: source/body/argument access uses that contract.
  Its boot-name snapshot now includes interpreted-function objects.

NeLisp `aref` can return a projected lexical environment with fresh pairs.
Use the canonical source-parts API, not those pairs, for compiler state.
GNU's canonical slots and NeLisp's backing forms are different adapters.
Minimal runtimes without a canonical accessor report
`canonical-source-unavailable`; do not coerce that into usable source.
Do not infer actual native execution from an object's representation.

From the candidate worktree, with a complete freshly built runtime:

```sh
export NELISP_FXIR_ROOT="$PWD" NDTW_AOT_ROOT="$PWD"
export NDTW_FXIR_BAD_RESULT=0
(ulimit -v 8388608
 bash "$NOTES_ROOT/bin/heavy-run.sh" timeout 60 "$EMACS" -Q --batch \
   -l test/standalone-function-info-check.el)
(ulimit -v 8388608
 bash "$NOTES_ROOT/bin/heavy-run.sh" timeout 60 target/nelisp \
   --load test/standalone-function-info-check.el)
(ulimit -v 8388608
 bash "$NOTES_ROOT/bin/heavy-run.sh" timeout 60 target/nelisp \
   --load test/standalone-fxir-analysis-check.el)
(ulimit -v 8388608
 bash "$NOTES_ROOT/bin/heavy-run.sh" timeout 60 target/nelisp \
   --load tools/ai/macroexp-source-fxir-probe.el)
```

Windows results: metadata 169 checks on both GNU/full NeLisp; compiler
analysis 87 checks. Re-run the same fixtures with these mutations and require
nonzero status and their explicit failure markers:

- `NDTW_FUNCTION_INFO_MUTATION=legacy-only`: 31/169 failed on each engine.
- `NDTW_FXIR_ANALYSIS_MUTATION=discard-reasons`: 9/87 failed.
- `NDTW_FXIR_BAD_RESULT=1`: the same supported input must fail its wrong
  expected-local-count check. Restore it to 0 afterwards.

Do not pipe a failing command into `tail` and use the pipeline's last exit
code as the result. Capture the command's own status and log completion.
The cyclic-alias decoder branch was not exercised: `fset` refused to create
the cycle. Do not bypass that guard to claim coverage.

The bare GNU host does not contain the genuine NeLisp FXIR compiler/prelude
helpers. Its source-FXIR probe is a known `BLOCKED-PROVIDER` result, separate
from GNU's passing metadata tests. Do not invent helpers or remove this red
criterion merely to raise progress.

## 4. Next implementation: conditional-source macroexpansion

The current source probe is deliberately fixed to **macroexp--all-forms**.
It has no batch-selection option yet. It compiled the genuine vendored GNU
definition to **185 code entries, 7 locals and 15 callee entries**. These are
not 185 functions or native rows. The probe took about three seconds and
did not require rebuilding the runtime.

1. Extend the inquiry additively to select exact source definitions, keeping
   the existing default/calibration behavior. Query
   `macroexp--expand-all`, `macroexp-macroexpand`, `macroexpand-1`,
   `macroexpand-all`, then the connected helpers reported by their actual IR.
   Analyze each once and retain exact reasons/conditions; never guess a node.
2. Source is `vendor/staged-emacs-lisp/macroexp.el`. Local macro prerequisites
   include `macroexp--accumulate` and
   `macroexp--with-extended-form-stack`; other installed macro providers
   include `pcase` and `with-demoted-errors`. Preserve dynamic declarations
   such as `macroexpand-all-environment`, `macroexp--dynvars` and
   `byte-compile-form-stack` using genuine providers. Querying is not an
   excuse to execute/install all target definitions. Save/restore any macro
   function cells changed for scoped analysis before returning.
3. Supply a genuine source provider, check both complete corpora against GNU,
   then harvest a dependency-appropriate native cluster. The existing source
   scanner does not include the vendor directory. Add explicit validated
   provenance rather than hand-written speed primitives or blanket admission.
4. Prove native execution before measuring gains. `nc_run` receives real VM
   records with negative NLOCALS; its missing-row `-3` result is a failure,
   not a bytecode fallback. `CONVERTED` alone proves selection, not execution.
   Reuse `tools/ai/nc1-native-check.sh`,
   `test/nelisp-nc-native-corpus.el` and
   `test/nelisp-fx-adopt-corpus.el`. Require checked values in off/VM/native
   modes and the existing mutant divergence control. Validate GC, dynamic
   unwind and redefinition separately. Do not fabricate pointer/Word frames.
5. Reduce generic call marshalling, temporary frames and unnecessary
   allocations across the hot graph through general compiler improvements.
   Compiling only the outer wrapper leaves recursion on the old path.

Useful existing host tests are `test/nelisp-cc-fxir-analysis-test.el`,
`test/nelisp-cc-fxir-admission-test.el`, `test/nelisp-cc-fxir-rows-test.el`,
`test/nelisp-eln-s63-all-forms-admission-test.el` and
`test/nelisp-eln-s64-macroexpand-admission-test.el` (via `make test-one FILE=...`).
Report genuine skips when pinned native artifacts/binaries are absent.
`tools/ai/fxir-analysis-query.el` diagnoses one actual FXIR row and optional
code datum; it does not generate a source inventory, and `:valid` is not
proof of native admission or execution.

## 5. Recreate Linux measurements; do not reuse Windows timings as Linux results

Delivered tools include `tools/ai/macroexp-threeway-check.sh` and
`macroexp-threeway-driver.el`. The `setup` mode prepares a fresh macroexp
source/bytecode provider using `NDTW_EMACS_BIN`, `NDTW_BASELINE_BIN` and unique
`MEXP_OUT`; use the correct complete baseline bundle. It also checks wrong
execution-kind controls. **The case runner uses macroexp-paired-win32.py and
cannot run on Debian.** The strict continuation adapter is Windows evidence
tooling, not a Linux runner.

Use the installed `nelisp-gnu-gap-bench` skill's Linux paired runner or add
an equivalent checked Linux path. It currently has no per-engine env option
or first-pass mode, and assumes SMT sibling `cpu+16`. Inspect actual sysfs
topology and fix/validate that assumption before trusting idle checks.
Do not label a two-trip-count incremental measurement as a fresh first pass.
Per-engine wrappers must preserve argument dispatch and assign unique sample
metadata IDs; GNU wrapper basenames must start with `emacs` for that runner.

Acceptance: actual checked GNU-equivalent result, seven fresh alternating
pairs, recorded CPU/sibling idle status, binary/driver/provider hashes,
8 GiB limits, a bad-result control and a basic-loop regression row. No speed
claim from instruction counts, one pair, stale native rows, or skipped tests.
Keep native/off/bytecode modes accurately labeled and inspect compiler
fallbacks. Do not leave REPLs or benchmark children running.

Game inputs need `NDTW_ROOT`: the existing newDTW checkout or an isolated
source copy. Relevant public remote is
`https://github.com/zawatton/newDTW-nelisp.git`. The tested Windows game
checkout had additional uncommitted runtime/host-kit changes; this NeLisp
checkpoint does **not** publish or overwrite those separate project files.
Record the actual game revision, dirty state and hashes of game-runner,
gamedata-state and gamedata-conditional before comparing results. If the
nelisp_runtime/nelisp-host kit is absent, report it as an external fixture
gap; core/source/FXIR work can proceed. Never touch real player saves.

## 6. Historical evidence and remaining work

Windows, original baseline vs combined AUX-link/commit-batching candidate:

| Workload | Before | After | Checked pairs |
|---|---:|---:|---:|
| state macroexpansion | 1.113618 s | 0.509634 s | 7/7 faster |
| conditional macroexpansion | 13.036092 s | 9.459680 s | 7/7 faster |
| basic bytecode loop vs AUX baseline | 113.0 ns | 116.9 ns | within 5% gate |

CPU0, 8 GiB jobs, explicit idle-check waiver. Conditional used all five
complete chronological prefix pairs plus two fresh pairs after the original
total-budget timeout; failed raw/orphan were preserved, not relabeled green.
Sixteen mutated-evidence controls failed. Compiled game load was 3.0 s
(limit 4), with nine frames identical to GNU. This is not a 5-10x result.

The Windows-only `VirtualAlloc(MEM_COMMIT)` batching does not accelerate
Linux. The AUX-link, function-contract and compiler work are cross-platform.
An earlier Windows binary also passed explicit **v4** `--cold-load-from`
restore and nine heap-image scenarios. Linux cold-v5 automatic startup is
a different route; do not call all Windows cold restore unsupported. The
latest Windows candidate has not rerun that explicit cold-image control.

Last Windows meter: function-info 2/3 (GNU FXIR provider red), Windows commits
5/6 (thread/cold criterion incomplete), native macroexpansion 2/6. Original
five source-load thresholds remain unchanged. `target/` evidence/binaries
are not Git content: a fresh clone's evidence-hash checks can fail because
the historical files are absent, not because a Linux execution failed.
Run real checks and record new evidence; never erase a red or lower a
threshold to make the meter green.

Known Windows baseline failures: frame-locals ownership, backtrace locals,
Linux `/proc/self/status` memory probe, and a separate native ABI callback
probe. No full-suite PASS was claimed. Source-injected copy-file tests and
ratchets had a historical Anvil PASS record; latest baked copy-file-focused
checks were not rerun. Debian should run the normal full validation and
record its actual failures/skips separately from Windows limitations.

Finish with the normal project validation (installed nelisp-validate skill,
including real gates/tests/fuzz), scoped diffs, negative controls, runtime
identities, updated ledgers and an Anvil worklog. Discover Debian Anvil
code-context/dependency/REPL tools before falling back. Do not enable a
shared server or bypass ABI/GC guards. Implementation defaults to an isolated
Sonnet worker under the workspace rules; do not switch models silently or
use Fable. Historical detailed records are in Windows Anvil worklogs
336, 403, 405, 408, 410, 416, 421 and 423; the API usage guide and this brief
are available directly in this commit even when that DB is not synchronized.
