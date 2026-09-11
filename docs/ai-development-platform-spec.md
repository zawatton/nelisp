# NeLisp AI Development Platform Specification

Status: **PROPOSED — implementation and acceptance verification required**  
Version: 0.2 (implementation design; not an implementation claim)  
Date: 2026-09-11

## 1. Purpose and interpretation

Make NeLisp easy for an unfamiliar developer or AI agent to discover, change,
debug, and verify, with a persistent REPL as the normal development interface.
The intended improvement is fewer unsuccessful edits, less context gathering,
shorter feedback cycles, and reliable evidence that a change works.

This document specifies future behavior. It does not certify the completion of
features discussed in previous sessions. Implementation MUST start by inspecting
the current repository and reusing existing facilities.

MUST denotes an acceptance requirement. SHOULD denotes a default that may be
deferred with a documented reason. Proposed command names below are illustrative:
map them to existing command conventions before adding public interfaces.

NeLisp MUST NOT claim higher overall development efficiency than other languages
without comparative measurements. Initial target workloads are Emacs integration,
business automation, and stateful interactive tools. Results for these workloads
must not be generalized to all software development.

## 2. Scope and design constraints

The implementation MUST preserve the project's Elisp compatibility goals and
existing build, test, and verification contracts. No new language syntax or
mandatory static type system is required.

The platform consists of a shared development backend, REPL functions, a scriptable
CLI, and optionally an MCP adapter. CLI and REPL MUST use the same operations and
result schemas. MCP MUST NOT be required to develop or debug NeLisp.

Reuse existing REPL diagnostics, reload facilities, test runners, and verification
scripts. Do not build a second independent gate system. Do not expand the native
runtime merely to implement tooling that can live in Elisp.

Out of scope for the first release:

- Arbitrary heap rollback or serialization of every live runtime object.
- Unrestricted replacement of native GC internals while collection is active.
- Complete static analysis of arbitrary dynamic Elisp and macros.
- Automatic replay of external side effects.
- A claim that AI models inherently prefer NeLisp over another language.

## 3. Phase 0: capability inventory and baseline

Before implementation, produce a concise capability map referencing actual
commands, public functions, source files, and tests for:

- Failure capture/retry, function source identity, session reproduction.
- Native rebuild/reload, publication boundaries, state preservation.
- GC/allocation metrics and available retention diagnostics.
- Compiler diagnostics, source maps, dependency information, test selection.
- Existing CLI, REPL bootstrap, and machine-readable output.

Classify each capability as verified, partial, absent, or unverified, per execution
target. A previous progress report is not verification evidence. Record current
source and artifact identities and unresolved runtime or memory regressions.

Choose one existing failing-call fixture as the end-to-end baseline. It MUST
exercise inspection, a source repair, reload, explicit retry, and an independently
checked result. Record a cold-start run and a warm-REPL run separately.

## 4. Shared operation protocol (DEV-001)

### 4.1 Public operation surface

The following describes capabilities, not a requirement for a new executable:

| Proposed operation | Required result |
| --- | --- |
| `nelisp dev capabilities --json` | Protocol versions, target support, available operations |
| `nelisp dev describe SYMBOL --json` | API contract, examples, source and loaded identity |
| `nelisp dev check FILE --json` | Static diagnostics and explicit analysis limitations |
| `nelisp dev diagnose FAILURE-ID --json` | Failure details, source precision, call chain |
| `nelisp dev impact SYMBOL --json` | Affected definitions, artifacts, tests, uncertainty |
| `nelisp dev test --affected --json` | Selection explanation and authoritative test outcome |
| `nelisp dev reload plan ... --json` | Candidate rebuild/reload plan and preconditions |
| `nelisp dev reload apply PLAN-ID --json` | Published identities and state of each component |
| `nelisp dev session export ... --json` | Reproduction manifest and replay script location |

Existing equivalent operations SHOULD be wrapped rather than renamed. Publish a
mapping from these capabilities to the chosen CLI and REPL entry points.

Read-only operations MUST NOT invoke application functions, execute arbitrary
macros, rebuild artifacts, or change the application state. Tool cache updates
must be isolated from application state and documented.

### 4.2 Result envelope

Each response MUST include:

- `schema_version`, `operation`, `request_id`.
- `status`: `ok`, `failed`, `inconclusive`, `unsupported`, or `cancelled`.
- `identity`: execution target, source revision/dirty-content identity, runtime
  artifact identity, and session ID/generation where applicable.
- `summary`, `diagnostics`, `data`, and `limitations`.
- `next_cursor` when more results are available; `null` otherwise.

Unknown values MUST be `null` with an explanation; do not substitute a successful
default. `status: ok` means the requested operation completed, not that unexamined
code is correct. A check with material unresolved coverage is `inconclusive`.

Example result envelope:

```json
{
  "schema_version": "1",
  "operation": "check",
  "request_id": "req-42",
  "status": "failed",
  "identity": {
    "target": "native",
    "source_revision": null,
    "source_content_hash": "sha256:EXAMPLE",
    "runtime_artifact_hash": null,
    "session_id": null,
    "generation": null
  },
  "summary": {"errors": 1, "warnings": 0},
  "diagnostics": [{
    "code": "NELISP-CHECK-ARITY",
    "severity": "error",
    "phase": "check",
    "message": "Call provides one argument; two are required.",
    "source": {"path": "src/example.el", "line": 8, "column": 3,
               "precision": "expression"},
    "expected": {"minimum_arguments": 2},
    "actual": {"arguments": 1}
  }],
  "data": {},
  "limitations": [],
  "next_cursor": null
}
```

Paths MUST be project-relative where possible. Source positions are one-based
Unicode character positions; include byte offsets separately when required by
native mappings. Document end-position conventions in the schema.

For the new frontend, define exit codes as 0 = ok, 1 = failed, 2 = invalid request,
3 = inconclusive, 4 = unsupported, and 5 = cancelled. Preserve existing command
exit-code contracts through adapters. An unexpected tool crash is a nonzero
internal error, never an empty success.

Machine mode MUST write exactly one JSON response to stdout; progress and human
logs go to stderr. Large results MUST support limits, paging, and detail handles.
The default response budget is 16 KiB. Truncation MUST be explicit and preserve
summary counts and a way to retrieve omitted detail. Diagnostic IDs and ordering
MUST be stable for identical inputs; IDs are independent of translated messages.

## 5. Required capabilities

### 5.1 Trustworthy verification (DEV-010)

Test execution MUST distinguish passed, failed, skipped, not-run, and inconclusive
cases. Zero executed tests MUST NOT produce a passing validation verdict. Skips
MUST include reasons and cannot satisfy required coverage. A caught REPL error
followed by a later success marker MUST NOT make a failed test pass.

Each run MUST identify the code and artifact actually tested. Stale or mismatched
artifacts make the validation inconclusive unless the requested target explicitly
is that older artifact. Timeouts, cancellation, and crashed workers are non-passes.

Use existing repository verification entry points. New required gates MUST follow
the existing mutation-evidence policy: a representative broken implementation
must make the gate fail. Assertions must check behavior independently, rather
than comparing output to another invocation of the same implementation.

### 5.2 Queryable API contracts (DEV-020)

`describe` MUST return parameter names and calling convention, result information,
documented conditions, effects, examples, source location, and target availability.
It MUST distinguish implementation-derived facts, declared contracts, inferred
facts, and unknowns.

Use one canonical metadata source per fact. Generate help, machine-readable API
descriptions, and relevant documentation from it. Examples MUST be executable in
tests. Dynamic definitions may register metadata at load time; absent metadata
must not imply that a function is pure, supported, or absent.

Initial effect categories: pure, state-read, state-write, filesystem, process,
network, and unknown. Effects form a set, and transitively unknown callees keep
the result unknown. Effect metadata is a planning aid, not a security boundary
or permission to replay a call automatically.

### 5.3 Conservative static checks (DEV-030)

The first checker MUST detect syntax errors, provable arity errors, unresolved
references within a declared closed dependency set, and use of APIs known to be
unsupported on the selected target. Dynamic loading and unknown calls MUST be
reported as limitations rather than falsely definitive errors or clean results.

Optional contracts SHOULD add obvious argument/result type mismatches and
nilability checks. They MUST remain compatible with unannotated Elisp. Do not
make a complete inference engine a prerequisite for the first useful release.

Checking MUST NOT evaluate arbitrary project macros in the live session. Use
existing safe compiler analysis; unsupported expansion is explicit. Expansion
requiring execution must be a separate, explicit operation in an isolated worker.

### 5.4 Structured failure localization (DEV-040)

A failure MUST expose a stable error code, condition data, function/call chain,
execution phase, loaded generation, and source identity when available.
Source precision MUST distinguish `expression`, `definition`, and `unknown`.

Preserve source mappings through reader, macro expansion, intermediate code, and
native compilation where the implementation supports them. Macro diagnostics
SHOULD show both expansion origin and expanded expression. Never present a
definition start as the exact failing expression.

Separate observed evidence from inferred possible causes. Changes to the source
after loading MUST be visible as a mismatch. Failure records MUST have bounded
retention, explicit argument-capture semantics, and a clear/release operation.
Avoid copying secrets or unbounded object graphs into exported diagnostics.

### 5.5 Dependency and change impact (DEV-050)

Track definition dependencies, imported files, macro expansion dependencies,
generated artifacts, direct native callers, and known test coverage links.
Each edge MUST carry its kind and provenance.

Impact results MUST distinguish a complete set within a declared scope from a
conservative approximation. Unknown dynamic dependencies MUST cause a broader
test/rebuild selection or an inconclusive result; never silently omit them.

Affected-test selection MUST report why each test was selected and what was not
covered. Persist dependency indexes using source/artifact identities; invalidate
them when compiler options, imports, macros, or relevant source changes.

### 5.6 Verifiable reload (DEV-060)

Reload MUST provide a plan, candidate build, validation, and publication stage.
A plan identifies input hashes, expected current generation, rebuild closure,
affected callers, compatibility requirements, and state-preservation limits.

Applying a stale plan MUST refuse publication. Candidate build or validation
failure MUST leave the active generation unchanged. Prefer atomic publication;
where unsupported, either refuse an atomic request or return an explicit partial
state and recovery instructions. Never label partial publication successful.

Function rebinding alone MUST NOT claim replacement of compiled direct callers.
Rebuild/relink their closure or identify the remaining old calls explicitly.
Native GC/allocation replacement MUST obey the runtime's actual safe-point,
layout, ownership, and migration constraints. Unknown compatibility is a refusal,
not an assumption. Restoring code is not rollback of heap or external effects.

Acceptance includes a live REPL example where a supported repair preserves
declared application state and a negative example where an incompatible native
change is rejected without corrupting the session.

### 5.7 Failure retry and reproducible sessions (DEV-070)

Reuse existing failure capture and retry functions. A saved call MUST record
whether arguments are copied values, live references, or serialized values;
mutable references cannot promise reproduction of their earlier contents.

Retry MUST be explicitly requested. It MUST report the old and current code
identities and warn through structured data about changed arguments or unknown
effects. Inspection and reload MUST NOT implicitly retry failures.

Session export MUST include relative file paths and hashes, runtime/build options,
dependency versions, explicit reproduction forms, seeds when relevant, and
required external fixtures. Environment capture is allowlisted; unsupported
resources are listed. Export is a recipe, not an arbitrary process snapshot.

A new process MUST be able to validate the manifest before executing the recipe.
Missing prerequisites produce a useful non-pass result. Replay with external
effects requires an explicit choice; provide fixture-backed replay where possible.

Experiments SHOULD run in a separate process with an isolated temporary workspace.
Report whether the worker merely separates memory/files or also restricts network
and processes. A separate process alone is not a security sandbox. Failed
experiments MUST NOT silently alter the live REPL or shared source checkout.

### 5.8 GC and allocation diagnostics (DEV-080)

Expose supported metrics for before/after live bytes, allocated bytes, reserved
memory, reclaimed bytes, collection count, and collection duration. Distinguish
heap reuse from memory returned to the operating system. Report target, units,
measurement interval, collection generation/epoch, and counter semantics.

Do not label whole-operation elapsed time as stop-the-world pause time unless
that pause is measured. Counter reset/wrap and incompatible snapshots MUST be
detected. Retaining paths/reasons are available only when collector evidence
supports them; otherwise return unavailable with a reason.

Diagnostic capture MUST be bounded and releasable. A regression fixture MUST
verify that clearing saved failures and diagnostic records removes their own
retained references. Memory acceptance must distinguish intentional live state,
allocator reservation, and a growing unreachable allocation leak.

### 5.9 Small, reliable source edits (DEV-090)

Provide symbol lookup, references, and bounded source context using the compiler's
reader/index where possible. Avoid mandatory whole-repository context dumps.

A symbol-edit operation SHOULD accept the expected source hash, parse the
candidate, and return a minimal diff. A changed source precondition MUST reject
the edit. Generated code MUST identify its canonical editable source. An edit
must not overwrite concurrent user or agent changes.

### 5.10 Fresh-session onboarding and ecosystem (DEV-100)

README and AI onboarding MUST lead to one tested path that installs/builds the
supported environment, starts the correctly bootstrapped REPL, inspects an API,
reproduces a failure, repairs/reloads it, retries it, and runs verification.
Use repository-relative commands; do not depend on a developer's private setup.

Provide a minimal automation template and an Emacs-integration template with
tests. Pin external dependencies and record integrity information where the
dependency mechanism permits it. Document supported operating systems and targets.

Inventory practical library support for filesystem, JSON, HTTP, SQLite, and
process execution. Reuse working Emacs/library/FFI integration instead of assuming
every capability needs a new standard-library implementation. Each advertised API
must have a target-specific smoke test or be explicitly experimental.

## 6. Implementation sequence and parallel boundaries

| Phase | Deliverables | Depends on | Completion evidence |
| --- | --- | --- | --- |
| P0 | Inventory, runtime blockers, baseline fixture | Current repository | Reproducible current-state report |
| P1 | DEV-001, DEV-010, minimum DEV-020/030 | P0 | JSON contract and against-bug tests |
| P2 | DEV-040/050 and metadata completion | P1 interfaces | Source precision and impact fixtures |
| P3 | DEV-060/070/080 integration | P1; P2 for precise impact | Warm repair and cold replay acceptance |
| P4 | DEV-090/100; optional MCP adapter | Stable backend | Fresh-session walkthrough and templates |
| P5 | Comparative evaluation | Accepted feature subset | Published methods, results, limitations |

The first usable milestone is P0 + P1 plus a wrapper around the existing REPL
repair/retry flow. Do not delay it for complete type inference, retaining-path
analysis, or all ecosystem integrations. Deferred requirements remain visibly
open; a partial milestone is not completion of the whole specification.

After agreeing on DEV-001 and metadata ownership, independent implementation
streams can cover API/static checks, diagnostics/source maps, dependency/reload,
and tests/onboarding. Shared runtime or GC files need one designated owner and
explicit integration points. Parallel work must not operate on one live REPL.

Runtime correctness or memory regressions discovered in P0 must be resolved before
using the affected path as evidence of reliable reload or improved efficiency.

## 7. Acceptance matrix

| ID | Fixture | Required observation |
| --- | --- | --- |
| A01 | Read-only describe/check repeated in live session | No application call or state mutation |
| A02 | Wrong arity, Unicode source, malformed source | Stable IDs and correctly defined positions |
| A03 | Unknown dynamic call or macro expansion | Explicit limitation; no false clean verdict |
| A04 | REPL catches error then prints success marker | Test still fails |
| A05 | Zero tests, required skips, timeout, stale artifact | Non-pass verdict with reason |
| A06 | Source edited after loading | Loaded/source identities visibly differ |
| A07 | Macro definition or native direct callee changes | Dependent rebuild or explicit refusal |
| A08 | Failed candidate build and stale reload plan | Active generation unchanged |
| A09 | Supported reload with live application state | New behavior and promised state preserved |
| A10 | Incompatible GC/layout change | Safe refusal; live session remains usable |
| A11 | Saved call has file/process effects | No automatic retry during inspect/reload |
| A12 | Export replayed in a fresh process | Same checked result, or precise missing prerequisite |
| A13 | Repeated capture/clear and collection | Retained diagnostic roots released; bounded growth |
| A14 | Stale expected hash on symbol edit | Concurrent changes preserved |
| A15 | Paged/truncated JSON response | Valid schema, accurate counts, retrievable detail |
| A16 | New developer follows repository documentation | Complete repair/retry/check without private setup |

Run fixtures on each advertised execution target. Unsupported targets are explicit
and excluded from support claims, not counted as successful tests. Cross-substrate
changes must include the repository-required parity evidence. Reuse existing
fixtures when their assertions meet these requirements.

## 8. Measuring development efficiency

### 8.1 Controlled implementation benchmark

Start with 30 tasks: 10 new features, 10 bug fixes, and 10 requirement changes.
Maintain separate strata for portable automation and Emacs-specific integration.
Python and TypeScript are useful comparison candidates for portable tasks;
Emacs Lisp is a necessary comparison for Emacs-specific workflows. This is an
evaluation choice, not a claim about the most frequently selected AI language.

Define externally observable acceptance criteria before running agents. Use
independent or hidden tests so an agent cannot make its own tests easier to pass.
Equivalent tasks require equivalent behavior, not identical implementation steps.

Pin model/version, tools, resource budget, starting repository, dependency setup,
and task instructions. Allow normal language tools under equivalent policies.
Run cold onboarding and warm-session repair separately, declaring exactly what
state or preparation a warm session receives. Include installation and setup
costs in cold runs. Record task/model order; vary it to reduce ordering effects.

Use at least three runs per task/model/language when affordable. Publish success
rate, wall time, input/output tokens, tool calls, unsuccessful edits, human
interventions, and replay success. Report unsuccessful runs and budget exhaustion,
not only medians among successes. Token cost and elapsed time are separate metrics.
Record source/artifact identities and distinguish infrastructure failures.

Provisional release targets, to be frozen before comparative runs:

- Against the same NeLisp baseline, at least 25% lower median tokens and median
  wall time for successful warm repair tasks, without lower observed success rate.
- Against comparison languages, report success-rate differences and paired
  time/token distributions with uncertainty; do not claim a winner from noise.
- If quality improves while cost rises, report that tradeoff explicitly.

These percentages are design targets, not measured improvements. The initial
sample is exploratory and cannot establish universal superiority. Expand the
task corpus before making broad public claims.

### 8.2 Language-choice experiment

Choice frequency is a different experiment from implementation efficiency. Give
agents equivalent task descriptions, balanced tool/documentation access, and no
instruction to favor a language. Randomize presentation order and record choice,
stated reason, and actual task outcome. Separate prompted awareness of NeLisp
from unprompted recognition.

A high selection rate with poor completion is not success. Better documentation,
working libraries, easy installation, and reliable feedback must translate into
successful tasks. Existing GitHub usage statistics do not measure autonomous AI
language preference.

## 9. Delivery and definition of done

The implementing session MUST deliver:

1. Capability mapping and explicit deferred/unsupported items.
2. Versioned schemas and shared backend/CLI/REPL contract tests.
3. Accepted feature implementations with regression and parity evidence.
4. Updated README, AI entry documentation, and runnable REPL walkthrough.
5. Benchmark fixtures and raw result format; actual results when executed.

Use the repository's existing check, targeted-test, gate, and aggregate verification
workflow. Record the exact validated source/artifact state. Documentation-only
changes do not require pretending that runtime gates were exercised.

Completion means the advertised scope meets its acceptance criteria, fresh-session
onboarding works, and remaining limitations are discoverable through both human
documentation and machine output. Do not mark this specification implemented based
only on merged interfaces or passing smoke tests.

## 10. Implementation design

This section chooses an implementation boundary for sections 3–7. Names marked
**planned** are not currently callable. The existing REPL extensions are a
substrate for this design, not completion of DEV-001 through DEV-100.

### 10.1 Inventory and evidence boundary

The inventory was inspected at core `185467cda`. The following Linux standalone
fixtures passed on the opt-in reader (SHA-256 prefix `86a0d4f947bc`), with separate
host ERT coverage. Native replacement remains Linux x86_64 only. macOS/Windows
host-side compilation and rejection tests do not certify native replacement.
The full 930-form user initialization audit is now complete. It had been
stopped at form 4 by a deterministic SIGSEGV in the arena's boundary reclaim,
which rewound a bump cursor back over a span the reader had already written to
and handed it out again without zeroing it -- unlike the free-list reuse path,
which zero-fills. The defect was present in every standalone binary built that
day, so it was never a recent regression; nothing had driven a shape that hit
it. With the fix (`b9cb89afd25c`), the audit reaches all 930 form boundaries,
prints `AUDIT_DONE 930` and exits 0 with no signal, in 36 seconds, leaving the
init file's hash unchanged. It records 322 `FORM_ERROR`s -- 238
`void-function`, 54 `file-missing`, 29 `void-variable`, 1 `error` -- which are
unimplemented Emacs APIs and absent files, not memory faults, and are a larger
number than the previous partial run precisely because far more of the file now
executes. `tools/nelisp-real-init-audit.sh` makes the run repeatable and
`nelisp-sexp-clone-bind-smoke` is a required Linux gate holding the mechanism
down. This audit is a completed correctness baseline; it is NOT a
development-efficiency measurement and must not be cited as one.

| Capability / current entry | Actual implementation and evidence | Platform requirement still open |
| --- | --- | --- |
| Capture/retry: `nelisp-repl-session-call`, `-failures`, `-retry`, `-clear` | `lisp/nelisp-repl-session.el`; `test/nelisp-repl-session-test.el` and `-smoke.sh`: verified scoped behavior | Partial DEV-070: live argument references, no failure-time loaded identity or effect contract |
| Loaded identity: `nelisp-repl-code-info`, `-forget` | `lisp/nelisp-repl-code.el`; code ERT and standalone smoke: verified for tracked publications | Partial DEV-020/040: untracked functions unknown; no expression-level failure map |
| Session recipe: `nelisp-repl-session-record`, `-record-setting`, `-record-load`, `-export` | Session tests and second-process replay smoke: verified explicit recipe | Partial DEV-070: file hashes are comments, not prerequisite validation; dependencies/fixtures need manifest |
| GC: `nelisp-repl-gc-snapshot`, `-compare`, `-collect` | `lisp/nelisp-repl-gc.el`; GC ERT and opt-in standalone smoke | Partial DEV-080: counter epoch checks absent, per-object retaining paths unavailable; host fields can be unavailable |
| Lisp reload: `nelisp-artifact-reload-source-file` | `lisp/nelisp-artifact.el`, REPL guide and code smoke | Partial DEV-060: shared stale-plan protocol absent; partial publication must remain visible |
| Native reload: `nelisp-runtime-rebuild-and-reload`, `nelisp-runtime-reload-status`, `-restore-originals` | `lisp/nelisp-runtime-development.el`, `lisp/nelisp-native-load.el`; `test/nelisp-native-runtime-repl-smoke.sh` checks actual collector A → private-helper B → original | Verified only for fixed ABI, opt-in Linux x86_64; no heap migration, no universal direct-caller replacement |
| User native units: `nelisp-native-unit-rebuild-and-reload`, `-stage`, `-publish`, `-address`, `-code-info`, `-resources`, `-reclaim`, `-retained-addresses` | `lisp/nelisp-native-unit.el`, `-development.el`; stable entry gates and single-use CAS publication; carried by the common `reload.plan`/`reload.apply` under `unit: native-unit`; `test/nelisp-native-unit-repl-smoke.sh` is a required Linux CI gate (`native-unit-repl-smoke`, 88 checks) covering multi-export switching, a caller in the old generation, the six-argument boundary, ten consecutive publications, state retention after a refused publish, and reclamation accounting; [REPL procedure](repl-development.md#replace-user-defined-native-functions) | Closed raw-v1 integer SysV units, Linux x86_64 only. **Existing executable direct calls are still not redirected** -- only calls through a unit's stable gate observe a new generation. Unpublished, failed, expired and refused candidates are reclaimed; a superseded PUBLISHED generation is deliberately kept mapped and reported as refused, because this runtime exposes no in-flight-call evidence |
| Definitions: `nelisp-defs-index-search`, `-signature`, `-references`, `-who-requires` | `packages/nelisp-defs-index/src/nelisp-defs-index.el`, package ERT | Existing SQLite-backed implementation; not qualified here for standalone; no complete dependency-closure claim |
| Compile checks: `nelisp-artifact-check-forms` / `nl-check-expanded-forms` | `lisp/nelisp-artifact.el`, `packages/nl-check/` | Existing expansion-time checks; not a safe general source checker. Missing optional checker returns no coverage |
| Verification: `tools/ai/nelisp-ai.sh check`, `test-one`, `test`, `verify` | Existing `target/gates/*.json` and `tools/ai/gates.expected` | Reuse reports; shared DEV envelope, artifact freshness binding and operation adapters planned |
| Common frontend, metadata/effects, impact plans | No accepted implementation identified | Absent as a unified DEV protocol; implement P1/P2 without pretending the rows above already supply it |

Host Emacs, normal standalone, and opt-in standalone are distinct target IDs.
Each capability response includes an evidence status, prerequisites, supported
scope, unavailable reason, and evidence identity. `fboundp` alone is discovery,
not verification. Capability discovery must not run a smoke or build implicitly.

### 10.2 One backend and explicit session routing

Add **planned** `lisp/nelisp-dev-protocol.el` for validation, envelopes and bounded
serialization, and `lisp/nelisp-dev.el` for operation dispatch. Adapter modules
own conversion of existing results; existing session, reload, GC and gate modules
remain the authorities for behavior. Do not duplicate their state machines.

The planned REPL call is `(nelisp-dev-dispatch request context)`, returning a
structured Elisp value. The planned CLI is
`tools/ai/nelisp-ai.sh dev --request FILE --json`; it reads data rather than
interpolating arbitrary command text into a shell or Lisp expression. The same
dispatcher is used by both. Existing commands and exit codes remain unchanged.

A CLI process cannot observe another process's live function cells or retry its
saved arguments. P1 therefore runs source-only operations in a fresh worker;
live operations require the explicit REPL session. A CLI request naming an
unconnected live session returns `unsupported`, never silently starts a new
session and presents it as the old one. A future transport or MCP adapter can
attach using an explicit session ID, but is not required for P1.

The P1 refusal has code `NELISP-DEV-LIVE-SESSION-REQUIRED`, exit 4, and
`data.required_interface: repl`; it includes no invented generation. A session
ID is a process-lifetime opaque identifier created by the backend, not a PID.
No session enumeration, background server or remote authentication protocol is
part of P1. Those require a separate transport design before implementation.

`context` supplies the project root, target identity, session ID, supported
adapter registry and bounded detail store. Read-only requests may read source
and tool indexes; they must not evaluate application code or expand arbitrary
macros. Their own allocations can trigger normal runtime GC. This does not
promise an unchanged heap counter, only no requested application effects.

| Operation key (planned) | Adapter / ownership | Minimum first behavior |
| --- | --- | --- |
| `capabilities`, `describe` | Protocol owner; defs index + code-info | Availability and bounded definition/signature; unknown contracts explicit |
| `check` | Analysis owner; reader + audited safe checks | Syntax and provable facts only; missing checker/dynamic calls inconclusive |
| `diagnose`, `retry` | Diagnostics owner; session API + frozen loaded identity | Inspect without execution; retry only as its own request |
| `impact`, `test` | Dependency owner; index + existing test runner | Explained conservative selection; full suite fallback for unknown closure |
| `reload.plan`, `reload.apply` | Reload owner; existing artifact/native APIs | Validated immutable plan; recheck source, session and ABI at publication |
| `session.export`, `session.validate`, `session.replay` | Session owner; recipe API | Validate manifest without execution; explicit replay request |
| `session.clear` | Session owner; session clear + code forget + adapter stores | Release retained diagnostics, details and plans; rotate diagnostic ID namespace |
| `gc.snapshot`, `gc.compare`, `gc.collect` | GC adapter owner; existing GC API | Epoch-safe metrics; collect is explicitly mutating |

No bulk registration of unknown application functions is required. Each
operation has a declared read-only/effectful classification. Effects of the
application call itself are a separate metadata set and default to `unknown`.

### 10.3 Schema, identities and bounded results

P1 defines **planned** `schemas/dev/request-v1.json`, `result-v1.json`,
`reload-plan-v1.json`, and `session-manifest-v1.json`. Contract fixtures feed
identical requests to CLI and REPL and compare canonical results after removing
only declared volatile fields (request ID, timestamp, session ID).

- A request contains `schema_version`, `operation`, `request_id`, `arguments`,
  `target`, `session_id`, and `limits`. Reject unknown operations, invalid types
  and unsupported major versions before dispatch.
- Source identity hashes raw file bytes, separately from the Git revision. A
  dirty source-set identity hashes sorted relative-path/content-hash pairs;
  it does not rely on mtime or the branch name. Generated files identify their
  generator, inputs and options. Missing source is `null`, not an empty hash.
- Loaded identity contains binary hash, backend, ABI hash, build options hash,
  publication generation and the source hash captured at publication. Preserve
  the loaded record when the current file changes. Failure capture freezes
  this small identity record; it does not retain extra function objects.
- Lines and columns are one-based Unicode scalar positions, with an exclusive
  end position. Byte offsets are zero-based UTF-8/raw-source byte offsets and
  carry an encoding label; adapters must not copy byte offsets into columns.
- Canonical JSON is UTF-8. `null`, false and an empty array are distinct in
  the Elisp serializer; preserve empty diagnostics as `[]`. Diagnostic IDs
  hash code, phase, source identity, span and normalized subject, excluding
  translated text and request IDs.
- Default budget is 16,384 encoded bytes; default page size 50, maximum 200.
  Store full details separately, then serialize the envelope and reduce the
  page until it fits. Never cut serialized bytes. Summary counts describe the
  full result and include returned/omitted counts.
- Cursors bind operation, input hash, sort order, session/index epoch and
  offset. Stale cursors fail with `NELISP-DEV-STALE-CURSOR`; they cannot mix
  pages from different source states. Planned defaults: 64 detail entries,
  1 MiB per entry, 16 MiB total and a 15-minute TTL. Explicit clear/expiry
  releases references. Oversized details return a documented limitation.

Tool transport failures are distinct from application diagnostics. Invalid
requests return exit 2 with a valid failed envelope when serialization is still
possible. Internal crashes return a nonzero error; preserve stderr diagnostics
and never emit a second success envelope. For legacy gates, adapter status is
derived from exit status, case counts, required coverage and artifact identity,
not from a trailing `PASS` string.

### 10.4 Reload and replay preconditions

A plan is immutable data with `plan_id`, session ID, expected publication
generation, input/import hashes, target/options/ABI hashes, known direct callers,
rebuild closure, requested atomicity, state-preservation declaration, limitations
and expiry. Its ID hashes the canonical plan payload. In P1 a plan belongs to
one process and cannot be restored from a file as publication authority.

State progression is `planned → building → validated → publishing → published`.
Build/validation failures are terminal non-publications. Immediately before
publishing, revalidate every precondition; source edits, session restart or a
different GC table generation produce `NELISP-DEV-STALE-PLAN`. Existing native
safe-point and CAS checks remain mandatory. An unknown direct-caller closure
refuses atomic native publication. A legacy Lisp reload reporting partial
publication returns `failed` with the exact published/unchanged components and
recovery instructions; it never becomes `ok` through the adapter.

Export writes two files: a versioned manifest and the existing explicit Elisp
recipe. The manifest hashes the recipe and every loaded source/fixture, records
runtime/options/dependencies, and identifies unsupported resources. Paths use
the manifest directory as their base, with explicitly registered external roots.
Validation checks all prerequisites without loading the recipe. It reports
every missing or mismatched prerequisite in a bounded result. A source hash in
an Elisp comment is not validation.

Replay is a separate request with an explicit effects policy; `unknown` effects
cannot be treated as pure. A worker receives only registered fixtures and
allowlisted environment entries. Report `isolation: process-and-tempdir` unless
additional restrictions were actually enforced. Retry reports reference-capture
semantics: argument mutation is `unknown` unless independently detected, not
`unchanged` merely because the object identity matches.

### 10.5 Dependency, contract and GC ownership

The definition index remains authoritative for parsed definitions/references.
Add edge kinds and provenance there (or an adapter-owned side table keyed by its
schema/content identity), not in an unrelated second crawler. In-memory indexing
is the fallback when SQLite is unavailable. Static call, require, macro-use,
artifact-import, direct-native-call and test-coverage edges are distinct.
Absent dynamic edges make closure incomplete; they select broader existing
gates or yield `inconclusive`. Rebuilding an index is an explicit operation;
queries must not silently rebuild from arbitrary macro evaluation.

Contracts store declarations once, with source identity and provenance. Parsed
arglists are implementation facts; result types, conditions and effects are
declared or unknown. Annotation strings and docstrings do not establish purity.
Initial checks use an allowlisted nonexecuting analysis path. Existing
`nelisp-artifact-check-forms` is reused only for supported inputs, with an explicit
coverage record; its optional-checker no-op cannot count as a successful check.

GC snapshots add session ID, binary/ABI identity, counter epoch, collection
generation, timestamp and field semantics. Cumulative counters, gauges and
last-collection values are separate. Across session/epoch/ABI changes, comparison
returns `inconclusive` with unavailable deltas. A decreasing cumulative counter
is a reset/wrap indication, not negative reclaimed memory. Last-collection
statistics are not subtracted as cumulative totals. Record collection counts
before and after snapshot allocation to expose non-atomic capture.

The GC adapter owns the epoch. A new session starts a new epoch; successful
native publication or restoration starts another because measurement semantics
can change even with a fixed layout. The adapter observes runtime generation
from `nelisp-runtime-reload-status` on every snapshot, including publication
through the legacy API. A detected cumulative-counter decrease advances the
epoch and marks that interval discontinuous. Unknown resets cannot be inferred
away: if the target cannot expose a dependable generation/reset boundary, the
adapter reports this limitation and refuses a cross-boundary delta.

The protocol adapter maintains one small identity record per tracked publication;
describe, failure capture and reload plans reference its immutable value. An
untracked `fset`/definition change invalidates the record instead of inheriting
the prior source hash. Publication validation rehashes the staged artifact and
exported native source as well as input files, then compares the current
function/publication identity with the plan. No public source-set identity API
exists yet: adding this adapter-owned record is P1/P3 work, not an existing
guarantee of `nelisp-repl-code-info`.

Capture/clear acceptance must allocate identifiable objects held only by saved
failures or provenance records, clear both stores, collect, and demonstrate
released roots over repeated bounded rounds. RSS alone cannot prove reclamation
because the allocator may retain reusable arenas. Pair allocator live/reclaimed
counters with reservation and OS-return measurements where available.

`session.clear` reports counts cleared from the session failure/recipe stores,
code-provenance store, pending plans and protocol detail store. It calls existing
clear/forget entry points rather than just dropping the JSON handles. The
acceptance fixture drops its own temporary references before collecting, checks
store counts and allocator evidence, and retains only scalar measurements. It
must not accidentally keep the tested objects alive in its result or assertion
history. Clearing diagnostic IDs rotates their namespace because the existing
session API resets its numeric IDs.

### 10.6 Reviewable milestones and parallel handoff

| Milestone | Owned files / interface freeze | Required evidence |
| --- | --- | --- |
| P0 inventory + baseline | This inventory; reuse session/code/native smoke inputs | Small failing-call repair in one REPL, same result in fresh replay; record raw timing and identities, no claimed speedup |
| P1 protocol + minimum queries | Protocol schemas/dispatcher and CLI adapter have one owner | A01–A05, A15; byte budget, missing checker, zero-test and stderr-after-marker negative cases |
| P2 diagnostics + impact | Separate adapter owners after schema freeze; index schema owner controls migrations | A02/A03/A06/A07 with Unicode, macro and dynamic-call fixtures |
| P3 reload/session/GC | One publication owner; other adapters use frozen plan API | A08–A13, including source race, mutable arguments, incompatible ABI, epoch reset and released diagnostic roots |
| P4 onboarding + edits | Docs/templates and optional transport independent of runtime | A14–A16 on clean temporary checkout and declared supported targets |
| P5 evaluation | Immutable task corpus and evaluator-owned assertions | Section 8 raw data, failures and paired uncertainty; no implementation self-grading |

Each stream uses its own REPL and generated artifacts. Schema changes require
updating producer and consumer contract fixtures together; no agent changes
shared GC/publication source without the designated owner's integration. The
first implementation PR should stop at P1 and an adapter for the existing repair
flow. The remaining A06–A16 rows stay open until separately executed. This design
does not authorize an automatic implementation of every later phase.

## 11. Design references

These references motivate tool discoverability and feedback design; they do not
provide measurements of NeLisp's performance:

- [Anthropic: Writing effective tools for agents](https://www.anthropic.com/engineering/writing-tools-for-agents)
  — clear tool contracts, bounded useful context, and agent-based evaluation.
- [OpenAI: Harness engineering](https://openai.com/index/harness-engineering/)
  — agent-readable repositories and mechanically checked development feedback.
- [GitHub: TypeScript, Python, and the AI feedback loop](https://github.blog/news-insights/octoverse/typescript-python-and-the-ai-feedback-loop-changing-software-development/)
  — ecosystem and feedback considerations; repository activity is not a ranking
  of AI language choice.
