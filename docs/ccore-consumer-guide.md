# C-core consumer guide for AI developers

Status: proposed shared development contract. This guide describes how to
expose qualified C-core libraries to new development sessions. Shared
activation and a semantic capability manifest remain implementation work.

## Entry points and repositories

Read this guide through the repository's AI entry document. Keep one canonical
guide and link it from agent instructions and the NeLisp development skill.
All sessions should resolve the same release/profile rather than copy a
private bootstrap sequence into each task.

The runtime checkout owns the executable, the persistent REPL and development
protocol. The library checkout owns compatibility providers, library bootstrap
generation and GNU parity probes. Resolve both roots explicitly; their tools
are not interchangeable.

From the runtime checkout, the existing orientation commands are:

```sh
tools/ai/nelisp-ai.sh doctor
tools/ai/nelisp-ai.sh dev --request examples/repl-development/capabilities.json --json
```

The second command reports host-side development adapters. Use its stated
limitations; it does not qualify a standalone executable's C-core behavior.

## Shared activation contract

A consumer launch should select an executable and a library profile together,
initialize the profile once and return a machine-readable receipt. The receipt
should identify the executable hash, library/bundle hash, profile version,
active public providers and relevant qualification evidence. A new session
must verify that receipt before using its advertised APIs.

Keep executable selection explicit, including `NELISP_BIN`. The installed
`nelisp` on PATH and a development worktree binary can be different builds.
Publish a qualified executable/profile pair as the shared default only after
its cold-start and semantic gates pass. Preserve the 55-second integration
limit and independent fresh-process checks.

The shared initializer must execute the same registration and initialization
path as formal startup. Focused development loads only the required providers
and their actual dependencies. Registration supplied solely by a probe cannot
establish consumer support.

## Semantic capability manifest

Generate the function reference from provider definitions and qualification
results, then add concise usage notes. Keep the machine manifest and prose
reference tied to the same executable/profile receipt.

| Field | Required meaning |
| --- | --- |
| Function and arguments | Public name, required/optional/rest arguments |
| Provider | Active implementation, source module and initialization dependency |
| State owner | Buffer/process/global owner and relevant lifetime rules |
| Behavior | Return value, type errors, side effects and supported options |
| Support status | Qualified, partial, unsupported or unknown in this profile |
| Evidence | Exact GNU inputs, comparison result and artifact/source hashes |
| Limitations | Remaining mismatches and unverified paths |

Name discovery through `fboundp` is useful, but semantic support requires
qualification evidence. Unknown or partial entries must remain visible. An
AI should read the relevant entries before implementing a consumer and avoid
reimplementing a qualified primitive or selecting a different provider silently.

## Development loop

1. Resolve the target receipt and required capability entries.
2. Load the shared profile's actual required providers once in an isolated
   process or persistent development REPL.
3. Reproduce the behavior with the same GNU inputs and complete error data.
4. Reload the edited source and run the smallest relevant checks.
5. Validate related edits together through full cold startup and the formal
   meter before publishing the profile.

A passing focused check requires exit zero, empty unexpected stderr, the exact
record count, one completion marker and exact GNU comparison. Missing records,
zero-form execution and extra printed load values are failures. A changed
acceptance script must reject broken/truncated input and missing completion.

Use a short smoke for receipt validation in a new session. Full compatibility
qualification belongs to integration; repeat it when the artifact or unresolved
question changes. A persistent REPL receipt must change after a source/provider
reload so consumers can identify the active implementation.

## Buffer and file-library ownership

Public buffers use their direct buffer-local owner. `buffer-file-name` has an
intrinsic initial nil independent from mutable defaults; an indirect buffer
owns its own nil. Current and inactive lookups, switching, dynamic bindings,
error unwind and provider reload are part of its contract.

File visiting, lookup and saving must share that owner. A public local nil
terminates filename lookup, including when an old table contains a filename.
Legacy EC buffer operations retain their dedicated helpers and state. The
files facade's public wrappers and late target definitions are relevant
providers; qualification must demonstrate the effective load order.

Successful save support requires exact written bytes, the correct modified
state, preserved current-buffer identity and failure behavior. Loading a
writer name alone does not establish those properties.

## Publication evidence

The library's [whole C-core ledger](../tools/ai/c-core-progress.org) retains the
complete objective. The [filename ledger](../tools/ai/buffer-filename-progress.org)
tracks its focused and full-startup acceptance separately. A passing focused
development profile should be described at that scope until formal consumer
startup also passes.

Keep current percentages and function counts in generated meter/manifest
output. The guide should link to that evidence rather than maintain another
hand-written completion count. Publish the common launcher, semantic manifest
and AI-entry links together so another session can reproduce activation.
