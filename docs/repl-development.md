# Develop NeLisp in a persistent REPL

Use this guide when starting a new development session. Everything needed
is in the checkout: the launcher, example source, and smoke test. No prior
chat history, Anvil installation, or files from another developer's `/tmp`
directory are needed.

## First setup

Start in the repository root. Install the build prerequisites described in
[README.org](../README.org): host Emacs 29.4 or newer and the tools for your
platform. This development launcher also uses a POSIX shell and `mkfifo`.
Its source-reload workflow is verified on Linux; other launcher platforms
need their own verification.

```sh
make standalone-reader
tools/ai/nelisp-ai.sh doctor
tools/ai/nelisp-ai.sh repl
```

The build creates the standalone binary. The launcher generates and loads
the full artifact command runtime, then connects your input to the same
live REPL. It uses host Emacs for that startup preparation. It does not
silently rebuild a missing binary. Set `EMACS` or `NELISP_BIN` when you need
to select a different host executable or a compatible standalone binary.
Use `tools/ai/nelisp-ai.sh repl --help` for the command options.

## Reproduce an error and repair it without restarting

Exit any previous REPL with `(exit)`. Prepare a disposable source file:

```sh
mkdir -p target/repl-demo
cp examples/repl-development/broken.el target/repl-demo/module.el
tools/ai/nelisp-ai.sh repl
```

Enter these forms in that REPL, one at a time:

```elisp
(nelisp-artifact-reload-source-file "target/repl-demo/module.el")
(setq nelisp-repl-demo-state 7)
(defun nelisp-repl-demo-run (x)
  (+ (nelisp-repl-demo-step x) nelisp-repl-demo-state))
(nelisp-repl-demo-run 0) ; => 17
(nelisp-repl-demo-run 1) ; => error: demo bug: input 1
```

Check that the reload result has `:status ok`. The failing invocation
prints its condition and a bounded, innermost-first call chain to stderr.
Find `nelisp-repl-demo-step` and its caller `nelisp-repl-demo-run` in that
chain. The REPL stays available after this Lisp error.

Keep this REPL open. In a second terminal at the repository root, replace
the source (or make the equivalent edit in your editor):

```sh
cp examples/repl-development/fixed.el target/repl-demo/module.el
```

Return to the original REPL:

```elisp
(nelisp-artifact-reload-source-file "target/repl-demo/module.el")
(nelisp-repl-demo-run 1) ; => 28
nelisp-repl-demo-state  ; => 7
```

The file contains only `nelisp-repl-demo-step`. The caller defined in the
REPL was not rebuilt; it reaches the new target by name. Unrelated state
remains 7. Ordinary same-path artifact loading can skip an already-loaded
artifact, so use the explicit source-reload API for this repair loop.

## Read the reload result

The result is a versioned plist (`:format nelisp-artifact-reload-v1`).

| Field | Meaning |
|---|---|
| `:status` | `ok`, `rejected`, `error`, or `partial` |
| `:phase` | Where preparation or publication reached or failed |
| `:published` | Definitions whose installation completed |
| `:attempted` | An uncertain installation when publication fails partway |
| `:definitions` | Candidate definition names and source spans when known |
| `:generation` | Session generation associated with the result |
| `:source-sha256`, `:artifact-sha256` | Candidate identity when available |

An optional second argument supplies your build identity. Syntax and
compilation failures leave the previous definitions callable. A `partial`
result is not a rollback: inspect the completed and attempted definitions
before continuing.

This first workflow accepts top-level `defun` forms only. Keep variables
and setup forms in separate session setup code; `defvar`, `provide`, and
arbitrary top-level effects are rejected. It does not remove definitions
deleted from a file or replace native direct-call sites. Macro dependency
rebuilds are separate work. Native allocator and GC replacement uses the
development build described below.
Reported source spans describe definitions, not the exact expression
that later fails. Unknown positions and caller dependencies stay unknown.
A native crash is different from a recoverable Lisp condition.

## Start again and verify

`(exit)` ends the REPL. A new process has fresh Lisp state: repeat the
example setup, or replay your project's saved setup forms. The launcher
and source-reload API remain available from the checkout in every session;
they do not depend on keeping this session's generated runtime file.

To run the automated repair-loop smoke against an existing binary:

```sh
sh test/nelisp-ai-repl-smoke.sh
```

The smoke checks an intentional error, its call chain, continuation,
target-only replacement through an existing caller, preserved state, and
invalid-source rejection. Then run the focused tests for your change and
the normal checks in [AI.md](../AI.md). Interactive success alone is not
a substitute for those checks.

## Keep failures, inspect code, and replay a session

After starting the development REPL, load the shared entry point:

```elisp
(require 'nelisp-repl-development)
(nelisp-repl-help)
```

Use `nelisp-repl-session-call` around a call you want to investigate. It
records a failure and re-signals the original condition; successful calls
keep their normal return value. No call is retried automatically.

```elisp
(nelisp-repl-session-call 'nelisp-repl-demo-step 1)
(nelisp-repl-session-failures)
;; Edit and reload the source, then explicitly retry a recorded ID.
(nelisp-repl-session-retry 1)
(nelisp-repl-code-info 'nelisp-repl-demo-step)
```

Failure records retain argument references, not immutable snapshots. A
later mutation can change what an explicit retry receives. Records with
truncated arguments refuse retry. Clear the records when finished, especially
when investigating memory retention:

```elisp
(nelisp-repl-session-clear)
(nelisp-repl-code-forget 'nelisp-repl-demo-step)
```

Code information tracks definitions published through source reload after
the module was loaded. It reports source spans, source/artifact hashes,
generation, and whether the function and source file still match the
record. Editing a file and replacing a function with `fset` are separate
changes. Untracked definitions report `:unknown`; stale provenance does
not claim to identify newly installed code. Source spans identify a
definition, not the exact failing expression.

Record only the settings, loads, and operations needed to recreate your
case. Registration does not execute the operation:

```elisp
(nelisp-repl-session-record-setting 'example-options '(fast verbose))
(nelisp-repl-session-record-load "target/repl-demo/module.el")
(nelisp-repl-session-record '(nelisp-repl-demo-step 1))
(nelisp-repl-session-export "target/reproduce-session.el")
```

In a new development REPL, load the exported file. It executes the selected
forms and resolves recorded load paths relative to the export. Keep those
files with the recipe when moving to another checkout. Recorded hashes
document file identity; they do not restore older file contents. The export
contains only explicitly registered values and operations, without scanning
the process environment or dumping all variables. A replay executes code
and can repeat side effects, so run it explicitly.
The recipe limit rejects additional records instead of dropping earlier
setup operations. Export and clear the recipe before registering more.

Run the host checks with `make repl-development-test`. Standalone checks
use the same launcher as interactive work:

```sh
NELISP_BIN=target/nelisp-runtime-reload sh test/nelisp-repl-session-smoke.sh
NELISP_BIN=target/nelisp-runtime-reload sh test/nelisp-repl-code-smoke.sh
NELISP_BIN=target/nelisp-runtime-reload sh test/nelisp-repl-gc-smoke.sh
```

## Compare GC and allocation diagnostics

```elisp
(setq gc-before (nelisp-repl-gc-snapshot))
;; Run the operation being investigated.
(setq gc-after (nelisp-repl-gc-snapshot))
(nelisp-repl-gc-compare gc-before gc-after)
(nelisp-repl-gc-collect)
```

Snapshots name the public arena and allocation counters. `collect` explicitly
requests GC and returns before/after snapshots, differences, and elapsed
call time. That duration includes the call overhead; it is not a separate
measurement of a stop-the-world pause. Taking diagnostics can itself
allocate, so these are observations rather than an atomic heap snapshot.
Unavailable counters remain `:unavailable`, never an invented zero.

The native development build also exposes the collector's retained
diagnostics. Conservative pins explain one category of retained memory;
individual object-to-root paths are not reconstructed by this API. Distinguish
heap usage, live bytes after the last collection, and bytes returned to the
OS when comparing results.

## Native allocator and GC development

This workflow requires Linux x86_64 and a host Emacs for compilation.
Start from the repository root:

```sh
make runtime-reload-reader
NELISP_BIN=target/nelisp-runtime-reload tools/ai/nelisp-ai.sh repl
```

Load the development commands and create state that should survive:

```elisp
(require 'nelisp-runtime-development)
(setq runtime-example-state (list "retained" (vector 1 2 3)))
(nelisp-runtime-reload-status)
```

Edit the allocator or collector definitions in
`scripts/nelisp-standalone-build.el`. Keep the REPL process running, then
compile and install the checkout's current definitions from that REPL:

```elisp
(nelisp-runtime-rebuild-and-reload)
(nelisp-runtime-reload-status)
runtime-example-state
```

Pass the repository directory explicitly if the REPL's current directory
has changed. `EMACS` selects the host compiler executable. Compilation runs
in a child process; installation happens in the original REPL. The result
reports the source snapshot and artifact paths, executable identity, and
publication result. Inspect the status and generation before repeating
the failing operation. Keep the returned artifacts when diagnosing a
candidate-specific failure.

To compile and validate a candidate without switching the running allocator
or collector, use the separate staging command:

```elisp
(setq runtime-candidate (nelisp-runtime-build-and-stage))
(plist-get runtime-candidate :status) ; staged, or rejected with a reason
(plist-get runtime-candidate :generation)
(nelisp-runtime-reload-status) ; generation is unchanged by staging
```

Staging maps the candidate code and returns `:alloc-handle` and `:gc-handle`
for the native installation API. It checks the running executable identity
and generation before and after compilation/loading. The convenience
`nelisp-runtime-rebuild-and-reload` uses this boundary and checks them again
immediately before publication. Calling it again builds another candidate.
These handles are process-local, and staging retains code mappings until
process exit. This is not a saved, immutable reload plan: source hashes,
session identity, and dependency closure are not frozen by this API. The
common protocol's [native `reload.plan` and `reload.apply`](development-protocol.md#plan-and-apply-a-native-allocatorgc-replacement)
add process-local, single-use authority and stale-input checks above this API.

The replacement contains the allocator and the complete collector unit.
Collector helpers can be added, removed, renamed, or rewritten within the
supported native compiler language. The public names and arities in
`lisp/nelisp-runtime-reload-abi.el`, shared state layout, heap object layout,
and root protocol must remain compatible with the running executable.
Changing that contract requires rebuilding and restarting; it does not
migrate an existing heap. Candidate-private static data sections are not
supported by this loader.

`sh test/nelisp-native-runtime-repl-smoke.sh` exercises the complete native
workflow. In one process it installs the checkout's GC, adds a private helper
to an isolated second source generation, observes the changed post-GC debt
threshold, and restores the original behavior while retaining Lisp data.

Repeat the edit and rebuild command for another generation. To return
future calls to the executable's original allocator and collector:

```elisp
(nelisp-runtime-reload-restore-originals)
(nelisp-runtime-reload-status)
runtime-example-state
```

Restoration changes code dispatch; it does not undo mutations to the heap
or recover a process that has already crashed. Old code mappings remain
alive until process exit. Publication refuses active worker threads or
an active allocation/collection, and rejects incompatible artifacts.
Use focused native tests, the normal gates, and the original memory-error
reproducer after the interactive loop. A new session starts by rebuilding
the development executable and replaying the setup forms above.

## Replace user-defined native functions

On the same Linux x86_64 development executable, a **closed raw native unit**
can expose stable executable entry addresses. Compile native callers against
those addresses once; later publications replace the whole unit without
recompiling those callers. This is a separate API from the allocator/GC
`reload.plan` adapter.

Create `target/score.el` containing:

```elisp
(defun helper (x) (+ x 1))
(defun score (x) (* (helper x) 2))
```

In the bootstrapped REPL, with the checkout as the current directory:

```elisp
(require 'nelisp-native-unit-development)
(setq created
      (nelisp-native-unit-rebuild-and-reload "target/score.el" nil '("score")))
;; Require :status published before taking the unit ID or calling it.
(setq score-unit (plist-get created :unit-id))
(setq score-entry (nelisp-native-unit-address score-unit "score"))
(nelisp-native-unit-call score-unit "score" '(5)) ; => 12
```

Change the file to `(defun helper_new (x) (+ x 10))` and
`(defun score (x) (* (helper_new x) 2))`, then run:

```elisp
(nelisp-native-unit-rebuild-and-reload "target/score.el" score-unit)
;; Inspect :status before rerunning the application operation.
(nelisp-native-unit-call score-unit "score" '(5)) ; => 30
(= score-entry (nelisp-native-unit-address score-unit "score")) ; => t
(nelisp-native-unit-status score-unit) ; generation, binary hash, public contract
```

Native callers can use the returned integer address as the literal target
of the raw compiler's `(call-ptr ADDRESS x)` operation. Addresses and unit IDs
are process-local: rebuild these callers during setup in a new session.
`sh test/nelisp-native-unit-repl-smoke.sh` demonstrates a caller compiled once,
private helper renaming, preserved Lisp state, and stale-candidate rejection.
Select the development executable with `NELISP_BIN`.

For separate preparation and publication, use
`(nelisp-native-unit-stage artifact unit-id)` followed by
`(nelisp-native-unit-publish candidate-id)`. Stage results carry the candidate
ID and expected generation. Publish consumes the candidate, checks source,
artifact and executable hashes, and swaps one immutable table with CAS.
Competing or expired candidates are rejected; discard a candidate with
`nelisp-native-unit-discard`. No application call is automatically retried.

The contract is fixed public names/order/arities, at most 64 exports and six
integer/pointer-word arguments under the raw-v1 SysV ABI. Imports and private
static data are unsupported; include internal direct callees in the unit.
This does not redirect calls already compiled to the executable's original
functions, support arbitrary C ABIs, or migrate heap layouts. An in-flight
call may finish on its old generation. Executable pages and generation tables
remain mapped until process exit, including discarded staged code; repeated
reloads therefore consume additional memory. At most 64 units and 64 pending
candidates are registered; candidates expire after 15 minutes. Restart the
REPL to reclaim mappings and repeat the setup above.
