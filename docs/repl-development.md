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
Some reader forms, including `defvar` and `defconst`, have native fast paths
ahead of macro dispatch. Check normal evaluation as well as explicit macro
expansions: reloading a macro does not replace those native paths.

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

## Reload declaration definitions

Reloading `lisp/nelisp-stdlib-eval-special.el` refreshes the identities used
to retain native `defvar`/`defconst` dispatch. This preserves declaration
scope and initialization rules during source development; a deliberate user
macro replacement still expands normally. The special-variable test suite
replays its declaration observations after loading that canonical source,
then checks user replacement and restoration of the function cell.

## Internal symbol GC qualification

`test/nelisp-native-symbol-build.el` builds a private reader fixture for the
tag-16 representation described in [the ABI](arch/sexp-abi.md). Select a separate
`NELISP_STANDALONE_READER_OUTPUT`; the fixture refuses `target/nelisp`. Run
`test/nelisp-native-symbol-test.py` with `NELISP_SYMBOL_TEST_BIN` pointing to
that executable. It checks clone identity, precise name-buffer marking, and
image restoration with an identity deliberately resembling a heap address.
The public `make-symbol` producer uses the same tag; the private constructor
additionally permits explicit identities for layout and relocation probes.

An ordinary collect-and-read probe can pass even when the precise marker
misses the name edge, because conservative stack roots retain it. The private
`symbol-test-mark` probe temporarily clears that buffer's mark, invokes the
actual precise slot marker, reads its result, and restores the saved mark.
It performs no collection. In a reader built with `NELISP_RUNTIME_RELOAD=1`,
this call uses the GC publication wrapper, so it can compare old and new
collectors in the same process. Drop values using an unsupported new tag
before restoring an older collector. These probes do not establish that a
collection actually moved the buffer. The inspection test compares public
symbol predicates, names, identity, ordinary printing, and soft lookup with
Emacs observations on two fresh symbols. The separate public acceptance suite
qualifies `make-symbol` and evaluation/assignment paths. Private frame/mirror
adapters now cover native key insertion, update, lookup, capture filtering,
and mirror-key image restoration. The frame checks force same-name keys into
one bucket and repeat with a multibyte name and multiple buckets. They do not
establish compatibility of source/native character-versus-byte hash functions
or complete the public evaluator's symbol-type guards.

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
### Identify the code a unit is running now

`nelisp-repl-code-info` answers for Lisp definitions and says nothing about a
published native unit. The native side has its own:

```elisp
(nelisp-native-unit-code-info score-unit)
;; :generation, :source, :source-sha256, :source-current,
;; :current-source-sha256, :artifact-sha256, :binary-sha256, :published-at,
;; :exports, :history
(nelisp-native-unit-code-info score-unit "score") ; adds :export-arity/:export-address
```

`:source-current` is recomputed against the file on disk at call time, so it
answers "is the source still what produced this code" rather than repeating a
value cached at publication. It is nil -- never a silent t -- when the file has
changed, or is missing or unreadable. The identity is recorded only after a
successful CAS: staging records nothing, and a refused publication leaves the
previous record untouched.

**Reproducing this state in a new process.** `:history` holds the last 16
publications as `(:generation :source-sha256 :artifact-sha256 :published-at)`,
which is the recipe: in a new process, recompile each recorded source in order
with `nelisp-native-unit-rebuild-and-reload`, against a binary whose
`:binary-sha256` matches, checking `:artifact-sha256` at each step. Unit IDs
and entry addresses are process-local and deliberately NOT part of that recipe
-- rebuild native callers during setup rather than storing an address.

### Account for what replacement retains

```elisp
(nelisp-native-unit-resources)
;; (:candidates N :units N :retired N :retained-bytes N
;;  :reclaimed-tables N :reclaimed-bytes N :retained-reason STR)
(nelisp-native-unit-reclaim)
;; (:released (...) :refused ((:generation N :reason STR) ...))
```

An unpublished candidate owns a generation table no executing code can reach,
because it was never installed into the unit's control word; discarding one --
explicitly, by TTL expiry, by a staging failure, or by a refused publication --
unmaps it and releases the candidate's mapped artifact. A **published**
generation that is later superseded is kept mapped on purpose: this runtime
exposes no way to observe that calls entered through a stable gate have
returned, so `nelisp-native-unit-reclaim` reports it refused with that reason
rather than guessing.

### Ask which callers a replacement would reach

```elisp
(nelisp-native-callsite-reachability "some_name")
;; :build-declared | :gate-only | :not-replaceable
```

`:build-declared` means existing direct callers inside the running executable
DO observe a replacement, because the name was declared in
`tools/nelisp-replaceable-entries.txt` when this binary was built.
`:gate-only` means only call sites written to go through
`nelisp-native-unit-address` are redirected. `:not-replaceable` means neither.
See [Doc 202](design/202-replaceable-call-sites.org) for what requires a
rebuilt binary and what a live REPL can change.

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

## Qualify native frame primitives

The source-level frame library can be reloaded in the ordinary development
REPL. Its native kind-search and capture counterparts have direct compiled
callers; source reload does not replace those callers. Build a separate reader
fixture to exercise the native primitives with actual Cell values:

```sh
NELISP_STANDALONE_READER_OUTPUT=target/nelisp-native-frame-kinds \
  emacs -Q --batch -L lisp -L src -L scripts \
  -l test/nelisp-native-frame-kind-build.el \
  -f nelisp-native-frame-kind-test-build
NELISP_FRAME_TEST_BIN=target/nelisp-native-frame-kinds \
  python3 test/nelisp-native-frame-kind-test.py
```

The builder installs test-only entry points in that executable. It refuses
the ordinary `target/nelisp` output. The test checks its Cell adapter before
checking capture, kind-specific lookup, old lexical-only frames, nil values,
unwinding, local declaration metadata, and GC. The private `frame-test-declare`
and `frame-test-local-special` entries call the actual native declaration
writer and lookup; combine them with `frame-test-capture` to inspect metadata
without rebuilding for each input. Bare names in a captured environment carry
declarations, while pairs carry value cells. Repeat the Python command against the same fixture for test
edits; rebuild when native source changes. Run compiler/build commands serially
because unit and artifact caches are shared. The fixture is not a distributable
runtime and does not prove automatic classification of `let` or formals.

## Compare an isolated reader with Emacs

After a native change, compare the candidate without replacing `target/nelisp`:

```sh
python3 tools/nelisp-reference-parity.py \
  --binary target/nelisp-candidate --emacs emacs-gtk \
  --prefix target/ai/candidate-parity
```

Use an executable for stock Emacs 30.x; the shared corpus is version-pinned.
The command records the binary and corpus hashes, reference version, full
stdout/stderr, exit codes, generated input, and a JSON result. Both processes
must finish successfully with empty stderr and identical complete nonempty
output. A matching prefix alone is insufficient. `--cases` selects another
corpus and `--timeout` bounds each subprocess. This command does not build a
reader or rerun the full CI matrix. Its process-failure and output controls run
through `test/nelisp-reference-parity-test.py` in the Linux CI lane.

For an individual host ERT failure, reuse the gate runner's selector instead
of re-running every case in the same test file during the repair:

```sh
NELISP_GATE_SELECTOR=nelisp-aot-compiler/frame-stack-find-uses-borrowed-words \
  tools/ai/nelisp-ai.sh test-one test/nelisp-aot-compiler-test.el
```

This writes an `ert-focus` report. Keep the selector scoped to this command;
a focused pass is not a full-suite result. Clear it before running
`tools/ai/nelisp-ai.sh test`. For borrowed frame lookups, the allocation-free
contract includes the identity comparison helper as well as the bucket walk.

Use the focused public symbol-identity acceptance suite when changing symbol
representation:

```sh
NELISP_BIN=target/nelisp-candidate EMACS=emacs-gtk \
  python3 test/nelisp-symbol-identity-test.py
```

It batches independent observations and reports each failing public contract.
Keep the reference test green; do not suppress native failures or infer full
identity compatibility from keyword classification alone. The Linux CI lane
runs it alongside the variable-declaration suite. It covers names, keyword
status, variable/function/property isolation, lexical capture, eq/equal tables,
CLI display, and image restoration plus fresh identity issuance afterward.
This does not qualify separate obarrays, all printing/read-syntax options,
or destructive modification of symbol-name strings.
The separate `nl_jit_make_symbol` trampoline shares the reader's tag-16
allocator and identity issuer. Qualify its actual native implementation with
the private fixture:

```sh
NELISP_STANDALONE_READER_OUTPUT=target/nelisp-symbol-fixture \
  emacs -Q --batch -L lisp -L src -L scripts \
  -l test/nelisp-native-symbol-build.el -f nelisp-native-symbol-test-build
NELISP_SYMBOL_TEST_BIN=target/nelisp-symbol-fixture EMACS=emacs-gtk \
  python3 test/nelisp-native-symbol-test.py
```

The fixture invokes the canonical C-ABI source through private native adapters.
It checks inline and boxed multibyte/unibyte names, mixed reader/JIT identity
issuance, and rejection without changing the output slot. Boxed-name tests
expose the truncation caused by using inline `str-len` on tags 6/15. Reuse the
fixture for input-only changes; rebuild after changing compiled native code.
This is a separate executable build, not native hot publication. It does not
qualify loading the entire legacy `nelisp-jit-strategy.el` Lisp bridge.

Source fast-hash tables and native frame/mirror code use the same UTF-8 byte
hash for multibyte names, and raw bytes for unibyte names. Older source tables
hashed Unicode character codes instead. After reloading the corrected source,
explicitly call `(nelisp--fast-hash-rehash! table)` on an affected table before
sharing it with native lookups. A frame's table is `(nelisp--record-ref frame 0)`.
This preserves the table and stored values; it does not migrate every live
table automatically. Duplicate keys or inconsistent counts abort before
publishing new buckets, so mixed legacy tables need inspection if repair fails.
The native symbol fixture checks source-to-native and native-to-source frame
access for ASCII, accented, Japanese, and supplementary Unicode names.

For generated-symbol hash-table cost, keep distinct before/after readers and
finish other builds/tests before running:

```sh
python3 tools/nelisp-symbol-index-bench.py \
  --before target/nelisp-before --after target/nelisp-after \
  --keys 1000 --repeat 3 --output target/ai/symbol-index-timing.json
```

The command inserts fresh symbols into an eq table, reads every value back,
checks the table count and checksum, and alternates measurement order. Its
JSON records input, expected output, executable hashes and paired wall times,
including startup and setup. It rejects identical executables, unexpected
output/stderr, and executables changed during measurement. A successful run
means the measurements are valid, not necessarily that performance improved.
