# Project CLI: first development milestone

The project frontend supports `new`, `run`, `test`, `build`, `fmt`, `repl`,
`check`, `clean`, `update`, `fetch`, `add`, `remove`, `search`, `doc`, `bench`, and
`debug-info`. Application
code and tests execute in the standalone NeLisp runtime. The frontend needs
Python 3.11 or newer (standard library only); host Emacs is needed to build
the runtime, executable builds, formatting, checking, documentation, package syntax validation,
and REPL syntax validation,
not for `new`, `run`, or `test`. This is not yet a self-hosted CLI
or completion of Strategy Phase 1. A Linux bundle can also
install these commands outside the source checkout, as described below.

The reviewable milestone is the Linux project workflow from a checkout or bundle. The
package, documentation, benchmark, and build-mode commands share that workflow
but also cover early parts of later strategy phases. Phase 1's cross-platform
installation and ten-minute first-time onboarding exit criterion remain open.

## Install the Linux project bundle

The existing standalone tarball builder accepts `--project-cli` for Linux
x86_64. This packages the current frontend, runtime, compiler sources, and
documentation together; it does not bundle Python or host Emacs. Build this
artifact from a checkout, then install it using the existing local-artifact
installer:

```sh
bash tools/build-standalone-tarball.sh --project-cli
nelisp_prefix="${XDG_DATA_HOME:-$HOME/.local/share}/nelisp/$(cat VERSION)"
bash release/stage-d-v3.0/install-v3.sh --from dist \
  --version "$(cat VERSION)" --prefix "$nelisp_prefix"
export PATH="$nelisp_prefix/bin:$PATH"
nelisp --version
nelisp new hello
cd hello
nelisp run
nelisp test
nelisp build
./target/hello
```

The archive retains the existing `anvil-VERSION-linux-x86_64.tar.gz` naming
contract. This option is not yet the default release configuration, and no
published project-CLI download is implied. The archive includes the current
standalone installer as `install.sh`, replacing the obsolete host-Anvil installer
that was previously copied there. Recipients need the archive and its adjacent
SHA-256 file for `install.sh --from DIRECTORY --version VERSION --prefix PREFIX`.

Installed project builds use `$XDG_CACHE_HOME/nelisp/build-v1/` (default
`~/.cache/nelisp/build-v1/`) for native units, so the installation can be
read-only. `XDG_CACHE_HOME` must be absolute; relative values fall back to the
default. Set `NELISP_BUILD_CACHE` to override the cache root, including for
source checkouts, which otherwise retain their existing `target/standalone-units`
cache. Entries are separated by toolchain location as well as the backend's
existing target/content keys. Moving an installation may therefore require a
cold build, and this does not add concurrent-writer support within one cache.
Application outputs remain in the project's own `target/`.
The installation can be moved as a directory; the
frontend resolves `libexec/nelisp-runtime` and support sources relative to its
own location. `NELISP_BIN` still takes precedence when explicitly set.
`run` and `test` use the bundled runtime without host Emacs. `build`, `fmt`,
`check`, `doc`, and source validation still need host Emacs. This is an installed
development toolchain, not self-hosted distribution or cross-platform qualification.

`python3 test/nelisp-project-distribution-test.py` builds an actual archive,
runs the existing tarball verifier, installs and relocates it, makes the prefix
read-only, exercises project commands and cold/warm builds, and deploys the
generated executable alone. Run this permission test as a non-root user.
It does not time the
installation of system prerequisites or prove first-time user onboarding.
The bundled `tools/ai` development tools retain their own working-directory
requirements; the read-only contract here covers project commands.

From the repository root, build the runtime once and put the checkout's
frontend on PATH:

```sh
make standalone-reader
export PATH="$PWD/bin:$PATH"
nelisp new hello
cd hello
nelisp run
nelisp test
nelisp build
./target/hello
nelisp fmt --check
nelisp check --json
nelisp doc
nelisp repl
```

`run` prints `Hello, world!`; `test` reports one passing test. Edit the
greeting in `src/main.nl`, then repeat both commands: the output changes
and the original test fails. No runtime rebuild or compiler bundle generation
is needed after application edits.

The POSIX launcher uses `PYTHON` (default `python3`). The Windows launcher
`bin/nelisp.cmd` uses `PYTHON` (default `python`). Windows and macOS execution
have not been qualified for this milestone. `NELISP_BIN` selects a runtime
path or an executable on PATH; otherwise the checkout's host-specific
`target/nelisp` or `target/nelisp.exe` is used. `new` needs no runtime.
The existing `--eval`, `--load`, and `--repl` entry points are forwarded to
the selected runtime without project discovery. `nelisp --version` reports
the project frontend's checkout `VERSION`, even before a runtime is built.
It does not identify an independently selected `NELISP_BIN` artifact.
The POSIX launcher also handles this single flag before starting Python, so
installation identity can be checked before Python is installed. Other project
commands, and the Windows launcher, still require Python.

## Project contract

```text
hello/
  nelisp.toml
  .gitignore
  src/main.nl
  test/main-test.el
```

```toml
[package]
name = "hello"
version = "0.1.0"

[application]
source = "src/main.nl"
entry = "main"
```

`new` accepts a lowercase name matching `[a-z][a-z0-9-]*` and refuses an
existing destination, including an empty directory. `run` and `test` find
the nearest `nelisp.toml`, including from a project subdirectory, and use
its directory as the runtime working directory. `source` must resolve to a
file inside that directory. `entry` names a zero-argument function.

Application arguments are available in the Emacs-compatible
`command-line-args-left` variable before the source is evaluated:

```sh
nelisp run -- --output "file with spaces" 日本語
./target/hello --output "file with spaces" 日本語
```

The frontend removes its first `--` separator; later `--` values are application
data. Empty strings, quotes, newlines, and UTF-8 arguments are preserved without
constructing Lisp code from them. The entry can consume the list with `pop` or
ordinary list operations. `test`, `repl`, and `bench` initialize it to an empty
list; those commands do not yet accept application arguments. The complete
Emacs startup/option-processing behavior and `command-line-args` are not provided
by this frontend.

This path requires a runtime rebuilt with the source-argument separator support.
At the low level, `target/nelisp --load FILE -- ARG...` and
`target/nelisp --eval EXPR -- ARG...` retain the arguments in
`nelisp-standalone-argv`; extra arguments without `--` remain usage errors.

The v0 manifest accepts the four fields above and an optional `[dependencies]`
table mapping package names to version requirements. Other unsupported fields
are rejected rather than silently ignored. Package
versions currently use `MAJOR.MINOR.PATCH`. New projects use `src/main.nl` as
specified by the strategy. Existing manifests naming `.el` sources remain valid.
Both extensions carry Lisp source; this adds no new language syntax.

For dependencies, use `nelisp add NAME --index FILE` to edit an ordinary
dependency table, or `nelisp update --index FILE` with a trusted index snapshot
to produce `nelisp.lock`, then `nelisp fetch --offline` to verify its cache.
Alternatively, set `NELISP_REGISTRY` to an HTTPS index URL and use
`nelisp search NAME`, `nelisp add NAME`, and `nelisp update` without a local
index file. Run/test/REPL/build consume the resulting lock offline. See the
[package workflow and index format](package-resolution.md) for the complete
current contract and limits.

`run` evaluates the source and calls the entry function. `test` evaluates the
source without calling the entry, then registers files matching
`test/*-test.el` and `test/*-test.nl` in lexical filename order. Keep application startup effects
inside the entry function. Both commands read current source on every call.

Tests use the existing `scripts/nelisp-ert-shim.el` subset: `ert-deftest`,
`should`, `should-not`, and `should-error`. This is not full GNU Emacs ERT;
its metadata such as `:tags` and `:expected-result` is not implemented.
Use ordinary tests without expected-failure or selection metadata.
The CLI requires a nonzero passing count, zero failures, a completion record,
a successful runtime exit, and empty runtime stderr. No test files, zero
registered tests, premature `(exit 0)`, startup errors, and failed assertions
all fail the command. Tests share one process and registration follows the
shim's existing duplicate-name replacement behavior.

Use `nelisp test --filter greeting` to run only test names containing `greeting`.
The filter is a literal, case-sensitive substring, not a regular expression or
filename pattern; an empty substring selects all tests. All test files still
load to register their definitions, so filtering does not suppress top-level
setup or syntax/startup failures. Zero matching tests is a failure.

`nelisp test --filter greeting --json` emits one JSON object. It includes
`schema_version: 1`, `scope: "standalone-ert"`, the filter, pass/fail/total
counts, the number of completion records, the child process exit code, and
captured text `stdout`/`stderr`. Captured application output does not corrupt the
JSON stream. Counts are null when no unique completion record can be trusted.
Status is `passed`, `failed`, `no-tests`, `incomplete`, or `error` (input/tooling
failure). CLI exit codes remain 0 for success, 1 for failed/incomplete/empty
runs, and 2 for input/tooling errors. Argument parsing errors and interrupts
retain the ordinary CLI diagnostics/exit behavior. This summary is not yet
per-test timing, coverage, property generation, or parallel execution.

Source is bundled into a temporary file and evaluated by the runtime's
top-level `--load` command, not evaluated in Python. Diagnostics currently
refer to this bundle; mapping them back to individual project files is open.
Locked package sources load before application source. Relative explicit loads
run from the project root. This is a local execution tool, not a sandbox.

## Validation and next milestones

### Formatting

`nelisp fmt` indents the manifest's source and `.el`/`.nl` files recursively under
`src/` and `test/`, using a clean host Emacs Lisp mode with spaces for indentation.
It is an indentation formatter, not a pretty-printer that rewrites every form
onto a canonical set of lines. Style follows the installed host Emacs version;
pin that version in CI when checking formatting across machines.

`nelisp fmt --check` reports files that would change, exits 1 when changes are
needed, and writes nothing. `fmt` returns 0 after applying changes. All snapshots
must parse, and the forms read before and after indentation must compare equal.
No project code, file-local variables, or project configuration is evaluated.
Invalid syntax anywhere rejects the batch before any file is written. The tool
checks snapshots again before publishing and preserves file modes. Each changed
file is replaced atomically; a filesystem failure during publication can leave a
partly applied multi-file batch. Inputs are UTF-8. Text outside the parsed Lisp
data (comments and indentation) may change; string data must not change.

### Project REPL

`nelisp repl` finds the project, validates its source as data with host Emacs,
then evaluates a snapshot in one standalone process without calling the entry
function. Top-level source effects still run: put application startup inside
`main`. A successful startup acknowledgement is required before stdin is
forwarded. Syntax errors, startup errors, early exit, or the startup deadline
fail the command. The default deadline is 30 seconds; override it with
`--startup-timeout SECONDS` (greater than 0, at most 3600).

```elisp
(setq saved 17)
(greeting)
(defun greeting () "Changed while running")
(greeting)
saved
(exit 0)
```

The two greeting calls return different strings and `saved` remains 17.
Recoverable evaluation errors are printed to stderr and the live REPL continues.
EOF closes the session; Ctrl-C terminates it. The frontend forwards input/output
with bounded nonblocking pipes, with no input-copy helper process or background
thread. Source loading is acknowledged before user input can execute. This v0
frontend currently requires POSIX and is qualified on Linux; it has no line
editing or prompt, automatic source-file reload, or full artifact-runtime preload.
Enter complete forms on individual lines. For compiled-source reload and native
publication tools, use the separate [development REPL guide](repl-development.md).

### Checking and cleaning

`nelisp check` validates the manifest and checks Lisp syntax in the manifest
source and `.el`/`.nl` files recursively under `src/` and `test/`. It does not
execute project forms, check whether called functions exist, type-check,
compile, or run tests. The output explicitly labels this scope as `syntax`.
One first syntax error per invalid file is reported; checking continues with
the other files. Exit codes are 0 for syntactically valid input, 1 for syntax
diagnostics, and 2 when input or the checking environment is unavailable.

`nelisp check --json` returns one JSON report with `schema_version: 1`, status,
scope, checked-file count, input SHA-256 values and diagnostics. Paths are
relative to the project root; lines and columns are one-based Unicode codepoint
positions (not byte offsets, terminal display columns, or LSP UTF-16 positions).
Positions come from the host reader and describe syntax errors, not native
runtime stack locations. Reports describe the captured snapshots; consumers
should compare source hashes before applying diagnostics to subsequently edited
files. Environment/manifest failures also return a structured error report,
with zero checked files and unknown source positions. CLI argument-usage errors
remain argparse's ordinary stderr usage messages.

`nelisp clean` removes this package's generated `target/doc/index.html` and
`target/doc/api.json`, plus its named executable, `.build.json`, and
`.build.log` from `target/`, `target/release/`, `target/profile/`, and `target/debug/`.
The debug build's `<name>.debug.json` is also removed.
Other files, source files, and the checkout's shared
runtime-unit caches are retained. This avoids turning a routine cleanup into a
full runtime rebuild. It works without a runtime or existing source file but
requires a valid project manifest. A target directory resolving outside the
project is rejected.

### Source documentation

`nelisp doc` writes `target/doc/index.html` and `target/doc/api.json`.
Open the HTML file directly in a browser: it contains its own style and search
logic, with no network requests or external assets. Search filters names,
signatures, documentation, and source paths. Docstrings are escaped as plain
text, including strings containing HTML tags.

The command reads the application source and `.el`/`.nl` files under `src/` in
one host Emacs process. Test files and cached dependencies are excluded.
The standalone runtime is not required. It reuses the development protocol's
nonexecuting reader and source positions via `nelisp-dev-source-symbols`.
Supported top-level declarations are `defun`, `defmacro`, `cl-defun`, `defvar`,
`defconst`, and `defcustom`. It lists private names too. Definitions hidden
inside wrappers, macros, generated code, aliases, and runtime redefinitions are
not inferred or evaluated. A file with no supported declarations is valid.

The JSON index records the scope, package, project-relative paths, raw source
SHA-256 hashes, signatures, literal documentation, and one-based Unicode
positions with zero-based UTF-8 byte offsets. Duplicate declarations remain
separate source records. These are source declarations, not proof of loaded
runtime definitions, inferred types, or complete public API visibility.

Identical snapshots produce identical output. A syntax failure occurs before
publication and preserves existing output. Each output file is replaced
atomically, but the HTML/JSON pair is not a cross-file transaction.
The command does not generate the NeLisp Book, expand documentation markup,
embed dependency docs, or implement the full self-hosted documentation system.
`python3 test/nelisp-project-doc-test.py` exercises the generation contract;
the generated search UI was also checked in a real browser.

### Executable build contract (Linux x86_64)

`nelisp build` produces `target/<package-name>`, a standalone ELF containing
the native reader/runtime and the application's source. The source is evaluated
by that reader: this is **not** AOT compilation of every application function.
The executable needs neither Python, Emacs, a separate NeLisp binary, nor the
source checkout to start. Files or libraries explicitly opened by the application
remain application dependencies; the builder embeds the manifest's source file
and every locked package source, in dependency order. Executable arguments are
forwarded to `command-line-args-left` in every build mode; cross-compilation remains open.

The default `dev` build retains native symbols. `nelisp build --release` writes
`target/release/<package-name>` and adjacent metadata/logs, preserving the dev
artifact. Release linking resolves all references before emitting only the entry
symbol in the ELF symbol table. This reduces distribution metadata while keeping
the same runtime code, Lisp semantics, and interpreted application source. It
does not remove runtime Lisp names/docstrings, generate additional optimized
application machine code, or provide secrecy for the embedded source.
The ELF entry and null symbol remain; this is not a completely stripped ELF.
Application AOT optimization remains open.

`nelisp build --debug` writes `target/debug/<package-name>` with native symbols
and an adjacent `<package-name>.debug.json`. This file stores the exact source
snapshots used by the build, including locked dependencies, with their hashes
and top-level declaration spans from the existing development-protocol reader.
Paths are project-relative or logical package identities, not build-machine
paths. The executable embeds the source map's SHA-256 in read-only data; the map
envelope records the executable's SHA-256 too. These identify a matching pair,
not publisher authenticity. Keep source-bearing debug artifacts accordingly.

```sh
nelisp build --debug
nelisp debug-info main
nelisp debug-info --binary ./app main --json
```

`debug-info` verifies the executable, embedded map digest, envelope, source
hashes, and selected spans before displaying anything. It needs no running
NeLisp, host Emacs, original project, or original source files when `--binary`
is supplied. Move `<name>.debug.json` with the executable and rename it to
`<new-executable-name>.debug.json` if the executable is renamed. Without a
symbol argument it lists all recorded declarations; duplicate declarations
remain separate instead of guessing which definition executed. A missing name,
map mismatch, or modified executable fails the command.

This is source-declaration inspection, not DWARF/native-PC mapping or an exact
failing-expression location. The map is a build snapshot and does not track
later runtime redefinitions. Breakpoints, stepping, live locals, and process
attachment remain debugger work; `debug-info` does not execute the binary.
Artifact files are each published atomically, not as one multi-file transaction;
an interrupted publication can leave a mismatching pair, which inspection rejects.
`python3 test/nelisp-project-debug-test.py` exercises relocation, Unicode spans,
reproducibility, source-free inspection, no host execution, and tamper detection.

`nelisp build --profile` writes `target/profile/<package-name>`, retaining native
symbols and embedding the profiling support library. It wraps the entry and
direct top-level `defun`/`cl-defun` declarations from the application/dependency
bundle for the duration of the entry call. Each wrapper records invocation and
normal-completion counts and inclusive elapsed microseconds. Return values and
Lisp errors propagate; original function cells are restored after the run unless
the application replaced them. Names used by the instrumentation itself cannot
be profiled. `--release`, `--profile`, and `--debug` are mutually exclusive.

The executable emits one stderr line beginning `NELISP_PROFILE_V1 ` followed by
JSON. Application stdout is unchanged. A Lisp error produces `status: "error"`
with partial counters before the ordinary error diagnostic. Explicit process
exit, a native crash, or a failure before entry instrumentation may produce no
report. `completed` counts distinguish returning calls from unwound calls.

This is function-cell instrumentation, not statistical sampling or CPU-time
measurement. Startup/source loading, macro bodies, definitions hidden inside
macros/other forms, new definitions created during the entry, saved function
objects, and native direct calls are outside its coverage. Nested/recursive
inclusive times overlap and must not be summed as application time. Wrappers
also add overhead, especially to short functions. The clock is the runtime's
real-time clock; negative intervals are discarded and counted in
`invalid_intervals`, but forward clock adjustments cannot be distinguished from
elapsed work. Monotonic timing, per-thread attribution, allocation profiling,
and self-time accounting remain future requirements.

`python3 test/nelisp-project-profile-build-test.py` checks an isolated executable,
Unicode function names, recursive counts, unchanged stdout, and partial reports
on an error. The host suite `test/nelisp-project-profile-test.el` also checks
backwards clock readings, return values, restoration, and live redefinition.

The builder uses host Emacs (`EMACS`, default `emacs`) and the existing Lisp
compiler/linker. It reads a source snapshot without evaluating application forms
on the host. Runtime units use the existing content-addressed cache; the adapter
and source data are linked into the application, without overwriting
`target/nelisp` in the checkout or generating its artifact-command cache.
First builds can take longer while runtime cache entries are prepared.

`target/<name>.build.json` records application source, combined bundle, and binary
SHA-256 digests, exact dependency records, entry function,
target, profile, native-symbol policy, and `embedded-reader` execution mode.
`target/<name>.build.log` holds
compiler diagnostics and the count of runtime units recompiled. Syntax and
compiler failures preserve the last executable and its metadata. New executables
are published by an atomic file replacement after successful linking and ELF
validation; the executable and JSON are separate file replacements, so consumers
must compare the recorded binary hash when reading across a concurrent build.

Move just the generated executable to another directory and run it there to
check deployment. The process working directory remains the caller's directory.
Successful entry return exits zero without printing a Lisp return value.
Explicit use of the runtime's legacy `exit` form inside application code retains
the `--eval` runtime behavior; use `nelisp--exit-process` for immediate process
termination. Runtime errors are not build-time errors: this first builder checks
source syntax and linking, and does not execute user code as part of a build.

Run the integration suite against an already built runtime:

```sh
python3 test/nelisp-project-cli-test.py
python3 test/nelisp-project-build-test.py  # slower; builds Linux ELF applications
```

For a focused native runtime check, run
`python3 test/nelisp-builtin-arity-test.py`. It reads the shared fixed-arity
table without executing the prelude and checks the covered native entry points
in one reader process. A separate session checks `car`/`cdr` condition data,
evaluation side effects, and continued execution after handled errors, followed
by unhandled-error exit checks. Set `NELISP_BIN` to qualify an isolated reader
build. This suite does not build or replace the runtime.
The native accessors require exactly one argument; the same contract is checked
through direct calls, `funcall`, `apply`, and explicit builtin values. Other
builtin argument contracts require their own runtime coverage. The shared table
also supplies `func-arity` metadata; 40 matching reader entries now enforce its
fixed counts at dispatch. Entries implemented outside that table and optional/
variadic contracts are not covered by this dispatch guard.
Arity conditions currently name the builtin symbol even when called through
`apply`; Emacs can place a subr object in that condition's function field.

`python3 test/nelisp-special-variables-test.py` checks declaration metadata for
initialized `defvar` and `defconst`, including failed initializers, preserved
existing values, forward declarations, and `special-variable-p`. Both native
declarations and explicit macro expansions register the declaration. The native
`boundp` entry checks global value cells and self-evaluating symbols directly.
Visibility of dynamically bound special variables still needs binding-time
scope metadata; a declaration made later must not reclassify an existing lexical
binding. `symbol-value` now signals `void-variable` for a plain lexical local
instead of returning it, matching Emacs.

For a complete review of this milestone, run the following from the repository
root. The mutation checks temporarily edit source files, so finish `check`
before running the other suites in the same checkout:

```sh
tools/ai/nelisp-ai.sh check
tools/ai/nelisp-ai.sh test
tools/ai/nelisp-ai.sh standalone
for suite in test/nelisp-project-*-test.py test/nelisp-package-*-test.py; do
  python3 "$suite" || exit "$?"
done
python3 test/nelisp-registry-test.py
python3 test/nelisp-lisp-compile-gate-test.py
python3 test/nelisp-mutation-recovery-test.py
tools/ai/nelisp-ai.sh gate project-cli-contract -- make project-cli-contract
tools/ai/nelisp-ai.sh gate lisp-byte-compile -- make lisp-byte-compile
tools/ai/nelisp-ai.sh verify
```

Run the Python files directly: standard unittest discovery ignores these
hyphenated module names and can otherwise leave the intended suites unexecuted.
The final `verify` aggregates reports, including earlier reports from other
tiers; it does not execute missing or stale tests. These commands qualify the
local Linux toolchain, not every CI platform or Emacs version. A complete
mutation sweep can take tens of minutes; keep individual suites as the normal
development loop rather than rebuilding and rerunning every tier per edit.
The `emacs-parity` gate requires an Emacs 30.x reference; if the default Emacs
differs, select an available 30.x executable with `EMACS` when running that gate.

The suite covers the public launcher, generation, execution, source edits,
test failures, zero tests, startup errors, malformed manifests, missing
runtimes, and paths containing spaces and Japanese text. It must fail when
the runtime is absent; it does not silently skip or build one.

For result-validation edits, `make project-cli-contract` runs the host-only
fake-runtime controls without a build. `make gate-mutation-verify
GATE=project-cli-contract` proves that reporting an incomplete test run as
successful makes this check fail. Real-runtime integration is a separate CI
step after the reader has been built.

The build acceptance suite checks isolated execution, byte-identical repeated
builds, zero runtime-unit recompilations on a warm build, Unicode source edits,
and preservation of the previous artifact after a syntax error. It does not
claim Windows/macOS qualification or AOT application performance.

### Benchmarking project invocations

```sh
nelisp bench --samples 10 --warmup 2
nelisp bench --samples 30 --warmup 3 --timeout 10 --json > results.json
```

The POSIX implementation measures wall time from process creation through
source loading, the application entry call, and process exit. It uses the same
offline locked dependency bundle as `run`, captured once before sampling.
It does not rebuild the runtime or application. Each warmup and sample runs
the real entry and its side effects in a fresh process; use a disposable
project/data set when appropriate. There is no per-function microbenchmark,
release executable benchmark, or automatic performance verdict yet.

The JSON report records every measured duration in nanoseconds, min/median/max,
warmup count, timestamp, platform, runtime path/hash, source and bundle hashes,
exact dependencies, entry, and each sample's stdout hash. Application output
is captured separately so it cannot corrupt JSON. Compare matching workload
identities and output expectations, control background load, and keep raw
samples when comparing changes. This command does not certify machine isolation.

Each invocation must return from the entry, emit a generated completion record,
exit successfully, and leave stderr empty. Premature `exit`, an error, timeout,
or a runtime executable change fails the command without a success report.
The owned process group is terminated before the next sample. Samples default
to 10 and warmups to 2 (limits 1..10000 and 0..10000); each run has a finite
timeout, default 30 seconds. Captured stdout over 16 MiB is rejected after the
run. Windows process-tree cleanup and benchmark qualification remain open.

Run `python3 test/nelisp-project-bench-test.py` against the existing runtime to
exercise success, identity metadata, invalid parameters, early exit, runtime
errors, and timeout handling. The CLI integration CI step runs this suite too.

Remaining milestones include live debugger operations, formatter/version policy,
REPL editing facilities, platform qualification, LSP, and a small MCP application.
A `.neln` artifact and an independent native executable are different outputs.
Package resolution and lock files are implemented under the contract linked
above. The Linux project bundle has local installation coverage; publishing it
and porting the frontend to NeLisp remain separate acceptance milestones.
See the implementation map for the full strategy scope.
