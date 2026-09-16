# Strategy implementation and acceptance map

This document preserves the scope of [NeLisp Strategy](nelisp-strategy.org).
It is an implementation plan, not a worklog or a completion declaration.
The strategy itself remains authoritative. A subsection's examples, named
artifacts, required commands, and exit criteria must be checked separately
before that subsection can be considered complete.

## Dependency order

1. Make the project workflow reliable: commands, diagnostics, project inputs,
   reproducible artifacts, installation, compatibility tests, and short feedback
   loops. Do not equate a checkout-only Python frontend with a self-hosted CLI,
   or an embedded reader executable with AOT application compilation.
2. Package manifests, resolution, lock/cache integrity, registry API, official
   libraries. These establish inputs shared by builds, documentation, and editors.
3. Source-aware development protocol, LSP/editor integration, debugger, and
   documentation. Reuse proven runtime inspection/publication facilities.
4. Concurrency, foreign-library integration, capability enforcement, and service
   operation. Build the agent/MCP application on those explicit contracts.
5. Flagship products, platform qualification, benchmark publication, ecosystem,
   and stability/release commitments. External adoption is not provable merely
   by adding code or declaring a milestone shipped.

This order changes implementation scheduling, not the requested end state.
The whole strategy remains open while any requirement lacks sufficient evidence.

## Requirement families and acceptance evidence

| Strategy sections | Required outcome | Evidence needed before completion |
|---|---|---|
| 1–2, 15, 22.1 | Native, interactive, self-hosted platform positioning; coherent branding and user-facing paths | Implemented workflow matches public claims; homepage/install/CLI review; no internal stage names in marketing |
| 3.1 | Published compatibility tiers | Executed compatibility corpus, named supported libraries/shims and explicit unsupported editor APIs |
| 3.2–3.3 | Modern modules/namespaces, matching/data/error facilities, concurrency/types/foreign/immutable/package facilities without losing Lisp semantics | Individual language designs, compatibility impact, executable examples, macro/reflection/live-redefinition tests; candidates need explicit disposition |
| 4.1 | Native independent executable workflow, build modes, reproducibility, cross-platform distribution | Clean-install and isolated-executable tests on supported platforms; distinguish evaluated and compiled application code |
| 4.2 | Function/variable/stack/heap inspection, dynamic loading/live patching, REPL attachment | Real running-process inspection and replacement with preserved state, failure containment, and limits |
| 4.3 | Reader/parser/macros/compiler/optimizer/writer/package/build/docs/test self-hosting | Bootstrap chain and commands executing without host Emacs/Python; each component qualified independently |
| 4.4 | Correct GC with diagnostics and latency/throughput/memory evidence | Allocator/collector correctness and long-running tests, live metrics, reproducible latency/memory measurements |
| 5 | Tasks/spawn/await/channels/select/parallel-map/cancellation/structured scopes | User-facing examples with real concurrency, cleanup, error propagation, and deterministic contract tests |
| 6 | `new run build test fmt repl add remove update doc check clean bench`; `nelisp.toml`, `nelisp.lock`; dev/release/debug/profile | Every command's successful and failing workflows, project layout, profile behavior, reproducibility and dependency integrity |
| 7 | Resolver/cache/registry and official package set | Version/ownership/dependencies/docs/advisories/metrics/hashes/yanking/search/API; real install/update/remove/lock tests; all 19 named initial libraries qualified |
| 8 | Simple C ABI FFI and header bindgen, then other bridges | Real external-library calls, generated bindings and ABI/error/resource tests; retain repository FFI boundary policy |
| 9 | All listed standard modules with stability labels | Public API inventory linked to tests and documentation; actual standalone workflows rather than feature-name presence |
| 10 | Deterministic formatter, full test-runner facilities, source-oriented diagnostics, debugger | Formatting and semantic preservation; unit/integration/property/filter/parallel/coverage/machine output; locations and debugger step/locals/attach/tasks |
| 11 | LSP and editor integrations led by VS Code | Protocol tests and real client qualification for completion/definition/references/rename/hover/signatures/diagnostics/tokens/format/actions/symbols; test/debug/package UI |
| 12 | Agents/tools/schemas/providers/state/MCP and controlled runtime mutation | Usable agent examples and provider contracts; transactional publication/rollback/audit/signature/policy/capability tests |
| 13 | HTTP1/2, TLS, WebSocket, routing/middleware/JSON/streaming, shutdown/logging/metrics/async | Real service and deployment tests, adverse network/resource behavior, standalone release build |
| 14 | NeLisp Book, searchable generated HTML API docs, language/compiler/runtime/FFI/package/compatibility/ABI references | All named chapters/references available and runnable; source-to-doc command and search verified |
| 16 | Public reproducible benchmark suite and performance goals | Startup/calls/allocator/GC/strings/maps/I/O/HTTP/concurrency/numerical/compiler/agent results, valid baselines and named artifacts across listed comparisons |
| 17 | Explicit capabilities and sandbox/resource isolation | Filesystem/network/process/env/FFI/package/mutation authority tests; selected sandbox design and adversarial isolation/resource tests |
| 18 | Governance/RFC/security/release roles and stability policy | Adopted policies and operational ownership; migrations and core/package/ABI compatibility checks; do not invent maintainers or commitments |
| 19 | Flagship apps, demonstrations, community infrastructure | Working products and all named demos; actual hosted discussions/docs/registry/blog/release/roadmap channels, with publication authorization when required |
| 20 | All five roadmap phases and their exit criteria | New-user timed onboarding; production CLI/service without Emacs; useful native agents; external adoption; full 1.0 stability requirements |
| 21–22 | Priorities and five immediate work streams | Positioning + unified CLI + package management + LSP + exceptional native agent; no one stream substitutes for all five |
| 23–24 | Product/adoption metrics and inspectable/transformable live systems | Real measurements with dates and provenance; external usage evidence; runtime behavior supporting the stated principles |

## Current implementation boundary

Native variable references and `symbol-value` reject a function-only entry's
internal unbound marker with `void-variable`. A nil value remains bound, and a
local value can shadow an unbound global value cell. The standalone declaration
suite and installed-toolchain probe cover these cases.
The source-level frame library now accepts explicit binding kinds, searches
lexical and dynamic cells independently, and excludes dynamic cells from
closure capture while preserving older lexical-only frames. Emacs/native
fixtures exercise explicit frames, unwinding, and GC. Native kind-specific
search and both filtered and unfiltered capture now honor explicit metadata,
with direct Cell-based acceptance tests in a private reader fixture. Native
`let` now classifies global and local special declarations at binding time.
Ordinary reads and `setq` prefer visible lexical cells; `boundp`, `symbol-value`,
and `set` use dynamic cells followed by global values. Native `defvar`
initializers fill an unbound global default when the current dynamic
value is bound. If the current dynamic value is void, they initialize that
binding instead, preserving outer values through unwinding. Initializer errors
and collection during initialization are covered by reference tests.
Native `defconst` declares the variable after successful initialization and
updates the current dynamic value. A lexical local remains unchanged, and
unwinding restores outer dynamic values. Ordinary declared constants remain
assignable, while nil, t, and keyword value changes are rejected.
The declaration suite covers closure capture, setters, GC, and unwinding;
installed-toolchain probes also exercise dynamic setters and closure capture.

Function calls now mark their outermost lexical frame as a scope boundary.
Lexical lookup and both native capture walkers include that frame and stop
before caller frames; dynamic lookup continues across the boundary. A captured
environment remains visible, and popping the callee restores caller lookup.
Source-frame and native fixtures cover the boundary, capture, GC, and unwinding;
the declaration suite and installed-toolchain probe exercise actual calls.

Sequential `let*` bindings now use separate frames, preserving earlier cells
captured by closures and allowing lexical and dynamic bindings of the same
name to coexist. Initializer and body failures unwind all frames introduced
by the form. Empty bodies return nil. Declaration tests, the parity corpus,
and the installed-toolchain probe cover sequential rebinding.

No-initializer `defvar` declarations now affect their lexical environment
without binding a value or setting the global `special-variable-p` flag.
Empty and dynamic-only lets share the enclosing declaration scope. Captured
environments carry a snapshot of declarations, including names bound inside
the closure body; unrelated callees do not inherit them. Top-level evaluation
has a root scope, and file/source-string loading keeps one scope across its
forms while isolating nested loads. Native/source fixtures, lexical-file
reference tests, and a runtime-image round trip qualify these paths.

Variable semantics remain incomplete: dynamic `eval` mode, explicit lexical
environment arguments, and file dialect selection still need implementation.
Public macro expansion preserves native `defvar` and `defconst` forms instead
of exposing their bootstrap fallback macros. Evaluating an expanded `defvar`
therefore retains native initialization semantics under dynamic locals.
Explicit macro environments and user replacement definitions still expand.
`makunbound` now voids dynamic/global value cells while retaining function
definitions, properties, and declaration metadata. Lexical cells remain
unchanged; voiding a dynamic local masks the global value until unwinding.
Nil, t, and interned keywords are rejected as constants. Colon-prefixed
uninterned symbols now classify like Emacs: `keywordp`, `boundp`,
`symbol-value`, and `makunbound` treat a `make-symbol`-created symbol as an
ordinary, non-interned symbol rather than a keyword, even when its name
starts with `:`.
`make-symbol` now returns a symbol whose name carries no synthetic identity
encoding: `symbol-name` and printing show the plain requested name, and
`intern-soft` does not treat the generated symbol as interned. The focused
`test/nelisp-symbol-identity-test.py` acceptance suite compares public names,
intern status, value access, and separate lexical/hash-table identities with
Emacs, and now passes on the native reader.
A representation change must preserve public names and intern status through
lookup, capture, GC and runtime images. The inline Symbol word at offset 8 is
string capacity, not unused space for an identity flag.
The source frame library now preserves symbols outside the default obarray as
identity keys, including capture/restore and local declarations. Its hash API
has an explicit symbol-key mode; the default string-key contract remains.
Identity-bearing frames use source capture instead of the existing native
name-comparison helper. The reader's own `make-symbol`/`intern-soft`
representation is now repaired as well, so classification agrees at both the
source-frame and reader levels.
Formal arguments retain lexical classification; they are not part of the new
declared-special `let` path. These limits prevent a claim of complete lexical
and dynamic binding compatibility.

The [package resolution contract](package-resolution.md) defines the initial
offline version solver and deterministic lock encoding. CLI update/fetch and
shared runtime dependency loading are now connected, with HTTPS artifact
acquisition, verified cache publication, and isolated application tests.
Add/remove now edit ordinary dependency tables and preserve existing pins.
Explicit HTTPS registry selection, name search, and URL-bound offline index
snapshots are connected and tested. Official registry operation, publisher
authentication/signed indexes, general TOML editing, crash recovery, richer package formats,
and self-hosting remain open; these contracts do not establish package-manager
completion.

Family 8 (foreign function interface) now has a declarative surface,
`packages/nl-ffi`: `ffi:library` and `ffi:defun` sit on top of the reader's
existing `nl-ffi-call` builtin (already exercised directly by
`standalone-reader-ffi-smoke` against libc, libm, SQLite, GnuTLS, and
FreeType). `nl-ffi-call` is not a `dlopen`/`dlsym` loader: it dispatches
through one fixed, build-time table of `(symbol, soname, arity, signature)`
rows already compiled into `scripts/nelisp-standalone-build.el`, resolved by
the OS loader when the process starts, not by anything Lisp calls at
runtime. `ffi:defun` can therefore give a typed, converting front end to a
symbol the running binary already imports, and cannot make a new C symbol
callable; `ffi:library` takes a SONAME spelled as the table spells it,
validates it against the table's own known SONAME set, and confirms
`nl-ffi-call` exists at all, and does not open anything, since resolution
is global and fixed rather than per-library. `ffi:defun` converts `:pointer` arguments (an
address, nil, or a Lisp string copied into a scratch buffer for the call)
and detects an unresolved symbol from the interpreter's own
nil-versus-boxed-number convention, signalling one of four named
conditions (`nl-ffi-wrong-arity`, `nl-ffi-unknown-type`,
`nl-ffi-unresolved-symbol`, `nl-ffi-unavailable`) instead of returning a
silent nil. `nl-ffi-call`, and therefore this whole surface, exists only in
the dynamic reader (`NELISP_READER_DYNAMIC=1`); calling an `ffi:defun`
wrapper on the default static reader signals `nl-ffi-unavailable` rather
than crashing, but no C symbol is callable there. This does not cover
closures/callbacks, struct layout, variadic C calls, or section 8.2's
header bindgen. The type vocabulary and vector call shape (`[RET ARG...]`,
return type first) follow the existing `dev/nelisp-ffi` fork of elisp-ffi
rather than section 8.1's illustrative labeled-argument sketch, since that
shape is what an already-fixed, per-symbol ABI table can actually be
checked against; see `packages/nl-ffi/README.org` for the full contract
and its "Symbol resolution" section for why `ffi:library` cannot do more
than this. This is step 1 of a staged rollout: a later step can add
`dlopen`/`dlsym` rows to the same table and call the resolved address
through `ptr-call` so a new binding stops needing a reader rebuild, and a
further step can grow `dev/nelisp-ffi/nelisp-ffi-pure.el` (a pure-elisp
ELF symbol reader, leaf functions only today) toward giving the default
statically linked reader the same reach; neither changes `ffi:library`'s
SONAME argument or `ffi:defun`'s signature vector, and the default binary
stays statically linked either way.

The project frontend in `tools/nelisp-project.py` currently provides a partial
Phase 1 workflow. Its integration suites and [project guide](project-cli.md)
describe actual behavior. It still relies on Python for orchestration and host
Emacs for builds/formatting/source validation. Linux executable builds embed a
reader and source, with no claim of whole-application AOT. The project REPL is
a live evaluator, not a full debugger or externally attachable process service.

`test --filter` selects literal substrings of registered test names, and
`test --json` reports counts, completion state, and captured output. Zero-match,
startup-failure, duplicate-completion, and process-error contracts are tested,
including the installed workflow. Parallel execution, property generation,
coverage, and richer per-test reporting remain open.

The standalone tarball builder's Linux `--project-cli` option now packages this
frontend and compiler sources with a runtime under `libexec/nelisp-runtime`.
The local-artifact installer, relocated installation, project workflow, and
isolated generated executable are tested, including project builds from a
read-only installation using a per-user native unit cache. Published project-CLI releases, system
prerequisite installation, self-hosting, other platforms, and first-time user
onboarding remain unqualified.

`doc` now generates searchable standalone HTML and a deterministic API JSON
index from top-level project source declarations, using the development
protocol reader. Source extraction and generation are tested, and browser
filtering is qualified. The NeLisp Book, full public API semantics, dependency
documentation, and self-hosted generation remain open.

The current development milestone includes application arguments for `run` and
Linux executables, plus dev/release/profile/debug builds. Release strips native
symbols; profile measures selected function cells; debug binds declaration maps
to the executable. These are deliberately narrower than optimized application
AOT compilation or a live debugger.

Missing or unverified areas include self-hosted distribution, the complete CLI
semantics, official registry service/publication, full formatter/test facilities,
platform qualification, LSP/debug/editor UI,
and the later platform/product requirements above. Existing repository modules
may cover parts of these requirements, but their presence is not completion
evidence: inspect and run the relevant public paths before reusing their claims.

The initial [stdio language server](language-server.md) connects client-owned
unsaved text to the existing source parser/formatter. It provides syntax
diagnostics, flat document symbols, and formatting, with version-scoped analysis
caches. Protocol and relocated-installation tests do not establish real editor
qualification. The other LSP capabilities and editor integrations in section 11
remain open, as does replacement of the host Emacs/Python tooling dependency.
Declaration completion now retains the reader's complete prefix for unfinished
documents and returns literal documentation/escaped insertion text. Scope,
builtin, dependency, and workspace candidates remain to be connected.
Incremental document synchronization applies ordered UTF-16 ranges atomically
per notification, retaining full-text updates. Unicode/line-ending and installed
workflow tests cover this path; it does not add incremental parser reuse yet.
Source definition/hover now resolves declaration names and known-context call
heads within one open document, preserving namespace and duplicate ambiguity.
Quoted/opaque/local-function contexts are excluded rather than guessed. Variable
references and simple parameters/let/let* bindings now preserve nested scopes
and initializer visibility. More complex binding forms and cross-file navigation
remain open. Navigation caches reuse known token spans across nearby positions.
Same-document `textDocument/references` now uses those source binding rules,
returns token locations with optional declarations, and is exercised through
the installed VS Code language client. One host request shares reader parsing
across matching candidates; results are bounded and cached per document version.
Local lexical rename now
validates the entire enclosing source form, rejects opaque syntax and symbol
collisions, and returns versioned edits through the installed VS Code client.
Global/cross-file rename and external/generated binding analysis remain open;
incomplete knowledge of opaque forms cannot justify rewriting every use.
Signature help now covers plain source parameter lists and unfinished calls,
with literal docs, active arguments, UTF-16 label ranges and version-local
query reuse. Unique external source declarations now supply signatures for
unresolved supported calls; workspace content hashes invalidate cached answers.
Keyword/destructuring, builtin signatures and module visibility remain open.
Workspace symbol search now indexes closed local `.nl`/`.el` files, overlays
open snapshots, tracks folder changes and reuses unchanged content hashes.
Changed files share one host request; declaration plans are also reused by
workspace declaration completion, including closed files and unsaved overlays.
Completion shares those plans without constructing navigation locations.
Simple cursor-local parameter and let/let* candidates now reuse navigation's
scope traversal and a version/context-local query cache. Unique external plain
function declarations permit local completion and nested signature traversal;
macros, duplicates and local callable shadowing remain conservative. General macro/builtin call
contexts and complex binding lists remain open.
This is bounded source discovery, not yet a
cross-file binding graph or dependency index.
Definition requests now use that index for otherwise unresolved tokens in
supported source contexts, preserving namespaces and local declarations while
returning all external source candidates. This does not establish module-load
visibility or extend traversal through unknown macros. Hover now shares these
external candidates and displays literal metadata only when one remains. Broad
declaration results share source line maps instead of rescanning each location.
Cross-file reference candidates now collect supported global occurrences after
checking declaration uniqueness, keeping local bindings separate. Changed files
are reparsed in one batch while unchanged per-file occurrences are reused.
Navigation and reference queries now traverse unique external plain function
arguments. Occurrence caches include callable classification identities;
documentation-only edits retain other files, while macro/duplicate changes
invalidate dependent contexts. Each navigation query uses one workspace snapshot.
Changed-file batches transmit callable providers once, and source reference
queries classify declarations once per file instead of once per matching token.
Completion now reads the toolchain's unconditional base reader builtin catalog
without evaluating the build driver, caches it per server session and lets
source declarations override builtin candidates. Native registration presence
is qualified separately from target functionality and argument contracts;
builtin signatures remain open. Base builtin classifications now permit argument
traversal in completion/navigation/references and nested source signatures, with
authored callables overriding the fallback. Builtin metadata is shared per batch.
Builtin-only names now support workspace reference searches through the same
bounded source-context traversal, without synthesizing source definitions.
Exact candidate queries filter name/namespace before constructing locations.
Literal symbol function objects (`#'name` / `(function name)`) now preserve
function-namespace navigation and references without reading ordinary quoted
data as code. Computed function objects, macro-generated references, unknown call arguments and a
complete binding graph remain open.

The [VS Code extension](../editors/vscode/README.md) now supplies the initial
real-editor path: `.nl` highlighting, the source server's current LSP features,
inline diagnostics, CLI tasks, and a persistent project REPL terminal. A real
Linux VS Code extension-host suite covers unsaved source and task/terminal
execution; TextMate tokenization and a local VSIX build cover grammar and
packaging. The Testing view now enumerates literal saved test declarations and
supports exact individual runs, exclusions, results, and process cancellation.
Selected tests in each project now share one runtime process in registration
order; batch case records are reconciled before displaying individual results.
Paired boundaries now attribute stdout and failure details to each test while
retaining setup/summary output and the compatible full batch log.
Dynamic test enumeration, coverage, and continuous/debug test profiles remain open.
Section 11.3 remains partial: the debug adapter,
package publication/authentication, Marketplace publication, and other platforms are not yet
qualified. The CLI/server still depend on the separately installed toolchain.
Package search/add/remove now have editor dialogs and real CLI task execution;
fetch/update share explicit registry/local-index/offline settings. Local and
URL-bound offline snapshots, cancellation and preserved manifest/lock failures
are covered in the real extension host. Public registry operation remains open.
