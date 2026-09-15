# NeLisp language server

Run `bin/nelisp-lsp --stdio` from a source toolchain, or `nelisp-lsp --stdio`
with the Linux project bundle's `bin/` on PATH. The server requires Python
3.11+ and host Emacs (`EMACS` overrides the executable). It does not require a
running NeLisp application. This initial source adapter is not self-hosted.

The transport follows the [LSP 3.17 base protocol](https://raw.githubusercontent.com/microsoft/language-server-protocol/gh-pages/_specifications/lsp/3.17/specification.md):
UTF-8 JSON-RPC messages with byte-counted `Content-Length` headers on stdio.
stdout contains protocol frames only. Positions use zero-based UTF-16 code
units, as defined in the [official LSP model](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/metaModel/metaModel.json).

The advertised features are incremental text synchronization, declaration completion,
source definition/hover, document symbols, and document formatting. Syntax diagnostics are published on open/change and
cleared on close. A client should send `initialize`, `initialized`, then
`textDocument/didOpen`; changes carry an increasing version and either complete
text or ranged replacements. Finish with `shutdown` and `exit`. Unsupported requests return a
method-not-found error; invalid notifications are logged to stderr without a
response. Invalid JSON with intact framing receives a parse error; ambiguous,
oversized, or truncated framing terminates the connection.

Changes within one notification are applied in order, with each range referring
to the text resulting from earlier changes in that notification, following the
[LSP synchronization contract](https://raw.githubusercontent.com/microsoft/language-server-protocol/gh-pages/_specifications/lsp/3.17/textDocument/didChange.md).
Ranges use UTF-16 columns with LF, CRLF, and CR line endings. Characters past
the line end clamp to that end; lines past the document clamp to its end.
Reversed ranges, split surrogate pairs, inconsistent optional `rangeLength`,
and oversized results are rejected. The whole batch is validated before its
snapshot/version/cache is replaced; a bad later edit does not publish an earlier
edit. Empty change arrays advance the version without changing the text.
Rejected notifications are logged; a later full-text update can resynchronize
the document. The host reader receives original text, including string line
endings, and its scalar positions are converted back to LSP positions.

Document methods analyze open client snapshots, including unsaved and `untitled:`
documents. Workspace symbol search additionally reads bounded local files from
the supplied workspace folders. The server never evaluates application
forms/macros, visits local variables, installs packages, or writes formatting
results. Formatting returns a whole-document edit using the same semantic
preservation check as `nelisp fmt`; it requires `insertSpaces=true` and retains
the existing language indentation policy. The client applies the edit.

Document symbols are a flat `SymbolInformation` list of top-level function,
macro, and variable declarations. Locations span declarations, not exact name
tokens. A syntax-invalid document returns no symbols. This does not claim
binding resolution or macro-generated declarations. Diagnostics report the
first syntax error per document. A missing/failing host parser is reported as
`NELISP-TOOLING`, distinct from `NELISP-SYNTAX`.

`textDocument/completion` returns top-level declarations from open documents and
the bounded workspace source index, including closed files,
including literal argument lists and plaintext documentation. Its separate
reader entry point retains declarations parsed before the first syntax error,
so a trailing unfinished call does not remove earlier candidates. The strict
inspection/formatting APIs still reject invalid syntax. Later declarations
after a reader error and dependencies outside indexed
folders are not currently indexed for completion. Duplicate names of the same
completion kind use the requesting document first, then the last URI in lexical
order; within a file the last parsed declaration wins. Function and variable
candidates remain distinct. Open buffers override disk content, and edits,
deletions, and document closure are reflected on the next request. Completion
shares declaration plans with navigation without constructing source locations.
Simple parameters and `let`/`let*` bindings visible at the cursor are appended
using the same scope traversal as definition lookup. Inner bindings override
same-name outer/global variable candidates. Parallel initializers retain the
outer scope; sequential initializers see earlier bindings. This local query
repairs the cursor prefix privately and caches up to 128 positions per document
version. Strings, comments, quoted data, function-name positions, unsupported
binding syntax and opaque call arguments do not contribute local candidates.
Unique plain functions declared in other indexed files permit argument traversal
for local completion and nested signature queries. A local callable declaration
takes precedence; macros and duplicate external definitions block traversal.
Local completion caches include workspace content identities so changing a
provider from function to macro cannot retain stale local candidates. Scope
queries transmit only callable names and kinds. Non-catalog builtin calls, general macro
scopes, and complete module/runtime binding resolution remain open. Definition,
hover and references use the same explicit external callable context. Navigation
refreshes the workspace once per request and resolves against that snapshot.
Candidates are source information, not a
claim that the corresponding definitions have been loaded.

Completion also includes the unconditional base builtin names registered in the
server toolchain's `scripts/nelisp-standalone-build.el`. The host reads this file
as data, validates the literal registration list, and never loads the build
driver or evaluates target/reload conditions. This catalog is cached until the
server restarts; it is not taken from host Emacs or a configured runtime override.
Workspace/local candidates take precedence over same-kind builtin items.
Builtin items have `builtin` detail and no inferred argument list. Registration
does not prove target-specific functionality; conditional/extra builtins,
prelude functions/macros and builtin signatures still require separate coverage.
Base reader builtins now provide fallback argument contexts for completion,
navigation and references, including nested source signature queries. Any
authored callable of the same name overrides that fallback; duplicate authored
functions remain ambiguous and macros remain opaque. The shared occurrence
batch carries builtin classification once, alongside its source provider table.
No builtin source location or unverified signature is synthesized.

The server sends a complete unfiltered candidate array using ordinary
`insertText` and `label` values; the client filters candidates and determines
word replacement according to its language configuration. This is one of the
[LSP completion models](https://raw.githubusercontent.com/microsoft/language-server-protocol/gh-pages/_specifications/lsp/3.17/language/completion.md).
Inserted symbol text retains Lisp reader escapes and is plain text, not a
snippet. Candidates are not suppressed in comments/strings. Completion item
resolution and server-computed text edits are not implemented. Cursor positions
are checked as UTF-16 coordinates; positions within surrogate pairs are rejected.

`textDocument/definition` and `textDocument/hover` connect declaration names and
known-context function/macro calls to declarations in the same open document.
Definition requests additionally fall back to the workspace index when the
reader identifies a supported-context token with no local declaration. Matches
use exact symbol spelling and the function/variable namespace; all remaining
candidates are returned instead of guessing between duplicate definitions.
Local bindings and same-document declarations retain priority. Open provider
snapshots override disk source, and the index is refreshed for each fallback.
This is source-candidate navigation, not proof of runtime loading or module
visibility. It does not make opaque macro arguments traversable. Hover uses the
same candidate lookup when no local declaration exists, showing a signature and
literal plaintext documentation only for one external candidate. Provider edits
are rechecked; duplicate external candidates and ambiguous local declarations
produce no hover. The highlighted range remains the token in the caller.
The reader supplies token spans, including escaped symbol names. The lookup
walks complete forms before the first reader error; strings, comments, quoted
data, local function scopes, and unrecognised macro argument contexts are opaque.
Known control forms, let initializers/bodies, and unique plain source-function
arguments are traversed. Variable references and `setq` targets resolve to
enclosing simple parameters or `let`/`let*` bindings before global declarations.
Required, `&optional`, and `&rest` parameter names are supported. `let`
initializers use the outer scope; `let*` initializers also see earlier bindings
in the same list. Nested shadowing is preserved. Unknown parameter/binding
syntax blocks fallback to possibly shadowed outer variables. Keyword/default
argument specifications, destructuring, local functions, and bindings introduced
by other macros remain unsupported. Call heads use the function
namespace, so a same-name variable does not become a function definition.
Literal function-object forms `#'name` and `(function name)` also resolve in
the function namespace, including reader-escaped names. They participate in
definition, hover and reference queries when their enclosing evaluation context
is supported. Ordinary quoted data is still excluded. Anonymous function bodies,
local function-binding scopes and uninterned function targets remain opaque.

Definition responses use ordinary `Location` arrays with full declaration
spans, compatible with clients that do not advertise definition-link support.
Duplicates return multiple source locations; hover returns plaintext signature
and literal documentation only for a single declaration. The hover range spans
the queried token. A caret immediately after a token before whitespace or `)`
is accepted, but a comment delimiter is not treated as part of the token.
These are source declarations, not loaded runtime definitions or debugger data.
The wire methods follow the official [definition](https://raw.githubusercontent.com/microsoft/language-server-protocol/gh-pages/_specifications/lsp/3.17/language/definition.md)
and [hover](https://raw.githubusercontent.com/microsoft/language-server-protocol/gh-pages/_specifications/lsp/3.17/language/hover.md) contracts.

Each accepted document version has an in-memory cache of parser/format plans.
`textDocument/references` resolves local parameters/let bindings within their open
document. Global names require one declaration candidate across indexed sources,
or a function name in the base builtin catalog with no authored function/macro
candidate. Supported occurrences are then collected across open and closed files.
Builtin-only queries return call and literal function-object uses without
inventing a declaration location. Authored callable candidates override builtins;
duplicates remain ambiguous and same-name variables use their own namespace. The
`context.includeDeclaration` flag controls declaration locations. Ambiguous
global declarations return no references. Strings, comments, quoted/opaque data
and shadowed local bindings are excluded. This remains source-context analysis,
not a complete runtime or module reference graph. Computed function objects,
macro-expanded uses and unknown call arguments remain open.

Changed files share one bounded host request, with up to 4096 matching-token
candidates per file and 20000 resulting locations across the workspace. The
existing 10-second planner timeout applies. Up to eight global symbol queries
retain per-file occurrences keyed by source hash and external callable
classification (names, kinds and duplicate counts). Body/documentation edits
reparse only the changed files; changing a function to a macro invalidates
dependent contexts too. Deletion removes locations, and declaration ambiguity
is rechecked before reuse. Local token/reference caches are also invalidated
when the callable context changes. If future traversal uses argument metadata,
module visibility or other properties, those must join the context identity.
Changed-file batches carry one shared callable table, including unchanged
providers, instead of repeating their declarations for every query file.
The host normalizes this table once and excludes each file's own declarations
from its external context. Declaration classification is also shared across
all matching tokens within each source reference query, guarded by the source,
reader records and external context identities. No application code is executed.
Exact navigation candidates are filtered by name and namespace before location
conversion; workspace-symbol substring searches retain their broader matching.

`textDocument/prepareRename` and `textDocument/rename` support local parameters
and let bindings in an explicitly lexical document. The first line must have a
commented `-*- lexical-binding: t; -*-` cookie. The enclosing top-level form must
use the supported plain parameter/binding/control syntax and known unique source
functions; opaque calls and macro contexts reject the operation. Syntax errors,
declared special variables and existing destination symbols also reject it.
Names are printed with reader escapes, including spaces and Unicode. Strings,
comments, quoted data and other bindings retain their text.

The server advertises rename only to clients supporting
`workspace.workspaceEdit.documentChanges`. Results include the open document's
version, so clients can reject a stale edit. Preparation is optional: a direct
rename request performs the same validation. Preparation does not authorize a
later request against an older snapshot. Validation and reference collection
share one reader snapshot. This is a conservative source rename, not analysis
of external special declarations, generated code or runtime bindings. Global,
cross-file and general macro-aware rename remain open.

`textDocument/signatureHelp` displays plain source signatures and literal
documentation for the nearest supported function/macro call. Required,
`&optional` and `&rest` parameters are supported; keyword/destructuring lambda
lists are not yet interpreted. Argument indices clamp to the last displayed
parameter, with arity diagnostics handled separately. Trigger characters are
`(` and space. Clients advertising label-offset support receive UTF-16 label
ranges; other clients receive parameter label strings.

Unfinished calls are analyzed by closing the prefix's open delimiters in a
private reader snapshot. No source text is changed or executed. Strings,
comments, quoted data, opaque macro arguments and ambiguous declarations yield
no signature. Complete documents retain forward declarations; an incomplete
document uses only declarations available in the repaired prefix. Up to 128
position results, including misses, are cached for the current document version
and workspace content hashes. Unresolved supported calls use a unique callable
declaration from other indexed files; local declarations take precedence.
Provider edits, overlays, closure, deletion and duplicate declarations invalidate
cached answers. Only name, kind, argument list and literal documentation are
sent to the signature query. Builtins, dependencies outside indexed folders and
module-load visibility remain open.

`workspace/symbol` searches declaration names with a case-insensitive substring.
It reads `.nl` and `.el` files under up to eight local `workspaceFolders` (or the
legacy `rootUri`), including closed files. Open snapshots override the same disk
file and are included even without a folder; closing a document restores disk
authority for the next query. Folder add/remove notifications are supported.
Incomplete source contributes declarations before its first reader error.

The scan skips symlinks, hidden entries, and `target`, `build`, `dist`,
`node_modules`, `__pycache__`, and `vendor` directories. It does not interpret
gitignore rules. Limits are 512 source files, 2 MiB per disk source, 16 MiB total
snapshot text, 32 directory levels and 20000 scanned entries. Exceeding a limit
reports an error rather than returning an apparently complete partial index.
Each query checks disk content hashes, so same-size/same-time changes are found.
Only new or changed snapshots are sent to one bounded host parsing request;
unchanged declarations and open-document completion plans are reused. No source
is executed, no registry access occurs, and no persistent index is written.
Workspace symbols are navigation candidates, not cross-file binding resolution.
Queries filter declaration names before converting source coordinates, so a
single-name lookup does not calculate locations for every unrelated declaration.
When multiple locations are returned, one LF reader map and one LSP line map
serve all declarations in a source snapshot. This retains UTF-16 coordinates
across LF, CRLF and CR without rescanning the full text for each location.

Repeated requests reuse that version; changing or closing the document drops
its old cache. There is no persistent disk index. Definition and hover share a
cache with at most 128 recent entries per document version. A known reader-token
span is reused as the cursor moves within the same identifier; unsuccessful
lookups remain specific to their position. Requests are processed
serially and a host parser invocation has a ten-second timeout. Cancellation
notifications do not interrupt an already-running parser. Limits are 32 open
documents, 2 MiB of UTF-8 source per document, 8 MiB per incoming message, and
8 KiB of headers. Server responses are not streamed as partial results.

`python3 test/nelisp-lsp-test.py` exercises the real server protocol, Unicode
positions, unsaved source, lifecycle, malformed messages, version replacement,
nonexecution, and missing tooling. The Linux distribution suite also starts
the installed server after relocating its read-only prefix. These are protocol
and installation tests, not qualification with a real editor client.

The [VS Code extension](../editors/vscode/README.md) now connects these features
to `.nl` documents. Its separate extension-host suite exercises VS Code 1.135.0
on Linux: language registration, unsaved Unicode definition/hover/completion,
outline, applied formatting, diagnostic publication/clearing, and restart with
unsaved-document resynchronization. It also runs CLI tasks and evaluates a
preloaded project function through the real REPL terminal. TextMate/Oniguruma
tests cover the basic syntax grammar. The locally built VSIX includes the
language client; the server and project toolchain remain separately installed.

Strategy section 11 remains open: binding/scope-aware completion and navigation,
complete cross-file references, general rename, builtin signatures, semantic tokens and code actions
remain unimplemented in this server. VS Code's initial project commands include
run/test/build/fetch/update and REPL. Its Testing view uses separate CLI static
discovery and exact-name runs, with per-test results and cancellation. A debug adapter,
package publication/authentication UI, Marketplace publication, and later editor integrations
remain separate deliverables.
Basic package search/add/remove and fetch/update commands now use the CLI from
VS Code with resource-scoped index/registry/offline settings.
