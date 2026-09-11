# Structured development queries

Start with [the persistent REPL guide](repl-development.md) to reproduce an
error, replace a function, and retry explicitly. The development protocol adds
a common structured request/response interface for tools and host Emacs Lisp.
It is an initial implementation of the
[AI development platform specification](ai-development-platform-spec.md), not
a declaration that every acceptance row in that specification is complete.

## First query from a new checkout

Run from the repository root, with host Emacs available:

```sh
tools/ai/nelisp-ai.sh dev --request examples/repl-development/capabilities.json --json
```

This needs no standalone build and starts no application. Standard output is
one UTF-8 JSON response; tool errors remain on standard error. `capabilities`
lists the registered operations and their limitations. A discovered adapter
does not establish that a native target has passed its acceptance tests.

Inspect the example definition, its exact reader span and raw source hash:

```sh
tools/ai/nelisp-ai.sh dev --request examples/repl-development/describe.json --json
```

`describe` and `impact` take `arguments.symbol` plus `arguments.path`, or an
explicit `arguments.files` array (at most 32 files). `check` takes the same file
selection without a symbol. Files must be inside the project, UTF-8, and at most
2 MiB each. There is no implicit crawl through generated build directories.
`check` validates reader syntax and checks argument counts for direct calls to
uniquely declared plain `defun` functions in the explicit input files. Required,
optional and rest arguments are supported; unknown macros, local function
bindings and unsupported lambda lists limit coverage. Diagnostics identify the
containing definition, not the exact failing expression. A check without errors
still returns `inconclusive` because complete dependency and runtime coverage
have not been established.
`impact` reports syntactic call candidates, excluding quoted data; unknown
macros and dynamic dispatch prevent a complete runtime-call claim.

To send the same request from an Emacs Lisp REPL:

```elisp
(add-to-list 'load-path (expand-file-name "lisp"))
(require 'nelisp-dev)
(let ((json-null :null) (json-false :false))
  (nelisp-dev-dispatch
   (nelisp-dev-protocol-string-keys
    (json-read-file "examples/repl-development/capabilities.json"))
   (nelisp-dev-context default-directory)))
```

The context above is a **host Emacs source-query context**. It does not attach
to the standalone process used by the development launcher. A CLI request
naming an unconnected live session returns `unsupported`, code
`NELISP-DEV-LIVE-SESSION-REQUIRED`, and exit 4. Continue live repairs through
the established REPL APIs; no automatic retry, replay or publication occurs.

## Read a response

| Status | Exit | Meaning |
| --- | --- | --- |
| `ok` | 0 | The requested, explicitly limited operation completed |
| `failed` | 1 | A diagnostic or operation failure was observed |
| Invalid request | 2 | A failed envelope identifies an invalid request or unknown operation |
| `inconclusive` | 3 | Missing evidence, incomplete analysis or an output limit prevents a clean verdict |
| `unsupported` | 4 | The current context lacks the target, session or adapter |
| `cancelled` | 5 | An adapter reported cancellation |

Read `diagnostics`, `limitations` and `identity` together. Unknown identities
remain JSON `null`. `test` currently **inspects** `target/gates/NAME.json` via
`arguments.gate`; it does not execute the gate. Its `data.executed` is false.
A failing report remains failed; a historical passing report is inconclusive
because the legacy format cannot verify its source/artifact binding. To execute
a test, use the existing `test-one`, `test`, or `gate` commands in
[AI.md](../AI.md), then inspect their evidence.

Requests use string-key objects, arrays, JSON null and booleans. The Elisp
representation uses string-key alists, vectors, `:null` and `:false`.
`limits.bytes` accepts 1024–16384; `limits.page_size` accepts 1–200 (default 50).
Diagnostic pages preserve full summary counts and expose `returned`, `omitted`
and `next_cursor`. Repeat the same request with `limits.cursor` to continue.
Changing source identity, diagnostic input, session or the live clear epoch
invalidates a cursor. Cursors expire after 15 minutes. An individual response
that cannot fit is reported as inconclusive; no serialized JSON is cut midway.

## Current boundaries

### Diagnose, repair and retry in one host REPL

Keep a live context in the host process where the failure was recorded:

```elisp
(setq dev-context (nelisp-dev-session-context default-directory))
(defun demo-call (value) (error "repair this definition"))
(condition-case nil (nelisp-repl-session-call 'demo-call 7) (error nil))
(setq failure-id (plist-get (car (last (nelisp-repl-session-failures))) :id))
(setq diagnosis
      (nelisp-dev-dispatch
       `(("schema_version" . "1") ("operation" . "diagnose")
         ("request_id" . "diagnose-demo")
         ("arguments" ("failure_id" . ,failure-id))
         ("target" . "host-emacs") ("session_id" . :null) ("limits"))
       dev-context))
(setq failure-handle (cdr (assoc "failure_handle" (cdr (assoc "data" diagnosis)))))
(defun demo-call (value) (setq demo-result (* value 2)))
(nelisp-dev-dispatch
 `(("schema_version" . "1") ("operation" . "retry")
   ("request_id" . "retry-demo")
   ("arguments" ("failure_handle" . ,failure-handle)
                ("effects_policy" . "explicit-only"))
   ("target" . "host-emacs") ("session_id" . :null) ("limits"))
 dev-context)
(= demo-result 14)
```

Diagnosis does not call the function or serialize its argument graph. Retry
requires the returned live handle and an explicit effects policy. Argument
objects are retained by reference, so their current values are used; this is
not a snapshot or rollback. An opaque result is summarized without exporting
the returned object graph. Missing call-chain, source and loaded-generation
evidence remains unknown. These operations do not attach a CLI worker to a
different REPL process.

Request `session.clear` with empty arguments in this same context to release
failure and replay records, code provenance, protocol details and loaded
adapter diagnostic caches. It invalidates retained failure handles and live
diagnostic cursors. It does not collect garbage, unload application functions,
or undo application effects. Native publication state is outside this clear
operation. Clear the diagnostics before measuring their retained memory;
allocator reservations and RSS may remain even after objects are collectible.

### Export and validate explicit host REPL records

In the host Emacs Lisp REPL used above, keep one context and register only
the setup operations you intend to export:

```elisp
(setq dev-context (nelisp-dev-session-context default-directory))
(nelisp-repl-session-record '(setq demo-state 7))
(nelisp-dev-dispatch
 '(("schema_version" . "1") ("operation" . "session.export")
   ("request_id" . "export-demo")
   ("arguments" ("recipe" . "target/dev-session/demo.el")
                ("manifest" . "target/dev-session/demo.json"))
   ("target" . "host-emacs") ("session_id" . :null) ("limits"))
 dev-context)
```

The context identifies this host process; it cannot export another process's
records. The recipe and JSON manifest contain explicit registered forms and
loaded-file hashes. To validate later, request `session.validate` with
`arguments.manifest` set to the JSON manifest path, using the CLI or dispatcher.
Paths are relative to the manifest, and Unicode paths are supported. Validation
checks required fields, file sizes and SHA-256 hashes without loading any code.
It does not establish native runtime, build-option or dependency compatibility.
Validation never starts replay automatically. To execute the host recipe in a
fresh worker, make a separate `session.replay` request through the common
dispatcher or CLI:

```json
{
  "schema_version": "1",
  "operation": "session.replay",
  "request_id": "replay-demo",
  "arguments": {
    "manifest": "target/dev-session/demo.json",
    "effects-policy": "explicit-only"
  },
  "target": "host-emacs",
  "session_id": null,
  "limits": {}
}
```

Save the request to a file and run `tools/ai/nelisp-ai.sh dev --request FILE --json`.
The worker validates the manifest again and evaluates the recipe bytes it
hash-checked. Registered loads resolve relative to the original recipe;
ordinary relative file operations use a temporary working directory. The parent
REPL's variables are not restored or modified by the worker. A separate process
and temporary directory do not restrict network, process or absolute-path file
effects. Source hashes are preflight checks, not a freeze of dependencies during
execution. Read the returned status and execution counts; output markers alone
do not establish success. The existing native REPL recipe workflow remains in
the [REPL guide](repl-development.md).

`arguments.timeout` defaults to 10 seconds and accepts 1–60 seconds. A timeout,
more than 64 KiB of combined captured output, an error, or an early exit without
a terminal record is a failed run. Empty recipes are inconclusive. Responses
include the number of completed top-level recipe forms, the worker exit code,
and up to 1 KiB of each output stream; `output_truncated` makes omitted output
explicit. These are previews, not a complete application log. Record explicit
assertions in the recipe to verify the reproduced behavior.
Effectful operations reject continuation cursors before execution; fetching
another diagnostic page must never rerun a recipe or retry a saved call.

### Plan and apply a native allocator/GC replacement

Start the opt-in Linux x86_64 runtime using the [native REPL setup](repl-development.md#native-allocator-and-gc-development).
Keep this process alive throughout the following requests:

```elisp
(require 'nelisp-dev-reload)
(setq native-context (nelisp-dev-reload-context default-directory))
(setq candidate
      (nelisp-dev-dispatch
       '(("schema_version" . "1") ("operation" . "reload.plan")
         ("request_id" . "gc-plan")
         ("arguments" ("unit" . "allocator-gc")
                      ("atomicity" . "runtime-unit")
                      ("effects_policy" . "explicit-only"))
         ("target" . "native-linux-x86_64") ("session_id" . :null) ("limits"))
       native-context))
(cdr (assoc "status" candidate)) ; must be "ok" before continuing
(setq candidate-id (cdr (assoc "plan_id" (cdr (assoc "data" candidate)))))
(nelisp-dev-dispatch
 (list '("schema_version" . "1") '("operation" . "reload.apply")
       '("request_id" . "gc-apply")
       (cons "arguments" (list (cons "plan_id" candidate-id)
                               '("effects_policy" . "explicit-only")))
       '("target" . "native-linux-x86_64") '("session_id" . :null) '("limits"))
 native-context)
(nelisp-runtime-reload-status)
```

Planning runs the host compiler and maps a validated candidate without changing
the publication generation. It is effectful and requires explicit consent in
the request. Applying never rebuilds or retries application code. Both operations
reject cursors. The CLI's host context cannot apply a native process's plan.

The plan ID binds the session, generation, binary and ABI hashes, named compiler
environment options, candidate/source hashes, and a conservative inventory of
`.el`/`.elc` files directly in `lisp`, `src`, and `scripts`. Files are limited to
16 MiB each, with at most 1024 files and 32 MiB total. File additions, deletions
and content edits invalidate the plan. Runtime identity is sampled again after
the apply-time file checks, immediately before calling the existing guarded
native installer. A stale request returns `NELISP-DEV-STALE-PLAN` and publishes
nothing. An authorized apply attempt consumes the ID, including a failed attempt.

Plans expire after 15 minutes and are limited to 64 per process. Use
`nelisp-dev-reload-clear` to revoke them and their contexts, then create a new
context. JSON reports are inspection data; importing one does not restore
publication authority. Clearing does not unmap candidate code. To restore the
executable's original GC, use `nelisp-runtime-reload-restore-originals`.

`allocator-gc` covers the fixed allocator/GC unit and its existing safe-point
contract. It does not establish a general native direct-caller closure, freeze
host compiler libraries, or lock files against concurrent edits. Keep inputs
unchanged while building or applying. It provides no heap migration or rollback.

### Plan and apply a user-defined native unit replacement

The same two operations also carry the general user native unit, under
`unit: "native-unit"` with `atomicity: "native-unit"`. Its arguments are
`source` (a repository-relative native source path), optional `unit_id`
(omit it to create a new unit), optional `exports` (the public entry names to
freeze at creation; omit for every entry), and the same required
`effects_policy: "explicit-only"`. Any other argument key is rejected, and the
two scope fields must agree: `native-unit` with `runtime-unit`, or the reverse,
is `NELISP-DEV-UNSUPPORTED-SCOPE`.

```elisp
(require 'nelisp-dev-reload)
(setq native-context (nelisp-dev-reload-context default-directory))
(setq plan
      (nelisp-dev-dispatch
       '(("schema_version" . "1") ("operation" . "reload.plan")
         ("request_id" . "unit-plan")
         ("arguments" ("unit" . "native-unit")
                      ("atomicity" . "native-unit")
                      ("source" . "target/score.el")
                      ("effects_policy" . "explicit-only"))
         ("target" . "native-linux-x86_64") ("session_id" . :null) ("limits"))
       native-context))
```

Planning compiles `source` in a host subprocess and stages an immutable
candidate; it does not publish, and the unit's generation is unchanged when it
returns. The plan binds the live session, the target unit's expected
generation, the source and staged-artifact hashes, the same conservative
compiler inventory and named build-option digest as `allocator-gc`, and the
running binary hash. `unit_id` in the report is `null` exactly when the request
did not name one.

Applying revalidates every one of those before publishing. Anything that moved
-- the source, the artifact, the compiler inventory, the build options, the
runtime identity, or the unit's generation, which a competing publication can
advance between plan and apply -- is `NELISP-DEV-STALE-PLAN`, and the refusal
**discards the staged candidate** so a rejected plan leaves nothing publishable
behind. Publication itself goes through the unit's CAS, which is the only store
to its control word. `data.published` lists the export names that became live.

Revoking plans revokes candidates with them: `nelisp-dev-reload-clear` and TTL
expiry both discard any candidate a plan still holds, and its return value
counts plans dropped plus candidates revoked. Discarding an unpublished
candidate also unmaps its generation table and releases its mapped artifact.

A superseded **published** generation is a different matter: it is moved to the
unit's retire list and deliberately kept mapped, because nothing in this
runtime reports whether a call is still in flight through a stable gate.
`nelisp-native-unit-resources` accounts for what is retained and
`nelisp-native-unit-reclaim` reports each retired generation as refused, with
that reason, rather than guessing.

The limitation that matters most is reported on every `native-unit` result and
is repeated here because it is easy to over-read a successful publication:
**already-compiled direct callers inside the running executable are not
redirected.** Only calls made through the unit's stable entry gate observe the
new generation. See [replaceable call sites](design/202-replaceable-call-sites.org)
for which names can be reached at all and what requires a rebuilt binary.

### Remaining acceptance work

The source adapter is a nonexecuting, bounded analysis path. It must report
unknown dynamic calls and macro behavior rather than treating parsing as a
proof that the program is correct. Source coordinates describe definitions or
diagnostics, not a guaranteed native failing instruction.

Native reload plans now cover the fixed allocator/GC unit and general user
native units. General **Lisp** reload plans, cross-process transport, hash-bound
editing and comparative development-efficiency study remain separate acceptance
work, as does redirecting call sites that were compiled before the name was
declared replaceable. Existing failure capture, code identity, GC inspection
and explicit replay are documented in the REPL guide. Do not infer their unified protocol support
from the availability of those legacy APIs.

Focused checks for protocol changes:

```sh
tools/ai/nelisp-ai.sh test-one test/nelisp-dev-protocol-test.el
tools/ai/nelisp-ai.sh test-one test/nelisp-dev-source-test.el
python3 test/nelisp-dev-cli-test.py
```
