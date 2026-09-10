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
rebuilds and arbitrary runtime-unit replacement require additional work.
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
