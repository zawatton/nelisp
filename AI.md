# Working in this repository

Read this file, then `target/ai/STATUS.md`.  Everything else here is
reference material to reach for on demand, not to read up front: some
160 design documents, a 260 KB findings file, a 1300-line Makefile and
several thousand ERT cases do not fit in one session, and skimming them
is how a session ends up confidently wrong.  STATUS has the exact
counts; this paragraph deliberately does not.

## Orient

```sh
tools/ai/nelisp-ai.sh doctor     # toolchain, branch, and which binary is in target/
tools/ai/nelisp-ai.sh status     # regenerate target/ai/STATUS.{json,md}, then read it
```

`STATUS` is generated, per machine, and untracked.  Do not hand-edit it,
and do not accept a claim about this tree that is not in it or in a gate
report.  A hand-written "1549 tests, 0 failures" in a sibling repository
stood unchallenged for months while the suite, once it could actually
run, reported 2859 tests and 127 unexpected results.

## Develop in a persistent REPL

For runtime behavior changes, start with the reproducible
[REPL development guide](docs/repl-development.md). It covers first setup,
an intentional error, its call chain, editing one file, and replacing its
definitions without restarting the process. Use
`tools/ai/nelisp-ai.sh repl`; plain `target/nelisp --repl` does not preload
the full artifact compilation runtime needed for source reload.

Keep one REPL process alive across the failing call and the repaired call.
Record the input, condition/call chain, reload result, and preserved state.
Inspect `:status`, `:phase`, and `:published` before rerunning. Definition
spans are not exact failing-expression locations; do not claim that native
direct callers were replaced. A new agent session can follow the guide
without prior conversation or local memory. A new REPL process starts with
fresh Lisp state; replay the setup instead of assuming the old state exists.
Run focused tests and the normal gates after the interactive repair.

For native allocator or GC changes, use the opt-in `runtime-reload-reader`
build and the native runtime section of the same guide. Edit the canonical
runtime source, then use `nelisp-runtime-rebuild-and-reload` in the existing
REPL. Record the executable hash, candidate identity, generation, and
preserved state. A host ERT pass does not demonstrate native publication:
exercise replacement and restoration in the standalone process as well.

Load `(require 'nelisp-repl-development)` for failure capture and explicit
retry, `nelisp-repl-code-info`, GC snapshots, and explicit replay recipes.
Consult `(nelisp-repl-help)` and the guide instead of assuming a command
exists. Replay has side effects; never automatically retry a failed call.
Clear retained diagnostic records when measuring memory after a repair.
REPL errors do not necessarily produce a nonzero process exit: smoke tests
must check their assertions, completion marker, and unexpected stderr.

## Where FFI is allowed

Decided 2026-09-12.  This runtime can call out: `ptr-call` and
`syscall-direct` exist, raw native units expose SysV-ABI entry addresses,
and the JIT already resolves symbols through `dlsym(RTLD_DEFAULT, ...)`.
The question was never capability.  It is scope, and the scope is:

**Bind external libraries.  Do not try to bind Emacs's C core.**

Allowed: the libraries Emacs itself treats as external and often loads
dynamically -- image decoding, fonts and shaping, TLS, compression, XML,
SQLite, crypto.  Reimplementing those in Elisp buys nothing; they have a
stable C ABI and someone else maintains them.

Not allowed, and not possible: `insdel.c`, `alloc.c`, `eval.c`, `xdisp.c`
and their neighbours.  Emacs's core is not a library.  It owns its own
Lisp object representation, its own GC and its own global state, and
there is no ABI seam to call through -- taking `insdel.c` means taking
`lisp.h`'s object layout and the collector with it, which is embedding
all of Emacs rather than replacing any of it.  Those stay reimplemented,
which is what this repository and nelisp-emacs-lib already are.

Two things to keep in view when reaching for it:

- It does not make anything faster.  The measured wall is this
  evaluator's own speed (`make standalone-call-floor-bench`), and glue
  written in interpreted Elisp around an FFI call makes that worse, not
  better.
- It sits against the "pure-elisp self-host" framing this tree adopted
  when it deleted its Rust.  C bindings are not Rust, so the LOC rule is
  untouched, but the claim in the README is.  Say what is bound.

## Carry a development-efficiency improvement with the work

Every session here is also asked to look for what makes the next session
slower than it needs to be, and to fix it in the same branch rather than
absorb it.  A loop that costs more than the question it answers is work in
scope, not friction to live with: an API that cannot be pointed at real
code, a probe that needs a whole bundle rebuilt to ask one thing, a gate
that reports success while leaving a runaway child behind, a cache that
takes longer to build than the starts it saves.

The rules for doing it are the same ones the rest of this file states:

- **Measure the friction before changing anything, and again after.**  Put
  both numbers in the commit message.  "It felt slow" is not a finding;
  "the REPL cut a two-line form in half and executed the readable half"
  is.
- **Fix the tool, do not weaken a contract to route around it.**  When a
  strict API refuses real input, add the honest path beside it (stage what
  is safe, name what was skipped) instead of relaxing the refusal.
- **A smoke that would pass without the fix proves nothing.**  Give the new
  path a control that fails on the old one.
- **Say what is still slow.**  A measured cost you decided not to pay is a
  finding worth writing down, not silence.

Worked examples, all from 2026-09-12: `nelisp-ai.sh repl --script FILE`
(the REPL reads one line per form, so scripted sessions had to flatten
every form by hand), `nelisp-repl-reload-defuns` (the strict reload API
refused every real module at its first `require`), and
`nelisp-bc--host-globals-p` (republished code read a namespace the live
session was not writing to, and said `:status ok` about it).

## Inner loop

For machine-readable queries, see
[the development protocol guide](docs/development-protocol.md). Start with
`tools/ai/nelisp-ai.sh dev --request examples/repl-development/capabilities.json --json`.
Use the returned capabilities and limitations; a host source-query context is
not an attachment to a standalone session, and reading an old gate report does
not run a test.

```sh
tools/ai/nelisp-ai.sh check                              # before every commit
tools/ai/nelisp-ai.sh test-one test/nelisp-FOO-test.el   # seconds
tools/ai/nelisp-ai.sh compile                            # byte-compile, error-on-warn
tools/ai/nelisp-ai.sh ns FILE...                         # namespace boundaries (nl-ns)
tools/ai/nelisp-ai.sh gate NAME -- make TARGET            # wrap an existing check
tools/ai/nelisp-ai.sh test                               # full ERT suite
tools/ai/nelisp-ai.sh verify                             # the verdict
tools/ai/nelisp-ai.sh presence                           # ~354-name fboundp sweep, minutes
```

`check` runs as one step of the CI Linux lane
(`.github/workflows/ci.yml`, "AI toolchain check-tier gates") and then
verifies, so "what will CI say" about that tier is answerable before
pushing rather than after.  The same Linux lane separately runs the
binary-tier gates this file's inner loop lists as their own commands
(`native-artifact`, `perf`, `smokes`, `extras`, plus `standalone-
reader-test`/`emacs-parity`/`binary-size-ratchet` individually rather
than through `standalone`, to avoid running `emacs-parity` twice), the
ERT suite (both JIT settings), and several checks `nelisp-ai.sh` does
not model at all (`bench-aot-tco`, `macho-acceptance-test`) -- none of
which `check` itself covers.  `check` deliberately does not run the
full ERT suite; `verify` holds you to the last `ert-full` report and
prints its age, so run `test` when that column says the evidence is
stale.  CI wiring was added 2026-08-22 by running these exact commands
locally and reading their exit codes and GATE-COUNT lines; its first
run on an actual GitHub Actions runner (run 32604739757, 2026-08-23)
found one defect no local run could see -- `check`'s own trailing
`verify` demanded reports from the binary-tier gates the same Linux
lane produces AFTER `check` runs, so a runner where every check-tier
gate had just passed still aborted the job right there, skipping every
later step.  `check` now scopes that trailing verify to its own gate
names (`check_tier_manifest` in `tools/ai/nelisp-ai.sh`; plain `verify`
stays unscoped), and the Linux lane gained a final, unscoped `verify`
step after every gate step, so "every required gate has a fresh report"
is asserted exactly once, at the one point in the job where every gate
has actually had a chance to run.  That fix has not itself been
observed passing on a runner yet.

`verify` is the only command whose exit code answers "is the tree good".
Every other command reports on itself; `verify` also knows which gates
produced no report at all.  See `tools/ai/README.md` for the report
contract, the `GATE-COUNT` line that brings an existing Makefile target
under it, and `bench-compare.sh` for measurements — which have a third
outcome of their own, since a ratio the machine invalidated is neither
a pass nor a failure.

The root `Makefile` still holds the real build, the standalone smokes and
the release targets.  Use it directly for those; `nelisp-ai.sh` does not
wrap them yet.

## Rules that exist because each was broken here

1. **A gate that executed zero cases is not green.**  A CI gate placed
   behind an already-red target never ran once and was reported as
   passing for its whole life.  A `test/*.el` glob swept in driver
   scripts, defined no test, and failed with an opaque exit code.  This
   is why gates report counts and why `tools/ai/gates.expected` exists.
2. **Verify in the configuration the user actually runs.**  An IME engine
   was validated by overriding the runner through an environment
   variable while users selected it through the registry.  The registry
   path was never exercised and shipped broken.
3. **A measurement is invalid unless its baseline is.**  A 2.5x
   "regression" was background load; a 1.00x "parity" was the same
   program compiled twice and compared with itself.  Re-measure the
   baseline, check that the two sides differ, and record what else was
   running.
4. **Read values out of files, not out of terminal output.**  Use
   `nelisp-ai.sh probe EXPR`, which writes stdout, stderr and the binary
   identity into a directory and prints only its path.  `tail -1` has
   returned the shell's own echo here more than once.
5. **Name the artifact a number came from.**  `target/` holds a dozen
   experimental builds at once; `doctor` and `probe` both record the
   binary's hash and size.  When reading a disassembly, cut the slice by
   the manifest's offset and size — reading by eye produced two confident
   and wrong diagnoses of the same fault in one day.
6. **Check `git worktree list` before switching branches.**  A dozen and
   more worktrees are attached to this repository at any time, and
   another session may already hold the branch you are about to check
   out.  STATUS counts them for you.
7. **Record generated data recipes next to the generator.**  A dictionary
   was nearly shipped at 60% of its intended size because the real
   two-input recipe lived only in a session transcript.

## Definition of Done

A feature, fix, or gate ships with:

- **Against-the-bug evidence.**  The check or fix shown red on the defect,
  then green on the fix -- run, not asserted, and said in the commit that
  lands it.
- **A parity corpus form**, when the change touches cross-substrate
  behavior.  `test/nelisp-shadow-differential-cases.el` feeds both
  `emacs-parity` and `parity-coverage`; `tools/nelisp-substrate-parity-
  corpus.el` feeds `substrate-parity-smoke`.  See `tools/ai/gates.expected`
  for which gate reads which corpus.
- **A mutation row** in `tools/gate-mutations.txt`, when the change adds a
  new required gate.  A gate nobody has shown a real defect to is a claim
  about a checker, not a checker -- see that file's header and the
  "gate-mutation" entry in `tools/ai/gates.expected` for why.
- **A `#+VERIFIED-BY: gate-name ...` line**, on any `docs/design/*.org` the
  change marks `#+STATUS: SHIPPED`, naming a gate that exists.  `make
  doc-claims` enforces this mechanically; legacy docs are tolerated via
  `tools/nelisp-doc-claims-baseline.txt` while they migrate.

This exists because "SHIPPED" had come to mean "claimed," not "run."  Doc
142 said SHIPPED for a `--kind elc` lane that crashed on
`void-variable: invocation-name` the first time anything outside its own
ERT fixture invoked it, and this file's own Inner-loop section (above)
claimed a CI wiring that did not exist until the same session that found
it wrong also fixed it.  Both were the same defect shape every rule in
the previous section already names -- something read as done and
silently was not.  The four requirements above are that lesson made
structural rather than remembered.

## Where things are

| path | what |
|---|---|
| `src/` | NeLisp core (reader, eval, allocator, ...) |
| `lisp/` | AOT compiler, assemblers, code generation |
| `packages/` | optional libraries: json, http, sqlite, network, process, x11, ... |
| `packages/nelisp-pkg/` | the package graph: `make pkg-graph` derives it and fails on a cycle |
| `test/` | ERT, `*-test.el` only — other names are not collected |
| `docs/design/` | numbered design documents; declare state with `#+STATUS:` |
| `docs/runtime-limitations.md` | what compiled code does *not* do like C |
| `recipes/` | how to build an application or service on NeLisp |
| `tools/ai/` | the tooling this file describes |
| `target/` | build output, gate reports, STATUS — untracked, per machine |

## Building something with NeLisp

Start from `recipes/README.org`, which lists the application shapes that
are known to work today and the ones that are not viable yet, with the
measurement behind each verdict.  Copy a recipe's `skeleton/`, then run
its `verify.sh` before writing anything of your own — a recipe that
cannot pass its own smoke on your machine is telling you something.

```sh
tools/ai/nelisp-ai.sh recipes    # every recipe smoke, against target/nelisp
```

Each smoke skips with a reason when the binary is absent, so this is
also the quickest way to find out which shapes your build supports.
