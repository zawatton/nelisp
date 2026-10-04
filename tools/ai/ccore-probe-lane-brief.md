# Lane brief: canonical C-core parity probes

You are the implementer for one bounded lane. The coordinator will review and
integrate your result. Work only in the current directory (the lane). Write
everything in English.

## Task

`names.txt` lists GNU Emacs 31.1 C primitives that have no canonical parity
probe yet. `unit.txt` holds this lane's unit name, UNIT. Create
`test/nelisp-emacs-lib/c-core-probes/UNIT.el` with exactly one entry per
assigned name, and `DIVERGENCES.md` describing every row where the NeLisp
standalone differs from GNU.

The probe file is data, read with `read`, one entry after another:

```elisp
;;; UNIT.el --- canonical probes  -*- lexical-binding: t; -*-
(insert-byte
 (with-temp-buffer (insert-byte 65 3) (buffer-string))
 (condition-case e (insert-byte 256 1) (error e)))
(set-binary-mode
 (set-binary-mode 'stdout t)
 (condition-case e (set-binary-mode 'bogus t) (error (car e))))
```

Each entry is `(NAME FORM FORM...)`. The driver evaluates every FORM with
lexical binding on GNU Emacs 31.1 (`emacs -Q --batch`) and on the standalone,
and prints one line per form: `P| NAME | VALUE` or `P| NAME | (ERR SYMBOL DATA)`.
The two transcripts are compared byte for byte.

## Rules for every entry

1. Two to four forms per name, and every form must actually call NAME.
   Cover the ordinary result first, then a boundary or second mode, then at
   most one error path. `(fboundp 'NAME)`-style forms do not count.
2. Values must be printable and identical on every machine and run: numbers,
   strings, symbols, lists, vectors. Never return buffers, windows, markers,
   processes, frames, functions, hash tables or other `#<...>` objects, nor
   absolute paths, user or host names, pids, times, random values, memory
   sizes or version strings. Reduce such results to stable facts (a type
   predicate, a length, a relation between two calls, a relative name).
3. For a wrong-number-of-arguments error return only the symbol, with
   `(condition-case e (NAME ...) (error (car e)))`: its data names a function
   object. For other errors return the whole `e` when it is plain data.
4. Leave no state behind: create text in `with-temp-buffer`, `let`-bind any
   global you change, kill buffers and delete temporary files you create
   (`unwind-protect`), cancel timers, restore match data and the current
   buffer. Never touch files outside a temporary file you created.
5. Nothing may be written to stderr on GNU (no `message`, `ding`, `print` to
   the echo area). No network, no subprocess that outlives the form, no
   sleeping beyond 0.1 s, nothing that waits for keyboard input.
6. Primitives that are destructive, interactive or terminal-bound
   (`kill-emacs`, `suspend-emacs`, `recursive-edit`, `read-event`,
   `dump-emacs-portable`, frame or terminal deletion, and the like) must be
   probed only through forms that cannot perform the action: argument
   validation errors or queries GNU answers deterministically in batch mode.
   Check what GNU batch really does before choosing; do not guess.
7. Use GNU Emacs 31.1 (`emacs`) as the only source of truth for expected
   behaviour. Do not write values from memory.
8. Truthfulness: when the standalone differs, keep the probe as it is and
   record the divergence. Never rewrite a form merely to make the two sides
   agree, and never special-case the standalone. The one exception is a form
   that aborts or hangs the standalone run so that the check cannot finish:
   replace it with another meaningful form and record the abort as a finding.

## Tools

Environment is exported: `NELISP_BIN`, `LIB` (library checkout, read-only for
you), `EMACS=emacs` (GNU Emacs 31.1).

```sh
emacs -Q --batch --eval '(prin1 (FORM))'            # explore GNU behaviour
bash "$LIB/tools/ai/ccore-lane.sh" check "$(cat unit.txt)"
```

The check validates the structure (assigned names, at least two forms, each
calling NAME), fails if GNU writes to stderr, runs both runtimes and prints
the differing rows (`<` GNU, `>` standalone) followed by
`LANE-CHECK unit=... rows=N differing=K standalone_stderr_bytes=B`. It exits 0
when the probes are well formed and both runs completed, whatever K is. One
check takes about 50 seconds because the standalone loads its whole bundle:
develop the forms against GNU first, then use at most 6 checks in total.
Existing probes in `$LIB/test/nelisp-emacs-lib/c-core-probes/` show the house
style; read a few, do not copy entries for other names.

## Invariants

- Write only `test/nelisp-emacs-lib/c-core-probes/UNIT.el` and
  `DIVERGENCES.md` (scratch files are fine if you delete them; `.lane-check/`
  is the tool's output directory).
- No git commands, no network, no edits outside this directory, no changes to
  the driver, the area table or the check tool.

## DIVERGENCES.md

One section per differing row or finding:

```
## NAME
- form: `(the probe form)`
- GNU: `value`
- standalone: `value`
- note: one line (missing behaviour, wrong error, stub, abort, ...)
```

Write `No divergences.` when K is 0. Do not investigate or fix the
standalone; reporting is the whole job.

## Stop rules

- If the sandbox prevents running `$NELISP_BIN` or the tool, finish the probe
  file against GNU, and report the exact error text.
- If a name cannot be probed under these rules, give it the two safest
  argument-validation forms you can verify on GNU and say so in the report.

## Report (final message, at most 25 lines)

1. The final `LANE-CHECK` line, verbatim, and the number of checks used.
2. Count of entries and forms; names probed only through validation errors.
3. Anything unverified or any deviation from this brief.
