# Lane brief: make C-core primitives match GNU Emacs 31.1

You are the implementer for one bounded lane. The coordinator will review and
integrate your result. Work only in the current directory (the lane). Write
all code, comments and docstrings in English.

## Task

The NeLisp standalone runtime re-implements GNU Emacs C primitives in Emacs
Lisp. For the names in `names.txt`, the probe rows in
`test/nelisp-emacs-lib/c-core-probes/UNIT.el` (UNIT is in `unit.txt`) differ
from GNU Emacs 31.1. `DIVERGENCES.txt` shows the differing rows (`<` GNU,
`>` standalone). Make every row of this unit match GNU by implementing the
real GNU behaviour of those primitives.

`KIND.txt` says which of three lane kinds this is:

- `package FILE...`: the live definitions are in the library files copied to
  `units/`. Edit those copies in place. Keep each definition's existing
  install guard (`(unless (fboundp 'NAME) ...)` or the file's own
  `...-install-...-p` predicate) and the file's style. When several files
  are listed, a primitive may be defined in more than one of them: work out
  which definition is live after the bundle loads (the files load in the
  listed order; read the install guards) and fix that one.
- `unit FILE`: the names have no real implementation (they are bulk stubs
  returning nil, or missing). Create `units/FILE` as a new C-core unit:
  header `;;; FILE --- SUMMARY  -*- lexical-binding: t; -*-`, each primitive as
  `(unless (fboundp 'NAME) (defun NAME ARGS "DOC" BODY...))`, private helpers
  named `PREFIX--helper` where PREFIX is FILE without `.el`, and a final
  `(provide 'PREFIX)`. A body that is only nil, `ignore` or a constant is
  rejected as an empty stub.
- `prelude FILE`: the live definitions are Lisp closures baked into the
  runtime binary's prelude. `$RUNTIME_PRELUDE` (read-only, large: search it,
  do not read it whole) holds the current definitions for reference. Write
  complete replacement definitions (plain unguarded
  `defun`s, plus any private helpers they need, prefixed `nelisp--`) into
  `units/FILE`; the coordinator splices them into the prelude. Only the
  primitives in `names.txt` may be redefined.

## Rules

1. GNU Emacs 31.1 (`emacs -Q --batch`) is the only source of truth. Explore
   it freely; implement the documented general behaviour, including argument
   validation and the exact error symbol and data GNU signals.
2. Never special-case the probe inputs, never detect the test harness, and
   never edit the probe file, the driver, the area table or the check tool.
   A fix must be correct for inputs the probes do not contain.
3. Lisp only. Do not add native entries. The standalone has no bignums, a
   byte-and-char string model close to Emacs, and real per-buffer primitives;
   check what exists with a short standalone run before relying on it. Avoid
   `cl-lib` in new code unless the file already uses it; prefer plain
   `let`/`while`/`dolist`.
4. Keep other behaviour of the edited file intact: do not rename, remove or
   reorder existing definitions, and do not reformat unrelated code.
   When you tighten argument validation in a helper or primitive, first
   search `$LIB/packages` for its callers: bundle files call these functions
   while the bundle loads and may pass internal representations (for example
   the runtime's native `nelisp--syntax-table` record as a char-table
   parent). Rejecting such a value makes the whole bundle fail to load, so
   keep accepting what existing internal callers pass.
5. If a row cannot match GNU without a native change or a change outside
   this lane's file, leave that primitive as it is and report it. Do not
   weaken anything to hide the gap.
6. Performance is part of correctness here. Add no work at file load time
   (no table built eagerly, no top-level loop), never loop over the whole
   character range or over every buffer, symbol or marker for an operation
   GNU does in constant time, and keep global state out of per-buffer
   behaviour: what GNU stores per buffer must be buffer-local here too. The
   bundle has a hard 50 s load budget and is close to it.

## Tools

Environment is exported: `NELISP_BIN`, `LIB` (library checkout, read-only for
you), `EMACS=emacs` (GNU Emacs 31.1).

```sh
emacs -Q --batch --eval '(prin1 (FORM))'                          # GNU behaviour
"$NELISP_BIN" --eval '(prin1 (FORM))' --eval nil                   # bare standalone, fast
bash check.sh                                                      # this lane's strict check
```

The check hot-loads this lane's `units/` files over the pinned bundle (after
unbinding the names in `rebind.txt`), runs the unit on both runtimes and prints the
differing rows, then `LANE-CHECK unit=... rows=N differing=K ...`. It exits 0
only when K is 0 and the standalone wrote nothing to stderr. One check takes
about 50 seconds because the standalone loads its whole bundle: reason about
the code first, then use at most 8 checks in total. The bare standalone
(second command) does not have the library bundle loaded; use it only for
core language questions.

Also run this before finishing:

```sh
for f in units/*.el; do emacs -Q --batch --eval "(with-temp-buffer (insert-file-contents \"$f\") (emacs-lisp-mode) (check-parens))" || echo "PARENS_FAIL $f"; done; echo PARENS_DONE
```

## Invariants

- Write only the `units/` files named in `KIND.txt` (scratch files are fine if you delete them;
  `.lane-check/` is the tool's output directory).
- No git commands, no network, no edits outside this directory.

## Stop rules

- If the sandbox prevents running `$NELISP_BIN` or the tool, finish the
  implementation, run the parens check, and report the exact error text.
- If a row still differs after two focused attempts, leave the best correct
  general implementation in place and report the row; do not keep guessing.
- If hot-loading `units/FILE` itself breaks the standalone run, report the
  error text and stop.

## Report (final message, at most 30 lines)

1. The final `LANE-CHECK` line and the parens output, verbatim; checks used.
2. Per primitive: fixed, or still differing with the reason (native gap,
   other file, unclear GNU semantics).
3. Anything unverified, and any behaviour you changed beyond the probe rows.
