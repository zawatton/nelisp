# NeLisp v1.1.2

The other half of v1.1.1's value-word boundary: `and` and `or`.

v1.1.1 left the connectives unconverted and said so in its own Known
issues, on the reasoning that boxing one would make a false answer read as
true.  That reasoning was right about boxing an **arm** and wrong about
leaving the **form** alone, and the difference is measurable:

| | Emacs | v1.1.1 |
|---|---|---|
| `(g (and 1 3) 2)` | 111 | **222** |
| `(g (and 1 (+ x 1)) 4)` | 111 | **SIGSEGV** |
| an `and` result returned, then passed on | 111 | **SIGSEGV** |

3 handed over raw and read back as a tagged immediate is `3 >> 2` = 0, so
the comparison answered on 0 instead of 3 while exiting 0.

## Why one register decides the shape of the fix

`--emit-logic` short-circuits on a zero test of the machine word, and the
arm that stops it is also the form's value.  One register serving as both
is the whole difficulty:

- an **arm** must stay raw, because a Sexp pointer is never zero, so a
  boxed false would never short-circuit;
- the **form's value** must be a Sexp wherever it crosses a boundary.

So the connective now works entirely in the raw domain -- arms unwrapped,
the form declared `raw-i64` -- and the conversion goes on the whole form
at the boundary, where it already goes for arithmetic and for literals.

## The scoping is the fix's own lesson

The change belongs to the runtime-entered lane only.  Unscoped it also
reached the reader's own sources, where a boxed arm is a Sexp being tested
for presence rather than an integer.  The binary that built came apart
across the whole `extras` tier -- while `ert-full` stayed at 0 unexpected,
because the host suite does not run what the compiler emitted.  A green
host suite is not evidence about generated code.

## Two more, uncovered by the first

**The `call` node was declaring a representation it had not established.**
It claimed `sexp-ptr` for every internally called defun, assuming the
return had been normalised.  It had not always been: a helper whose whole
body is `(mut-str-push-byte out 68)` answers with a raw sentinel, the
caller read 1 as an immediate, got `1 >> 2` = 0, and the enclosing `and`
went false -- the report came out `0` while still exiting 0.  The
declaration is now **computed**, a greatest fixpoint over the call graph,
and a callee nothing can classify declines instead of guessing.

**The string grammar's returns are now classified from what their own emit
comments say.**  `mut-str-push-byte`, `mut-str-push-codepoint` and
`str-codepoint-at` answer with a sentinel and are boxed at a boundary;
`mut-str-make-empty` and `mut-str-finalize` answer with the caller's slot
and must not be.  Getting that pair backwards wraps a string in an
integer.

## Fixed

- `(g (and A B))` and `(g (or A B))` crashing or answering wrongly when
  the connective's value crosses a defun boundary.
- A defun whose body is a connective returning a word its caller then
  dereferences.
- A `call` result unwrapped on the strength of a declaration the callee
  did not honour.

## Verified

33 gates at `findings=0` across the standalone, smokes, selfhost and
extras tiers -- including `nelisp-nelix-native-hot-gate` 6/6 and
`precise-root-coverage` 51/51 -- `ert-full` 5526 with 0 unexpected,
`unsafe-inventory` unchanged at 703, and `nelisp-ai.sh verify` PASS over
70 gates.

## Known issues

None outstanding from v1.1.1's list.  Its one entry, the connectives, is
what this release closes.
