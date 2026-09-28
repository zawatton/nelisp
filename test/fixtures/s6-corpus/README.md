# S6 compiler-tier corpora

This directory holds argument-list corpora for `test/nelisp-eln-s6-measure.sh`
(tools/ai/eln-progress.org, S6.3-S6.15). Each `<function>.el` file is a single
sexp: a list of argument lists, exactly as `nelisp-eln-s6-measure-driver.el`'s
`nelisp-eln-s6-measure-read-corpus` expects. The harness applies the named
function to each argument list directly (`(apply fn args)`), captures either
`(:ok VALUE)` or `(:err CONDITION)`, and compares that capture across host
GNU Emacs 31.1, the NeLisp VM (source-interpreted), and NeLisp's native-load
of the same .eln.

All entries below were verified on host GNU Emacs 31.1 (comp-abi-hash
`ba35c031`) by loading `bytecomp`/`cconv`/`macroexp` and calling the real
functions through `nelisp-eln-s6-measure-capture`, run twice to confirm
stability. Results are recorded in `~/.cache/tmp/s6-corpus/host-results.el`;
the generating script is alongside it as `gen-host-results.el`.

## Direct corpora (5)

These functions are self-contained: every special variable they touch either
has a global default or is bound by the function itself, so calling them with
no ambient dynamic state (as the harness does) is faithful to normal usage.

- `macroexp--all-forms.el` (S6.3) - 5 entries.
- `macroexpand-1.el` (S6.4) - 6 entries.
- `macroexp-parse-body.el` (S6.5) - 6 entries.
- `cconv-closure-convert.el` (S6.6) - 6 entries; `cconv-closure-convert` is
  the closure-conversion entry point and `let`-binds its own working state
  (`cconv-freevars-alist`, `cconv-var-classification`,
  `cconv--dynbound-variables`) before calling `cconv-analyze-form`/
  `cconv-convert`, so it needs nothing from the caller.
- `cconv--set-diff.el` (S6.8) - 5 entries; a pure list function.

Each corpus covers its normal paths plus at least one genuine
wrong-type-argument error path, and avoids gensyms/uninterned symbols and
address-bearing values so results stay `equal`-comparable across runs.

## Needs-wrapper functions (8) - no corpus file here

The remaining 8 functions all read from special variables that bytecomp.el
or cconv.el declares with `(defvar SYM)` and **no value form**. In Emacs,
that form only marks SYM as dynamically-scoped for the rest of the *file it
appears in* - it does not give SYM a global value, and it does not make a
plain `(let ((SYM v)) ...)` written in a different, unrelated file bind SYM
dynamically either (such a `let` creates a lexical binding instead, invisible
to the compiled function, and the call fails with `void-variable`). This was
confirmed empirically: a raw call from a fresh process reaches `void-variable`
for `byte-compile--for-effect`, `byte-compile-free-references`,
`byte-compile-free-assignments`, `byte-compile--\#$`, and `cconv-freevars-alist`/
`cconv-var-classification` for the respective functions below. Because the
harness always calls `FUNCTION` with `ARGS` directly and cannot inject any
extra dynamic context, a corpus file for these would either error on every
entry or silently exercise the wrong code path (e.g. `byte-compile-form`
always returns nil when its caller never bound `byte-compile--for-effect` to
a true value beforehand) - writing one would not honestly measure the
function. Below is, for each, the dynamic state required and a thin wrapper
that supplies it; both were tested on host Emacs 31.1 (results in
`~/.cache/tmp/s6-corpus/host-results.el`, keys ending `/wrapper`). Adopting
these as an actual S6 corpus is future work: it needs either a harness change
that lets a corpus name a wrapper function instead of the raw vendor symbol,
or a wrapper checked in next to the extracted vendor source so the same
symbol name resolves consistently under host/VM/native.

Shared setup (mirrors the state `byte-compile-top-level`/
`byte-compile-close-variables` establish in bytecomp.el before any
byte-compile-* handler runs):

```elisp
(defvar byte-compile--for-effect)
(defvar byte-compile-free-references)
(defvar byte-compile-free-assignments)
(defvar byte-compile--\#$)

(defmacro s6-corpus--byte-compile-env (&rest body)
  `(let ((byte-compile--for-effect nil)
         (byte-compile-constants nil)
         (byte-compile-variables nil)
         (byte-compile-tag-number 0)
         (byte-compile-depth 0)
         (byte-compile-maxdepth 0)
         (byte-compile--lexical-environment nil)
         (byte-compile-reserved-constants 0)
         (byte-compile-output nil)
         (byte-compile-jump-tables nil)
         (byte-compile-free-references nil)
         (byte-compile-free-assignments nil)
         (byte-compile-macro-environment (copy-alist byte-compile-initial-macro-environment))
         (byte-compile-function-environment nil)
         (byte-compile-bound-variables nil)
         (byte-compile-lexical-variables nil)
         (byte-compile-const-variables nil)
         (byte-compile--\#$ nil)
         (byte-native-compiling nil))
     ,@body))
```

The return value of most byte-compile-* handlers is not self-contained (e.g.
`byte-compile-form` returns nil whenever FOR-EFFECT is falsy, regardless of
what it compiled); each wrapper therefore returns a snapshot of
`byte-compile-output` (the emitted LAP, a list of symbols/small
integers/constants - no addresses, `equal`-comparable) alongside the raw
return value.

- **`byte-compile-constant`** (S6.15) - needs `byte-compile--for-effect`.
  ```elisp
  (defun s6-corpus--byte-compile-constant (const &optional for-effect)
    (s6-corpus--byte-compile-env
     (setq byte-compile--for-effect for-effect)
     (let ((r (byte-compile-constant const)))
       (list :return r :output (reverse byte-compile-output)))))
  ```
  Verified: `(42 nil)` -> for-effect stays nil, `:output ((byte-constant 42))`;
  `(42 t)` -> for-effect is cleared, nothing pushed (`:output nil`).

- **`byte-compile-setq`** (S6.13) - needs `byte-compile--for-effect` (via the
  `byte-compile-form`/`byte-compile-out`/`byte-compile-variable-set` chain it
  calls).
  ```elisp
  (defun s6-corpus--byte-compile-setq (form)
    (s6-corpus--byte-compile-env
     (let ((r (byte-compile-setq form)))
       (list :return r :output (reverse byte-compile-output)))))
  ```
  Verified: `((setq foo 1))` -> `:output ((byte-constant 1) (byte-dup . 0)
  (byte-varset foo))`.

- **`byte-compile-if`** (S6.12) - needs `byte-compile-free-references`/
  `byte-compile-free-assignments` (via `byte-compile-maybe-guarded`).
  ```elisp
  (defun s6-corpus--byte-compile-if (form)
    (s6-corpus--byte-compile-env
     (let ((r (byte-compile-if form)))
       (list :return r :output (reverse byte-compile-output)))))
  ```
  Verified for both the 2-clause and 3-clause forms of `if`.

- **`byte-compile-funcall`** (S6.14) - the non-empty path needs nothing extra
  (it delegates to `byte-compile-form`, which self-binds
  `byte-compile--for-effect`); the zero-argument error path reads
  `byte-compile--for-effect` directly and needs it bound.
  ```elisp
  (defun s6-corpus--byte-compile-funcall (form)
    (s6-corpus--byte-compile-env
     (let ((r (byte-compile-funcall form)))
       (list :return r :output (reverse byte-compile-output)))))
  ```
  Verified: `((funcall 'foo 1 2))` -> normal call, `:return 3`; `((funcall))`
  -> logs a compiler error and rewrites to a `(signal 'wrong-number-of-
  arguments '(funcall 0))` call rather than signalling a Lisp error itself
  (`:return nil`, deterministic `:output`).

- **`byte-compile-form`** (S6.10) - the general dispatcher; needs the full
  environment above (constants/warnings/lexical-variable bookkeeping it may
  consult for any form).
  ```elisp
  (defun s6-corpus--byte-compile-form (form &optional for-effect)
    (s6-corpus--byte-compile-env
     (let ((r (byte-compile-form form for-effect)))
       (list :return r :output (reverse byte-compile-output)))))
  ```
  Verified for a constant and a bare variable reference.

- **`byte-compile-make-closure`** (S6.11) - same environment; internally
  calls `byte-compile-lambda`/`byte-compile-form`.
  ```elisp
  (defun s6-corpus--byte-compile-make-closure (form)
    (s6-corpus--byte-compile-env
     (let ((r (byte-compile-make-closure form)))
       (list :return r :output (reverse byte-compile-output)))))
  ```
  Verified for `(internal-make-closure (x) (y) nil x)`.

- **`byte-compile-lambda`** (S6.9) - needs `byte-compile--\#$` and the rest of
  `byte-compile-close-variables`'s bindings (it calls `byte-compile-top-level`
  itself, which re-establishes depth/output/constants, so those need not be
  pre-bound).
  ```elisp
  (defun s6-corpus--byte-compile-lambda (fun &optional reserved-csts)
    (s6-corpus--byte-compile-env
     (byte-compile-lambda fun reserved-csts)))
  ```
  Verified: `((lambda (x) x))` -> a byte-code function object (Emacs's
  `equal`, unlike `eq`, compares byte-code objects structurally, confirmed on
  host: two independent compiles of the same lambda are `equal`); `(not-a-
  lambda)` -> `(error "Not a lambda list: not-a-lambda")`.

- **`cconv--convert-function`** (S6.7) - needs `cconv-freevars-alist` (a list
  of `(BODY . FREE-VARS)` pairs that `cconv-convert` normally maintains
  during its own recursion; `cconv--convert-function` pops the first entry
  and asserts its car is `equal` to the BODY argument) and
  `cconv-var-classification`.
  ```elisp
  (defvar cconv-freevars-alist)
  (defvar cconv-var-classification)
  (defvar cconv--dynbound-variables)

  (defun s6-corpus--cconv--convert-function
      (freevars-alist args body env parentform &optional docstring)
    (let ((cconv-freevars-alist freevars-alist)
          (cconv-var-classification nil)
          (cconv--dynbound-variables nil))
      (cconv--convert-function args body env parentform docstring)))
  ```
  Verified: `((((x))) (a) (x) nil nil)` (freevars-alist has one entry whose
  car `(x)` matches BODY `(x)`, with no free vars) -> `#'(lambda (a) x)`;
  `((((z))) (a) (y) nil nil)` (car `(z)` does not match BODY `(y)`) ->
  `(cl-assertion-failed (equal body (caar cconv-freevars-alist)))`, a genuine
  and deterministic error path.
