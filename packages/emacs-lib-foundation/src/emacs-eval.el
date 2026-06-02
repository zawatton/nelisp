;;; emacs-eval.el --- NeLisp port of Emacs C core eval.c data-cell APIs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 1.6 — Layer 2.
;;
;; Ports the function-cell accessors (`defalias', `fset') and the
;; bytecomp hint macro (`declare-function') from Emacs's C core +
;; subr.el.  These are tiny but essential — `defalias' is the
;; runtime equivalent of `defun' for an existing function, and
;; macroexpansion uses it internally.  `declare-function' has no
;; runtime semantics under Emacs either; it is purely a
;; byte-compiler hint, so a no-op macro is the correct port.

;;; Code:

;; `fset' — install FUNCTION as the function-cell of SYMBOL.  NeLisp's
;; bootstrap evaluator may not expose this primitive (= function cells
;; are settable only via `defun' at evaluation time).  We approximate by
;; installing a forwarding `defun' that applies FUNCTION to its args.
;;
;; This is NOT a true alias (= the forwarder is a distinct function
;; object) but is observationally equivalent for every caller that just
;; invokes the symbol via `funcall' / `(SYMBOL ARGS...)'.  Callers that
;; inspect `symbol-function' get the forwarder, not FUNCTION — that is a
;; known limitation, called out here so future debugging knows where to
;; look.  Phase 2 will lobby NeLisp to expose true `fset' as a builtin.
(unless (fboundp 'fset)
  (defun fset (symbol function)
    "Polyfill: forward calls to SYMBOL through FUNCTION.
Quote FUNCTION when splicing into the forwarder body so the reader
does not later evaluate it as a bare variable reference (= avoids
`void-variable: null' style failures when FUNCTION is a symbol whose
value cell is unbound but function cell is fine)."
    (eval (list 'defun symbol '(&rest args)
                (list 'apply (list 'quote function) 'args)))
    function))

(when (or (not (boundp 'emacs-version))
          (not (fboundp 'defalias)))
  (defun defalias (symbol definition &optional docstring)
    "Polyfill: alias SYMBOL to DEFINITION.
DOCSTRING is accepted for arglist parity and currently ignored
(= the polyfill does not yet wire docstrings into the function cell)."
    (ignore docstring)
    (if (and (symbolp definition)
             (not (fboundp definition)))
        ;; NeLisp's native `fset' resolves symbol functions eagerly.
        ;; Generate a late-bound forwarder for `#'foo' before `foo' is
        ;; defined, as seen in vendor easy-mmode.el.
        (eval (list 'defun symbol '(&rest args)
                    (list 'apply (list 'quote definition) 'args)))
      (fset symbol definition))
    symbol))

;; `declare-function' — Emacs byte-compiler hint, signalling that a
;; function will be defined elsewhere at runtime.  Has no execution
;; semantics under interpreted Elisp.  Implementing as a no-op macro is
;; correct and matches Emacs's own treatment when not byte-compiling.
(unless (fboundp 'declare-function)
  (defmacro declare-function (fn file &optional arglist fileonly)
    "Polyfill: no-op macro (NeLisp standalone has no byte compiler)."
    (ignore fn file arglist fileonly)
    nil))

;; `interactive' — Emacs special form marking a defun as interactively
;; callable + parsing arg-spec for `M-x'.  Under NeLisp standalone there
;; is no interactive call surface; the form serves only as a load-time
;; marker that `defun' has already consumed.  Polyfill as a no-op fn so
;; `(interactive ...)' inside a defun body evaluates to nil harmlessly.
(unless (fboundp 'interactive)
  (defun interactive (&rest _args)
    "Polyfill: no-op for NeLisp standalone (no interactive call surface)."
    nil))

;; `autoload' — Emacs's lazy-loading hint.  NeLisp standalone has no
;; autoload mechanism (= every module is loaded eagerly via the
;; AnvilModuleRegistry chain), so the polyfill is a no-op.  Callers
;; that walk `autoload-file-name' / `commandp' may need follow-up
;; polyfills.
(unless (fboundp 'autoload)
  (defun autoload (function file &optional docstring interactive type)
    "Polyfill: no-op for NeLisp standalone."
    (ignore function file docstring interactive type)
    nil))

;; Obsoletion-tracking aliases.  In Emacs these record metadata for
;; deprecation warnings; under NeLisp standalone we just install the
;; alias and skip the bookkeeping.
(unless (fboundp 'define-obsolete-function-alias)
  (defun define-obsolete-function-alias
      (obsolete-name current-name &optional when docstring)
    "Polyfill: route through `defalias', drop deprecation metadata."
    (ignore when docstring)
    (defalias obsolete-name current-name)))

(unless (fboundp 'define-obsolete-variable-alias)
  (defun define-obsolete-variable-alias
      (obsolete-name current-name &optional when docstring)
    "Polyfill: route through `defvaralias' (= defalias for vars).
NeLisp may not have `defvaralias' yet either; in that case we just
set OBSOLETE-NAME's value cell to track CURRENT-NAME's value at this
moment.  Live aliasing of subsequent assignments is Phase 4."
    (ignore when docstring)
    (when (boundp current-name)
      (set obsolete-name (symbol-value current-name)))
    obsolete-name))

(unless (fboundp 'make-obsolete)
  (defun make-obsolete (obsolete-name current-name &optional when)
    "Polyfill: no-op deprecation hint."
    (ignore obsolete-name current-name when)
    nil))

(unless (fboundp 'make-obsolete-variable)
  (defun make-obsolete-variable (obsolete-name current-name &optional when access-type)
    "Polyfill: no-op deprecation hint."
    (ignore obsolete-name current-name when access-type)
    nil))

;; `internal-make-var-non-special' is an Emacs C-core helper used by a
;; few dump/bootstrap files to mark variables as lexically bindable.
;; NeLisp does not model specialness separately yet, so this is a
;; metadata-only no-op on the standalone path.
(unless (fboundp 'internal-make-var-non-special)
  (defun internal-make-var-non-special (symbol)
    "Polyfill: accept SYMBOL and return nil."
    (ignore symbol)
    nil))

;; `defsubst' — Emacs special form for an inline-hinted function.  The
;; inlining is a byte compiler optimisation; semantically identical to
;; `defun' under interpreted Elisp.
(unless (fboundp 'defsubst)
  (defmacro defsubst (name arglist &rest body)
    "Polyfill: defsubst as plain defun (no inline hint)."
    (cons 'defun (cons name (cons arglist body)))))

;; Compile-time eval markers.  Under interpreted Elisp these reduce
;; to the same body; the byte-compiler distinction does not matter.
(unless (fboundp 'eval-when-compile)
  (defmacro eval-when-compile (&rest body) (cons 'progn body)))

(unless (fboundp 'eval-and-compile)
  (defmacro eval-and-compile (&rest body) (cons 'progn body)))

;; Misc Emacs metadata declarations that are no-ops at run-time.
(unless (fboundp 'declare)
  (defmacro declare (&rest _decls)
    "Polyfill: no-op (compile-time hint surface)."
    nil))

(unless (fboundp 'with-no-warnings)
  (defmacro with-no-warnings (&rest body)
    "Polyfill: just eval BODY; no compiler around."
    (cons 'progn body)))

(unless (fboundp 'with-suppressed-warnings)
  (defmacro with-suppressed-warnings (_warnings &rest body)
    "Polyfill: ignore WARNINGS, eval BODY."
    (cons 'progn body)))


(provide 'emacs-eval)

;;; emacs-eval.el ends here
