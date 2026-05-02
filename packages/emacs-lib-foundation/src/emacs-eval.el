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
;; bootstrap evaluator does NOT expose this primitive (= function cells
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
    "Polyfill: forward calls to SYMBOL through FUNCTION via `apply'."
    ;; Use a lambda that captures FUNCTION.  Bootstrap eval supports
    ;; lambda + apply.  defalias-via-defun would also work; lambda is
    ;; one less indirection.
    (eval (list 'defun symbol '(&rest args)
                (list 'apply function 'args)))
    function))

(unless (fboundp 'defalias)
  (defun defalias (symbol definition &optional docstring)
    "Polyfill: alias SYMBOL to DEFINITION via `fset'.
DOCSTRING is accepted for arglist parity and currently ignored
(= the polyfill does not yet wire docstrings into the function cell)."
    (ignore docstring)
    (fset symbol definition)
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


(provide 'emacs-eval)

;;; emacs-eval.el ends here
