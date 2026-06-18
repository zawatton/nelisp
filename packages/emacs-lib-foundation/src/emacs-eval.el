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

(unless (boundp 'lexical-binding)
  (defvar lexical-binding t
    "Standalone default: evaluated vendor files are treated as lexical."))

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

(unless (fboundp 'defalias)
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

(unless (and (fboundp 'purecopy)
             (not (get 'purecopy 'emacs-stub-bulk)))
  (defun purecopy (object)
    "Standalone polyfill: return OBJECT unchanged.
NeLisp has no pure storage area, so copying into one has no runtime
effect."
    object)
  (put 'purecopy 'emacs-stub-bulk nil))

;; `declare-function' — Emacs byte-compiler hint, signalling that a
;; function will be defined elsewhere at runtime.  Has no execution
;; semantics under interpreted Elisp.  Implementing as a no-op macro is
;; correct and matches Emacs's own treatment when not byte-compiling.
(unless (fboundp 'declare-function)
  (defmacro declare-function (fn file &optional arglist fileonly)
    "Polyfill: no-op macro (NeLisp standalone has no byte compiler)."
    (ignore fn file arglist fileonly)
    nil))

(unless (and (fboundp 'format-message)
             (not (get 'format-message 'emacs-stub-bulk)))
  (defun format-message (string &rest objects)
    "Standalone polyfill for `format-message'.
Routes through `format' and intentionally omits Emacs's text-quoting
substitution."
    (apply #'format string objects))
  (put 'format-message 'emacs-stub-bulk nil))

(unless (and (fboundp 'with-local-quit)
             (not (get 'with-local-quit 'emacs-stub-bulk)))
  (defmacro with-local-quit (&rest body)
    "Standalone polyfill: evaluate BODY with quits locally enabled."
    (declare (indent 0) (debug (body)))
    (cons 'let (cons '((inhibit-quit nil)) body)))
  (put 'with-local-quit 'emacs-stub-bulk nil))

(unless (and (fboundp 'with-demoted-errors)
             (not (get 'with-demoted-errors 'emacs-stub-bulk)))
  (defmacro with-demoted-errors (format &rest body)
    "Standalone polyfill: run BODY and turn `error' signals into messages."
    (declare (indent 1) (debug (form body)))
    `(condition-case err
         (progn ,@body)
       (error
        (let ((msg (if (fboundp 'error-message-string)
                       (error-message-string err)
                     (format "%S" err))))
          (when (fboundp 'message)
            (message ,format msg))
          nil))))
  (put 'with-demoted-errors 'emacs-stub-bulk nil))

;; `interactive' — Emacs special form marking a defun as interactively
;; callable + parsing arg-spec for `M-x'.  Under NeLisp standalone there
;; is no interactive call surface; the form serves only as a load-time
;; marker that `defun' has already consumed.  Polyfill as a no-op fn so
;; `(interactive ...)' inside a defun body evaluates to nil harmlessly.
(unless (fboundp 'interactive)
  (defmacro interactive (&rest _args)
    "Polyfill: no-op macro for NeLisp standalone (no interactive call surface).
Must be a macro, not a `defun': a function evaluates its arguments, so
`(interactive (list (read-directory-name ...)))' in a command body would
fire `read-directory-name' and abort in batch (no minibuffer) even when
the command was called programmatically with explicit args.  Expanding to
nil mirrors real Emacs, where `interactive' is a no-op outside
`call-interactively'.  `commandp' / `interactive-form' inspect the literal
body form (pre-expansion), so command-ness is preserved."
    nil))

(unless (fboundp 'interactive-form)
  (defun emacs-eval--lambda-interactive-form (function)
    "Return FUNCTION's leading `(interactive ...)' form, or nil."
    (let ((body (cond
                 ((and (consp function) (eq (car function) 'lambda))
                  (cdr (cdr function)))
                 ((and (consp function) (eq (car function) 'closure))
                  (cdr (cdr (cdr function))))
                 (t nil))))
      (when (and body
                 (stringp (car body)))
        (setq body (cdr body)))
      (and (consp (car body))
           (eq (car (car body)) 'interactive)
           (car body))))

  (defun interactive-form (function)
    "Return FUNCTION's interactive spec, or nil.
This standalone polyfill first checks symbol metadata installed by
macro fallbacks such as `define-minor-mode', then falls back to
inspecting a literal lambda / closure body."
    (cond
     ((symbolp function)
      (or (get function 'interactive-form)
          (and (fboundp function)
               (interactive-form (symbol-function function)))))
     ((and (consp function)
           (eq (car function) 'autoload))
      (and (car (cdr (cdr (cdr function))))
           (list 'interactive)))
     (t
      (emacs-eval--lambda-interactive-form function)))))

(unless (fboundp 'commandp)
  (defun commandp (function &optional _for-call-interactively)
    "Return non-nil when FUNCTION is interactively callable."
    (cond
     ((symbolp function)
      (and (fboundp function)
           (or (interactive-form function)
               (commandp (symbol-function function)))))
     (t
      (and (interactive-form function) t)))))

;; `autoload' — Emacs's lazy-loading hint.  NeLisp standalone has no
;; autoload mechanism (= every module is loaded eagerly via the
;; AnvilModuleRegistry chain), so the polyfill is a no-op.  Callers
;; that walk `autoload-file-name' / `commandp' may need follow-up
;; polyfills.
(unless (fboundp 'autoload)
  (defun autoload (function file &optional docstring interactive type)
    "Lazy-load FUNCTION from FILE on first call (standalone autoload).

When FUNCTION is not yet defined, install a thunk that `load's FILE — which
is expected to redefine FUNCTION — on the first call, then invokes the real
definition with the original arguments.  Functions that are already defined
(the common case under eager module loading) are left untouched, so this is
a no-op for them.  DOCSTRING/INTERACTIVE/TYPE are accepted for
call-compatibility and otherwise ignored.

This keeps startup bounded: vendor libraries can be declared with `autoload'
and only paid for when a workflow actually calls them."
    (ignore docstring interactive type)
    (when (and (symbolp function) (not (fboundp function)))
      (let (thunk)
        (setq thunk
              (lambda (&rest args)
                (load file)
                (when (eq (symbol-function function) thunk)
                  (error "Autoloading `%s' from %s did not define it"
                         function file))
                (apply function args)))
        (fset function thunk)))
    function))

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

;; Compile-time eval markers.  Under interpreted Elisp these usually
;; reduce to BODY.  Vendor `let-when-compile' relies on
;; `macroexpand-all' forcing nested `eval-when-compile' forms while
;; compile-time variables are temporarily bound, so the standalone path
;; returns a quoted constant like host Emacs's macroexpansion does.
(unless (fboundp 'eval-when-compile)
  (defmacro eval-when-compile (&rest body)
    (list 'quote (eval (cons 'progn body) t))))

(unless (fboundp 'eval-and-compile)
  (defmacro eval-and-compile (&rest body) (cons 'progn body)))

(unless (fboundp 'macroexp-progn)
  (defun macroexp-progn (body)
    "Return BODY as one expression, preserving side-effect order."
    (cond
     ((null body) nil)
     ((null (cdr body)) (car body))
     (t (cons 'progn body)))))

(unless (fboundp 'macroexp-parse-body)
  (defun macroexp-parse-body (body)
    "Split BODY into declarations and remaining forms.
Return (DECLARATIONS . BODY-FORMS), matching the shape used by Emacs
macro helpers such as `iter-defun'.  A leading docstring and any
following `(declare ...)' forms are treated as declarations."
    (let ((declarations nil)
          (cur body))
      (when (and cur (stringp (car cur)))
        (setq declarations (cons (car cur) declarations))
        (setq cur (cdr cur)))
      (while (and cur (consp (car cur)) (eq (car (car cur)) 'declare))
        (setq declarations (cons (car cur) declarations))
        (setq cur (cdr cur)))
      (cons (nreverse declarations) cur))))

(unless (fboundp 'macroexpand-all)
  (defun macroexpand-all (form &optional _environment)
    "Minimal recursive macro expander for standalone load-time forms.
This handles one macro layer before structural recursion.  That is
still much smaller than Emacs's full macroexp engine, but it is enough
for vendored load-time helpers such as generator.el's `cl-macrolet'
rewrite of `iter-yield'."
    (cond
     ((not (consp form)) form)
     ((eq (car form) 'quote) form)
     ((eq (car form) 'function) form)
     ((eq (car form) 'eval-when-compile)
      (list 'quote (eval (macroexp-progn (cdr form)) t)))
     (t
      (let ((expanded (macroexpand-1 form _environment)))
        (if (not (equal expanded form))
            (macroexpand-all expanded _environment)
          (let ((out nil)
                (cur form))
            (while cur
              (setq out (cons (macroexpand-all (car cur) _environment)
                              out))
              (setq cur (cdr cur)))
            (nreverse out))))))))

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
