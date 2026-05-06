;;; emacs-pcase.el --- Minimal pcase polyfill for NeLisp standalone  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 10 — extracted from `emacs-stub.el' (= the Phase 4
;; batch 2 placeholder).  The vendored `pcase.el' uses an old `\,'
;; symbol-escape syntax that NeLisp's reader does not parse, so we
;; provide a minimal pcase implementation that covers the pattern
;; subset cl-macs / cl-loop / cl-some etc. expand into.
;;
;; Pattern syntax supported:
;;   `_'                — catch-all
;;   INTEGER / STRING   — `equal' test
;;   `(quote X)' / `'X' — `eq' test
;;   SYMBOL (bare)      — bind to value, always match
;;   `(pred FN)'        — call (FN value), match if non-nil
;;   `(and P1 P2 ...)'  — match if every P matches (binds ALL)
;;   `(or P1 P2 ...)'   — match if any P matches (no bindings)
;;   `(guard EXPR)'     — match if EXPR true
;;   `(let PAT EXPR)'   — bind PAT to result of EXPR (always match)
;;   `(backquote PAT)'  — destructure PAT.  Inside PAT:
;;     - `(comma SYM)'  → bind SYM to value-at-position
;;     - literal cons   → recursive shape match
;;     - literal atom   → equality test
;;
;; Backquote-pattern is the critical one — cl-macs uses it heavily
;; for destructuring.  E.g. `\`(,a ,b)' matches a 2-elem cons; binds
;; a=(car val), b=(cadr val).
;;
;; Each definition is gated on `unless (fboundp ...)' so loading
;; under host Emacs (= where vendor pcase.el is already loaded) is
;; a cheap no-op.

;;; Code:

;; Phase 4 B (2026-05-06): the helper functions
;; `emacs-pcase--test' / `--and' / `--or' / `--backquote' MUST exist
;; even on host emacs because the nelisp-driver pcase override below
;; references them.  Previously they lived inside `(unless (fboundp
;; 'pcase) ...)' and were never defined under host driver, breaking
;; subprocess `bin/nemacs --batch --eval' invocations that set
;; `nelisp-emacs-vendor-root' and trigger the override.  Moved out.

(defun emacs-pcase--test (pattern value-form)
    "Build (TEST-FORM . BINDINGS) for matching PATTERN against VALUE-FORM.
VALUE-FORM is an elisp expression that evaluates to the value being
tested.  TEST-FORM is an elisp expression that evaluates to non-nil
when the pattern matches.  BINDINGS is a list of (SYMBOL FORM) pairs
to let-bind in the case body when matched."
    (cond
     ;; `_' wildcard.
     ((eq pattern '_) (cons t nil))
     ;; Self-evaluating keyword (= `:foo'): match by `eq'.  Without
     ;; this guard the bare-symbol clause below would bind the
     ;; keyword as a variable, making every keyword `pcase' branch
     ;; match the first case unconditionally.
     ((keywordp pattern)
      (cons (list 'eq value-form pattern) nil))
     ;; `nil' / `t' literals — treat as eq-test, not as bind pattern.
     ((or (null pattern) (eq pattern t))
      (cons (list 'eq value-form (list 'quote pattern)) nil))
     ;; Bare symbol: bind to value, always match.
     ((symbolp pattern)
      (cons t (list (list pattern value-form))))
     ;; Number / string literal.
     ((or (integerp pattern) (stringp pattern))
      (cons (list 'equal value-form pattern) nil))
     ;; Cons cell — examine head for pattern type.
     ((consp pattern)
      (let ((head (car pattern))
            (rest (cdr pattern)))
        (cond
         ;; (quote X)
         ((eq head 'quote)
          (cons (list 'eq value-form (list 'quote (car rest))) nil))
         ;; (pred FN)
         ((eq head 'pred)
          (let ((fn (car rest)))
            (cons (list 'funcall (list 'function fn) value-form) nil)))
         ;; (guard EXPR)
         ((eq head 'guard)
          (cons (car rest) nil))
         ;; (let PAT EXPR)
         ((eq head 'let)
          (let* ((sub-pat (car rest))
                 (sub-expr (car (cdr rest)))
                 (built (emacs-pcase--test sub-pat sub-expr)))
            (cons (car built) (cdr built))))
         ;; (and P1 P2 ...)
         ((eq head 'and)
          (emacs-pcase--and rest value-form))
         ;; (or P1 P2 ...)
         ((eq head 'or)
          (emacs-pcase--or rest value-form))
         ;; (backquote ...) - destructure cons / atom shape
         ((eq head 'backquote)
          (emacs-pcase--backquote (car rest) value-form))
         ;; Unknown — treat as opaque catch-all (= permissive).
         (t (cons t nil)))))
     ;; Other atom (symbol via symbolp above; vector etc.)
     (t (cons (list 'equal value-form (list 'quote pattern)) nil))))

  (defun emacs-pcase--and (patterns value-form)
    "Build (TEST . BINDINGS) for an `and' pattern (= all PATTERNS match)."
    (let ((tests nil)
          (bindings nil)
          (cur patterns))
      (while cur
        (let* ((built (emacs-pcase--test (car cur) value-form))
               (t1 (car built))
               (b1 (cdr built)))
          (setq tests (cons t1 tests))
          (setq bindings (append bindings b1)))
        (setq cur (cdr cur)))
      (cons (cons 'and (let ((rev nil))
                         (while tests (setq rev (cons (car tests) rev)) (setq tests (cdr tests)))
                         rev))
            bindings)))

  (defun emacs-pcase--or (patterns value-form)
    "Build (TEST . BINDINGS) for an `or' pattern.  No bindings (= ambiguous)."
    (let ((tests nil)
          (cur patterns))
      (while cur
        (let* ((built (emacs-pcase--test (car cur) value-form))
               (t1 (car built)))
          (setq tests (cons t1 tests)))
        (setq cur (cdr cur)))
      (cons (cons 'or (let ((rev nil))
                        (while tests (setq rev (cons (car tests) rev)) (setq tests (cdr tests)))
                        rev))
            nil)))

  (defun emacs-pcase--backquote (pat value-form)
    "Build (TEST . BINDINGS) for a backquote-pattern.
Walks PAT recursively; `(comma SYM)' binds SYM to corresponding
position; literal cons recurses with `car'/`cdr' index forms; atom
does `equal' check."
    (cond
     ;; (comma SYM) — bind SYM to value-form, always match.
     ((and (consp pat) (eq (car pat) 'comma))
      (let ((sym (car (cdr pat))))
        (cond
         ((eq sym '_) (cons t nil))
         ((symbolp sym) (cons t (list (list sym value-form))))
         (t (emacs-pcase--test sym value-form)))))
     ;; (comma-at SYM) — bind SYM to remaining list (= value-form is tail).
     ((and (consp pat) (eq (car pat) 'comma-at))
      (let ((sym (car (cdr pat))))
        (cons t (list (list sym value-form)))))
     ;; Cons cell — recursively destructure car / cdr.
     ((consp pat)
      (let* ((head-build (emacs-pcase--backquote
                          (car pat) (list 'car value-form)))
             (tail-build (emacs-pcase--backquote
                          (cdr pat) (list 'cdr value-form))))
        (cons (list 'and
                    (list 'consp value-form)
                    (car head-build)
                    (car tail-build))
              (append (cdr head-build) (cdr tail-build)))))
     ;; nil at end of list — match nil tail.
     ((null pat)
      (cons (list 'null value-form) nil))
     ;; Other atom — equality test.
     (t
      (cons (list 'equal value-form (list 'quote pat)) nil))))

(unless (fboundp 'pcase)
  (defmacro pcase (expr &rest cases)
    "Phase 10 (= ex-Phase 4 batch 2) pcase: dispatch EXPR through CASES.
See `emacs-pcase--test' for supported pattern shapes."
    (let ((value-sym (make-symbol "--pcase-value--"))
          (cond-clauses nil))
      (dolist (case cases)
        (let* ((pat (car case))
               (body (cdr case))
               (built (emacs-pcase--test pat value-sym))
               (test (car built))
               (bindings (cdr built)))
          (push (list test
                      (if bindings
                          (cons 'let (cons bindings body))
                        (cons 'progn body)))
                cond-clauses)))
      (let ((forward nil))
        (while cond-clauses
          (setq forward (cons (car cond-clauses) forward))
          (setq cond-clauses (cdr cond-clauses)))
        (list 'let (list (list value-sym expr))
              (cons 'cond forward))))))

;; Phase 4 B (2026-05-06): override `pcase' ONLY under the nelisp
;; Rust runtime.  Detection: host emacs ships
;; `comp-trampoline-compile' (= native-comp 28+ feature that NeLisp
;; lacks); when that symbol is unbound we are running on NeLisp.
;; Under bin/nemacs --driver=host we are still on host emacs even
;; though `nelisp-emacs-vendor-root' is set, so vendor-root alone is
;; the wrong signal.  Keeping host's richer C-native pcase intact is
;; required because `make test' subprocess-launches bin/nemacs and
;; expects host semantics.
;;
;; Caveat: NeLisp routes macro expansion through an internal Rust-
;; side registry that is not visible from elisp, so this override
;; only applies when callers use `symbol-function' / `macroexpand'
;; explicitly.  Full un-skip of grammar-rich callers (= nelisp-regex's
;; `(or :star :plus :opt)' pattern) requires upstream NeLisp to either
;; expand its built-in pcase grammar or expose a registry-override
;; API.

;; Direct override attempts (`fset' / `defalias' to either a closure or
;; a quoted `(macro lambda …)' form) failed under the nelisp driver:
;;   - closure form    → `wrong-type-argument function closure'
;;   - quoted lambda    → `wrong-type-argument function lambda'
;; NeLisp's macro dispatcher uses an internal Rust-side lookup that
;; doesn't honour elisp-level `defalias'.  Real un-skip of grammar-
;; rich pcase callers (= nelisp-regex.el's `(or :star :plus :opt)'
;; pattern) requires upstream NeLisp to either expand its built-in
;; pcase grammar or expose a registry-override API.  For our local
;; src/nelisp-regex.el copy we instead rewrite affected pcase forms
;; into `cond'.

(unless (fboundp 'pcase-let)
  (defmacro pcase-let (bindings &rest body)
    "Stub: equivalent to plain `let'."
    (cons 'let (cons bindings body))))

(unless (fboundp 'pcase-let*)
  (defmacro pcase-let* (bindings &rest body)
    "Stub: equivalent to plain `let*'."
    (cons 'let* (cons bindings body))))

(unless (fboundp 'pcase-dolist)
  (defmacro pcase-dolist (spec &rest body)
    "Stub: equivalent to plain `dolist'."
    (cons 'dolist (cons spec body))))

(unless (featurep 'pcase) (provide 'pcase))

(provide 'emacs-pcase)

;;; emacs-pcase.el ends here
