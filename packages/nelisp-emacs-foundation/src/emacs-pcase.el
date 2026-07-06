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
;;   `(quote X)' / `'X' — `equal' test (structural, not identity)
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

(defun emacs-pcase--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this module."
  (or (not (boundp 'emacs-version))
      (get symbol 'emacs-stub-bulk)
      (not (fboundp symbol))))

(defun emacs-pcase--pred-form (fn value-form)
  "Build a predicate test form for (pred FN) against VALUE-FORM."
  (if (and (consp fn) (eq (car fn) 'not))
      (list 'not (emacs-pcase--pred-form (car (cdr fn)) value-form))
    (list 'funcall (list 'function fn) value-form)))

(defun emacs-pcase--macroexpand-pattern (pattern)
  "Expand a single pcase macro PATTERN when it names a local expander."
  (if (and (consp pattern)
           (symbolp (car pattern))
           (not (memq (car pattern)
                      '(quote pred guard let and or cons backquote \`)))
           (get (car pattern) 'pcase-macroexpander))
      (emacs-pcase--macroexpand-pattern
       (apply (get (car pattern) 'pcase-macroexpander) (cdr pattern)))
    pattern))

(defun emacs-pcase--case-patterns (pattern)
  "Return the top-level alternative patterns for PATTERN."
  (let ((expanded (emacs-pcase--macroexpand-pattern pattern)))
    (if (and (consp expanded) (eq (car expanded) 'or))
        (let ((alts nil)
              (cur (cdr expanded)))
          (while cur
            (setq alts (append alts (emacs-pcase--case-patterns (car cur))))
            (setq cur (cdr cur)))
          alts)
      (list expanded))))

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
         ;; (quote DATUM)
         ((eq head 'quote)
          ;; `equal', not `eq': a `(quote DATUM)' pattern must match any
          ;; value that is STRUCTURALLY the same, not merely the same
          ;; object.  `eq' happens to work for the common case of a
          ;; quoted symbol (interned, so `eq'-comparable) but silently
          ;; never matches a quoted compound datum (list/vector/string)
          ;; compared against a freshly-consed runtime value of the same
          ;; shape -- `(eq (list t t) '(t t))' is nil in both this
          ;; polyfill and real Emacs.  That silent non-match let a later,
          ;; structurally-overlapping backquote-pattern clause win
          ;; instead, selecting the wrong helper macro out of a `pcase'
          ;; dispatch that assumed exact-match precedence -- root cause
          ;; of the Magit bridge M2 blocker (nelisp-emacs-lib Doc 33 item
          ;; 239's `cond-let*' repro `(cond-let* ([x 1] [x (+ x 1)] x)
          ;; (t 99))' => `void-variable: x', mirrors dev/nelisp commit
          ;; 71de60a6).
          (cons (list 'equal value-form (list 'quote (car rest))) nil))
         ;; (pred FN)
         ((eq head 'pred)
          (let ((fn (car rest)))
            (cons (emacs-pcase--pred-form fn value-form) nil)))
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
         ;; (cons P1 P2)
         ((eq head 'cons)
          (emacs-pcase--cons rest value-form))
         ;; (backquote ...) - destructure cons / atom shape
         ((or (eq head 'backquote) (eq head '\`))
          (emacs-pcase--backquote (car rest) value-form))
         ;; (CUSTOM ...) from `pcase-defmacro'.
         ((and (symbolp head) (get head 'pcase-macroexpander))
          (emacs-pcase--test (emacs-pcase--macroexpand-pattern pattern)
                             value-form))
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
      ;; A sibling `(guard EXPR)' (or `(let PAT EXPR)') in the same `and' may
      ;; reference variables bound by the other sub-patterns, but those bindings
      ;; were only applied to the case BODY, not to the test — so e.g.
      ;; `(and n (guard (> n 3)))' tested `(> n 3)' with `n' UNBOUND (an
      ;; uncatchable void-variable abort on the bare reader).  Evaluate the whole
      ;; `and' test with the accumulated bindings in scope.  All sub-patterns test
      ;; the same (side-effect-free) value-form, so re-binding for the test as
      ;; well as the body is safe; `let*' covers bindings that depend on earlier
      ;; ones, and order-independence handles a guard written before its binder.
      (let ((and-test (cons 'and (let ((rev nil))
                                   (while tests (setq rev (cons (car tests) rev)) (setq tests (cdr tests)))
                                   rev))))
        (cons (if bindings (list 'let* bindings and-test) and-test)
              bindings))))

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

  (defun emacs-pcase--cons (patterns value-form)
    "Build (TEST . BINDINGS) for a `(cons P1 P2)' pattern."
    (let* ((head-pattern (car patterns))
           (tail-pattern (car (cdr patterns)))
           (head-built (emacs-pcase--test head-pattern (list 'car value-form)))
           (tail-built (emacs-pcase--test tail-pattern (list 'cdr value-form))))
      (cons (list 'and
                  (list 'consp value-form)
                  (car head-built)
                  (car tail-built))
            (append (cdr head-built) (cdr tail-built)))))

  (defun emacs-pcase--backquote (pat value-form)
    "Build (TEST . BINDINGS) for a backquote-pattern.
Walks PAT recursively; `(comma SYM)' binds SYM to corresponding
position; literal cons recurses with `car'/`cdr' index forms; atom
does `equal' check."
    (cond
     ;; (comma SYM) — bind SYM to value-form, always match.
     ((and (consp pat) (or (eq (car pat) 'comma)
                           (eq (car pat) '\,)))
      (let ((sym (car (cdr pat))))
        (cond
         ((eq sym '_) (cons t nil))
         ((symbolp sym) (cons t (list (list sym value-form))))
         (t (emacs-pcase--test sym value-form)))))
     ;; (comma-at SYM) — bind SYM to remaining list (= value-form is tail).
     ((and (consp pat) (or (eq (car pat) 'comma-at)
                           (eq (car pat) '\,@)))
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

(when (emacs-pcase--install-function-p 'pcase)
  (defmacro pcase (expr &rest cases)
    "Phase 10 (= ex-Phase 4 batch 2) pcase: dispatch EXPR through CASES.
See `emacs-pcase--test' for supported pattern shapes."
    (let ((value-sym (make-symbol "--pcase-value--"))
          (cond-clauses nil))
      (dolist (case cases)
        (let ((patterns (emacs-pcase--case-patterns (car case)))
              (body (cdr case)))
          (dolist (pat patterns)
            (let* ((built (emacs-pcase--test pat value-sym))
                   (test (car built))
                   (bindings (cdr built)))
              (push (list test
                          (if bindings
                              (cons 'let (cons bindings body))
                            (cons 'progn body)))
                    cond-clauses)))))
      (let ((forward nil))
        (while cond-clauses
          (setq forward (cons (car cond-clauses) forward))
          (setq cond-clauses (cdr cond-clauses)))
        (list 'let (list (list value-sym expr))
              (cons 'cond forward))))))

;; NeLisp upstream now ships the pcase macro as Elisp under
;; `lisp/nelisp-pcase.el', loaded as part of the standalone stdlib
;; prelude.  The nelisp driver therefore
;; exposes the rich grammar (or / and / pred / guard / cons /
;; backquote / let) directly — no override hack needed here.
;; Under host driver, host emacs's C-native pcase is left intact.

(when (emacs-pcase--install-function-p 'pcase-defmacro)
  (defmacro pcase-defmacro (name args &rest body)
    "Define NAME as a lightweight pcase pattern expander."
    (let ((fsym (intern (format "%s--pcase-macroexpander" name))))
      `(progn
         (defun ,fsym ,args ,@body)
         (put ',name 'pcase-macroexpander ',fsym)
         ',name))))

(defun emacs-pcase--let-binding (binding)
  "Return (TEMP TEST BINDINGS) for a single pcase-let BINDING."
  (let* ((pattern (car binding))
         (expr (car (cdr binding)))
         (value-sym (make-symbol "--pcase-let-value--"))
         (built (emacs-pcase--test pattern value-sym)))
    (list (list value-sym expr) (car built) (cdr built))))

(when (emacs-pcase--install-function-p 'pcase-let)
  (defmacro pcase-let (bindings &rest body)
    "Minimal `pcase-let' supporting the local pcase pattern subset."
    (let ((forms body)
          (rev-bindings nil))
      (dolist (binding bindings)
        (push binding rev-bindings))
      (dolist (binding rev-bindings)
        (let* ((built (emacs-pcase--let-binding binding))
               (temp-binding (car built))
               (test (car (cdr built)))
               (pattern-bindings (car (cdr (cdr built)))))
          (setq forms
              (list (list 'let (list temp-binding)
                            (if pattern-bindings
                                (list 'when test
                                      (cons 'let (cons pattern-bindings forms)))
                              (cons 'when (cons test forms))))))))
      (if bindings (car forms) (cons 'progn body)))))

(when (emacs-pcase--install-function-p 'pcase-let*)
  (defmacro pcase-let* (bindings &rest body)
    "Minimal `pcase-let*' supporting sequential pcase bindings."
    (if bindings
        (list 'pcase-let (list (car bindings))
              (cons 'pcase-let* (cons (cdr bindings) body)))
      (cons 'progn body))))

(when (emacs-pcase--install-function-p 'pcase-dolist)
  (defmacro pcase-dolist (spec &rest body)
    "Minimal `pcase-dolist' supporting the local pcase pattern subset."
    (let* ((pattern (car spec))
           (list-form (car (cdr spec)))
           (result-form (car (cdr (cdr spec))))
           (value-sym (make-symbol "--pcase-dolist-value--"))
           (built (emacs-pcase--test pattern value-sym))
           (test (car built))
           (pattern-bindings (cdr built)))
      (list 'dolist (list value-sym list-form result-form)
            (if pattern-bindings
                (list 'when test
                      (cons 'let (cons pattern-bindings body)))
              (cons 'when (cons test body)))))))

;; Task #17 M2: real `pcase-exhaustive' (was a nil-expanding bulk stub).
;; Magit/transient use it in load-bearing dispatch positions
;; (`magit-diff--get-value' maps 'status to the right
;; `magit-*-use-buffer-arguments' option, `transient--insert-suffix'
;; dispatches insert/append/replace, `transient-infix-value' dispatches
;; on multi-value shape) -- a nil expansion there is not a safe
;; degradation, it silently changes behavior.  Semantics follow real
;; pcase.el: exactly `pcase', plus signal an error when no clause
;; matches instead of returning nil.
(when (emacs-pcase--install-function-p 'pcase-exhaustive)
  (defmacro pcase-exhaustive (expr &rest cases)
    "Like `pcase' EXPR CASES, but signal an error when nothing matches."
    (let ((value-sym (make-symbol "--pcase-exhaustive-value--")))
      (list 'let (list (list value-sym expr))
            (cons 'pcase
                  (cons value-sym
                        (append cases
                                (list (list '_
                                            (list 'error
                                                  "No clause matching %S"
                                                  value-sym))))))))))

;; Doc 16 breadth round 26: pcase-lambda (was void).
(when (emacs-pcase--install-function-p 'pcase-lambda)
  (defmacro pcase-lambda (lambda-list &rest body)
    "A `lambda' whose parameters may be `pcase' patterns, destructured on call.
Plain-symbol parameters (including &optional / &rest markers) pass through
unchanged; a cons pattern (e.g. a backquote pattern) is bound via a hidden
indexed temporary and `pcase-let*'."
    (let ((params nil) (bindings nil) (i 0))
      (dolist (pat lambda-list)
        (if (symbolp pat)
            (setq params (cons pat params))
          (let ((sym (make-symbol (format "--pcl-%d--" i))))
            (setq i (1+ i))
            (setq params (cons sym params))
            (setq bindings (cons (list pat sym) bindings)))))
      (list 'lambda (nreverse params)
            (cons 'pcase-let* (cons (nreverse bindings) body))))))

(unless (featurep 'pcase) (provide 'pcase))

(provide 'emacs-pcase)

;;; emacs-pcase.el ends here
