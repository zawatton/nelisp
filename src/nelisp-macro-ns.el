;;; nelisp-macro-ns.el --- T149 / Phase 7+D macro namespace  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; T149 / Doc 40 §3.D — Phase 7+D `nelisp-macro-' namespace.
;;
;; New cl-defstruct-based macro API layered on top of `nelisp-closure'.
;; The legacy `nelisp-macroexpand-*' / `nelisp--macros' table inside
;; `src/nelisp-macro.el' is preserved verbatim because nelisp-eval /
;; nelisp-load / nelisp-tramp / nelisp-gc still dispatch through it
;; (Phase 1 anchor; the self-host probes in
;; `test/nelisp-self-host-test.el' read every form of nelisp-macro.el
;; through `nelisp-eval', which does not yet support `cl-defstruct',
;; so the new namespace lives here in a sibling file the self-host
;; tests do not load).
;;
;; Phase 7+D adds an independent registry so anvil-* and the Phase
;; 7+B/+C evaluator can use a self-contained, tested macro layer:
;;
;;   - `nelisp-macro' = cl-defstruct (name / arglist / body / env /
;;     source-marker)
;;   - `nelisp-macro-registry' = hash-table NAME -> struct
;;   - `nelisp-macro-define' / `nelisp-macro-defmacro' install entries
;;   - `nelisp-macro-p' = struct predicate
;;   - `nelisp-macro-expand-1 FORM &optional ENV' = single step,
;;     ENV is an alist `(NAME . MACRO-STRUCT)' for `cl-macrolet'
;;     local-macro semantics (Doc 40 §4.4)
;;   - `nelisp-macro-expand'      = fixpoint over expand-1
;;   - `nelisp-macro-expand-all'  = recursive walk into subforms
;;   - `nelisp-macro-cache-clear' / `nelisp-macro-cache-stats' =
;;     memoize cache control (Doc 40 §3.D risks; cache is keyed by
;;     `(form . env-hash)' and flushed on every define / undefine)
;;   - `declare' forms inside macro bodies are skipped at definition
;;     time (Doc 40 §3.D scope: `(declare (debug ...) (indent ...))'
;;     no-op).
;;
;; Macro body application uses `nelisp-closure-make' +
;; `nelisp-closure-apply' so that &optional / &rest argument
;; destructuring matches the Phase 7+C closure semantics.  The macro
;; body is evaluated under a host-Elisp environment because Phase 7+D
;; predates the bytecode compile path; the result returned by
;; `nelisp-closure-apply' is the expansion form that callers then
;; feed back to `nelisp-special-forms-eval'.
;;
;; pcase support (Doc 40 §3.D scope) is implemented as a built-in
;; expander registered in the registry under the symbol `pcase'.
;; The expander handles three pattern shapes that anvil-* exercises:
;;   - literal value             (= eq / equal comparison)
;;   - (quote SYM)               (= eq comparison)
;;   - bare symbol               (= unconditional bind)
;;   - (cons PCAR PCDR)          (= car/cdr destructuring)
;;   - (guard EXPR) inside body  (= conjoins with the test)
;;
;; Anything else signals `nelisp-macro-pcase-unsupported' so users
;; spot missing patterns rather than silently no-op.  Phase 7+E /
;; v1.x extends the matcher; this MVP unblocks anvil-* corpus
;; usage (= mostly literal and cons patterns).

;;; Code:

(require 'cl-lib)
(require 'nelisp-closure)

;;; -----------------------------------------------------------------
;;; Errors
;;; -----------------------------------------------------------------

(define-error 'nelisp-macro-error
  "NeLisp macro error (Phase 7+D)")

(define-error 'nelisp-macro-undefined
  "NeLisp macro: name not registered"
  'nelisp-macro-error)

(define-error 'nelisp-macro-pcase-unsupported
  "NeLisp macro: unsupported pcase pattern"
  'nelisp-macro-error)

;;; -----------------------------------------------------------------
;;; cl-defstruct
;;; -----------------------------------------------------------------

(cl-defstruct (nelisp-macro
               (:constructor nelisp-macro--raw-make)
               (:copier nelisp-macro-copy)
               (:predicate nelisp-macro-p))
  "Phase 7+D macro object.

Slots:
  NAME           Symbol the macro is registered under (nil for
                 anonymous macros built via `nelisp-macro--raw-make'
                 directly).
  ARGLIST        Lambda list (may include &optional / &rest).
  BODY           List of forms; `declare' top-level forms are
                 stripped at construction time.
  ENV            Alist of `(SYM . VAL)' captured at definition time
                 — currently always nil (macros expand under the
                 caller's env), kept as a slot for forward
                 compatibility with `cl-macrolet' (Doc 40 §4.4).
  SOURCE-MARKER  Optional `(FILE . POS)' cons recorded for
                 diagnostics."
  name arglist body env source-marker)

;;; -----------------------------------------------------------------
;;; Registry + cache
;;; -----------------------------------------------------------------

(defvar nelisp-macro--registry (make-hash-table :test 'eq)
  "Hash-table NAME -> `nelisp-macro' struct.
Independent of legacy `nelisp--macros' registry: this table is the
authoritative store for the `nelisp-macro-' namespace and is
consulted by `nelisp-macro-expand-*' before any host fallback.")

(defvar nelisp-macro--cache (make-hash-table :test 'equal)
  "Memoize cache for `nelisp-macro-expand-1' results.
Keys are `(FORM . ENV-COOKIE)' conses.  Invalidated on every
`nelisp-macro-define' / `nelisp-macro-undefine' / cache-clear.
Hit / miss counts are maintained in `nelisp-macro--cache-stats'.")

(defvar nelisp-macro--cache-stats (list :hit 0 :miss 0)
  "Plist of cache hit / miss counters.")

(defun nelisp-macro--bump-stat (key)
  "Increment KEY (`:hit' or `:miss') in `nelisp-macro--cache-stats'."
  (plist-put nelisp-macro--cache-stats key
             (1+ (or (plist-get nelisp-macro--cache-stats key) 0))))

(defun nelisp-macro--strip-declare (body)
  "Return BODY with leading `(declare ...)' forms removed.
Doc 40 §3.D: `(declare (debug ...) (indent ...))' is no-op for the
interpret path but must not survive into expansion."
  (cond
   ((null body) nil)
   ((and (consp (car body))
         (eq (caar body) 'declare))
    (nelisp-macro--strip-declare (cdr body)))
   (t body)))

(defun nelisp-macro--cache-key (form env)
  "Build a cache key for FORM under ENV.
ENV is collapsed to its registered local-macro names so that two
calls with the same form + same active locals share cache entries."
  (cons form (and env (mapcar #'car env))))

(defun nelisp-macro--cache-flush ()
  "Invalidate the entire memoize cache.
Called on every `nelisp-macro-define' / `nelisp-macro-undefine'
since a redefinition silently invalidates every prior expansion
that referenced the changed name."
  (clrhash nelisp-macro--cache)
  (setq nelisp-macro--cache-stats (list :hit 0 :miss 0)))

(defun nelisp-macro-cache-clear ()
  "Public form of `nelisp-macro--cache-flush'.
Returns t after clearing the cache."
  (nelisp-macro--cache-flush)
  t)

(defun nelisp-macro-cache-stats ()
  "Return the current cache stats plist `(:hit N :miss N :size N)'."
  (list :hit  (plist-get nelisp-macro--cache-stats :hit)
        :miss (plist-get nelisp-macro--cache-stats :miss)
        :size (hash-table-count nelisp-macro--cache)))

;;; -----------------------------------------------------------------
;;; Built-in macros
;;; -----------------------------------------------------------------

(defun nelisp-macro--install-builtins ()
  "Re-install built-in macros (currently only `pcase') into the registry.
Idempotent.  Called from `nelisp-macro-registry-clear' so tests
that wipe the registry still see `pcase' afterwards."
  (puthash 'pcase
           (nelisp-macro--raw-make
            :name 'pcase
            :arglist '(&rest _args)
            :body nil
            :env nil
            :source-marker '(:builtin . pcase))
           nelisp-macro--registry))

;;; -----------------------------------------------------------------
;;; Registry CRUD
;;; -----------------------------------------------------------------

(defun nelisp-macro-define (name arglist body &optional source-marker)
  "Register macro NAME with ARGLIST and BODY in the Phase 7+D registry.

SOURCE-MARKER is an optional `(FILE . POS)' cons for diagnostics.
ARGLIST follows the closure grammar (positional + &optional +
&rest) — validated through `nelisp-closure--validate-arglist' at
build time so malformed lists fail at definition rather than first
expansion.

Always returns the resulting `nelisp-macro' struct.  Flushes the
memoize cache so subsequent expansions see the new definition."
  (unless (symbolp name)
    (signal 'nelisp-macro-error (list "macro name must be a symbol" name)))
  (let* ((stripped (nelisp-macro--strip-declare body))
         ;; Validate arglist eagerly via the closure helper — same
         ;; grammar (positional + &optional + &rest).
         (_        (nelisp-closure--validate-arglist arglist))
         (m (nelisp-macro--raw-make
             :name name
             :arglist arglist
             :body stripped
             :env nil
             :source-marker source-marker)))
    (puthash name m nelisp-macro--registry)
    (nelisp-macro--cache-flush)
    m))

(defun nelisp-macro-defmacro (name arglist body &optional source-marker)
  "Alias for `nelisp-macro-define', keyed for symmetry with
`nelisp-defun' (Doc 40 §4.4).  Returns the registered struct."
  (nelisp-macro-define name arglist body source-marker))

(defun nelisp-macro-undefine (name)
  "Remove macro NAME from the Phase 7+D registry.
Returns t if NAME was present, nil otherwise.  Flushes the
memoize cache."
  (let ((present (not (eq (gethash name nelisp-macro--registry 'absent)
                          'absent))))
    (when present
      (remhash name nelisp-macro--registry))
    (nelisp-macro--cache-flush)
    present))

(defun nelisp-macro-lookup (name &optional env)
  "Return the macro struct for NAME under ENV, or nil when none.
ENV is searched first (deepest-first alist of local macros), then
the global `nelisp-macro--registry'."
  (when (symbolp name)
    (or (cdr (assq name env))
        (let ((m (gethash name nelisp-macro--registry 'absent)))
          (and (not (eq m 'absent)) m)))))

(defun nelisp-macro-registry-clear ()
  "Drop every user-defined macro from the registry and flush the cache.
Built-in macros (currently `pcase') are re-installed before
returning so the dispatch table remains usable.  Returns t."
  (clrhash nelisp-macro--registry)
  (nelisp-macro--install-builtins)
  (nelisp-macro--cache-flush)
  t)

;;; -----------------------------------------------------------------
;;; Expansion
;;; -----------------------------------------------------------------

(defun nelisp-macro--apply-expander (macro args)
  "Apply MACRO to literal ARGS list, return the expansion form.
Builds a transient `nelisp-closure' from the macro's arglist /
body / env and dispatches through `nelisp-closure-apply' so that
&optional / &rest destructuring matches Phase 7+C semantics."
  (let ((closure (nelisp-closure-make
                  (or (nelisp-macro-env macro) nil)
                  (nelisp-macro-arglist macro)
                  (nelisp-macro-body macro)
                  (nelisp-macro-source-marker macro))))
    (nelisp-closure-apply closure args)))

(defun nelisp-macro-expand-1 (form &optional env)
  "Expand the outermost macro call in FORM once under ENV.
Return FORM unchanged if its car is not a registered macro.

ENV is an alist of `(NAME . MACRO-STRUCT)' for `cl-macrolet'-style
local macros.  Lookups consult ENV before the global registry.

Results are memoized in `nelisp-macro--cache'; cache hit / miss
counters live in `nelisp-macro--cache-stats'."
  (cond
   ((not (consp form)) form)
   (t
    (let ((key (nelisp-macro--cache-key form env)))
      (let ((hit (gethash key nelisp-macro--cache 'miss)))
        (cond
         ((not (eq hit 'miss))
          (nelisp-macro--bump-stat :hit)
          hit)
         (t
          (nelisp-macro--bump-stat :miss)
          (let* ((head     (car form))
                 (expander (and (symbolp head)
                                (nelisp-macro-lookup head env)))
                 (result   (cond
                            ((null expander) form)
                            ((eq head 'pcase)
                             (nelisp-macro--expand-pcase (cdr form)))
                            (t
                             (nelisp-macro--apply-expander
                              expander (cdr form))))))
            (puthash key result nelisp-macro--cache)
            result))))))))

(defun nelisp-macro-expand (form &optional env)
  "Expand the outermost macro call in FORM until fixpoint under ENV.
Return the form once its car is no longer a registered macro."
  (let ((cur form)
        (prev nil))
    (while (and (not (eq cur prev))
                (consp cur)
                (nelisp-macro-lookup (car cur) env))
      (setq prev cur
            cur  (nelisp-macro-expand-1 cur env)))
    cur))

(defun nelisp-macro--map-expand-all (forms env)
  "Apply `nelisp-macro-expand-all' to each element of FORMS under ENV."
  (mapcar (lambda (f) (nelisp-macro-expand-all f env)) forms))

(defun nelisp-macro-expand-all (form &optional env)
  "Recursively expand every macro call reachable from FORM under ENV.

Walks into bodies of `let' / `let*' / `lambda' / `function' /
`defun' / `defmacro' / `defvar' / `defconst' / `setq' / `cond' /
`condition-case' and ordinary call forms.  Atoms and `quote'
sub-forms pass through unchanged."
  (cond
   ((not (consp form)) form)
   (t
    (let ((expanded (nelisp-macro-expand form env)))
      (cond
       ((not (consp expanded)) expanded)
       (t
        (let ((head (car expanded)))
          (cond
           ((eq head 'quote) expanded)
           ((eq head 'function)
            (let ((arg (cadr expanded)))
              (if (and (consp arg) (eq (car arg) 'lambda))
                  (list 'function
                        (cons 'lambda
                              (cons (cadr arg)
                                    (nelisp-macro--map-expand-all
                                     (cddr arg) env))))
                expanded)))
           ((eq head 'lambda)
            (cons 'lambda
                  (cons (cadr expanded)
                        (nelisp-macro--map-expand-all
                         (cddr expanded) env))))
           ((memq head '(let let*))
            (let* ((bindings (cadr expanded))
                   (body     (cddr expanded))
                   (new-bindings
                    (mapcar (lambda (b)
                              (cond
                               ((symbolp b) b)
                               ((and (consp b) (consp (cdr b)))
                                (list (car b)
                                      (nelisp-macro-expand-all
                                       (cadr b) env)))
                               (t b)))
                            bindings)))
              (cons head
                    (cons new-bindings
                          (nelisp-macro--map-expand-all body env)))))
           ((memq head '(defun defmacro))
            (cons head
                  (cons (cadr expanded)
                        (cons (caddr expanded)
                              (nelisp-macro--map-expand-all
                               (cdddr expanded) env)))))
           ((eq head 'defvar)
            (let ((rest (cdr expanded)))
              (cond
               ((null rest) expanded)
               ((null (cdr rest)) (list 'defvar (car rest)))
               (t (append (list 'defvar (car rest)
                                (nelisp-macro-expand-all
                                 (cadr rest) env))
                          (cddr rest))))))
           ((eq head 'defconst)
            (let ((name (cadr expanded))
                  (rest (cddr expanded)))
              (cond
               ((null rest) expanded)
               (t (append (list 'defconst name
                                (nelisp-macro-expand-all
                                 (car rest) env))
                          (cdr rest))))))
           ((eq head 'setq)
            (let ((rest (cdr expanded))
                  (out nil))
              (while rest
                (push (car rest) out)
                (setq rest (cdr rest))
                (when rest
                  (push (nelisp-macro-expand-all (car rest) env) out)
                  (setq rest (cdr rest))))
              (cons 'setq (nreverse out))))
           ((eq head 'cond)
            (cons 'cond
                  (mapcar (lambda (clause)
                            (if (consp clause)
                                (nelisp-macro--map-expand-all clause env)
                              clause))
                          (cdr expanded))))
           ((eq head 'condition-case)
            (let ((var       (cadr expanded))
                  (protected (caddr expanded))
                  (handlers  (cdddr expanded)))
              (cons 'condition-case
                    (cons var
                          (cons (nelisp-macro-expand-all protected env)
                                (mapcar
                                 (lambda (h)
                                   (if (consp h)
                                       (cons (car h)
                                             (nelisp-macro--map-expand-all
                                              (cdr h) env))
                                     h))
                                 handlers))))))
           (t
            (cons head
                  (nelisp-macro--map-expand-all (cdr expanded) env)))))))))))

;;; -----------------------------------------------------------------
;;; pcase MVP — Doc 40 §3.D scope
;;; -----------------------------------------------------------------
;;
;; The expander rewrites `(pcase EXPR (PAT1 BODY1...) ...)' into a
;; `let' + `cond' that captures EXPR once and tries each clause in
;; order.  Three pattern shapes are supported (= literal / cons /
;; guard, plus bare-symbol bind for the catch-all).  See header
;; commentary for the full grammar.

(defvar nelisp-macro--pcase-scrut-counter 0
  "Monotonic counter producing fresh `pcase--scrut-N' temporaries.")

(defun nelisp-macro--pcase-fresh-sym (prefix)
  "Return a fresh interned symbol of the form PREFIX-N.
Uses an incrementing counter rather than `gensym' so generated
forms remain `equal'-comparable across test runs."
  (cl-incf nelisp-macro--pcase-scrut-counter)
  (intern (format "%s-%d" prefix nelisp-macro--pcase-scrut-counter)))

(defun nelisp-macro--pcase-extract-guard (body)
  "Split BODY at the first `(guard EXPR)' form.
Returns `(GUARD-EXPR . REMAINING-BODY)'.  GUARD-EXPR is nil if
BODY contains no guard."
  (let ((guard nil)
        (out nil)
        (rest body))
    (while rest
      (let ((f (car rest)))
        (cond
         ((and (consp f) (eq (car f) 'guard) (consp (cdr f)) (null (cddr f)))
          (when guard
            (signal 'nelisp-macro-pcase-unsupported
                    (list "multiple guards in clause" body)))
          (setq guard (cadr f)))
         (t (push f out))))
      (setq rest (cdr rest)))
    (cons guard (nreverse out))))

(defun nelisp-macro--pcase-pattern-test (scrut-sym pattern)
  "Build a Lisp test form that returns non-nil iff SCRUT-SYM matches PATTERN.
Supports literal, `(quote ...)', `(cons PCAR PCDR)' and bare-symbol
binding (always matches).  Other shapes raise
`nelisp-macro-pcase-unsupported'."
  (cond
   ;; t / underscore / bare symbol = always matches (binding handled
   ;; in `nelisp-macro--pcase-pattern-bindings').
   ((or (eq pattern '_) (eq pattern t)) t)
   ((symbolp pattern) t)
   ;; nil literal
   ((null pattern) (list 'null scrut-sym))
   ;; numbers / strings / keywords
   ((or (numberp pattern) (stringp pattern) (keywordp pattern))
    (list 'equal scrut-sym (list 'quote pattern)))
   ;; (quote SYM)
   ((and (consp pattern) (eq (car pattern) 'quote))
    (list 'eq scrut-sym (list 'quote (cadr pattern))))
   ;; (cons PCAR PCDR)
   ((and (consp pattern) (eq (car pattern) 'cons))
    (let ((car-test (nelisp-macro--pcase-pattern-test
                     (list 'car scrut-sym) (cadr pattern)))
          (cdr-test (nelisp-macro--pcase-pattern-test
                     (list 'cdr scrut-sym) (caddr pattern))))
      (list 'and (list 'consp scrut-sym) car-test cdr-test)))
   (t
    (signal 'nelisp-macro-pcase-unsupported
            (list "unrecognised pcase pattern" pattern)))))

(defun nelisp-macro--pcase-pattern-bindings (scrut-form pattern)
  "Return a let-binding list `((SYM SCRUT-FORM) ...)' for PATTERN.
A bare non-nil non-`_' symbol binds SCRUT-FORM to that symbol.
Cons patterns recurse into car / cdr.  Literals contribute no
bindings."
  (cond
   ((or (eq pattern '_) (eq pattern t) (null pattern)) nil)
   ((and (symbolp pattern) (not (keywordp pattern)))
    (list (list pattern scrut-form)))
   ((or (numberp pattern) (stringp pattern) (keywordp pattern)) nil)
   ((and (consp pattern) (eq (car pattern) 'quote)) nil)
   ((and (consp pattern) (eq (car pattern) 'cons))
    (append (nelisp-macro--pcase-pattern-bindings
             (list 'car scrut-form) (cadr pattern))
            (nelisp-macro--pcase-pattern-bindings
             (list 'cdr scrut-form) (caddr pattern))))
   (t
    (signal 'nelisp-macro-pcase-unsupported
            (list "unrecognised pcase pattern" pattern)))))

(defun nelisp-macro--pcase-clause (scrut-sym clause)
  "Compile CLAUSE = `(PATTERN BODY...)' into a `cond' arm.
Returns `(TEST-FORM . LET-WRAPPED-BODY)'."
  (unless (and (consp clause) (consp (cdr clause)))
    (signal 'nelisp-macro-pcase-unsupported
            (list "malformed pcase clause" clause)))
  (let* ((pattern (car clause))
         (raw-body (cdr clause))
         (split (nelisp-macro--pcase-extract-guard raw-body))
         (guard (car split))
         (body  (cdr split))
         (test  (nelisp-macro--pcase-pattern-test scrut-sym pattern))
         (bindings (nelisp-macro--pcase-pattern-bindings
                    scrut-sym pattern))
         (final-test (if guard (list 'and test
                                     (if bindings
                                         (list 'let bindings guard)
                                       guard))
                       test))
         (cond-body (if bindings
                        (list (cons 'let
                                    (cons bindings body)))
                      body)))
    (cons final-test cond-body)))

(defun nelisp-macro--expand-pcase (args)
  "Expand `(pcase EXPR CLAUSE...)' into `let' + `cond'.
Builds a fresh scrutinee variable so EXPR is evaluated once even
when guards reference it indirectly."
  (unless (consp args)
    (signal 'nelisp-macro-pcase-unsupported
            (list "pcase requires at least an expression" args)))
  (let* ((expr (car args))
         (clauses (cdr args))
         (scrut (nelisp-macro--pcase-fresh-sym 'pcase--scrut))
         (cond-arms (mapcar (lambda (c)
                              (nelisp-macro--pcase-clause scrut c))
                            clauses)))
    (list 'let (list (list scrut expr))
          (cons 'cond cond-arms))))

;;; -----------------------------------------------------------------
;;; Bootstrap
;;; -----------------------------------------------------------------

;; Install built-in macros at file-load time.  `pcase' is a sentinel
;; entry — `nelisp-macro-expand-1' shortcuts on `(eq head 'pcase)'
;; before calling `nelisp-macro--apply-expander', so the struct's
;; arglist / body are unused at expansion time, but we still want
;; `nelisp-macro-lookup' / `nelisp-macro-p' to recognise pcase.
(nelisp-macro--install-builtins)

(provide 'nelisp-macro-ns)

;;; nelisp-macro-ns.el ends here
