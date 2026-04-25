;;; nelisp-closure.el --- Phase 7+C closure / lexical binding (Doc 40 §3.C) -*- lexical-binding: t; -*-

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
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Phase 7+C closure / lexical binding completion (Doc 40 LOCKED v2
;; §3.C / §3.3).  This module owns the canonical NeLisp closure
;; representation used downstream by the native compiler frontend
;; (Phase 7.1.X SHIPPED) and the cold-init self-host bootstrap
;; (Phase 7.5 SHIPPED).
;;
;; Design choices encoded here:
;;
;; 1. Closure object = `cl-defstruct' record (4 slots: ENV / ARGLIST /
;;    BODY / SOURCE-MARKER).  Records carry a real predicate and tag
;;    discipline so downstream consumers can dispatch with
;;    `cl-typep' / `nelisp-closure-p' instead of a raw `(consp x)' +
;;    `eq head' comparison (= what `nelisp-special-forms' does for the
;;    Phase 7+B parallel evaluator).
;;
;; 2. ENV is an alist of `(SYM . VAL)' cells in deepest-first order,
;;    matching the Phase 7+B special-forms env shape.  This keeps
;;    `setcdr'-based capture-mutate semantics intact (= shared cell
;;    mutation across closure invocations, Doc 40 §3.3 captured-mutate
;;    risk pin).
;;
;; 3. Lambda list grammar = positional + `&optional' (default nil) +
;;    `&rest' (rest collects to a list).  Default-value evaluation for
;;    `&optional' parameters is *per-call* (= Emacs Lisp semantics),
;;    handled by `nelisp-closure--bind-params' through a small embedded
;;    eval shim that dispatches into `nelisp-special-forms-eval'.
;;
;; 4. Lexical-binding default = t (`nelisp-closure-lexical-binding-default').
;;    Per-file `;; -*- lexical-binding: nil -*-' headers are not yet
;;    consulted in this module — Doc 40 §2.3 LOCKED choice A pushes
;;    that into the reader (Phase 7+A) and into per-form `defvar'
;;    annotations (Phase 7+C scope tail item, audit-only here).
;;
;; 5. `nelisp-closure-capture-vars' is a free-variable analyzer that
;;    walks BODY and returns the set of symbols that are *captured*
;;    from ENV (= referenced free in BODY but bound in ENV).  This is
;;    a helper used by the native-compile frontend (Phase 7.1.X) for
;;    closure-conversion / env-vector layout.
;;
;; 6. `nelisp-closure-apply' is the canonical entry point.  It binds
;;    ARGS to the lambda list, extends ENV, and evaluates BODY with
;;    `nelisp-special-forms-eval'.  Recursive / mutual-recursive /
;;    captured-mutate / over-captured forms all flow through this
;;    single dispatcher (= Doc 40 §3.3 four-form smoke gate).
;;
;; Public API (Doc 40 §4.2 augmentation):
;;
;;   (nelisp-closure-make ENV ARGLIST BODY &optional SOURCE-MARKER)
;;       Build a closure record.  ENV is an alist of (SYM . VAL).
;;
;;   (nelisp-closure-p OBJECT)
;;       Predicate.
;;
;;   (nelisp-closure-env CLOSURE)
;;   (nelisp-closure-arglist CLOSURE)
;;   (nelisp-closure-body CLOSURE)
;;       Slot accessors.
;;
;;   (nelisp-closure-apply CLOSURE ARGS)
;;       Apply CLOSURE to ARGS (a list).
;;
;;   (nelisp-closure-capture-vars FORM ENV)
;;       Return the subset of ENV-bound symbols referenced free in
;;       FORM (= captured set).
;;
;;   (nelisp-closure-lexical-binding-default &optional NEW)
;;       Read or update the lexical-binding default flag (nil = update
;;       no-op when called with no arg).
;;
;; Out of scope (carry-forward per Doc 40):
;;   - macro expansion (= Phase 7+D / §3.4)
;;   - condition-case / unwind-protect / catch / throw (= Phase 7+E /
;;     §3.5)
;;   - native-compile integration (= Phase 7.1.X SHIPPED, this module
;;     provides the data shape they consume — they call us, not vice
;;     versa)

;;; Code:

(require 'cl-lib)
(require 'nelisp-special-forms)

(define-error 'nelisp-closure-error
  "NeLisp closure error (Phase 7+C)")

(define-error 'nelisp-closure-arity-error
  "NeLisp closure: argument count mismatch"
  'nelisp-closure-error)

(define-error 'nelisp-closure-malformed-arglist
  "NeLisp closure: malformed argument list"
  'nelisp-closure-error)

;;; Lexical-binding default --------------------------------------------

(defvar nelisp-closure--lexical-binding-default t
  "Default value of `lexical-binding' for fresh closures.
Doc 40 §2.3 LOCKED choice A: Emacs Lisp default-踏襲.  Set to nil
only by explicit `;; -*- lexical-binding: nil -*-' file headers
(reader scope, Phase 7+A); programmatic override via
`nelisp-closure-lexical-binding-default' for tests / experiments.")

(defun nelisp-closure-lexical-binding-default (&optional new-value)
  "Return the current lexical-binding default; if NEW-VALUE is
non-nil, set the default to NEW-VALUE first and return its new value.
Calling with no argument is a pure read.  Returning a boolean
(t/nil) is part of the public API contract."
  (when new-value
    (setq nelisp-closure--lexical-binding-default
          (if (eq new-value 'nil-explicit) nil new-value)))
  nelisp-closure--lexical-binding-default)

;;; Closure record -----------------------------------------------------

(cl-defstruct (nelisp-closure
               (:constructor nelisp-closure--raw-make)
               (:copier nelisp-closure-copy)
               (:predicate nelisp-closure-p))
  "Phase 7+C closure object.

Slots:
  ENV            Alist of (SYM . VAL), deepest-first.
  ARGLIST        Lambda list (positional + &optional + &rest).
  BODY           List of forms.
  SOURCE-MARKER  Optional source position (file . pos) or nil."
  env arglist body source-marker)

(defun nelisp-closure-make (env arglist body &optional source-marker)
  "Build a closure capturing ENV / ARGLIST / BODY.

ENV is an alist of `(SYM . VAL)' (deepest-first per Doc 40 §3.3).
ARGLIST is a lambda list.  BODY is a list of forms.  SOURCE-MARKER
is an optional `(FILE . POS)' cons recorded for diagnostics.

Validates ARGLIST shape eagerly; signals
`nelisp-closure-malformed-arglist' on failure so that closures
constructed from `defun' / `lambda' surface errors at definition
time rather than first-call time."
  (nelisp-closure--validate-arglist arglist)
  (nelisp-closure--raw-make
   :env env
   :arglist arglist
   :body body
   :source-marker source-marker))

;;; Arglist validation -------------------------------------------------

(defun nelisp-closure--validate-arglist (arglist)
  "Signal `nelisp-closure-malformed-arglist' if ARGLIST is not well-formed.
Grammar: REQUIRED* [&optional OPT*] [&rest REST]"
  (let ((mode 'required)
        (rest-name nil)
        (saw-rest nil))
    (dolist (p arglist)
      (cond
       ((eq p '&optional)
        (unless (eq mode 'required)
          (signal 'nelisp-closure-malformed-arglist
                  (list "&optional after &optional/&rest" arglist)))
        (setq mode 'optional))
       ((eq p '&rest)
        (when saw-rest
          (signal 'nelisp-closure-malformed-arglist
                  (list "duplicate &rest" arglist)))
        (setq saw-rest t)
        (setq mode 'rest))
       ((eq mode 'rest)
        (when rest-name
          (signal 'nelisp-closure-malformed-arglist
                  (list "&rest expects exactly one symbol" arglist)))
        (unless (and (symbolp p) (not (keywordp p)))
          (signal 'nelisp-closure-malformed-arglist
                  (list "&rest argument must be a non-keyword symbol" p)))
        (setq rest-name p))
       ((symbolp p)
        (when (or (eq p nil) (eq p t) (keywordp p))
          (signal 'nelisp-closure-malformed-arglist
                  (list "parameter must be a non-keyword symbol" p))))
       ((and (consp p) (eq mode 'optional))
        ;; (NAME DEFAULT) — Emacs cl-defun extended syntax, accepted
        ;; as a defaulted optional parameter.
        (unless (and (symbolp (car p)) (not (keywordp (car p))))
          (signal 'nelisp-closure-malformed-arglist
                  (list "optional default param name must be a non-keyword symbol" p))))
       (t
        (signal 'nelisp-closure-malformed-arglist
                (list "unrecognised parameter form" p)))))
    (when (and saw-rest (not rest-name))
      (signal 'nelisp-closure-malformed-arglist
              (list "&rest with no name" arglist)))
    t))

;;; Parameter binding --------------------------------------------------

(defun nelisp-closure--param-name (p)
  "Return the symbol name carried by parameter P.
Accepts a bare symbol or `(NAME DEFAULT)' form."
  (cond
   ((symbolp p) p)
   ((consp p)   (car p))
   (t (signal 'nelisp-closure-malformed-arglist (list "bad param" p)))))

(defun nelisp-closure--param-default (p env)
  "Return the default value for optional parameter P under ENV.
Bare symbol -> nil.  `(NAME DEFAULT)' -> evaluate DEFAULT under ENV."
  (cond
   ((symbolp p) nil)
   ((and (consp p) (cdr p))
    (nelisp-special-forms-eval (cadr p) env))
   (t nil)))

(defun nelisp-closure--bind-params (arglist args env)
  "Bind ARGLIST to ARGS using ENV for default expressions.

Default-value expressions on `&optional' parameters are evaluated
in an environment that includes the parameters bound earlier in
the same call (Emacs Lisp semantics: each default sees previously
bound positional and optional names).  Returns a frame alist
(deepest-first) suitable for prepending to the closure's captured
ENV."
  (let ((frame nil)
        (state 'required)
        (remaining args))
    (dolist (p arglist)
      (cond
       ((eq p '&optional) (setq state 'optional))
       ((eq p '&rest)     (setq state 'rest))
       ((eq state 'required)
        (when (null remaining)
          (signal 'nelisp-closure-arity-error
                  (list "too few arguments"
                        :param p :supplied (length args))))
        (push (cons p (car remaining)) frame)
        (setq remaining (cdr remaining)))
       ((eq state 'optional)
        (let ((name (nelisp-closure--param-name p)))
          (cond
           (remaining
            (push (cons name (car remaining)) frame)
            (setq remaining (cdr remaining)))
           (t
            ;; Evaluate default with previously-bound params visible.
            (let ((default-env (append frame env)))
              (push (cons name
                          (nelisp-closure--param-default p default-env))
                    frame))))))
       ((eq state 'rest)
        (push (cons p remaining) frame)
        (setq remaining nil))))
    (when remaining
      (signal 'nelisp-closure-arity-error
              (list "too many arguments"
                    :extra remaining :supplied (length args))))
    (nreverse frame)))

;;; Apply --------------------------------------------------------------

(defun nelisp-closure-apply (closure args)
  "Apply CLOSURE to ARGS (a list); return the result of BODY's last form.
Captured ENV is preserved by reference: `setq' inside BODY that
targets a captured symbol mutates the shared cell, matching Phase
3 SHIPPED captured-mutate semantics (Doc 40 §3.3)."
  (unless (nelisp-closure-p closure)
    (signal 'nelisp-closure-error (list "not a closure" closure)))
  (let* ((env     (nelisp-closure-env closure))
         (arglist (nelisp-closure-arglist closure))
         (body    (nelisp-closure-body closure))
         (frame   (nelisp-closure--bind-params arglist args env))
         (call-env (append frame env)))
    (nelisp-closure--eval-body body call-env)))

(defun nelisp-closure--eval-body (forms env)
  "Sequentially evaluate FORMS under ENV, return the last form's value."
  (let ((last nil))
    (dolist (f forms)
      (setq last (nelisp-special-forms-eval f env)))
    last))

;;; Free-variable / captured-set analysis ------------------------------

(defun nelisp-closure-capture-vars (form env)
  "Return symbols bound in ENV that are referenced free in FORM.

Walks FORM, computes its free-variable set, and intersects with the
keys of ENV.  Used by the native-compile frontend (Phase 7.1.X) for
closure-conversion env-vector layout.

ENV is the standard alist of `(SYM . VAL)'; only the keys are
inspected.  The result is a fresh list of symbols, in alist key
order, with duplicates removed."
  (let* ((env-keys (mapcar #'car env))
         (free     (nelisp-closure--free-vars form nil))
         (captured nil))
    (dolist (k env-keys)
      (when (and (memq k free) (not (memq k captured)))
        (push k captured)))
    (nreverse captured)))

(defun nelisp-closure--free-vars (form bound)
  "Return the free-variable set of FORM given the BOUND symbol list.
BOUND is the list of locally-bound symbols at this point.  The
returned list is de-duplicated but order is unspecified."
  (cond
   ((null form) nil)
   ((symbolp form)
    (cond
     ((eq form t) nil)
     ((keywordp form) nil)
     ((memq form bound) nil)
     (t (list form))))
   ((not (consp form)) nil)
   (t
    (let ((head (car form))
          (tail (cdr form)))
      (cond
       ((eq head 'quote) nil)
       ((eq head 'function)
        (let ((arg (car tail)))
          (if (and (consp arg) (eq (car arg) 'lambda))
              (nelisp-closure--free-vars-lambda (cdr arg) bound)
            nil)))
       ((eq head 'lambda)
        (nelisp-closure--free-vars-lambda tail bound))
       ((eq head 'let)
        (nelisp-closure--free-vars-let tail bound nil))
       ((eq head 'let*)
        (nelisp-closure--free-vars-let tail bound t))
       ((eq head 'setq)
        (nelisp-closure--free-vars-setq tail bound))
       ((eq head 'condition-case)
        ;; (condition-case VAR BODYFORM HANDLERS...)
        (let* ((var (car tail))
               (body-form (cadr tail))
               (handlers (cddr tail))
               (acc nil)
               (inner-bound (if (and var (symbolp var) (not (eq var nil)))
                                (cons var bound)
                              bound)))
          (setq acc (nelisp-closure--merge-free
                     acc (nelisp-closure--free-vars body-form bound)))
          (dolist (h handlers)
            (when (consp h)
              (dolist (form (cdr h))
                (setq acc (nelisp-closure--merge-free
                           acc (nelisp-closure--free-vars form inner-bound))))))
          acc))
       (t
        (let ((acc nil))
          (dolist (sub form)
            (setq acc (nelisp-closure--merge-free
                       acc (nelisp-closure--free-vars sub bound))))
          acc)))))))

(defun nelisp-closure--free-vars-lambda (lambda-tail bound)
  "Free vars of LAMBDA-TAIL = `(ARGLIST BODY...)' under BOUND."
  (let* ((arglist (car lambda-tail))
         (body    (cdr lambda-tail))
         (new-bound bound)
         (acc nil))
    (dolist (p arglist)
      (cond
       ((memq p '(&optional &rest)) nil)
       ((symbolp p)
        (setq new-bound (cons p new-bound)))
       ((consp p)
        (when (cdr p)
          (setq acc (nelisp-closure--merge-free
                     acc (nelisp-closure--free-vars (cadr p) bound))))
        (setq new-bound (cons (car p) new-bound)))))
    (dolist (f body)
      (setq acc (nelisp-closure--merge-free
                 acc (nelisp-closure--free-vars f new-bound))))
    acc))

(defun nelisp-closure--free-vars-let (let-tail bound sequential)
  "Free vars of let / let* tail = `(BINDINGS BODY...)'.
SEQUENTIAL t = let* (each binding sees previous), nil = let
(parallel: bindings see only the outer BOUND)."
  (let* ((bindings (car let-tail))
         (body     (cdr let-tail))
         (rhs-bound bound)
         (new-bound bound)
         (acc nil))
    (dolist (b bindings)
      (let ((sym (cond ((symbolp b) b)
                       ((consp b)   (car b))
                       (t nil)))
            (rhs (cond ((symbolp b) nil)
                       ((consp b)   (cadr b))
                       (t nil))))
        (when rhs
          (setq acc (nelisp-closure--merge-free
                     acc (nelisp-closure--free-vars rhs rhs-bound))))
        (when sym
          (setq new-bound (cons sym new-bound))
          (when sequential
            (setq rhs-bound new-bound)))))
    (dolist (f body)
      (setq acc (nelisp-closure--merge-free
                 acc (nelisp-closure--free-vars f new-bound))))
    acc))

(defun nelisp-closure--free-vars-setq (setq-tail bound)
  "Free vars of (setq SYM VAL ...) = both SYM and VAL contribute."
  (let ((acc nil)
        (rest setq-tail))
    (while rest
      (let ((sym (car rest))
            (val (cadr rest)))
        (when (and (symbolp sym) (not (memq sym bound))
                   (not (eq sym nil)) (not (eq sym t))
                   (not (keywordp sym)))
          (setq acc (cons sym acc)))
        (when val
          (setq acc (nelisp-closure--merge-free
                     acc (nelisp-closure--free-vars val bound))))
        (setq rest (cddr rest))))
    acc))

(defun nelisp-closure--merge-free (a b)
  "Merge two free-variable sets, preserving uniqueness."
  (let ((acc a))
    (dolist (x b)
      (unless (memq x acc)
        (setq acc (cons x acc))))
    acc))

;;; Bridge to `nelisp-special-forms' tag (Doc 40 §3.3 risk pin) ---------

(defun nelisp-closure-from-sf-tag (sf-closure &optional source-marker)
  "Convert a Phase 7+B `(nelisp-sf-closure ENV PARAMS BODY)' tag
SF-CLOSURE into a Phase 7+C `nelisp-closure' record."
  (unless (and (consp sf-closure)
               (eq (car sf-closure) 'nelisp-sf-closure))
    (signal 'nelisp-closure-error (list "not a sf-closure" sf-closure)))
  (nelisp-closure-make
   (nth 1 sf-closure)
   (nth 2 sf-closure)
   (nth 3 sf-closure)
   source-marker))

(defun nelisp-closure-to-sf-tag (closure)
  "Convert a Phase 7+C CLOSURE record to a Phase 7+B
`(nelisp-sf-closure ENV PARAMS BODY)' tag list.

Used to hand a Phase 7+C-built closure to the older tree-walk
dispatcher in `nelisp-special-forms.el' without re-binding."
  (unless (nelisp-closure-p closure)
    (signal 'nelisp-closure-error (list "not a closure" closure)))
  (list 'nelisp-sf-closure
        (nelisp-closure-env closure)
        (nelisp-closure-arglist closure)
        (nelisp-closure-body closure)))

(provide 'nelisp-closure)

;;; nelisp-closure.el ends here
