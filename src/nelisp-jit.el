;;; nelisp-jit.el --- NeLisp JIT to host Elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 3b.8a ships the minimal JIT.  Bcl (Phase 3b's bytecode VM)
;; remains as the semantic reference — this path is the performance
;; path.  The translator walks a NeLisp `lambda' form and emits an
;; equivalent host Elisp form, which `byte-compile' lowers to a host
;; closure.  A JIT closure has shape `(nelisp-jit HOST-LAMBDA)' and is
;; dispatched by `nelisp--apply' like any other callable.
;;
;; Scope after Phase 3b.8b:
;;   - top-level defuns AND captured-env closures (let-wrapped lambdas,
;;     partial application).  The captured values are embedded as a
;;     literal vector in the compiled constants pool and read via
;;     `aref' — see `nelisp-jit-try-compile-lambda'.
;;   - captured-env closures that MUTATE their captured variables via
;;     `setq' are rejected (return nil) so bcl / interpreter handle
;;     the counter-pattern.  The let-wrapped read form re-binds on
;;     every call, so mutation wouldn't survive across invocations.
;;   - inline primitives are emitted as direct host calls (see
;;     `nelisp-jit--inline-primitives').  User-defined functions and
;;     unknown symbols are dispatched through `nelisp--apply' so
;;     NeLisp-level redefinitions stay observable.
;;   - a known set of special forms gets per-form handling; anything
;;     else signals `nelisp-jit-unsupported' and the caller falls back.
;;
;; The opt-in is `nelisp-jit-enabled'; when set, `nelisp--make-closure'
;; tries the JIT before bcl compilation.

;;; Code:

(require 'cl-lib)
(require 'nelisp-eval)

(defvar nelisp-jit-enabled nil
  "When non-nil, `nelisp--make-closure' tries JIT compilation first.
Failure on unsupported forms falls through to bcl / interpreter.")

(define-error 'nelisp-jit-unsupported
  "NeLisp JIT cannot translate this form"
  'nelisp-eval-error)

(defconst nelisp-jit--inline-primitives
  '(+ - * / mod 1+ 1- < > = <= >=
    eq equal null not
    cons car cdr caar cadr cdar cddr
    list length nth nthcdr append reverse
    member memq assoc assq
    consp symbolp numberp integerp floatp stringp listp atom
    zerop)
  "Primitives that the JIT emits as direct host calls.
Redefining these at the NeLisp level will not affect JIT-compiled
bodies — they are inlined at compile time.  The set deliberately
excludes anything mutable (e.g. `message') to keep side effects
observable through `nelisp--apply'.")

(defvar nelisp-jit--primitive-set (make-hash-table :test 'eq)
  "O(1) membership check for `nelisp-jit--inline-primitives'.")

(defun nelisp-jit--refresh-primitive-set ()
  "Rebuild `nelisp-jit--primitive-set' from the defconst list."
  (clrhash nelisp-jit--primitive-set)
  (dolist (p nelisp-jit--inline-primitives)
    (puthash p t nelisp-jit--primitive-set)))
(nelisp-jit--refresh-primitive-set)

(defsubst nelisp-jit--primitive-p (sym)
  (gethash sym nelisp-jit--primitive-set))

(defun nelisp-jit--param-symbols (params)
  "Return lexical symbol names declared by PARAMS.
Filters out `&optional' and `&rest' markers so the result is a
plain list suitable for membership checks during translation."
  (let (out)
    (dolist (p params)
      (unless (memq p '(&optional &rest))
        (push p out)))
    (nreverse out)))

(defun nelisp-jit--translate-body (forms env)
  "Translate each of FORMS with ENV; return the list of translated forms."
  (mapcar (lambda (f) (nelisp-jit--translate f env)) forms))

(defun nelisp-jit--translate (form env)
  "Translate NeLisp FORM to host Elisp.
ENV is the list of lexical symbols in scope.  Unknown symbols are
emitted as NeLisp-globals lookups; unknown function heads are
dispatched through `nelisp--apply'.  Signals `nelisp-jit-unsupported'
for constructs the MVP does not handle."
  (cond
   ;; Self-evaluating.
   ((or (integerp form) (floatp form) (stringp form)
        (vectorp form) (booleanp form) (null form))
    form)
   ;; Symbol reference.
   ((symbolp form)
    (cond
     ((or (eq form t) (eq form nil) (keywordp form)) form)
     ((memq form env) form)
     (t
      ;; Global NeLisp binding.  `nelisp--unbound' sentinel discriminates
      ;; "no binding" from "bound to nil".
      `(let ((v (gethash ',form nelisp--globals nelisp--unbound)))
         (if (eq v nelisp--unbound)
             (signal 'nelisp-unbound-variable (list ',form))
           v)))))
   ;; Compound form.
   ((consp form)
    (nelisp-jit--translate-compound (car form) (cdr form) env))
   (t
    (signal 'nelisp-jit-unsupported (list form)))))

(defun nelisp-jit--translate-compound (head rest env)
  "Translate (HEAD . REST) given lexical ENV."
  (cond
   ;; ----- special forms -------------------------------------------------
   ((eq head 'quote) (cons 'quote rest))
   ((eq head 'function)
    ;; `(function X)'.  If X is a lambda, recursively translate its body
    ;; as a host lambda (still subject to JIT rules).  Bare symbols pass
    ;; through and caller can funcall them.
    (let ((arg (car rest)))
      (if (and (consp arg) (eq (car arg) 'lambda))
          (let ((sub-params (cadr arg))
                (sub-body (cddr arg)))
            `(lambda ,sub-params
               ,@(nelisp-jit--translate-body
                  sub-body
                  (append (nelisp-jit--param-symbols sub-params) env))))
        `(function ,arg))))
   ((eq head 'if)
    `(if ,(nelisp-jit--translate (car rest) env)
         ,(nelisp-jit--translate (cadr rest) env)
       ,@(nelisp-jit--translate-body (cddr rest) env)))
   ((eq head 'cond)
    `(cond ,@(mapcar (lambda (clause)
                       (nelisp-jit--translate-body clause env))
                     rest)))
   ((memq head '(when unless))
    `(,head ,(nelisp-jit--translate (car rest) env)
            ,@(nelisp-jit--translate-body (cdr rest) env)))
   ((memq head '(and or progn prog1 prog2))
    `(,head ,@(nelisp-jit--translate-body rest env)))
   ((eq head 'while)
    `(while ,(nelisp-jit--translate (car rest) env)
       ,@(nelisp-jit--translate-body (cdr rest) env)))
   ((eq head 'let)
    (let* ((bindings (car rest))
           (new-syms (mapcar (lambda (b) (if (consp b) (car b) b)) bindings))
           (translated
            (mapcar (lambda (b)
                      (cond ((symbolp b) b)
                            (t (list (car b)
                                     (nelisp-jit--translate (cadr b) env)))))
                    bindings))
           (new-env (append new-syms env)))
      `(let ,translated
         ,@(nelisp-jit--translate-body (cdr rest) new-env))))
   ((eq head 'let*)
    (let ((working env)
          (out nil))
      (dolist (b (car rest))
        (cond
         ((symbolp b) (push b out) (push b working))
         (t (push (list (car b) (nelisp-jit--translate (cadr b) working)) out)
            (push (car b) working))))
      `(let* ,(nreverse out)
         ,@(nelisp-jit--translate-body (cdr rest) working))))
   ((eq head 'setq)
    (let (forms (pairs rest))
      (while pairs
        (let ((var (car pairs))
              (val (nelisp-jit--translate (cadr pairs) env)))
          (push (if (memq var env)
                    `(setq ,var ,val)
                  `(puthash ',var ,val nelisp--globals))
                forms)
          (setq pairs (cddr pairs))))
      (if (cdr forms) `(progn ,@(nreverse forms)) (car forms))))
   ((eq head 'lambda)
    ;; Bare (lambda params body) used as a value (e.g. for mapcar arg).
    ;; Treat like (function (lambda ...)).
    (let ((sub-params (car rest))
          (sub-body (cdr rest)))
      `(lambda ,sub-params
         ,@(nelisp-jit--translate-body
            sub-body
            (append (nelisp-jit--param-symbols sub-params) env)))))
   ;; ----- call dispatch -------------------------------------------------
   ((and (symbolp head) (nelisp-jit--primitive-p head))
    ;; Direct host call on an assumed-stable primitive.
    `(,head ,@(nelisp-jit--translate-body rest env)))
   ((symbolp head)
    ;; Unknown symbol: route through `nelisp--apply' so redefinitions
    ;; and NeLisp-level semantics (bcl / jit / interp) stay observable.
    `(nelisp--apply ',head
                    (list ,@(nelisp-jit--translate-body rest env))))
   (t
    (signal 'nelisp-jit-unsupported (list (cons head rest))))))

(defun nelisp-jit--wrap-as-bcl (host-fn)
  "Package HOST-FN as a bcl-shaped closure so `nelisp-bc-run'
dispatches through its JIT fast-path instead of the VM loop.
Reusing the bcl tag means `nelisp--apply' needs no changes — a
JIT callable dispatches via the existing bcl cond arm, which
keeps the self-host `max-lisp-eval-depth' budget unchanged."
  ;; Shape: (nelisp-bcl MARKER nil nil HOST-FN nil nil).
  ;; `nelisp-bc-env' returns MARKER; `nelisp-bc-code' returns HOST-FN.
  (list 'nelisp-bcl 'nelisp-jit-marker nil nil host-fn nil nil))

(defun nelisp-jit--setq-on-env-p (form env-syms)
  "Return non-nil if FORM contains (setq SYM ...) for any SYM in ENV-SYMS.
Used as a pre-translation guard: captured-env closures that mutate
their captures cannot use the read-only `aref'-in-let embedding
because the binding is re-established on every call."
  (cond
   ((atom form) nil)
   ((and (consp form) (eq (car form) 'setq))
    (let ((pairs (cdr form)))
      (catch 'found
        (while pairs
          (when (memq (car pairs) env-syms)
            (throw 'found t))
          (setq pairs (cddr pairs)))
        nil)))
   ((consp form)
    (or (nelisp-jit--setq-on-env-p (car form) env-syms)
        (nelisp-jit--setq-on-env-p (cdr form) env-syms)))
   (t nil)))

(defun nelisp-jit--body-mutates-env-p (body env-syms)
  "Return non-nil if any form in BODY mutates an ENV-SYMS variable."
  (catch 'found
    (dolist (f body)
      (when (nelisp-jit--setq-on-env-p f env-syms)
        (throw 'found t)))
    nil))

(defun nelisp-jit-try-compile-lambda (env params body)
  "Attempt to JIT-compile a NeLisp lambda.
Returns a bcl-shaped JIT closure on success (see
`nelisp-jit--wrap-as-bcl'), or nil on unsupported forms /
captured-env closures that mutate their captures."
  (condition-case _err
      (let* ((lexical (nelisp-jit--param-symbols params))
             (env-syms (mapcar #'car env))
             (full-env (append env-syms lexical)))
        (cond
         ;; Captured-env closures that write to their captures cannot use
         ;; the `aref'-in-let embedding (the let re-initialises on every
         ;; call, so a `setq' would not persist).  Fall through to bcl /
         ;; interpreter which handle the counter pattern correctly.
         ((and env (nelisp-jit--body-mutates-env-p body env-syms)) nil)
         (t
          (let* ((translated (nelisp-jit--translate-body body full-env))
                 (host-form
                  (if env
                      (let* ((env-vec (vconcat (mapcar #'cdr env)))
                             (binds
                              (cl-loop for sym in env-syms
                                       for i from 0
                                       collect `(,sym (aref ,env-vec ,i)))))
                        `(lambda ,params
                           (let ,binds ,@translated)))
                    `(lambda ,params ,@translated)))
                 (host-fn (byte-compile host-form)))
            (nelisp-jit--wrap-as-bcl host-fn)))))
    (nelisp-jit-unsupported nil)))

(defun nelisp-jit--bc-try-advice (orig-fn env params body)
  "When `nelisp-jit-enabled', try JIT before bcl compilation.
On unsupported forms the JIT returns nil and we fall through to
the original bcl path (which itself may fall through to
interpreter closure via `nelisp--make-closure')."
  (if nelisp-jit-enabled
      (or (nelisp-jit-try-compile-lambda env params body)
          (funcall orig-fn env params body))
    (funcall orig-fn env params body)))

(defun nelisp-jit-install ()
  "Wire the JIT into `nelisp-bc-try-compile-lambda' via advice.
Call this once per session after loading `nelisp-jit'.
Set `nelisp-jit-enabled' to toggle JIT on/off without uninstalling."
  (interactive)
  (advice-add 'nelisp-bc-try-compile-lambda
              :around #'nelisp-jit--bc-try-advice))

(defun nelisp-jit-uninstall ()
  "Remove the JIT advice from `nelisp-bc-try-compile-lambda'."
  (interactive)
  (advice-remove 'nelisp-bc-try-compile-lambda
                 #'nelisp-jit--bc-try-advice))

(defsubst nelisp-jit-bcl-p (obj)
  "Non-nil if OBJ is a JIT-wrapped bcl closure."
  (and (consp obj)
       (eq (car obj) 'nelisp-bcl)
       (eq (cadr obj) 'nelisp-jit-marker)))

(provide 'nelisp-jit)
;;; nelisp-jit.el ends here
