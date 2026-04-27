;;; nelisp-tramp.el --- Trampoline (explicit-stack) NeLisp evaluator  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 2.5 trampoline evaluator.  Produces the same observable result
;; as `nelisp-eval-form' (the recursive evaluator in nelisp-eval.el) but
;; runs the dispatch as a single host-level `while' loop driving an
;; explicit work-stack.  One NeLisp call costs one heap-allocated work
;; record, not one host C frame, so NeLisp-on-NeLisp execution
;; (cycle-1 / cycle-2) no longer blows the host C stack.
;;
;; Trampoline state is two values:
;;   WORK   — list (LIFO) of pending work records
;;   RESULT — the value left by the most recent `:eval' completion
;;
;; A work record is a list whose CAR is a tag symbol; the rest are
;; tag-specific payload.  The loop pops one record per iteration,
;; produces a value (which becomes the new RESULT) or pushes follow-up
;; records (continuations).  When WORK empties, RESULT is the answer.
;;
;; Phase 2.5b additionally moves all four control-flow constructs that
;; previously delegated to host primitives — `catch' / `throw',
;; `unwind-protect', `condition-case' — onto the same work-stack.
;; Markers placed on the stack record the *intent* to catch / clean up;
;; non-local exits use a private `nelisp-tramp--throw' signal that the
;; outer driver catches and unwinds via stack walk.  As a result no
;; sub-evaluation recurses into `nelisp-tramp-eval' itself, and
;; cycle-1 / cycle-2 NeLisp-on-NeLisp execution runs in O(1) host
;; frames regardless of NeLisp recursion depth.
;;
;; Phase 2.5b subset (this file):
;;   atom literals, symbol lookup, quote, function, if, cond,
;;   and, or, when, unless, progn, prog1, prog2, let, let*, lambda,
;;   defun, defvar, defconst, setq, while, condition-case (work-stack),
;;   unwind-protect (work-stack), catch / throw (work-stack), defmacro,
;;   function call (closure / primitive symbol / lambda).

;;; Code:

(require 'nelisp-eval)
(declare-function nelisp-macroexpand "nelisp-macro" (form))
(declare-function nelisp--eval-defmacro "nelisp-macro" (args env))

;;; Internal throw signal ----------------------------------------------

(define-error 'nelisp-tramp--throw
  "internal trampoline non-local exit (catch/throw)")

;;; Driver -------------------------------------------------------------

(defun nelisp-tramp-eval (form &optional env)
  "Evaluate FORM under lexical ENV and return its value.
Same observable semantics as `nelisp-eval-form', but the dispatch
runs on an explicit work-stack so that NeLisp closures executing
NeLisp closures do not consume host C stack frames per call."
  (let ((work   (list (list :eval form (or env '()))))
        (result nil))
    (while work
      (let ((step (pop work)))
        (condition-case err
            (let ((nltv (car step))) (cond

              ;; ---- :eval FORM ENV ----
              ((eq nltv :eval)
               (let ((f (nth 1 step))
                     (e (nth 2 step)))
                 (cond
                  ;; self-evaluating literals
                  ((or (numberp f) (stringp f) (vectorp f)
                       (eq f nil) (eq f t) (keywordp f))
                   (setq result f))
                  ;; symbols → lookup
                  ((symbolp f)
                   (setq result (nelisp--lookup f e)))
                  ;; cons forms
                  ((consp f)
                   (let ((head (car f))
                         (args (cdr f)))
                     (cond
                      ((eq head 'quote)
                       (setq result (car args)))
                      ((eq head 'function)
                       (setq result (nelisp-tramp--eval-function (car args) e)))
                      ((eq head 'if)
                       (push (list :if-branch (cdr args) e) work)
                       (push (list :eval (car args) e) work))
                      ((eq head 'cond)
                       (push (list :cond-clauses args e) work))
                      ((eq head 'and)
                       (cond
                        ((null args) (setq result t))
                        (t
                         (push (list :and-rest (cdr args) e) work)
                         (push (list :eval (car args) e) work))))
                      ((eq head 'or)
                       (cond
                        ((null args) (setq result nil))
                        (t
                         (push (list :or-rest (cdr args) e) work)
                         (push (list :eval (car args) e) work))))
                      ((eq head 'when)
                       (push (list :when-body (cdr args) e) work)
                       (push (list :eval (car args) e) work))
                      ((eq head 'unless)
                       (push (list :unless-body (cdr args) e) work)
                       (push (list :eval (car args) e) work))
                      ((eq head 'progn)
                       (push (list :begin args e) work))
                      ((eq head 'prog1)
                       (push (list :prog1-rest (cdr args) e) work)
                       (push (list :eval (car args) e) work))
                      ((eq head 'prog2)
                       (push (list :prog2-second (cdr args) e) work)
                       (push (list :eval (car args) e) work))
                      ((eq head 'let)
                       (push (list :let-init args nil e) work))
                      ((eq head 'let*)
                       (push (list :let*-init (car args) (cdr args) nil e) work))
                      ((eq head 'lambda)
                       (setq result (nelisp--make-closure e (car args) (cdr args))))
                      ((eq head 'defun)
                       (setq result (nelisp--eval-defun args e)))
                      ((eq head 'defvar)
                       (let ((name (car args)))
                         (unless (symbolp name)
                           (signal 'nelisp-eval-error
                                   (list "defvar needs a symbol" name)))
                         (puthash name t nelisp--specials)
                         (cond
                          ((and (cdr args)
                                (eq (gethash name nelisp--globals nelisp--unbound)
                                    nelisp--unbound))
                           (push (list :defvar-store name) work)
                           (push (list :eval (cadr args) e) work))
                          (t (setq result name)))))
                      ((eq head 'defconst)
                       (let ((name (car args)))
                         (unless (symbolp name)
                           (signal 'nelisp-eval-error
                                   (list "defconst needs a symbol" name)))
                         (unless (cdr args)
                           (signal 'nelisp-eval-error
                                   (list "defconst needs a value" name)))
                         (puthash name t nelisp--specials)
                         (push (list :defconst-store name) work)
                         (push (list :eval (cadr args) e) work)))
                      ((eq head 'setq)
                       (push (list :setq-pairs args e nil) work))
                      ((eq head 'while)
                       (push (list :while-test (car args) (cdr args) e) work)
                       (push (list :eval (car args) e) work))
                      ((eq head 'catch)
                       (push (list :catch-tag-eval (cdr args) e) work)
                       (push (list :eval (car args) e) work))
                      ((eq head 'throw)
                       (push (list :throw-eval-val (cadr args) e) work)
                       (push (list :eval (car args) e) work))
                      ((eq head 'unwind-protect)
                       ;; Push :unwind-mark FIRST so it sits below the body
                       ;; on the stack — the body runs to completion, then
                       ;; the marker fires (:unwind-mark dispatch below
                       ;; runs the cleanup forms preserving body's result).
                       ;; A non-local exit unwinds through the marker,
                       ;; which the loop's signal handlers run before
                       ;; continuing the unwind walk.
                       (push (list :unwind-mark (cdr args) e) work)
                       (push (list :eval (car args) e) work))
                      ((eq head 'condition-case)
                       (let ((var (car args))
                             (bodyform (cadr args))
                             (handlers (cddr args)))
                         (unless (or (null var) (symbolp var))
                           (signal 'nelisp-eval-error
                                   (list "condition-case VAR must be symbol or nil"
                                         var)))
                         (push (list :cc-mark var handlers e) work)
                         (push (list :eval bodyform e) work)))
                      ((eq head 'defmacro)
                       (setq result (nelisp--eval-defmacro args e)))
                      ;; macro call?
                      ((and (symbolp head)
                            (not (eq (gethash head nelisp--macros nelisp--unbound)
                                     nelisp--unbound)))
                       (push (list :eval (nelisp-macroexpand (cons head args)) e)
                             work))
                      ;; ordinary call
                      (t
                       (push (list :call-collect head args nil e) work)))))
                  (t (signal 'nelisp-eval-error (list "cannot evaluate" f))))))

              ;; ---- :begin FORMS ENV ----
              ((eq nltv :begin)
               (let ((forms (nth 1 step))
                     (e (nth 2 step)))
                 (cond
                  ((null forms) (setq result nil))
                  ((null (cdr forms))
                   (push (list :eval (car forms) e) work))
                  (t
                   (push (list :begin (cdr forms) e) work)
                   (push (list :eval (car forms) e) work)))))

              ;; ---- :if-branch (THEN-FORM ELSE-FORMS...) ENV ----
              ((eq nltv :if-branch)
               (let ((arms (nth 1 step))
                     (e (nth 2 step)))
                 (if result
                     (push (list :eval (car arms) e) work)
                   (push (list :begin (cdr arms) e) work))))

              ;; ---- :cond-clauses CLAUSES ENV ----
              ((eq nltv :cond-clauses)
               (let ((clauses (nth 1 step))
                     (e (nth 2 step)))
                 (cond
                  ((null clauses) (setq result nil))
                  (t
                   (let ((cl (car clauses)))
                     (unless (consp cl)
                       (signal 'nelisp-eval-error
                               (list "cond clause must be a list" cl)))
                     (push (list :cond-after-test cl (cdr clauses) e) work)
                     (push (list :eval (car cl) e) work))))))
              ((eq nltv :cond-after-test)
               (let ((cl (nth 1 step))
                     (rest (nth 2 step))
                     (e (nth 3 step)))
                 (cond
                  (result
                   (cond
                    ((cdr cl) (push (list :begin (cdr cl) e) work))
                    (t nil)))
                  (t
                   (push (list :cond-clauses rest e) work)))))

              ;; ---- :and-rest FORMS ENV ----
              ((eq nltv :and-rest)
               (let ((forms (nth 1 step))
                     (e (nth 2 step)))
                 (cond
                  ((null result))
                  ((null forms))
                  (t
                   (push (list :and-rest (cdr forms) e) work)
                   (push (list :eval (car forms) e) work)))))

              ;; ---- :or-rest FORMS ENV ----
              ((eq nltv :or-rest)
               (let ((forms (nth 1 step))
                     (e (nth 2 step)))
                 (cond
                  (result)
                  ((null forms))
                  (t
                   (push (list :or-rest (cdr forms) e) work)
                   (push (list :eval (car forms) e) work)))))

              ;; ---- :when-body BODY ENV ----
              ((eq nltv :when-body)
               (let ((body (nth 1 step))
                     (e (nth 2 step)))
                 (if result (push (list :begin body e) work)
                   (setq result nil))))

              ;; ---- :unless-body BODY ENV ----
              ((eq nltv :unless-body)
               (let ((body (nth 1 step))
                     (e (nth 2 step)))
                 (if result (setq result nil)
                   (push (list :begin body e) work))))

              ;; ---- :prog1-rest FORMS ENV ----
              ((eq nltv :prog1-rest)
               (let ((forms (nth 1 step))
                     (e (nth 2 step))
                     (saved result))
                 (push (list :prog1-restore saved) work)
                 (push (list :begin forms e) work)))
              ((eq nltv :prog1-restore)
               (setq result (nth 1 step)))

              ;; ---- :prog2-second (SECOND BODY...) ENV ----
              ((eq nltv :prog2-second)
               (let ((rest (nth 1 step))
                     (e (nth 2 step)))
                 (push (list :prog2-after (cdr rest) e) work)
                 (push (list :eval (car rest) e) work)))
              ((eq nltv :prog2-after)
               (let ((forms (nth 1 step))
                     (e (nth 2 step))
                     (saved result))
                 (push (list :prog1-restore saved) work)
                 (push (list :begin forms e) work)))

              ;; ---- :let-init (BINDINGS . BODY) ACC ENV ----
              ((eq nltv :let-init)
               (let ((args (nth 1 step))
                     (acc (nth 2 step))
                     (e (nth 3 step)))
                 (let ((bindings (car args))
                       (body (cdr args)))
                   (cond
                    ((null bindings)
                     (push (list :let-bind acc body e) work))
                    (t
                     (let ((b (car bindings)))
                       (cond
                        ((symbolp b)
                         (push (list :let-init
                                     (cons (cdr bindings) body)
                                     (cons (cons b nil) acc)
                                     e)
                               work))
                        ((and (consp b) (symbolp (car b)))
                         (push (list :let-init-after-init
                                     (car b) (cdr bindings) body acc e)
                               work)
                         (push (list :eval (cadr b) e) work))
                        (t
                         (signal 'nelisp-eval-error
                                 (list "malformed let binding" b))))))))))
              ((eq nltv :let-init-after-init)
               (let ((sym (nth 1 step))
                     (rest-bindings (nth 2 step))
                     (body (nth 3 step))
                     (acc (nth 4 step))
                     (e (nth 5 step)))
                 (push (list :let-init
                             (cons rest-bindings body)
                             (cons (cons sym result) acc)
                             e)
                       work)))
              ((eq nltv :let-bind)
               (let* ((acc  (nth 1 step))
                      (body (nth 2 step))
                      (e    (nth 3 step))
                      (lex-pairs nil)
                      (dyn-saves nil))
                 (dolist (p acc)
                   (let ((sym (car p))
                         (val (cdr p)))
                     (cond
                      ((gethash sym nelisp--specials)
                       (push (list sym val
                                   (gethash sym nelisp--globals nelisp--unbound))
                             dyn-saves))
                      (t
                       (push (cons sym val) lex-pairs)))))
                 (dolist (d dyn-saves)
                   (puthash (car d) (nth 1 d) nelisp--globals))
                 (let ((env2 (append (nreverse lex-pairs) e)))
                   (push (list :restore-dyn dyn-saves) work)
                   (push (list :begin body env2) work))))
              ((eq nltv :restore-dyn)
               (nelisp--restore-dynamic (nth 1 step)))

              ;; ---- :let*-init BINDINGS BODY DYN-SAVES ENV ----
              ((eq nltv :let*-init)
               (let ((bindings (nth 1 step))
                     (body (nth 2 step))
                     (dyn-saves (nth 3 step))
                     (e (nth 4 step)))
                 (cond
                  ((null bindings)
                   (push (list :restore-dyn dyn-saves) work)
                   (push (list :begin body e) work))
                  (t
                   (let ((b (car bindings)))
                     (cond
                      ((symbolp b)
                       (let ((env2 (cons (cons b nil) e)))
                         (push (list :let*-init (cdr bindings) body dyn-saves env2)
                               work)))
                      ((and (consp b) (symbolp (car b)))
                       (push (list :let*-after (car b) (cdr bindings)
                                   body dyn-saves e)
                             work)
                       (push (list :eval (cadr b) e) work))
                      (t
                       (signal 'nelisp-eval-error
                               (list "malformed let* binding" b)))))))))
              ((eq nltv :let*-after)
               (let ((sym (nth 1 step))
                     (rest-bindings (nth 2 step))
                     (body (nth 3 step))
                     (dyn-saves (nth 4 step))
                     (e (nth 5 step))
                     (val result))
                 (cond
                  ((gethash sym nelisp--specials)
                   (let ((old (gethash sym nelisp--globals nelisp--unbound)))
                     (puthash sym val nelisp--globals)
                     (push (list :let*-init rest-bindings body
                                 (cons (list sym val old) dyn-saves) e)
                           work)))
                  (t
                   (push (list :let*-init rest-bindings body dyn-saves
                               (cons (cons sym val) e))
                         work)))))

              ;; ---- :defvar-store SYM ----
              ((eq nltv :defvar-store)
               (puthash (nth 1 step) result nelisp--globals)
               (setq result (nth 1 step)))
              ;; ---- :defconst-store SYM ----
              ((eq nltv :defconst-store)
               (puthash (nth 1 step) result nelisp--globals)
               (setq result (nth 1 step)))

              ;; ---- :setq-pairs ARGS ENV LAST ----
              ((eq nltv :setq-pairs)
               (let ((args (nth 1 step))
                     (e (nth 2 step)))
                 (cond
                  ((null args)
                   (setq result (nth 3 step)))
                  (t
                   (unless (cdr args)
                     (signal 'nelisp-eval-error (list "setq with odd args")))
                   (let ((sym (car args)))
                     (unless (symbolp sym)
                       (signal 'nelisp-eval-error (list "setq non-symbol" sym)))
                     (push (list :setq-after sym (cddr args) e) work)
                     (push (list :eval (cadr args) e) work))))))
              ((eq nltv :setq-after)
               (let* ((sym (nth 1 step))
                      (rest (nth 2 step))
                      (e (nth 3 step))
                      (cell (assq sym e))
                      (val result))
                 (if cell (setcdr cell val)
                   (puthash sym val nelisp--globals))
                 (push (list :setq-pairs rest e val) work)))

              ;; ---- :while-test TEST BODY ENV ----
              ((eq nltv :while-test)
               (let ((test (nth 1 step))
                     (body (nth 2 step))
                     (e (nth 3 step)))
                 (cond
                  ((null result) (setq result nil))
                  (t
                   (push (list :while-test test body e) work)
                   (push (list :eval test e) work)
                   (push (list :begin body e) work)))))

              ;; ---- catch (work-stack version) ----
              ((eq nltv :catch-tag-eval)
               (let ((tag result)
                     (body (nth 1 step))
                     (e (nth 2 step)))
                 (push (list :catch-mark tag) work)
                 (push (list :begin body e) work)))
              ((eq nltv :catch-mark)
               ;; body completed without throw — drop the marker
               nil)

              ;; ---- throw (work-stack version) ----
              ((eq nltv :throw-eval-val)
               (let ((tag result)
                     (val-form (nth 1 step))
                     (e (nth 2 step)))
                 (push (list :throw-final tag) work)
                 (push (list :eval val-form e) work)))
              ((eq nltv :throw-final)
               (signal 'nelisp-tramp--throw
                       (list (nth 1 step) result)))

              ;; ---- unwind-protect ----
              ((eq nltv :unwind-mark)
               (let ((cleanup (nth 1 step))
                     (e (nth 2 step))
                     (saved result))
                 (cond
                  ((null cleanup) nil)
                  (t
                   (push (list :unwind-restore saved) work)
                   (push (list :begin cleanup e) work)))))
              ((eq nltv :unwind-restore)
               (setq result (nth 1 step)))

              ;; ---- condition-case (body completed without error) ----
              ((eq nltv :cc-mark)
               nil)

              ;; ---- :call-collect HEAD ARGS-FORMS COLLECTED ENV ----
              ((eq nltv :call-collect)
               (let ((head (nth 1 step))
                     (rest (nth 2 step))
                     (acc (nth 3 step))
                     (e (nth 4 step)))
                 (cond
                  ((null rest)
                   (let ((args (nreverse acc)))
                     (cond
                      ;; trampoline-aware funcall: dispatch via :tramp-apply
                      ;; so a closure target is invoked through the work-stack
                      ;; instead of host nelisp--apply (which would eat a host
                      ;; frame).  Same for apply / mapcar / mapc / mapconcat
                      ;; below — these are the "host primitive that calls back
                      ;; into a NeLisp closure" cases that drive cycle-1's
                      ;; host-stack growth.
                      ((and (symbolp head) (eq head 'funcall) args)
                       (push (list :tramp-apply (car args) (cdr args)) work))
                      ((and (symbolp head) (eq head 'apply) args)
                       ;; (apply FN A1 A2 ... LAST-LIST)
                       (let* ((target (car args))
                              (rest-args (cdr args))
                              (init (butlast rest-args 1))
                              (tail (car (last rest-args))))
                         (push (list :tramp-apply target
                                     (append init tail))
                               work)))
                      ((and (symbolp head) (eq head 'mapcar))
                       (push (list :mapcar-fold (car args) (cadr args) nil)
                             work))
                      ((and (symbolp head) (eq head 'mapc))
                       (push (list :mapc-fold (car args) (cadr args)
                                   (cadr args))
                             work))
                      ((and (symbolp head) (eq head 'mapconcat))
                       (push (list :mapconcat-fold (car args) (cadr args)
                                   (caddr args) nil)
                             work))
                      (t
                       (let ((fn (cond
                                  ((symbolp head) (nelisp--function-of head))
                                  ((and (consp head) (eq (car head) 'lambda))
                                   (nelisp--make-closure e (cadr head)
                                                         (cddr head)))
                                  ((nelisp--closure-p head) head)
                                  (t (signal 'nelisp-eval-error
                                             (list "not callable" head))))))
                         (cond
                          ((nelisp--closure-p fn)
                           (let* ((params (nelisp--closure-params fn))
                                  (body (nelisp--closure-body fn))
                                  (cenv (nelisp--closure-env fn))
                                  (call-env
                                   (nelisp--bind-params params args cenv)))
                             (push (list :begin body call-env) work)))
                          (t
                           ;; Bcls and primitives both go through
                           ;; `nelisp--apply', which is bcl-aware.
                           (setq result (nelisp--apply fn args)))))))))
                  (t
                   (push (list :call-after head (cdr rest) acc e) work)
                   (push (list :eval (car rest) e) work)))))
              ((eq nltv :call-after)
               (let ((head (nth 1 step))
                     (rest (nth 2 step))
                     (acc (nth 3 step))
                     (e (nth 4 step)))
                 (push (list :call-collect head rest (cons result acc) e) work)))

              ;; ---- :tramp-apply TARGET ARGS-LIST ----
              ;; Dispatch a callable onto an already-evaluated arg list,
              ;; through the work-stack.  Used by trampoline-aware funcall /
              ;; apply / mapcar et al so closure targets do not consume host
              ;; stack frames.
              ((eq nltv :tramp-apply)
               (let ((target (nth 1 step))
                     (args (nth 2 step)))
                 (cond
                  ((nelisp--closure-p target)
                   (let* ((params (nelisp--closure-params target))
                          (body (nelisp--closure-body target))
                          (cenv (nelisp--closure-env target))
                          (call-env (nelisp--bind-params params args cenv)))
                     (push (list :begin body call-env) work)))
                  ((and (consp target) (eq (car target) 'nelisp-bcl))
                   ;; Bytecode closures execute on the VM, which has its
                   ;; own dispatch loop and so consumes a single host
                   ;; stack frame regardless of NeLisp call depth.
                   (setq result (nelisp--apply target args)))
                  ((symbolp target)
                   (let ((nfn (gethash target nelisp--functions
                                       nelisp--unbound)))
                     (cond
                      ((eq nfn nelisp--unbound)
                       (cond
                        ((functionp target)
                         (setq result (apply target args)))
                        (t (signal 'nelisp-void-function (list target)))))
                      (t
                       (push (list :tramp-apply nfn args) work)))))
                  ((functionp target)
                   (setq result (apply target args)))
                  (t (signal 'nelisp-eval-error
                             (list "not a function" target))))))

              ;; ---- :mapcar-fold FN REMAINING-LIST ACC-RESULTS ----
              ((eq nltv :mapcar-fold)
               (let ((fn (nth 1 step))
                     (lst (nth 2 step))
                     (acc (nth 3 step)))
                 (cond
                  ((null lst)
                   (setq result (nreverse acc)))
                  (t
                   (push (list :mapcar-collect fn (cdr lst) acc) work)
                   (push (list :tramp-apply fn (list (car lst))) work)))))
              ((eq nltv :mapcar-collect)
               (let ((fn (nth 1 step))
                     (rest (nth 2 step))
                     (acc (nth 3 step)))
                 (push (list :mapcar-fold fn rest (cons result acc)) work)))

              ;; ---- :mapc-fold FN REMAINING-LIST ORIG-LIST ----
              ((eq nltv :mapc-fold)
               (let ((fn (nth 1 step))
                     (lst (nth 2 step))
                     (orig (nth 3 step)))
                 (cond
                  ((null lst)
                   (setq result orig))
                  (t
                   (push (list :mapc-step fn (cdr lst) orig) work)
                   (push (list :tramp-apply fn (list (car lst))) work)))))
              ((eq nltv :mapc-step)
               (let ((fn (nth 1 step))
                     (rest (nth 2 step))
                     (orig (nth 3 step)))
                 (push (list :mapc-fold fn rest orig) work)))

              ;; ---- :mapconcat-fold FN REMAINING-LIST SEP ACC-STRINGS ----
              ((eq nltv :mapconcat-fold)
               (let ((fn (nth 1 step))
                     (lst (nth 2 step))
                     (sep (nth 3 step))
                     (acc (nth 4 step)))
                 (cond
                  ((null lst)
                   ;; reverse and join with separator
                   (setq result
                         (mapconcat #'identity (nreverse acc) (or sep ""))))
                  (t
                   (push (list :mapconcat-collect fn (cdr lst) sep acc) work)
                   (push (list :tramp-apply fn (list (car lst))) work)))))
              ((eq nltv :mapconcat-collect)
               (let ((fn (nth 1 step))
                     (rest (nth 2 step))
                     (sep (nth 3 step))
                     (acc (nth 4 step)))
                 (push (list :mapconcat-fold fn rest sep (cons result acc))
                       work)))

              (t (signal 'nelisp-eval-error
                         (list "trampoline: unknown step" step)))))

          ;; ---- non-local exits dispatched via outer condition-case ----
          (nelisp-tramp--throw
           ;; (signal 'nelisp-tramp--throw (list TAG VAL)) — unwind for catch
           (let* ((data (cdr err))
                  (tag (car data))
                  (val (cadr data))
                  (handled nil))
             (catch 'nelisp-tramp--unwind-done
               (while work
                 (let ((m (pop work)))
                   (cond
                    ((and (eq (car m) :catch-mark) (eq (nth 1 m) tag))
                     (setq result val)
                     (setq handled t)
                     (throw 'nelisp-tramp--unwind-done nil))
                    ((eq (car m) :unwind-mark)
                     (let ((saved result))
                       (nelisp-tramp-eval (cons 'progn (nth 1 m)) (nth 2 m))
                       (setq result saved)))
                    ;; otherwise pop & discard
                    ))))
             (unless handled
               (signal 'nelisp-tramp--throw (list tag val)))))
          (error
           ;; native error or signaled NeLisp error — search for cc handler
           (let* ((conds (get (car err) 'error-conditions))
                  (handled nil))
             (catch 'nelisp-tramp--unwind-done
               (while work
                 (let ((m (pop work)))
                   (cond
                    ((eq (car m) :cc-mark)
                     (let ((var (nth 1 m))
                           (handlers (nth 2 m))
                           (e (nth 3 m))
                           (matched nil))
                       (catch 'nelisp-tramp--cc-match
                         (dolist (h handlers)
                           (unless (consp h)
                             (signal 'nelisp-eval-error
                                     (list "condition-case handler must be a list"
                                           h)))
                           (when (nelisp--handler-matches-p (car h) conds)
                             (setq matched h)
                             (throw 'nelisp-tramp--cc-match nil))))
                       (when matched
                         (let ((henv (if var (cons (cons var err) e) e)))
                           (push (list :begin (cdr matched) henv) work))
                         (setq handled t)
                         (throw 'nelisp-tramp--unwind-done nil))))
                    ((eq (car m) :unwind-mark)
                     (let ((saved result))
                       (nelisp-tramp-eval (cons 'progn (nth 1 m)) (nth 2 m))
                       (setq result saved)))
                    ;; otherwise discard
                    ))))
             (unless handled
               (signal (car err) (cdr err))))))))
    result))

(defun nelisp-tramp--eval-function (form env)
  "Trampoline equivalent of `nelisp--eval-function'."
  (cond
   ((and (consp form) (eq (car form) 'lambda))
    (nelisp--make-closure env (cadr form) (cddr form)))
   ((symbolp form) (nelisp--function-of form))
   (t (signal 'nelisp-eval-error
              (list "cannot take function value of" form)))))

(provide 'nelisp-tramp)

;;; nelisp-tramp.el ends here
