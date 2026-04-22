;;; nelisp-eval.el --- Elisp evaluator in pure Elisp  -*- lexical-binding: t; -*-

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

;; Phase 1 Week 5-8 evaluator core.  Enough of Elisp to run `fib' and
;; `factorial' with closures that capture their lexical environment.
;;
;; Implemented special forms:
;;   quote, function, if, progn, let, let*, lambda, defun, defvar,
;;   setq, while
;;
;; Installed builtins (delegated to the host Elisp functions):
;;   car cdr cons list null not atom consp symbolp
;;   eq equal + - * / < <= > >= =
;;
;; Deferred to Week 9+ (remains out of scope for this file):
;;   cond / and / or / when / unless / defconst
;;   catch / throw / unwind-protect / condition-case
;;   defmacro / macroexpand / macroexpand-all
;;   funcall / apply / mapcar (higher-order primitives)
;;   string / format / intern / boundp / fboundp / prin1 / princ
;;   true dynamic binding (defvar currently only marks the symbol)
;;
;; NeLisp keeps its own function / global tables rather than writing to
;; the host Emacs symbol cells, so running `(nelisp-eval '(defun fib …))'
;; does not pollute the host environment and vice versa.  Primitive
;; function bindings are still borrowed from Emacs — they will be
;; re-implemented natively when Phase 5 rewrites the relevant C code
;; in NeLisp.

;;; Code:

(require 'nelisp-read)

(define-error 'nelisp-eval-error
  "NeLisp evaluation error")

(define-error 'nelisp-unbound-variable
  "NeLisp: symbol's value is void"
  'nelisp-eval-error)

(define-error 'nelisp-void-function
  "NeLisp: symbol's function is void"
  'nelisp-eval-error)

;;; Global state -------------------------------------------------------

(defvar nelisp--functions (make-hash-table :test 'eq)
  "Symbol -> primitive function or NeLisp closure.
Populated by `defun' and by `nelisp--install-primitives'.")

(defvar nelisp--globals (make-hash-table :test 'eq)
  "Symbol -> value for top-level definitions (defvar, global setq).")

(defvar nelisp--specials (make-hash-table :test 'eq)
  "Set of symbols declared special via `defvar'.
Reserved for the Week 17+ lexical/dynamic binding split — currently
populated but not yet consulted during binding.")

(defconst nelisp--unbound (make-symbol "nelisp-unbound")
  "Sentinel returned from hash-table lookups when a key is missing.")

;;; Closure representation --------------------------------------------

(defsubst nelisp--closure-p (x)
  (and (consp x) (eq (car x) 'nelisp-closure)))

(defsubst nelisp--make-closure (env params body)
  (list 'nelisp-closure env params body))

(defsubst nelisp--closure-env    (c) (nth 1 c))
(defsubst nelisp--closure-params (c) (nth 2 c))
(defsubst nelisp--closure-body   (c) (nth 3 c))

;;; Lookup -------------------------------------------------------------

(defun nelisp--lookup (sym env)
  "Return SYM's value in ENV (alist) then in globals.
Signal `nelisp-unbound-variable' if neither binds it."
  (cond
   ((eq sym nil) nil)
   ((eq sym t) t)
   ((keywordp sym) sym)
   (t
    (let ((cell (assq sym env)))
      (if cell
          (cdr cell)
        (let ((g (gethash sym nelisp--globals nelisp--unbound)))
          (if (eq g nelisp--unbound)
              (signal 'nelisp-unbound-variable (list sym))
            g)))))))

(defun nelisp--function-of (sym)
  "Return the callable bound to SYM in `nelisp--functions'.
Signal `nelisp-void-function' if none is bound."
  (let ((f (gethash sym nelisp--functions nelisp--unbound)))
    (if (eq f nelisp--unbound)
        (signal 'nelisp-void-function (list sym))
      f)))

;;; Evaluator ----------------------------------------------------------

(defun nelisp-eval-form (form env)
  "Evaluate FORM in lexical ENV (alist of (symbol . value))."
  (cond
   ((or (numberp form) (stringp form) (keywordp form)
        (eq form nil) (eq form t))
    form)
   ((symbolp form)
    (nelisp--lookup form env))
   ((consp form)
    (let ((head (car form))
          (args (cdr form)))
      (cond
       ((eq head 'quote)    (car args))
       ((eq head 'function) (nelisp--eval-function (car args) env))
       ((eq head 'if)       (nelisp--eval-if args env))
       ((eq head 'progn)    (nelisp--eval-body args env))
       ((eq head 'let)      (nelisp--eval-let args env))
       ((eq head 'let*)     (nelisp--eval-let* args env))
       ((eq head 'lambda)
        (nelisp--make-closure env (car args) (cdr args)))
       ((eq head 'defun)    (nelisp--eval-defun args env))
       ((eq head 'defvar)   (nelisp--eval-defvar args env))
       ((eq head 'setq)     (nelisp--eval-setq args env))
       ((eq head 'while)    (nelisp--eval-while args env))
       (t (nelisp--eval-call head args env)))))
   (t
    (signal 'nelisp-eval-error (list "cannot evaluate" form)))))

(defun nelisp--eval-body (forms env)
  "Evaluate FORMS sequentially in ENV, return the last value."
  (let ((last nil))
    (while forms
      (setq last (nelisp-eval-form (car forms) env)
            forms (cdr forms)))
    last))

(defun nelisp--eval-if (args env)
  "(if TEST THEN ELSE...) — ELSE is an implicit progn."
  (if (nelisp-eval-form (car args) env)
      (nelisp-eval-form (cadr args) env)
    (nelisp--eval-body (cddr args) env)))

(defun nelisp--eval-function (form env)
  "(function FORM) — quote a lambda as a NeLisp closure over ENV."
  (cond
   ((and (consp form) (eq (car form) 'lambda))
    (nelisp--make-closure env (cadr form) (cddr form)))
   ((symbolp form) (nelisp--function-of form))
   (t (signal 'nelisp-eval-error
              (list "cannot take function value of" form)))))

(defun nelisp--split-binding (b env)
  "Return (SYM . VAL) from a let binding spec B, evaluating in ENV.
B is either a bare symbol or a two-element list (SYM INIT)."
  (cond
   ((symbolp b) (cons b nil))
   ((and (consp b) (symbolp (car b)))
    (cons (car b) (nelisp-eval-form (cadr b) env)))
   (t (signal 'nelisp-eval-error
              (list "malformed let binding" b)))))

(defun nelisp--eval-let (args env)
  "(let ((VAR VAL) ...) BODY...) — parallel binding."
  (let ((bindings (car args))
        (body (cdr args))
        (pairs nil))
    (dolist (b bindings)
      (push (nelisp--split-binding b env) pairs))
    (nelisp--eval-body body (append (nreverse pairs) env))))

(defun nelisp--eval-let* (args env)
  "(let* ((VAR VAL) ...) BODY...) — sequential binding."
  (let ((bindings (car args))
        (body (cdr args))
        (new-env env))
    (dolist (b bindings)
      (setq new-env (cons (nelisp--split-binding b new-env) new-env)))
    (nelisp--eval-body body new-env)))

(defun nelisp--eval-defun (args env)
  "(defun NAME (PARAMS) BODY...) — install a global closure."
  (let ((name (car args))
        (params (cadr args))
        (body (cddr args)))
    (unless (symbolp name)
      (signal 'nelisp-eval-error (list "defun needs a symbol" name)))
    (puthash name (nelisp--make-closure env params body)
             nelisp--functions)
    name))

(defun nelisp--eval-defvar (args env)
  "(defvar NAME [INITVAL [DOCSTRING]]) — declare special, maybe init."
  (let ((name (car args)))
    (unless (symbolp name)
      (signal 'nelisp-eval-error (list "defvar needs a symbol" name)))
    (puthash name t nelisp--specials)
    (when (and (cdr args)
               (eq (gethash name nelisp--globals nelisp--unbound)
                   nelisp--unbound))
      (puthash name (nelisp-eval-form (cadr args) env) nelisp--globals))
    name))

(defun nelisp--eval-setq (args env)
  "(setq SYM VAL [SYM VAL ...]) — update lexical if bound, else global."
  (let ((last nil))
    (while args
      (unless (cdr args)
        (signal 'nelisp-eval-error (list "setq with odd args")))
      (let* ((sym (car args))
             (val (nelisp-eval-form (cadr args) env))
             (cell (assq sym env)))
        (unless (symbolp sym)
          (signal 'nelisp-eval-error (list "setq non-symbol" sym)))
        (if cell
            (setcdr cell val)
          (puthash sym val nelisp--globals))
        (setq last val)
        (setq args (cddr args))))
    last))

(defun nelisp--eval-while (args env)
  "(while TEST BODY...) — returns nil."
  (while (nelisp-eval-form (car args) env)
    (nelisp--eval-body (cdr args) env))
  nil)

(defun nelisp--eval-call (head args env)
  "Evaluate a function call (HEAD ARGS...) in ENV."
  (let ((fn (cond
             ((symbolp head) (nelisp--function-of head))
             ((and (consp head) (eq (car head) 'lambda))
              (nelisp--make-closure env (cadr head) (cddr head)))
             ((nelisp--closure-p head) head)
             (t (signal 'nelisp-eval-error
                        (list "not callable" head))))))
    (nelisp--apply fn
                   (mapcar (lambda (a) (nelisp-eval-form a env)) args))))

;;; Apply --------------------------------------------------------------

(defun nelisp--apply (fn args)
  "Apply FN to the already-evaluated ARGS list."
  (cond
   ((nelisp--closure-p fn)
    (nelisp--apply-closure fn args))
   ((functionp fn)
    (apply fn args))
   (t
    (signal 'nelisp-eval-error (list "not a function" fn)))))

(defun nelisp--apply-closure (fn args)
  (let* ((params (nelisp--closure-params fn))
         (body   (nelisp--closure-body fn))
         (cenv   (nelisp--closure-env fn))
         (call-env (nelisp--bind-params params args cenv)))
    (nelisp--eval-body body call-env)))

(defun nelisp--bind-params (params args env)
  "Extend ENV by binding PARAMS to ARGS; support &optional and &rest."
  (let ((state 'required)
        (done nil))
    (while (and params (not done))
      (let ((p (car params)))
        (cond
         ((eq p '&optional)
          (setq state 'optional)
          (setq params (cdr params)))
         ((eq p '&rest)
          (setq params (cdr params))
          (unless (and params (symbolp (car params)))
            (signal 'nelisp-eval-error (list "&rest without symbol")))
          (setq env (cons (cons (car params) args) env))
          (setq args nil)
          (setq params nil)
          (setq done t))
         (t
          (cond
           ((and (eq state 'required) (null args))
            (signal 'nelisp-eval-error (list "too few args")))
           (t
            (setq env (cons (cons p (car args)) env))
            (setq args (cdr args))
            (setq params (cdr params))))))))
    (when args
      (signal 'nelisp-eval-error (list "too many args")))
    env))

;;; Primitive install --------------------------------------------------

(defconst nelisp--primitive-symbols
  '(car cdr cons list null not atom consp symbolp
        eq equal
        + - * / < <= > >= =)
  "Host Elisp primitives borrowed wholesale by Phase 1 NeLisp.")

(defun nelisp--builtin-funcall (fn &rest args)
  "NeLisp-aware `funcall': accepts NeLisp closures and primitives."
  (nelisp--apply fn args))

(defun nelisp--builtin-apply (fn &rest args)
  "NeLisp-aware `apply': splices the last argument like Elisp `apply'."
  (let ((flat (cond
               ((null args) nil)
               (t (append (butlast args) (car (last args)))))))
    (nelisp--apply fn flat)))

(defun nelisp--install-primitives ()
  "Bind every primitive symbol in `nelisp--functions'.
Host Emacs functions cover pure data ops; `funcall' and `apply'
must dispatch through the NeLisp-aware wrappers so closures work."
  (dolist (sym nelisp--primitive-symbols)
    (puthash sym (symbol-function sym) nelisp--functions))
  (puthash 'funcall #'nelisp--builtin-funcall nelisp--functions)
  (puthash 'apply   #'nelisp--builtin-apply   nelisp--functions))

;;; Public API ---------------------------------------------------------

;;;###autoload
(defun nelisp-eval (form)
  "Evaluate FORM (as returned by `nelisp-read') and return its value."
  (nelisp-eval-form form nil))

;;;###autoload
(defun nelisp-eval-string (str)
  "Read STR as one sexp and evaluate it."
  (nelisp-eval (nelisp-read str)))

(defun nelisp--reset ()
  "Clear all global NeLisp state and reinstall primitives.
Intended for test hygiene; callers should expect to re-run every
`defun' / `defvar' from scratch afterwards."
  (clrhash nelisp--functions)
  (clrhash nelisp--globals)
  (clrhash nelisp--specials)
  (nelisp--install-primitives))

(nelisp--install-primitives)

(provide 'nelisp-eval)

;;; nelisp-eval.el ends here
