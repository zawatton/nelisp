;;; nelisp-macro.el --- Macro system for NeLisp  -*- lexical-binding: t; -*-

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

;; Phase 1 Week 11-12: unhygienic macros on top of the evaluator.
;; Provides:
;;   - `defmacro NAME (PARAMS) BODY...' — installs an expander closure
;;     in `nelisp--macros' (dispatched from `nelisp-eval-form')
;;   - `nelisp-macroexpand-1'  — expand the outermost macro call once
;;   - `nelisp-macroexpand'    — expand the outermost macro call until
;;                                the head is no longer a macro
;;   - `nelisp-macroexpand-all' — expand every macro call reachable
;;                                inside FORM, one pass
;;
;; Semantics match Elisp's unhygienic model (no gensym, plain symbol
;; substitution).  Macro bodies run through `nelisp--apply' so they
;; can construct result forms using the evaluator's own `list' / `cons'
;; primitives.  Backquote (`,@) is not yet handled by the Week 3-4
;; reader, so macros must build forms with explicit `list' / `cons'
;; until Phase 2 ships backquote.
;;
;; Macros cannot shadow special forms: the evaluator dispatches on
;; built-in heads (quote, if, let, cond, …) before consulting the macro
;; table, so `(defmacro if …)' will register but never be invoked.
;;
;; Deferred to Phase 2+:
;;   - Hygiene / gensym / uninterned symbols
;;   - `cl-defmacro' destructuring and `&key'
;;   - `macroexpand-all-environment'
;;   - Compiler macros (`define-compiler-macro')

;;; Code:

(require 'nelisp-eval)

;;; Macro expansion primitives ----------------------------------------

(defun nelisp--eval-defmacro (args env)
  "Handle (defmacro NAME (PARAMS) BODY...) in ENV.
Install a macro closure in `nelisp--macros'.  Signal
`nelisp-eval-error' if NAME already names a regular function.  Return
NAME."
  (let ((name (car args))
        (params (cadr args))
        (body (cddr args)))
    (unless (symbolp name)
      (signal 'nelisp-eval-error (list "defmacro needs a symbol" name)))
    (unless (eq (gethash name nelisp--functions nelisp--unbound)
                nelisp--unbound)
      (signal 'nelisp-eval-error
              (list "name is already bound as a function" name)))
    (puthash name (nelisp--make-closure env params body)
             nelisp--macros)
    name))

(defun nelisp--macro-lookup (head)
  "Return the macro expander bound to HEAD, or nil when none."
  (and (symbolp head)
       (let ((m (gethash head nelisp--macros nelisp--unbound)))
         (and (not (eq m nelisp--unbound)) m))))

;;;###autoload
(defun nelisp-macroexpand-1 (form)
  "Expand the outermost macro call in FORM once.
Return FORM unchanged if its car is not a registered macro."
  (if (consp form)
      (let ((expander (nelisp--macro-lookup (car form))))
        (if expander
            (nelisp--apply expander (cdr form))
          form))
    form))

;;;###autoload
(defun nelisp-macroexpand (form)
  "Expand the outermost macro call in FORM as many times as needed.
Return the form once its car is no longer a macro."
  (let ((cur form)
        (next nil))
    (while (and (consp cur)
                (setq next (nelisp--macro-lookup (car cur))))
      (setq cur (nelisp--apply next (cdr cur))))
    cur))

;;; macroexpand-all walker dispatch ------------------------------------

(defvar nelisp--macroexpand-walkers nil
  "Alist (HEAD-SYMBOL . FN) used by `nelisp-macroexpand-all'.
Each FN receives the whole form (after top-level macro expansion
already settled) and returns its expanded form.  Heads without an
entry fall back to `nelisp--macroexpand-all-args' (recur into every
cdr position).  Later phases extend the table in place.")

(defun nelisp--macroexpand-map (forms)
  "Return FORMS with `nelisp-macroexpand-all' applied to each element."
  (mapcar #'nelisp-macroexpand-all forms))

(defun nelisp--macroexpand-all-args (form)
  "Default walker: preserve the car and recur into every cdr position."
  (cons (car form) (nelisp--macroexpand-map (cdr form))))

(defun nelisp--macroexpand-walk-quote (form)
  "Walker for `quote' — do not recur into the quoted datum."
  form)

(defun nelisp--macroexpand-walk-function (form)
  "Walker for `function' — recur into body only if arg is a lambda."
  (let ((arg (cadr form)))
    (if (and (consp arg) (eq (car arg) 'lambda))
        (list 'function
              (cons 'lambda
                    (cons (cadr arg)
                          (nelisp--macroexpand-map (cddr arg)))))
      form)))

(defun nelisp--macroexpand-walk-lambda (form)
  "Walker for `lambda' — params literal, recur into body."
  (cons 'lambda
        (cons (cadr form)
              (nelisp--macroexpand-map (cddr form)))))

(defun nelisp--macroexpand-walk-let (form)
  "Walker for `let' / `let*' — recur into init forms and body."
  (let* ((head (car form))
         (bindings (cadr form))
         (body (cddr form))
         (new-bindings
          (mapcar (lambda (b)
                    (cond
                     ((symbolp b) b)
                     ((and (consp b) (consp (cdr b)))
                      (list (car b) (nelisp-macroexpand-all (cadr b))))
                     (t b)))
                  bindings)))
    (cons head (cons new-bindings (nelisp--macroexpand-map body)))))

(defun nelisp--macroexpand-walk-defun (form)
  "Walker for `defun' / `defmacro' — name+params literal, recur body."
  (let ((head (car form))
        (name (cadr form))
        (params (caddr form))
        (body (cdddr form)))
    (cons head (cons name (cons params (nelisp--macroexpand-map body))))))

(defun nelisp--macroexpand-walk-defvar (form)
  "Walker for `defvar' — recur into the init form when present."
  (let ((head (car form))
        (rest (cdr form)))
    (cond
     ((null rest) form)
     ((null (cdr rest)) (list head (car rest)))
     (t (append (list head (car rest)
                      (nelisp-macroexpand-all (cadr rest)))
                (cddr rest))))))

(defun nelisp--macroexpand-walk-defconst (form)
  "Walker for `defconst' — recur into the value form."
  (let ((head (car form))
        (name (cadr form))
        (rest (cddr form)))
    (cond
     ((null rest) form)
     (t (append (list head name (nelisp-macroexpand-all (car rest)))
                (cdr rest))))))

(defun nelisp--macroexpand-walk-setq (form)
  "Walker for `setq' — every even-position element is a value form."
  (let ((rest (cdr form))
        (out nil))
    (while rest
      (push (car rest) out)
      (setq rest (cdr rest))
      (when rest
        (push (nelisp-macroexpand-all (car rest)) out)
        (setq rest (cdr rest))))
    (cons 'setq (nreverse out))))

(defun nelisp--macroexpand-walk-cond (form)
  "Walker for `cond' — recur into every element of every clause."
  (cons 'cond
        (mapcar (lambda (clause)
                  (if (consp clause)
                      (nelisp--macroexpand-map clause)
                    clause))
                (cdr form))))

(defun nelisp--macroexpand-walk-condition-case (form)
  "Walker for `condition-case' — VAR literal, recur protected + handlers."
  (let ((head (car form))
        (var (cadr form))
        (protected (caddr form))
        (handlers (cdddr form)))
    (cons head
          (cons var
                (cons (nelisp-macroexpand-all protected)
                      (mapcar (lambda (h)
                                (if (consp h)
                                    (cons (car h)
                                          (nelisp--macroexpand-map (cdr h)))
                                  h))
                              handlers))))))

(setq nelisp--macroexpand-walkers
      (list (cons 'quote          #'nelisp--macroexpand-walk-quote)
            (cons 'function       #'nelisp--macroexpand-walk-function)
            (cons 'lambda         #'nelisp--macroexpand-walk-lambda)
            (cons 'let            #'nelisp--macroexpand-walk-let)
            (cons 'let*           #'nelisp--macroexpand-walk-let)
            (cons 'defun          #'nelisp--macroexpand-walk-defun)
            (cons 'defmacro       #'nelisp--macroexpand-walk-defun)
            (cons 'defvar         #'nelisp--macroexpand-walk-defvar)
            (cons 'defconst       #'nelisp--macroexpand-walk-defconst)
            (cons 'setq           #'nelisp--macroexpand-walk-setq)
            (cons 'cond           #'nelisp--macroexpand-walk-cond)
            (cons 'condition-case #'nelisp--macroexpand-walk-condition-case)))

;;;###autoload
(defun nelisp-macroexpand-all (form)
  "Expand every macro call reachable from FORM recursively.
Walks into bodies of let, let*, lambda, defun, defmacro, if, progn,
while, setq, defvar, defconst, cond, and, or, when, unless, catch,
throw, unwind-protect, condition-case, and ordinary function call
forms.  Atoms and `quote' subforms pass through unchanged."
  (cond
   ((not (consp form)) form)
   (t
    (let ((expanded (nelisp-macroexpand form)))
      (cond
       ((not (consp expanded)) expanded)
       (t
        (let* ((head (car expanded))
               (walker (cdr (assq head nelisp--macroexpand-walkers))))
          (if walker
              (funcall walker expanded)
            (nelisp--macroexpand-all-args expanded)))))))))

(provide 'nelisp-macro)

;;; nelisp-macro.el ends here
