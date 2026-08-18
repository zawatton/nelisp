;;; nelisp-pcase-test.el --- isolated host tests for nelisp-pcase  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host-side regression tests for the local `nelisp-pcase' implementation.
;; Each test loads the implementation in isolation and restores the host
;; bindings afterward so the global test session stays clean.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst nelisp-pcase-test--this-file
  (or load-file-name buffer-file-name)
  "Absolute path to this test file.")

(defconst nelisp-pcase-test--impl-file
  (expand-file-name "../lisp/nelisp-pcase.el"
                    (file-name-directory nelisp-pcase-test--this-file))
  "Absolute path to the canonical nelisp-pcase implementation.")

(defconst nelisp-pcase-test--symbols
  '(pcase
    nelisp-pcase--outer-bindings
    nelisp-pcase--wildcard-p
    nelisp-pcase--test
    nelisp-pcase--and
    nelisp-pcase--or
    nelisp-pcase--cons
    nelisp-pcase--backquote)
  "Symbols whose definitions/values are swapped in by the implementation.")

(defmacro nelisp-pcase-test--with-implementation (&rest body)
  "Load the implementation, run BODY, then restore host bindings."
  (declare (indent 0))
  `(let ((saved-fns (mapcar (lambda (sym)
                              (cons sym (and (fboundp sym)
                                             (symbol-function sym))))
                            nelisp-pcase-test--symbols))
         (saved-outer-bindings-boundp (boundp 'nelisp-pcase--outer-bindings))
         (saved-outer-bindings (and (boundp 'nelisp-pcase--outer-bindings)
                                    (symbol-value 'nelisp-pcase--outer-bindings))))
     (unwind-protect
         (progn
           (load nelisp-pcase-test--impl-file nil 'nomessage)
           ,@body)
       (dolist (entry saved-fns)
         (if (cdr entry)
             (fset (car entry) (cdr entry))
           (fmakunbound (car entry))))
       (if saved-outer-bindings-boundp
           (set 'nelisp-pcase--outer-bindings saved-outer-bindings)
         (makunbound 'nelisp-pcase--outer-bindings)))))

(defun nelisp-pcase-test--with-temp-fdefinition (symbol value thunk)
  "Temporarily bind SYMBOL to VALUE, run THUNK, then restore the old binding."
  (let ((orig (and (fboundp symbol) (symbol-function symbol))))
    (unwind-protect
        (progn
          (fset symbol value)
          (funcall thunk))
      (if orig
          (fset symbol orig)
        (fmakunbound symbol)))))

(defun nelisp-pcase-test--eval-exact-nested-macroexp-form ()
  "Evaluate the exact nested macroexp shape in NeLisp pattern form."
  (let* ((pattern
          (list 'backquote
                (cons (list 'comma
                            (list 'and 'fun
                                  (list 'or (list 'quote 'let)
                                        (list 'quote 'let*))))
                      (list 'comma
                            (list 'or
                                  (list 'backquote
                                        (cons (list 'comma 'bindings)
                                              (list 'comma 'body)))
                                  'pcase--dontcare)))))
         (form
          (list 'pcase
                (list 'quote (list 'let (list (list 'x 1)) (list 'foo 'x)))
                (list pattern (list 'list 'fun 'bindings 'body))
                (list '_ ':bad))))
    (eval form)))

(ert-deftest nelisp-pcase-backquote-or-falls-back-to-dontcare ()
  "`or' between a backquote cons pattern and `pcase--dontcare' falls back cleanly."
  (nelisp-pcase-test--with-implementation
    (should
     (equal
      (eval
       '(list
         (pcase '(a . b)
           ((or (cons x _) pcase--dontcare) x)
           (_ :bad))
         (pcase nil
           ((or (cons x _) pcase--dontcare) x)
           (_ :bad))
         (pcase 'atom
           ((or (cons x _) pcase--dontcare) x)
           (_ :bad))))
      '(a nil nil)))))

(ert-deftest nelisp-pcase-or-first-matching-alternative-wins ()
  "When two `or' alternatives both match, the first binding wins."
  (nelisp-pcase-test--with-implementation
    (should
     (equal
      (eval
       '(pcase '(1 . 2)
          ((or (cons x _) (cons y x))
           x)
          (_ :bad)))
      1))))

(ert-deftest nelisp-pcase-pred-side-effect-runs-once ()
  "`pred' should evaluate its predicate exactly once."
  (nelisp-pcase-test--with-implementation
    (let ((calls 0))
      (nelisp-pcase-test--with-temp-fdefinition
       'nelisp-pcase-test--counting-pred
       (lambda (_value)
         (setq calls (1+ calls))
         t)
       (lambda ()
         (should
          (equal
           (eval
            '(pcase 'value
               ((pred nelisp-pcase-test--counting-pred) 'ok)
               (_ 'bad)))
           'ok))
         (should (= calls 1)))))))

(ert-deftest nelisp-pcase-exact-nested-macroexp-pattern ()
  "Exact nested macroexp.el shape should bind FUN, BINDINGS, and BODY."
  (nelisp-pcase-test--with-implementation
    (should
     (equal
      (nelisp-pcase-test--eval-exact-nested-macroexp-form)
      '(let ((x 1)) ((foo x)))))))

;; nelisp-pcase-test.el ends here
