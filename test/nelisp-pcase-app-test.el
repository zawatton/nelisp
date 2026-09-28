;;; nelisp-pcase-app-test.el --- app extended-form + pcase-defmacro ERT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Covers the GNU Emacs 31.1 pcase surface that genuine `map.el'/`seq.el'
;; patterns rely on and that `nelisp-pcase.el' previously did not support:
;;
;;   - `app' with an "extended" (F ARG1 .. ARGn) FUN, incl. the `_'
;;     placeholder and the append-when-absent fallback `pcase--flip' uses.
;;   - `pcase-defmacro' patterns actually being consulted by the dispatcher
;;     (registration already worked via `put'; the tester never read the
;;     `pcase-macroexpander' property back), incl. a macro pattern whose
;;     own expansion is itself another macro pattern.
;;   - End-to-end `map'/`seq'-shaped patterns and `map-let'/`seq-let'-style
;;     macros built the same way GNU's do, using local `pt-' stand-ins
;;     (this repo does not vendor map.el/seq.el; the helpers below are
;;     original, minimal reimplementations of their pcase-facing shape
;;     only, for testing the engine, not the libraries).
;;
;; Also pins the pre-existing pattern set (literal/backquote/pred/guard/
;; or/and/let/cons/cl-type/plain `app') so this change does not regress it.
;;
;; Runs under `make test-one FILE=test/nelisp-pcase-app-test.el' (host
;; Emacs interprets `nelisp-pcase.el' directly, via the explicit `load'
;; below, rather than falling back to Emacs's own built-in `pcase') and
;; loads cleanly on the standalone NeLisp binary via `standalone-compat/
;; ert.el' (`ert-deftest' / `should' / `should-not' / `should-error' /
;; `ert-run-tests-batch-and-exit' only -- no `:tags', no explanations).

;;; Code:

(require 'ert)
;; Exercise NeLisp's OWN pcase re-implementation, not host Emacs's built-in
;; one: `load' (not `require', no `provide' in this file) redefines the
;; global `pcase' macro from lisp/nelisp-pcase.el for the rest of this
;; process, which is deliberate -- see nelisp-pcase.el's "Kept in step
;; with the copy in scripts/nelisp-stdlib-prelude.el" header.
(load "nelisp-pcase")

;;; -----------------------------------------------------------------
;;; Pre-existing patterns, unchanged by this feature (regression pin)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-pcase-app-regression-literal-and-symbol ()
  (should (eq (pcase 5 (5 'five) (_ 'other)) 'five))
  (should (eq (pcase 6 (5 'five) (_ 'other)) 'other))
  (should (eq (pcase 'a ('a 'is-a) (_ 'other)) 'is-a))
  (should (equal (pcase "x" ("x" 'sx) (_ 'other)) 'sx))
  (should (equal (pcase 5 (n (list 'bound n))) '(bound 5))))

(ert-deftest nelisp-pcase-app-regression-backquote-pred-guard ()
  (should (equal (pcase '(1 2) (`(,a ,b) (list a b)) (_ 'other)) '(1 2)))
  (should (eq (pcase 5 ((pred integerp) 'int) (_ 'other)) 'int))
  (should (eq (pcase "s" ((pred integerp) 'int) (_ 'other)) 'other))
  (should (eq (pcase 5 ((and n (guard (> n 3))) 'big) (_ 'small)) 'big))
  (should (eq (pcase 2 ((and n (guard (> n 3))) 'big) (_ 'small)) 'small)))

(ert-deftest nelisp-pcase-app-regression-or-and-let-cons-cltype ()
  (should (equal (pcase 5 ((or (and (pred integerp) n) n) n)) 5))
  (should (equal (pcase 3 ((or 1 2 n) n)) 3))
  (should (equal (pcase 5 ((let n 5) n)) 5))
  (should (equal (pcase (cons 1 2) ((cons a b) (list a b))) '(1 2)))
  (should (eq (pcase 5 ((cl-type integer) 'int) (_ 'other)) 'int))
  (should (eq (pcase "s" ((cl-type integer) 'int) (_ 'other)) 'other)))

(ert-deftest nelisp-pcase-app-regression-plain-app-unaffected ()
  (should (eq (pcase 5 ((app 1+ 6) 'six) (_ 'other)) 'six))
  (should (eq (pcase 5 ((app 1+ 7) 'seven) (_ 'other)) 'other))
  (should (eq (pcase 25 ((app (lambda (v) (* v v)) 625) 'ok) (_ 'other)) 'ok)))

;;; -----------------------------------------------------------------
;;; `app' extended (F ARG1 .. ARGn) forms
;;; -----------------------------------------------------------------

(ert-deftest nelisp-pcase-app-extended-underscore-placeholder ()
  ;; `_' can appear anywhere among the extra arguments.
  (should (equal (pcase '(:a 1 :b 2) ((app (plist-get _ :a) x) x)) 1))
  (should (equal (pcase '(:a 1 :b 2) ((app (plist-get _ :z) x) x)) nil))
  (should (equal (pcase [10 20 30] ((app (aref _ 1) x) x)) 20))
  (should (equal (pcase "hello" ((app (substring _ 1 3) x) x)) "el")))

(ert-deftest nelisp-pcase-app-extended-append-when-absent ()
  ;; No `_': EXPVAL is appended as the (n+1)'th argument.
  (should (equal (pcase 5 ((app (+ 1) x) x)) 6))
  (should (equal (pcase 2 ((app (expt 3) x) x)) 9))
  ;; A function whose value-position is NOT last still errs the same way
  ;; on both engines -- append is positional, not type-aware.
  (should-error (pcase 3 ((app (nth '(a b c d)) x) x))))

(ert-deftest nelisp-pcase-app-pcase-flip ()
  (should (equal (pcase--flip cons 1 2) '(2 . 1)))
  (should (equal
           (progn
             (defun nelisp-pcase-app-test--map-elt (map key)
               (cdr (assq key map)))
             (pcase (list (cons :x 9))
               ((app (pcase--flip nelisp-pcase-app-test--map-elt :x) x) x)))
           9)))

;;; -----------------------------------------------------------------
;;; `pcase-defmacro' actually consulted by the dispatcher
;;; -----------------------------------------------------------------

(pcase-defmacro nelisp-pcase-app-test--even ()
  '(pred (lambda (v) (= 0 (% v 2)))))
;; A macro pattern whose own expansion is itself another macro pattern.
(pcase-defmacro nelisp-pcase-app-test--even-arg (n)
  `(and ,n (guard (= 0 (% ,n 2)))))
(pcase-defmacro nelisp-pcase-app-test--alias (n)
  `(nelisp-pcase-app-test--even-arg ,n))

(ert-deftest nelisp-pcase-app-defmacro-basic ()
  (should (eq (pcase 4 ((nelisp-pcase-app-test--even) 'even) (_ 'odd)) 'even))
  (should (eq (pcase 5 ((nelisp-pcase-app-test--even) 'even) (_ 'odd)) 'odd)))

(ert-deftest nelisp-pcase-app-defmacro-expands-to-macro-pattern ()
  (should (equal (pcase 8 ((nelisp-pcase-app-test--alias n) (list 'even n)) (_ 'odd))
                 '(even 8)))
  (should (eq (pcase 7 ((nelisp-pcase-app-test--alias n) (list 'even n)) (_ 'odd))
              'odd)))

(ert-deftest nelisp-pcase-app-defmacro-unknown-head-still-errors ()
  (should-error (pcase 5 ((nelisp-pcase-app-test--no-such-pattern) 'x) (_ 'y))))

;;; -----------------------------------------------------------------
;;; Genuine map.el-shaped `(map ...)' patterns, end to end
;;; -----------------------------------------------------------------
;; Local stand-ins only (this repo does not vendor map.el): `pt-map-elt'/
;; `pt-mapp' play the role of map.el's `map-elt'/`mapp', and
;; `pcase-defmacro pt-map' mirrors map.el's `map--make-pcase-bindings'
;; (emacs-major-version >= 30 branch: `_' placeholder, no `pcase--flip').

(defun nelisp-pcase-app-test--map-get (map key &optional default)
  (cond
   ((hash-table-p map) (gethash key map default))
   ((and (consp map) (consp (car map))) (let ((c (assoc key map))) (if c (cdr c) default)))
   ((listp map) (let ((v (plist-member map key))) (if v (cadr v) default)))
   (t default)))
(defun nelisp-pcase-app-test--mapp (x)
  (or (listp x) (hash-table-p x)))

(pcase-defmacro nelisp-pcase-app-test--map (&rest args)
  `(and (pred nelisp-pcase-app-test--mapp)
        ,@(mapcar
           (lambda (elt)
             (cond
              ((consp elt)
               `(app (nelisp-pcase-app-test--map-get _ ,(car elt) ,(car (cdr (cdr elt))))
                     ,(car (cdr elt))))
              ((keywordp elt)
               (let ((var (intern (substring (symbol-name elt) 1))))
                 `(app (nelisp-pcase-app-test--map-get _ ,elt) ,var)))
              (t `(app (nelisp-pcase-app-test--map-get _ ',elt) ,elt))))
           args)))

(defmacro nelisp-pcase-app-test--map-let (keys map &rest body)
  `(pcase-let ((,(if (listp keys)
                     `(nelisp-pcase-app-test--map ,@keys)
                   `(nelisp-pcase-app-test--map ,keys))
                ,map))
     ,@body))

(ert-deftest nelisp-pcase-app-map-pattern-plist-keyword ()
  (should (equal (pcase '(:a 1 :b 2) ((nelisp-pcase-app-test--map :a) a)) 1))
  (should (equal (pcase '(:a 1 :b 2) ((nelisp-pcase-app-test--map :a :b) (list a b)))
                 '(1 2))))

(ert-deftest nelisp-pcase-app-map-pattern-default-and-missing ()
  (should (equal (pcase '(:a 1) ((nelisp-pcase-app-test--map (:z z 'missing)) z))
                 'missing)))

(ert-deftest nelisp-pcase-app-map-pattern-alist ()
  (should (equal (pcase '((a . 1) (b . 2)) ((nelisp-pcase-app-test--map ('a x)) x)) 1)))

(ert-deftest nelisp-pcase-app-map-pattern-hash-table ()
  (should (equal (let ((h (make-hash-table)))
                   (puthash 'k 42 h)
                   (pcase h ((nelisp-pcase-app-test--map ('k v)) v)))
                 42)))

(ert-deftest nelisp-pcase-app-map-let ()
  ;; Symbol shorthand ('SYMBOL SYMBOL) looks up a bare-symbol key, so it
  ;; needs an alist keyed by symbols rather than a keyword plist -- the
  ;; keyword-shorthand case is covered separately below.
  (should (equal (nelisp-pcase-app-test--map-let (a b) '((a . 1) (b . 2)) (list a b))
                 '(1 2)))
  (should (equal (nelisp-pcase-app-test--map-let (:a :b) '(:a 1 :b 2) (list a b))
                 '(1 2))))

;;; -----------------------------------------------------------------
;;; Genuine seq.el-shaped `(seq ...)' patterns, end to end
;;; -----------------------------------------------------------------
;; `pcase-defmacro pt-seq' mirrors seq.el's `seq--make-pcase-bindings':
;; every element of the pattern list is used AS a pcase sub-pattern
;; directly, so nesting is just an ordinary sub-pattern (no shorthand
;; expansion at this layer -- that is `seq-let''s separate job below).

(defun nelisp-pcase-app-test--seqp (x) (or (listp x) (arrayp x)))
(defun nelisp-pcase-app-test--seq-elt-safe (sequence n)
  (ignore-errors (seq-elt sequence n)))

(defun nelisp-pcase-app-test--seq-bindings (args)
  (let ((bindings nil) (index 0) (rest-marker nil) (cur args))
    (while (and cur (not rest-marker))
      (let ((name (car cur)))
        (if (eq name '&rest)
            (progn (push `(app (seq-drop _ ,index) ,(car (cdr cur))) bindings)
                   (setq rest-marker t))
          (push `(app (nelisp-pcase-app-test--seq-elt-safe _ ,index) ,name) bindings)))
      (setq index (1+ index))
      (setq cur (cdr cur)))
    (nreverse bindings)))

(pcase-defmacro nelisp-pcase-app-test--seq (&rest patterns)
  `(and (pred nelisp-pcase-app-test--seqp)
        ,@(nelisp-pcase-app-test--seq-bindings patterns)))

(defun nelisp-pcase-app-test--seq-patterns (args)
  (cons 'nelisp-pcase-app-test--seq
        (mapcar (lambda (elt)
                  (if (nelisp-pcase-app-test--seqp elt)
                      (nelisp-pcase-app-test--seq-patterns elt)
                    elt))
                args)))

(defmacro nelisp-pcase-app-test--seq-let (args sequence &rest body)
  `(pcase-let ((,(nelisp-pcase-app-test--seq-patterns args) ,sequence)) ,@body))

(ert-deftest nelisp-pcase-app-seq-pattern-list-vector-string ()
  (should (equal (pcase '(1 2 3) ((nelisp-pcase-app-test--seq a b c) (list a b c)))
                 '(1 2 3)))
  (should (equal (pcase [1 2 3] ((nelisp-pcase-app-test--seq a b c) (list a b c)))
                 '(1 2 3)))
  (should (equal (pcase "abc" ((nelisp-pcase-app-test--seq a b c) (list a b c)))
                 '(97 98 99))))

(ert-deftest nelisp-pcase-app-seq-pattern-rest ()
  (should (equal (pcase '(1 2 3 4)
                   ((nelisp-pcase-app-test--seq a &rest rest) (list a rest)))
                 '(1 (2 3 4)))))

(ert-deftest nelisp-pcase-app-seq-pattern-nested ()
  (should (equal (pcase '(1 (2 3) 4)
                   ((nelisp-pcase-app-test--seq a (nelisp-pcase-app-test--seq b c) d)
                    (list a b c d)))
                 '(1 2 3 4))))

(ert-deftest nelisp-pcase-app-seq-let ()
  (should (equal (nelisp-pcase-app-test--seq-let (a b &rest rest) '(1 2 3 4)
                   (list a b rest))
                 '(1 2 (3 4)))))

;;; nelisp-pcase-app-test.el ends here
