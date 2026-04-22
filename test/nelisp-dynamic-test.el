;;; nelisp-dynamic-test.el --- ERT tests for Week 17-20 dynamic binding  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 1 Week 17-20: let / let* use dynamic binding for symbols
;; declared special via defvar / defconst.  Verifies shadow, restore,
;; unwind on error / throw, closures sample the live global, and
;; mixed dynamic + lexical bindings in the same `let'.

;;; Code:

(require 'ert)
(require 'nelisp-eval)

;;; Shadow / restore --------------------------------------------------

(ert-deftest nelisp-dyn-let-shadows-global ()
  (nelisp--reset)
  (nelisp-eval '(defvar *x* :outer))
  (should (eq (nelisp-eval '(let ((*x* :inner)) *x*)) :inner))
  (should (eq (nelisp-eval '*x*) :outer)))

(ert-deftest nelisp-dyn-let-nested-stack ()
  (nelisp--reset)
  (nelisp-eval '(defvar *x* 0))
  (nelisp-eval '(defvar *peek* nil))
  (nelisp-eval '(let ((*x* 1))
                  (let ((*x* 2))
                    (setq *peek* *x*))))
  (should (= (nelisp-eval '*peek*) 2))
  (should (= (nelisp-eval '*x*) 0)))

(ert-deftest nelisp-dyn-let-restores-on-error ()
  (nelisp--reset)
  (nelisp-eval '(defvar *x* 0))
  (should-error
   (nelisp-eval '(let ((*x* 99)) (no-such-function)))
   :type 'nelisp-void-function)
  (should (= (nelisp-eval '*x*) 0)))

(ert-deftest nelisp-dyn-let-restores-on-throw ()
  (nelisp--reset)
  (nelisp-eval '(defvar *x* 0))
  (should (eq (nelisp-eval '(catch (quote out)
                              (let ((*x* 99))
                                (throw (quote out) :thrown))))
              :thrown))
  (should (= (nelisp-eval '*x*) 0)))

;;; Visibility across function calls ----------------------------------

(ert-deftest nelisp-dyn-visible-in-called-function ()
  "defun'd function reads the special through the dynamic stack."
  (nelisp--reset)
  (nelisp-eval '(defvar *current* "default"))
  (nelisp-eval '(defun who () *current*))
  (should (equal (nelisp-eval '(who)) "default"))
  (should (equal (nelisp-eval '(let ((*current* "alice")) (who)))
                 "alice"))
  (should (equal (nelisp-eval '(who)) "default")))

(ert-deftest nelisp-dyn-closure-sees-current-not-captured ()
  "A closure captured before a dynamic rebinding sees the rebinding
at call time; dynamic vars are not closed over."
  (nelisp--reset)
  (nelisp-eval '(defvar *v* :before))
  (nelisp-eval '(defun make-reader () (lambda () *v*)))
  (nelisp-eval '(defvar *reader* (make-reader)))
  (should (eq (nelisp-eval '(let ((*v* :during)) (funcall *reader*)))
              :during))
  (should (eq (nelisp-eval '(funcall *reader*)) :before)))

;;; setq semantics inside a dynamic let -------------------------------

(ert-deftest nelisp-dyn-setq-inside-let-mutates-shadow ()
  "setq on a dynamic var inside its let mutates the let's binding,
not the outer global."
  (nelisp--reset)
  (nelisp-eval '(defvar *d* :init))
  (should (eq (nelisp-eval '(let ((*d* :shadow))
                              (setq *d* :mutated)
                              *d*))
              :mutated))
  (should (eq (nelisp-eval '*d*) :init)))

;;; let* (sequential) -------------------------------------------------

(ert-deftest nelisp-dyn-let*-sequential-reads-prior ()
  "In `let*', the second dynamic binding can read the first."
  (nelisp--reset)
  (nelisp-eval '(defvar *a* 0))
  (nelisp-eval '(defvar *b* 0))
  (should (= (nelisp-eval '(let* ((*a* 10) (*b* (+ *a* 5)))
                             (+ *a* *b*)))
             25))
  (should (= (nelisp-eval '*a*) 0))
  (should (= (nelisp-eval '*b*) 0)))

(ert-deftest nelisp-dyn-let*-partial-unwind-on-error ()
  "If `let*' errors mid-binding, all prior dynamic binds must restore."
  (nelisp--reset)
  (nelisp-eval '(defvar *a* :a-init))
  (nelisp-eval '(defvar *b* :b-init))
  (should-error
   (nelisp-eval '(let* ((*a* :a-shadow)
                        (*b* (no-such-fn))
                        (*c* :never))
                   :body))
   :type 'nelisp-void-function)
  (should (eq (nelisp-eval '*a*) :a-init))
  (should (eq (nelisp-eval '*b*) :b-init)))

;;; Mixed dynamic + lexical -------------------------------------------

(ert-deftest nelisp-dyn-mixed-with-lexical ()
  "A single `let' can mix special and non-special bindings — specials
go dynamic, the rest go lexical."
  (nelisp--reset)
  (nelisp-eval '(defvar *dyn* 0))
  (should (= (nelisp-eval '(let ((lex 10) (*dyn* 20)) (+ lex *dyn*)))
             30))
  (should (= (nelisp-eval '*dyn*) 0)))

(ert-deftest nelisp-dyn-pure-lexical-still-lexical ()
  "Symbols that were never defvar'd stay lexical even when used with
the same name shape as a special variable."
  (nelisp--reset)
  (should (= (nelisp-eval '(let ((y 1)) (let ((y 2)) y))) 2)))

;;; defconst also marks special ---------------------------------------

(ert-deftest nelisp-dyn-defconst-is-special-too ()
  (nelisp--reset)
  (nelisp-eval '(defconst *pi* 3))
  (should (= (nelisp-eval '(let ((*pi* 4)) *pi*)) 4))
  (should (= (nelisp-eval '*pi*) 3)))

(provide 'nelisp-dynamic-test)

;;; nelisp-dynamic-test.el ends here
