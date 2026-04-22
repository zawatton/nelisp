;;; nelisp-phase2-stdlib-test.el --- ERT tests for Phase 2 evaluator stdlib  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 2 evaluator stdlib: hash tables, `define-error', list
;; mutators, symbol utilities, and the NeLisp-side core macros
;; (`dolist', `dotimes', `push', `pop', `defsubst',
;; `declare-function').  The anchor is running the kinds of forms
;; `nelisp-eval.el' itself uses, so that a future self-host pass can
;; actually evaluate this file.

;;; Code:

(require 'ert)
(require 'nelisp)

;;; Hash tables ------------------------------------------------------

(ert-deftest nelisp-phase2-hash-table-basic ()
  (nelisp--reset)
  (let ((h (nelisp-eval '(make-hash-table :test 'eq))))
    (should (hash-table-p h))
    (should (= (hash-table-count h) 0))))

(ert-deftest nelisp-phase2-hash-table-put-get ()
  (nelisp--reset)
  (nelisp-eval '(defvar *h* (make-hash-table :test 'eq)))
  (nelisp-eval '(puthash 'a 1 *h*))
  (nelisp-eval '(puthash 'b 2 *h*))
  (should (= (nelisp-eval '(gethash 'a *h*)) 1))
  (should (= (nelisp-eval '(gethash 'b *h*)) 2))
  (should (eq (nelisp-eval '(gethash 'missing *h*)) nil))
  (should (= (nelisp-eval '(hash-table-count *h*)) 2)))

(ert-deftest nelisp-phase2-hash-table-remove-clear ()
  (nelisp--reset)
  (nelisp-eval '(defvar *h* (make-hash-table :test 'eq)))
  (nelisp-eval '(puthash 'a 1 *h*))
  (nelisp-eval '(puthash 'b 2 *h*))
  (nelisp-eval '(remhash 'a *h*))
  (should (eq (nelisp-eval '(gethash 'a *h*)) nil))
  (should (= (nelisp-eval '(hash-table-count *h*)) 1))
  (nelisp-eval '(clrhash *h*))
  (should (= (nelisp-eval '(hash-table-count *h*)) 0)))

(ert-deftest nelisp-phase2-hash-table-string-keys ()
  (nelisp--reset)
  (nelisp-eval '(defvar *h* (make-hash-table :test 'equal)))
  (nelisp-eval '(puthash "foo" 1 *h*))
  (should (= (nelisp-eval '(gethash "foo" *h*)) 1)))

;;; define-error -----------------------------------------------------

(ert-deftest nelisp-phase2-define-error-creates-condition ()
  (nelisp--reset)
  (nelisp-eval '(define-error 'my-phase2-error "Phase 2 user error"))
  (should (eq (nelisp-eval
               '(condition-case err
                    (signal 'my-phase2-error '("boom"))
                  (my-phase2-error :caught)))
              :caught))
  ;; And the new condition inherits from `error' so a generic
  ;; handler still catches it.
  (should (eq (nelisp-eval
               '(condition-case err
                    (signal 'my-phase2-error '("boom"))
                  (error :generic)))
              :generic)))

(ert-deftest nelisp-phase2-define-error-with-parent ()
  (nelisp--reset)
  (nelisp-eval '(define-error 'my-phase2-parent "parent"))
  (nelisp-eval '(define-error 'my-phase2-child  "child" 'my-phase2-parent))
  (should (eq (nelisp-eval
               '(condition-case err
                    (signal 'my-phase2-child '("x"))
                  (my-phase2-parent :caught)))
              :caught)))

;;; List mutation ----------------------------------------------------

(ert-deftest nelisp-phase2-nreverse ()
  (should (equal (nelisp-eval '(nreverse (list 1 2 3))) '(3 2 1))))

(ert-deftest nelisp-phase2-setcar-setcdr ()
  (nelisp--reset)
  (nelisp-eval '(defvar *p* (cons 1 2)))
  (nelisp-eval '(setcar *p* 99))
  (should (= (nelisp-eval '(car *p*)) 99))
  (nelisp-eval '(setcdr *p* 100))
  (should (= (nelisp-eval '(cdr *p*)) 100)))

;;; Symbol utilities -------------------------------------------------

(ert-deftest nelisp-phase2-gensym-unique ()
  "Two calls to `gensym' return distinct symbols."
  (nelisp--reset)
  (nelisp-eval '(defvar *g1* (gensym "probe-")))
  (nelisp-eval '(defvar *g2* (gensym "probe-")))
  (should (symbolp (nelisp-eval '*g1*)))
  (should (symbolp (nelisp-eval '*g2*)))
  (should-not (eq (nelisp-eval '*g1*) (nelisp-eval '*g2*))))

(ert-deftest nelisp-phase2-keywordp ()
  (should (eq (nelisp-eval '(keywordp :foo)) t))
  (should (eq (nelisp-eval '(keywordp 'foo)) nil)))

(ert-deftest nelisp-phase2-identity-ignore ()
  (should (= (nelisp-eval '(identity 42)) 42))
  (should (eq (nelisp-eval '(ignore 1 2 3)) nil)))

;;; Core macros: defsubst / declare-function ---------------------------

(ert-deftest nelisp-phase2-defsubst ()
  "`defsubst' expands to `defun' and defines a callable function."
  (nelisp--reset)
  (nelisp-eval '(defsubst twice (x) (* 2 x)))
  (should (= (nelisp-eval '(twice 21)) 42)))

(ert-deftest nelisp-phase2-declare-function-is-noop ()
  (nelisp--reset)
  (should (eq (nelisp-eval '(declare-function foo "file" (x y))) nil)))

;;; Core macros: push / pop -------------------------------------------

(ert-deftest nelisp-phase2-push ()
  (nelisp--reset)
  (nelisp-eval '(defvar *stack* nil))
  (nelisp-eval '(push 1 *stack*))
  (nelisp-eval '(push 2 *stack*))
  (nelisp-eval '(push 3 *stack*))
  (should (equal (nelisp-eval '*stack*) '(3 2 1))))

(ert-deftest nelisp-phase2-pop ()
  (nelisp--reset)
  (nelisp-eval '(defvar *stack* (list 1 2 3)))
  (should (= (nelisp-eval '(pop *stack*)) 1))
  (should (equal (nelisp-eval '*stack*) '(2 3))))

;;; Core macros: dolist -----------------------------------------------

(ert-deftest nelisp-phase2-dolist-basic ()
  (nelisp--reset)
  (nelisp-eval '(defvar *acc* 0))
  (nelisp-eval '(dolist (x (list 1 2 3 4))
                  (setq *acc* (+ *acc* x))))
  (should (= (nelisp-eval '*acc*) 10)))

(ert-deftest nelisp-phase2-dolist-result ()
  "dolist returns the optional result expression."
  (nelisp--reset)
  (nelisp-eval '(defvar *acc* 0))
  (should (eq (nelisp-eval '(dolist (x (list 1 2 3) :done)
                              (setq *acc* (+ *acc* x))))
              :done)))

(ert-deftest nelisp-phase2-dolist-nested ()
  (nelisp--reset)
  (nelisp-eval '(defvar *pairs* nil))
  (nelisp-eval '(dolist (x (list 1 2))
                  (dolist (y (list 10 20))
                    (push (cons x y) *pairs*))))
  (should (equal (nelisp-eval '*pairs*)
                 '((2 . 20) (2 . 10) (1 . 20) (1 . 10)))))

(ert-deftest nelisp-phase2-dolist-nil-list ()
  (nelisp--reset)
  (nelisp-eval '(defvar *touched* nil))
  (nelisp-eval '(dolist (x nil) (setq *touched* t)))
  (should (eq (nelisp-eval '*touched*) nil)))

;;; Core macros: dotimes ---------------------------------------------

(ert-deftest nelisp-phase2-dotimes-basic ()
  (nelisp--reset)
  (nelisp-eval '(defvar *sum* 0))
  (nelisp-eval '(dotimes (i 5) (setq *sum* (+ *sum* i))))
  ;; i ranges 0..4, sum = 0+1+2+3+4 = 10
  (should (= (nelisp-eval '*sum*) 10)))

(ert-deftest nelisp-phase2-dotimes-result ()
  (nelisp--reset)
  (should (eq (nelisp-eval '(dotimes (i 3 :done) i))
              :done)))

(ert-deftest nelisp-phase2-dotimes-zero-count ()
  (nelisp--reset)
  (nelisp-eval '(defvar *touched* nil))
  (nelisp-eval '(dotimes (i 0) (setq *touched* t)))
  (should (eq (nelisp-eval '*touched*) nil)))

(provide 'nelisp-phase2-stdlib-test)

;;; nelisp-phase2-stdlib-test.el ends here
