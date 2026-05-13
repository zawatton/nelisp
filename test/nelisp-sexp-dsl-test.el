;;; nelisp-sexp-dsl-test.el --- ERT tests for Doc 95 §95.a-d Sexp DSL  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 95 §95.a acceptance: smoke-test the 13 constructors / tag /
;; predicates / per-variant accessors / structural equality.  No byte-
;; layout test (= deferred to §95.c).

;;; Code:

(require 'ert)
(add-to-list 'load-path
             (expand-file-name
              "../lisp"
              (file-name-directory
               (or load-file-name buffer-file-name))))
(require 'nelisp-sexp-dsl)

;;; --- constructor smoke (13 tests) ---------------------------------

(ert-deftest nelisp-sexp-dsl-make-nil ()
  (let ((s (nelisp-sexp-make-nil)))
    (should (nelisp-sexp-p s))
    (should (eq (nelisp-sexp-tag s) nil))))

(ert-deftest nelisp-sexp-dsl-make-t ()
  (let ((s (nelisp-sexp-make-t)))
    (should (nelisp-sexp-p s))
    (should (eq (nelisp-sexp-tag s) t))))

(ert-deftest nelisp-sexp-dsl-make-int ()
  (let ((s (nelisp-sexp-make-int 42)))
    (should (eq (nelisp-sexp-tag s) 'int))
    (should (= (nelisp-sexp-int-value s) 42)))
  (should-error (nelisp-sexp-make-int 1.0)
                :type 'wrong-type-argument)
  (should-error (nelisp-sexp-make-int "x")
                :type 'wrong-type-argument))

(ert-deftest nelisp-sexp-dsl-make-float ()
  (let ((s (nelisp-sexp-make-float 3.14)))
    (should (eq (nelisp-sexp-tag s) 'float))
    (should (= (nelisp-sexp-float-value s) 3.14)))
  (should-error (nelisp-sexp-make-float 1)
                :type 'wrong-type-argument))

(ert-deftest nelisp-sexp-dsl-make-str ()
  (let ((s (nelisp-sexp-make-str "hello")))
    (should (eq (nelisp-sexp-tag s) 'str))
    (should (equal (nelisp-sexp-str-value s) "hello")))
  (should-error (nelisp-sexp-make-str 0)
                :type 'wrong-type-argument))

(ert-deftest nelisp-sexp-dsl-make-mut-str ()
  (let ((s (nelisp-sexp-make-mut-str "world")))
    (should (eq (nelisp-sexp-tag s) 'mut-str))
    (should (equal (nelisp-sexp-mut-str-value s) "world"))))

(ert-deftest nelisp-sexp-dsl-make-symbol ()
  (let ((s1 (nelisp-sexp-make-symbol "foo"))
        (s2 (nelisp-sexp-make-symbol 'foo)))
    (should (eq (nelisp-sexp-tag s1) 'symbol))
    (should (equal (nelisp-sexp-symbol-name s1) "foo"))
    (should (equal (nelisp-sexp-symbol-name s2) "foo"))
    (should (nelisp-sexp-eq s1 s2))))

(ert-deftest nelisp-sexp-dsl-make-cons ()
  (let* ((a (nelisp-sexp-make-int 1))
         (b (nelisp-sexp-make-int 2))
         (c (nelisp-sexp-make-cons a b)))
    (should (eq (nelisp-sexp-tag c) 'cons))
    (should (nelisp-sexp-eq (nelisp-sexp-cons-car c) a))
    (should (nelisp-sexp-eq (nelisp-sexp-cons-cdr c) b))))

(ert-deftest nelisp-sexp-dsl-make-vector ()
  (let* ((elts (list (nelisp-sexp-make-int 10)
                     (nelisp-sexp-make-int 20)
                     (nelisp-sexp-make-int 30)))
         (v (nelisp-sexp-make-vector elts)))
    (should (eq (nelisp-sexp-tag v) 'vector))
    (should (= (length (nelisp-sexp-vector-elts v)) 3))
    (should (nelisp-sexp-eq (car (nelisp-sexp-vector-elts v))
                            (nelisp-sexp-make-int 10)))))

(ert-deftest nelisp-sexp-dsl-make-char-table ()
  (let* ((default (nelisp-sexp-make-nil))
         (extras (list (nelisp-sexp-make-int 7)))
         (ct (nelisp-sexp-make-char-table default extras)))
    (should (eq (nelisp-sexp-tag ct) 'char-table))
    (should (nelisp-sexp-eq (plist-get ct :default) default))
    (should (= (length (plist-get ct :extras)) 1))))

(ert-deftest nelisp-sexp-dsl-make-bool-vector ()
  (let ((bv (nelisp-sexp-make-bool-vector 8 t)))
    (should (eq (nelisp-sexp-tag bv) 'bool-vector))
    (should (= (plist-get bv :len) 8))
    (should (eq (plist-get bv :init) t)))
  (should-error (nelisp-sexp-make-bool-vector -1 nil)
                :type 'wrong-type-argument))

(ert-deftest nelisp-sexp-dsl-make-cell ()
  (let* ((inner (nelisp-sexp-make-int 99))
         (c (nelisp-sexp-make-cell inner)))
    (should (eq (nelisp-sexp-tag c) 'cell))
    (should (nelisp-sexp-eq (plist-get c :inner) inner))))

(ert-deftest nelisp-sexp-dsl-make-record ()
  (let* ((slots (list (nelisp-sexp-make-int 1)
                      (nelisp-sexp-make-symbol "x")))
         (r (nelisp-sexp-make-record 'point slots)))
    (should (eq (nelisp-sexp-tag r) 'record))
    (should (eq (nelisp-sexp-record-type-sym r) 'point))
    (should (= (length (nelisp-sexp-record-slots r)) 2)))
  (should-error (nelisp-sexp-make-record "not-symbol" nil)
                :type 'wrong-type-argument))

;;; --- predicate coverage (13 tests) --------------------------------

(ert-deftest nelisp-sexp-dsl-predicate-nil ()
  (should (nelisp-sexp-nil-p (nelisp-sexp-make-nil)))
  (should-not (nelisp-sexp-nil-p (nelisp-sexp-make-t)))
  (should-not (nelisp-sexp-nil-p (nelisp-sexp-make-int 0))))

(ert-deftest nelisp-sexp-dsl-predicate-t ()
  (should (nelisp-sexp-t-p (nelisp-sexp-make-t)))
  (should-not (nelisp-sexp-t-p (nelisp-sexp-make-nil))))

(ert-deftest nelisp-sexp-dsl-predicate-int ()
  (should (nelisp-sexp-int-p (nelisp-sexp-make-int 7)))
  (should-not (nelisp-sexp-int-p (nelisp-sexp-make-float 7.0))))

(ert-deftest nelisp-sexp-dsl-predicate-float ()
  (should (nelisp-sexp-float-p (nelisp-sexp-make-float 1.0)))
  (should-not (nelisp-sexp-float-p (nelisp-sexp-make-int 1))))

(ert-deftest nelisp-sexp-dsl-predicate-str ()
  (should (nelisp-sexp-str-p (nelisp-sexp-make-str "a")))
  (should-not (nelisp-sexp-str-p (nelisp-sexp-make-mut-str "a"))))

(ert-deftest nelisp-sexp-dsl-predicate-mut-str ()
  (should (nelisp-sexp-mut-str-p (nelisp-sexp-make-mut-str "b")))
  (should-not (nelisp-sexp-mut-str-p (nelisp-sexp-make-str "b"))))

(ert-deftest nelisp-sexp-dsl-predicate-symbol ()
  (should (nelisp-sexp-symbol-p (nelisp-sexp-make-symbol "s")))
  (should-not (nelisp-sexp-symbol-p (nelisp-sexp-make-str "s"))))

(ert-deftest nelisp-sexp-dsl-predicate-cons ()
  (should (nelisp-sexp-cons-p
           (nelisp-sexp-make-cons (nelisp-sexp-make-nil)
                                  (nelisp-sexp-make-nil))))
  (should-not (nelisp-sexp-cons-p (nelisp-sexp-make-vector nil))))

(ert-deftest nelisp-sexp-dsl-predicate-vector ()
  (should (nelisp-sexp-vector-p (nelisp-sexp-make-vector nil)))
  (should-not (nelisp-sexp-vector-p
               (nelisp-sexp-make-cons (nelisp-sexp-make-nil)
                                      (nelisp-sexp-make-nil)))))

(ert-deftest nelisp-sexp-dsl-predicate-char-table ()
  (should (nelisp-sexp-char-table-p
           (nelisp-sexp-make-char-table (nelisp-sexp-make-nil) nil)))
  (should-not (nelisp-sexp-char-table-p (nelisp-sexp-make-vector nil))))

(ert-deftest nelisp-sexp-dsl-predicate-bool-vector ()
  (should (nelisp-sexp-bool-vector-p
           (nelisp-sexp-make-bool-vector 4 nil)))
  (should-not (nelisp-sexp-bool-vector-p (nelisp-sexp-make-vector nil))))

(ert-deftest nelisp-sexp-dsl-predicate-cell ()
  (should (nelisp-sexp-cell-p
           (nelisp-sexp-make-cell (nelisp-sexp-make-nil))))
  (should-not (nelisp-sexp-cell-p (nelisp-sexp-make-nil))))

(ert-deftest nelisp-sexp-dsl-predicate-record ()
  (should (nelisp-sexp-record-p (nelisp-sexp-make-record 'pt nil)))
  (should-not (nelisp-sexp-record-p (nelisp-sexp-make-vector nil))))

;;; --- accessor type-checks -----------------------------------------

(ert-deftest nelisp-sexp-dsl-int-value-rejects-non-int ()
  (should-error (nelisp-sexp-int-value (nelisp-sexp-make-float 1.0))
                :type 'wrong-type-argument))

(ert-deftest nelisp-sexp-dsl-cons-car-rejects-non-cons ()
  (should-error (nelisp-sexp-cons-car (nelisp-sexp-make-int 1))
                :type 'wrong-type-argument))

(ert-deftest nelisp-sexp-dsl-record-slots-rejects-non-record ()
  (should-error (nelisp-sexp-record-slots (nelisp-sexp-make-vector nil))
                :type 'wrong-type-argument))

;;; --- structural equality ------------------------------------------

(ert-deftest nelisp-sexp-dsl-eq-atomic ()
  (should (nelisp-sexp-eq (nelisp-sexp-make-nil) (nelisp-sexp-make-nil)))
  (should (nelisp-sexp-eq (nelisp-sexp-make-t) (nelisp-sexp-make-t)))
  (should (nelisp-sexp-eq (nelisp-sexp-make-int 5)
                          (nelisp-sexp-make-int 5)))
  (should-not (nelisp-sexp-eq (nelisp-sexp-make-int 5)
                              (nelisp-sexp-make-int 6)))
  (should-not (nelisp-sexp-eq (nelisp-sexp-make-int 5)
                              (nelisp-sexp-make-float 5.0))))

(ert-deftest nelisp-sexp-dsl-eq-cons-recursive ()
  (let ((a (nelisp-sexp-make-cons
            (nelisp-sexp-make-int 1)
            (nelisp-sexp-make-cons
             (nelisp-sexp-make-int 2)
             (nelisp-sexp-make-nil))))
        (b (nelisp-sexp-make-cons
            (nelisp-sexp-make-int 1)
            (nelisp-sexp-make-cons
             (nelisp-sexp-make-int 2)
             (nelisp-sexp-make-nil))))
        (c (nelisp-sexp-make-cons
            (nelisp-sexp-make-int 1)
            (nelisp-sexp-make-int 2))))
    (should (nelisp-sexp-eq a b))
    (should-not (nelisp-sexp-eq a c))))

(ert-deftest nelisp-sexp-dsl-eq-vector-recursive ()
  (let ((v1 (nelisp-sexp-make-vector
             (list (nelisp-sexp-make-int 1)
                   (nelisp-sexp-make-symbol "x"))))
        (v2 (nelisp-sexp-make-vector
             (list (nelisp-sexp-make-int 1)
                   (nelisp-sexp-make-symbol "x"))))
        (v3 (nelisp-sexp-make-vector
             (list (nelisp-sexp-make-int 1)
                   (nelisp-sexp-make-symbol "y")))))
    (should (nelisp-sexp-eq v1 v2))
    (should-not (nelisp-sexp-eq v1 v3))))

(ert-deftest nelisp-sexp-dsl-eq-record-recursive ()
  (let ((r1 (nelisp-sexp-make-record
             'point (list (nelisp-sexp-make-int 3)
                          (nelisp-sexp-make-int 4))))
        (r2 (nelisp-sexp-make-record
             'point (list (nelisp-sexp-make-int 3)
                          (nelisp-sexp-make-int 4))))
        (r3 (nelisp-sexp-make-record
             'pair (list (nelisp-sexp-make-int 3)
                         (nelisp-sexp-make-int 4))))
        (r4 (nelisp-sexp-make-record
             'point (list (nelisp-sexp-make-int 3)))))
    (should (nelisp-sexp-eq r1 r2))
    (should-not (nelisp-sexp-eq r1 r3))
    (should-not (nelisp-sexp-eq r1 r4))))

(ert-deftest nelisp-sexp-dsl-eq-rejects-junk ()
  (should-not (nelisp-sexp-eq (nelisp-sexp-make-int 1) 1))
  (should-not (nelisp-sexp-eq nil (nelisp-sexp-make-nil))))

;;; --- printer (smoke) ----------------------------------------------

(ert-deftest nelisp-sexp-dsl-pp-smoke ()
  (should (stringp (nelisp-sexp-pp (nelisp-sexp-make-nil))))
  (should (stringp (nelisp-sexp-pp (nelisp-sexp-make-int 7))))
  (should (stringp (nelisp-sexp-pp
                    (nelisp-sexp-make-cons (nelisp-sexp-make-int 1)
                                           (nelisp-sexp-make-nil)))))
  (should (stringp (nelisp-sexp-pp
                    (nelisp-sexp-make-record
                     'pt (list (nelisp-sexp-make-int 1))))))
  ;; Non-Sexp input shouldn't crash.
  (should (stringp (nelisp-sexp-pp 42))))

;;; --- §95.b: mutators ----------------------------------------------

(ert-deftest nelisp-sexp-dsl-cons-set-car ()
  (let ((c (nelisp-sexp-make-cons (nelisp-sexp-make-int 1)
                                  (nelisp-sexp-make-int 2))))
    (nelisp-sexp-cons-set-car c (nelisp-sexp-make-int 99))
    (should (nelisp-sexp-eq (nelisp-sexp-cons-car c)
                            (nelisp-sexp-make-int 99))))
  (should-error
   (nelisp-sexp-cons-set-car (nelisp-sexp-make-int 1)
                             (nelisp-sexp-make-int 2))
   :type 'wrong-type-argument))

(ert-deftest nelisp-sexp-dsl-cons-set-cdr ()
  (let ((c (nelisp-sexp-make-cons (nelisp-sexp-make-int 1)
                                  (nelisp-sexp-make-int 2))))
    (nelisp-sexp-cons-set-cdr c (nelisp-sexp-make-nil))
    (should (nelisp-sexp-eq (nelisp-sexp-cons-cdr c)
                            (nelisp-sexp-make-nil)))))

(ert-deftest nelisp-sexp-dsl-vector-aset ()
  (let* ((elts (list (nelisp-sexp-make-int 10)
                     (nelisp-sexp-make-int 20)
                     (nelisp-sexp-make-int 30)))
         (v (nelisp-sexp-make-vector elts)))
    (nelisp-sexp-vector-aset v 1 (nelisp-sexp-make-int 99))
    (should (nelisp-sexp-eq
             (nth 1 (nelisp-sexp-vector-elts v))
             (nelisp-sexp-make-int 99)))
    (should-error (nelisp-sexp-vector-aset v 5 (nelisp-sexp-make-int 0))
                  :type 'args-out-of-range)
    (should-error (nelisp-sexp-vector-aset v -1 (nelisp-sexp-make-int 0))
                  :type 'args-out-of-range)))

(ert-deftest nelisp-sexp-dsl-record-set-slot ()
  (let* ((slots (list (nelisp-sexp-make-int 1)
                      (nelisp-sexp-make-int 2)))
         (r (nelisp-sexp-make-record 'pt slots)))
    (nelisp-sexp-record-set-slot r 0 (nelisp-sexp-make-int 7))
    (should (nelisp-sexp-eq
             (nth 0 (nelisp-sexp-record-slots r))
             (nelisp-sexp-make-int 7)))
    (should-error (nelisp-sexp-record-set-slot r 9 (nelisp-sexp-make-int 0))
                  :type 'args-out-of-range)))

;;; --- §95.b: length / arity ----------------------------------------

(ert-deftest nelisp-sexp-dsl-cons-length-proper ()
  (let ((lst (nelisp-sexp-cons-from-list
              (list (nelisp-sexp-make-int 1)
                    (nelisp-sexp-make-int 2)
                    (nelisp-sexp-make-int 3)))))
    (should (= 3 (nelisp-sexp-cons-length lst))))
  (should (= 0 (nelisp-sexp-cons-length (nelisp-sexp-make-nil)))))

(ert-deftest nelisp-sexp-dsl-cons-length-improper-signals ()
  (let ((improper (nelisp-sexp-make-cons
                   (nelisp-sexp-make-int 1)
                   (nelisp-sexp-make-int 2))))
    (should-error (nelisp-sexp-cons-length improper)
                  :type 'wrong-type-argument)))

(ert-deftest nelisp-sexp-dsl-vector-length ()
  (let ((v (nelisp-sexp-make-vector
            (list (nelisp-sexp-make-int 1)
                  (nelisp-sexp-make-int 2)))))
    (should (= 2 (nelisp-sexp-vector-length v))))
  (should (= 0 (nelisp-sexp-vector-length (nelisp-sexp-make-vector nil)))))

(ert-deftest nelisp-sexp-dsl-record-arity ()
  (let ((r (nelisp-sexp-make-record
            'pt (list (nelisp-sexp-make-int 1)
                      (nelisp-sexp-make-int 2)
                      (nelisp-sexp-make-int 3)))))
    (should (= 3 (nelisp-sexp-record-arity r)))))

;;; --- §95.b: iteration ---------------------------------------------

(ert-deftest nelisp-sexp-dsl-walk-counts-subsexps ()
  (let* ((tree (nelisp-sexp-make-cons
                (nelisp-sexp-make-int 1)
                (nelisp-sexp-make-cons
                 (nelisp-sexp-make-vector
                  (list (nelisp-sexp-make-int 2)
                        (nelisp-sexp-make-int 3)))
                 (nelisp-sexp-make-nil))))
         (count 0))
    (nelisp-sexp-walk tree (lambda (_) (setq count (1+ count))))
    ;; tree (cons) + 1 (int) + cons2 + vector + 2 (int) + 3 (int) + nil = 7
    (should (= count 7))))

(ert-deftest nelisp-sexp-dsl-walk-non-sexp-noop ()
  (let ((count 0))
    (nelisp-sexp-walk 42 (lambda (_) (setq count (1+ count))))
    (should (= count 0))))

(ert-deftest nelisp-sexp-dsl-map-cons ()
  (let* ((lst (nelisp-sexp-cons-from-list
               (list (nelisp-sexp-make-int 1)
                     (nelisp-sexp-make-int 2)
                     (nelisp-sexp-make-int 3))))
         (doubled (nelisp-sexp-map-cons
                   (lambda (x)
                     (nelisp-sexp-make-int
                      (* 2 (nelisp-sexp-int-value x))))
                   lst))
         (expected (nelisp-sexp-cons-from-list
                    (list (nelisp-sexp-make-int 2)
                          (nelisp-sexp-make-int 4)
                          (nelisp-sexp-make-int 6)))))
    (should (nelisp-sexp-eq doubled expected))))

(ert-deftest nelisp-sexp-dsl-map-cons-empty ()
  (let ((result (nelisp-sexp-map-cons #'identity (nelisp-sexp-make-nil))))
    (should (nelisp-sexp-nil-p result))))

(ert-deftest nelisp-sexp-dsl-foldl-sum ()
  (let ((lst (nelisp-sexp-cons-from-list
              (list (nelisp-sexp-make-int 1)
                    (nelisp-sexp-make-int 2)
                    (nelisp-sexp-make-int 3)
                    (nelisp-sexp-make-int 4)))))
    (should (= 10 (nelisp-sexp-foldl
                   (lambda (acc x) (+ acc (nelisp-sexp-int-value x)))
                   0 lst)))))

(ert-deftest nelisp-sexp-dsl-foldl-improper-signals ()
  (let ((improper (nelisp-sexp-make-cons
                   (nelisp-sexp-make-int 1)
                   (nelisp-sexp-make-int 2))))
    (should-error (nelisp-sexp-foldl #'+ 0 improper)
                  :type 'wrong-type-argument)))

;;; --- §95.b: conversion --------------------------------------------

(ert-deftest nelisp-sexp-dsl-cons-from-list-round-trip ()
  (let* ((elts (list (nelisp-sexp-make-int 1)
                     (nelisp-sexp-make-symbol "foo")
                     (nelisp-sexp-make-str "bar")))
         (chain (nelisp-sexp-cons-from-list elts))
         (back (nelisp-sexp-list-from-cons chain)))
    (should (= (length back) 3))
    (should (nelisp-sexp-eq (nth 0 back) (nth 0 elts)))
    (should (nelisp-sexp-eq (nth 1 back) (nth 1 elts)))
    (should (nelisp-sexp-eq (nth 2 back) (nth 2 elts)))))

(ert-deftest nelisp-sexp-dsl-cons-from-empty-list ()
  (should (nelisp-sexp-nil-p (nelisp-sexp-cons-from-list nil))))

(ert-deftest nelisp-sexp-dsl-list-from-cons-improper-signals ()
  (let ((improper (nelisp-sexp-make-cons
                   (nelisp-sexp-make-int 1)
                   (nelisp-sexp-make-int 2))))
    (should-error (nelisp-sexp-list-from-cons improper)
                  :type 'wrong-type-argument)))

(ert-deftest nelisp-sexp-dsl-from-elisp-atomic ()
  (should (nelisp-sexp-nil-p (nelisp-sexp-from-elisp nil)))
  (should (nelisp-sexp-t-p (nelisp-sexp-from-elisp t)))
  (should (= 7 (nelisp-sexp-int-value (nelisp-sexp-from-elisp 7))))
  (should (= 3.5 (nelisp-sexp-float-value (nelisp-sexp-from-elisp 3.5))))
  (should (equal "hi" (nelisp-sexp-str-value
                       (nelisp-sexp-from-elisp "hi"))))
  (should (equal "foo" (nelisp-sexp-symbol-name
                        (nelisp-sexp-from-elisp 'foo)))))

(ert-deftest nelisp-sexp-dsl-from-elisp-recursive ()
  (let ((s (nelisp-sexp-from-elisp (cons 1 (cons 2 nil)))))
    (should (nelisp-sexp-cons-p s))
    (should (= 1 (nelisp-sexp-int-value (nelisp-sexp-cons-car s))))
    (should (nelisp-sexp-cons-p (nelisp-sexp-cons-cdr s)))))

(ert-deftest nelisp-sexp-dsl-to-from-elisp-round-trip ()
  ;; int: lossless
  (should (eql 42 (nelisp-sexp-to-elisp
                   (nelisp-sexp-from-elisp 42))))
  ;; nested cons: lossless
  (should (equal '(1 2 3) (nelisp-sexp-to-elisp
                           (nelisp-sexp-from-elisp '(1 2 3)))))
  ;; vector: lossless
  (should (equal [1 2 3] (nelisp-sexp-to-elisp
                          (nelisp-sexp-from-elisp [1 2 3])))))

;;; --- §95.b: hash --------------------------------------------------

(ert-deftest nelisp-sexp-dsl-hash-stable-same-shape ()
  (let ((a (nelisp-sexp-make-cons
            (nelisp-sexp-make-int 1)
            (nelisp-sexp-make-int 2)))
        (b (nelisp-sexp-make-cons
            (nelisp-sexp-make-int 1)
            (nelisp-sexp-make-int 2))))
    (should (= (nelisp-sexp-hash a) (nelisp-sexp-hash b)))))

(ert-deftest nelisp-sexp-dsl-hash-different-shapes-differ ()
  (let ((a (nelisp-sexp-make-int 1))
        (b (nelisp-sexp-make-int 2))
        (c (nelisp-sexp-make-float 1.0)))
    ;; sxhash collisions on small ints are theoretically possible but
    ;; tag mixing makes this triple safe in practice.
    (should-not (= (nelisp-sexp-hash a) (nelisp-sexp-hash b)))
    (should-not (= (nelisp-sexp-hash a) (nelisp-sexp-hash c)))))

(ert-deftest nelisp-sexp-dsl-hash-usable-as-hash-key ()
  (let ((tbl (make-hash-table :test 'eql))
        (s1 (nelisp-sexp-make-symbol "alpha"))
        (s2 (nelisp-sexp-make-symbol "alpha")))
    (puthash (nelisp-sexp-hash s1) 'found tbl)
    (should (eq 'found (gethash (nelisp-sexp-hash s2) tbl)))))

;;; --- §95.b: deep copy / deep-eq -----------------------------------

(ert-deftest nelisp-sexp-dsl-copy-deep-equal-not-eq ()
  (let* ((orig (nelisp-sexp-make-cons
                (nelisp-sexp-make-int 1)
                (nelisp-sexp-make-cons
                 (nelisp-sexp-make-vector
                  (list (nelisp-sexp-make-int 2)))
                 (nelisp-sexp-make-nil))))
         (dup (nelisp-sexp-copy orig)))
    (should (nelisp-sexp-deep-eq orig dup))
    (should-not (eq orig dup))
    ;; Mutation on dup must not bleed into orig.
    (nelisp-sexp-cons-set-car dup (nelisp-sexp-make-int 99))
    (should (= 1 (nelisp-sexp-int-value (nelisp-sexp-cons-car orig))))))

(ert-deftest nelisp-sexp-dsl-copy-record ()
  (let* ((orig (nelisp-sexp-make-record
                'pt (list (nelisp-sexp-make-int 1)
                          (nelisp-sexp-make-int 2))))
         (dup (nelisp-sexp-copy orig)))
    (should (nelisp-sexp-deep-eq orig dup))
    (nelisp-sexp-record-set-slot dup 0 (nelisp-sexp-make-int 99))
    (should (= 1 (nelisp-sexp-int-value
                  (nth 0 (nelisp-sexp-record-slots orig)))))))

(ert-deftest nelisp-sexp-dsl-deep-eq-alias ()
  ;; `defalias' may produce different function objects under byte
  ;; compilation; just verify operational equivalence on a deeply
  ;; nested value.
  (let ((a (nelisp-sexp-make-cons
            (nelisp-sexp-make-vector
             (list (nelisp-sexp-make-int 1)
                   (nelisp-sexp-make-symbol "x")))
            (nelisp-sexp-make-record
             'pt (list (nelisp-sexp-make-int 2)))))
        (b (nelisp-sexp-make-cons
            (nelisp-sexp-make-vector
             (list (nelisp-sexp-make-int 1)
                   (nelisp-sexp-make-symbol "x")))
            (nelisp-sexp-make-record
             'pt (list (nelisp-sexp-make-int 2))))))
    (should (nelisp-sexp-deep-eq a b))
    (should (eq (nelisp-sexp-deep-eq a b) (nelisp-sexp-eq a b)))))

;;; --- §95.b: type coercion -----------------------------------------

(ert-deftest nelisp-sexp-dsl-as-int-positive ()
  (should (= 7 (nelisp-sexp-as-int (nelisp-sexp-make-int 7))))
  (should-error (nelisp-sexp-as-int (nelisp-sexp-make-float 7.0))
                :type 'wrong-type-argument))

(ert-deftest nelisp-sexp-dsl-as-float-positive ()
  (should (= 3.14 (nelisp-sexp-as-float (nelisp-sexp-make-float 3.14))))
  (should-error (nelisp-sexp-as-float (nelisp-sexp-make-int 3))
                :type 'wrong-type-argument))

(ert-deftest nelisp-sexp-dsl-as-str-accepts-both ()
  (should (equal "a" (nelisp-sexp-as-str (nelisp-sexp-make-str "a"))))
  (should (equal "b" (nelisp-sexp-as-str (nelisp-sexp-make-mut-str "b"))))
  (should-error (nelisp-sexp-as-str (nelisp-sexp-make-int 1))
                :type 'wrong-type-argument))

(ert-deftest nelisp-sexp-dsl-as-symbol-positive ()
  (should (equal "foo"
                 (nelisp-sexp-as-symbol (nelisp-sexp-make-symbol "foo"))))
  (should-error (nelisp-sexp-as-symbol (nelisp-sexp-make-str "foo"))
                :type 'wrong-type-argument))

(ert-deftest nelisp-sexp-dsl-as-cons-vector-record ()
  (let ((c (nelisp-sexp-make-cons (nelisp-sexp-make-nil)
                                  (nelisp-sexp-make-nil)))
        (v (nelisp-sexp-make-vector nil))
        (r (nelisp-sexp-make-record 'pt nil)))
    (should (eq c (nelisp-sexp-as-cons c)))
    (should (eq v (nelisp-sexp-as-vector v)))
    (should (eq r (nelisp-sexp-as-record r)))
    (should-error (nelisp-sexp-as-cons v) :type 'wrong-type-argument)
    (should-error (nelisp-sexp-as-vector r) :type 'wrong-type-argument)
    (should-error (nelisp-sexp-as-record c) :type 'wrong-type-argument)))

;;; --- §95.c byte-layout serializer round-trip tests ===============
;;
;; Per Doc 95 §8 §95.c acceptance: round-trip for each of 13 variants,
;; i64 corner cases (most-positive-fixnum etc.), NaN preservation, +/-
;; zero, +/- inf, subnormal float, UTF-8 multibyte, deep cons (= 100
;; elements), nested vector/cons/vector, truncated input → signal,
;; unknown tag → signal.  Equality uses `nelisp-sexp-eq' because plist
;; tail order is implementation-defined but structural identity is the
;; wire-format contract.

(defmacro nelisp-sexp-dsl-test--should-roundtrip (sexp)
  "Assert that SEXP survives `nelisp-sexp-bytes-round-trip-p'."
  `(should (nelisp-sexp-bytes-round-trip-p ,sexp)))

(ert-deftest nelisp-sexp-dsl-bytes-nil-round-trip ()
  (nelisp-sexp-dsl-test--should-roundtrip (nelisp-sexp-make-nil))
  (let ((bytes (nelisp-sexp-write-bytes (nelisp-sexp-make-nil))))
    (should (= 1 (length bytes)))
    (should (= 0 (aref bytes 0)))))

(ert-deftest nelisp-sexp-dsl-bytes-t-round-trip ()
  (nelisp-sexp-dsl-test--should-roundtrip (nelisp-sexp-make-t))
  (let ((bytes (nelisp-sexp-write-bytes (nelisp-sexp-make-t))))
    (should (= 1 (length bytes)))
    (should (= 1 (aref bytes 0)))))

(ert-deftest nelisp-sexp-dsl-bytes-int-zero ()
  (nelisp-sexp-dsl-test--should-roundtrip (nelisp-sexp-make-int 0))
  (let ((bytes (nelisp-sexp-write-bytes (nelisp-sexp-make-int 0))))
    (should (= 9 (length bytes)))
    (should (= 2 (aref bytes 0)))
    (dotimes (i 8) (should (= 0 (aref bytes (1+ i)))))))

(ert-deftest nelisp-sexp-dsl-bytes-int-one-minus-one ()
  (nelisp-sexp-dsl-test--should-roundtrip (nelisp-sexp-make-int 1))
  (nelisp-sexp-dsl-test--should-roundtrip (nelisp-sexp-make-int -1))
  (let ((b1 (nelisp-sexp-write-bytes (nelisp-sexp-make-int 1)))
        (bm1 (nelisp-sexp-write-bytes (nelisp-sexp-make-int -1))))
    (should (= 1 (aref b1 1)))
    (dotimes (i 8) (should (= #xFF (aref bm1 (1+ i)))))))

(ert-deftest nelisp-sexp-dsl-bytes-int-corners ()
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-int most-positive-fixnum))
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-int most-negative-fixnum))
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-int (1- (ash 1 63))))
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-int (- (ash 1 63)))))

(ert-deftest nelisp-sexp-dsl-bytes-int-out-of-range ()
  ;; i64 max + 1 = 2^63, just over range.
  (should-error (nelisp-sexp-write-bytes
                 (nelisp-sexp-make-int (ash 1 63)))
                :type 'args-out-of-range)
  ;; i64 min - 1.
  (should-error (nelisp-sexp-write-bytes
                 (nelisp-sexp-make-int (1- (- (ash 1 63)))))
                :type 'args-out-of-range))

(ert-deftest nelisp-sexp-dsl-bytes-float-zero ()
  (nelisp-sexp-dsl-test--should-roundtrip (nelisp-sexp-make-float 0.0))
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-float -0.0))
  (let* ((bz (nelisp-sexp-write-bytes (nelisp-sexp-make-float 0.0)))
         (back (car (nelisp-sexp-read-bytes bz 0)))
         (bnz (nelisp-sexp-write-bytes (nelisp-sexp-make-float -0.0)))
         (back-neg (car (nelisp-sexp-read-bytes bnz 0))))
    (should (= 0.0 (nelisp-sexp-float-value back)))
    (should (< (copysign 1.0 (nelisp-sexp-float-value back-neg)) 0))))

(ert-deftest nelisp-sexp-dsl-bytes-float-one ()
  (nelisp-sexp-dsl-test--should-roundtrip (nelisp-sexp-make-float 1.0))
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-float -1.0))
  (nelisp-sexp-dsl-test--should-roundtrip (nelisp-sexp-make-float 1.5))
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-float 3.141592653589793)))

(ert-deftest nelisp-sexp-dsl-bytes-float-inf ()
  (let ((inf (let ((x (expt 10.0 308))) (* x x))))
    (nelisp-sexp-dsl-test--should-roundtrip
     (nelisp-sexp-make-float inf))
    (nelisp-sexp-dsl-test--should-roundtrip
     (nelisp-sexp-make-float (- inf)))))

(ert-deftest nelisp-sexp-dsl-bytes-float-nan-preserved ()
  (let* ((nan (/ 0.0 0.0))
         (sexp (nelisp-sexp-make-float nan))
         (bytes (nelisp-sexp-write-bytes sexp))
         (back (car (nelisp-sexp-read-bytes bytes 0))))
    (should (isnan (nelisp-sexp-float-value back)))))

(ert-deftest nelisp-sexp-dsl-bytes-float-subnormal ()
  ;; Smallest positive subnormal = 5e-324 in IEEE 754.
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-float 5e-324)))

(ert-deftest nelisp-sexp-dsl-bytes-float-large ()
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-float 1e100))
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-float 1.7976931348623157e308)))

(ert-deftest nelisp-sexp-dsl-bytes-symbol-ascii ()
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-symbol "foo"))
  (let ((bytes (nelisp-sexp-write-bytes
                (nelisp-sexp-make-symbol "foo"))))
    (should (= 4 (aref bytes 0)))
    (should (= 3 (aref bytes 1)))))

(ert-deftest nelisp-sexp-dsl-bytes-symbol-empty ()
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-symbol "")))

(ert-deftest nelisp-sexp-dsl-bytes-symbol-multibyte ()
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-symbol "日本語"))
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-symbol "🎉")))

(ert-deftest nelisp-sexp-dsl-bytes-str-ascii ()
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-str "hello, world"))
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-str "")))

(ert-deftest nelisp-sexp-dsl-bytes-str-multibyte ()
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-str "日本語テスト 🎉 αβγ"))
  ;; Verify the wire bytes themselves contain UTF-8 multibyte encoding.
  (let* ((sexp (nelisp-sexp-make-str "あ"))
         (bytes (nelisp-sexp-write-bytes sexp)))
    (should (= 5 (aref bytes 0)))            ; tag = 0x05 = Str
    (should (= 3 (aref bytes 1)))            ; UTF-8 "あ" = 3 bytes
    (should (= #xE3 (aref bytes 5)))))

(ert-deftest nelisp-sexp-dsl-bytes-mut-str-round-trip ()
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-mut-str "mutable text"))
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-mut-str "日本"))
  ;; Tag byte must distinguish from Str.
  (should (= 6 (aref (nelisp-sexp-write-bytes
                      (nelisp-sexp-make-mut-str "x"))
                     0))))

(ert-deftest nelisp-sexp-dsl-bytes-cons-pair ()
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-cons (nelisp-sexp-make-int 1)
                          (nelisp-sexp-make-int 2))))

(ert-deftest nelisp-sexp-dsl-bytes-cons-improper ()
  ;; Improper tail = non-Nil/non-Cons CDR; wire format does not care.
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-cons (nelisp-sexp-make-symbol "a")
                          (nelisp-sexp-make-int 99))))

(ert-deftest nelisp-sexp-dsl-bytes-cons-100-element-list ()
  (let ((acc (nelisp-sexp-make-nil)))
    (dotimes (i 100)
      (setq acc (nelisp-sexp-make-cons (nelisp-sexp-make-int i) acc)))
    (nelisp-sexp-dsl-test--should-roundtrip acc)
    (should (= 100 (nelisp-sexp-cons-length acc)))))

(ert-deftest nelisp-sexp-dsl-bytes-vector-empty ()
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-vector nil)))

(ert-deftest nelisp-sexp-dsl-bytes-vector-mixed ()
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-vector
    (list (nelisp-sexp-make-int 7)
          (nelisp-sexp-make-str "x")
          (nelisp-sexp-make-symbol "sym")
          (nelisp-sexp-make-t)
          (nelisp-sexp-make-nil)))))

(ert-deftest nelisp-sexp-dsl-bytes-char-table-empty ()
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-char-table (nelisp-sexp-make-nil) nil)))

(ert-deftest nelisp-sexp-dsl-bytes-char-table-with-extras ()
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-char-table
    (nelisp-sexp-make-int 0)
    (list (nelisp-sexp-make-symbol "a")
          (nelisp-sexp-make-str "b")
          (nelisp-sexp-make-int 42)))))

(ert-deftest nelisp-sexp-dsl-bytes-bool-vector-zero-len ()
  ;; Len=0 has no bit storage so :init is unrecoverable from wire —
  ;; round-trip yields :init=nil regardless of input.  Documented
  ;; limitation; semantic difference between (0 . nil) and (0 . t) is
  ;; zero since there are no bits to inspect.
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-bool-vector 0 nil))
  (let* ((bytes (nelisp-sexp-write-bytes
                 (nelisp-sexp-make-bool-vector 0 t)))
         (decoded (car (nelisp-sexp-read-bytes bytes 0))))
    (should (nelisp-sexp-bool-vector-p decoded))
    (should (= 0 (plist-get decoded :len)))))

(ert-deftest nelisp-sexp-dsl-bytes-bool-vector-small ()
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-bool-vector 1 nil))
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-bool-vector 1 t))
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-bool-vector 8 t))
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-bool-vector 17 nil))
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-bool-vector 64 t)))

(ert-deftest nelisp-sexp-dsl-bytes-cell-round-trip ()
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-cell (nelisp-sexp-make-int 13)))
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-cell
    (nelisp-sexp-make-cons (nelisp-sexp-make-symbol "x")
                           (nelisp-sexp-make-nil)))))

(ert-deftest nelisp-sexp-dsl-bytes-record-round-trip ()
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-record 'point
                            (list (nelisp-sexp-make-int 3)
                                  (nelisp-sexp-make-int 5))))
  (nelisp-sexp-dsl-test--should-roundtrip
   (nelisp-sexp-make-record 'empty nil)))

(ert-deftest nelisp-sexp-dsl-bytes-nested-vec-cons-vec ()
  ;; Vector of Cons of Vector — exercises arity / length plumbing.
  (let ((deep
         (nelisp-sexp-make-vector
          (list
           (nelisp-sexp-make-cons
            (nelisp-sexp-make-vector
             (list (nelisp-sexp-make-int 1)
                   (nelisp-sexp-make-int 2)))
            (nelisp-sexp-make-vector
             (list (nelisp-sexp-make-symbol "inner-cdr"))))
           (nelisp-sexp-make-cell
            (nelisp-sexp-make-str "nested"))))))
    (nelisp-sexp-dsl-test--should-roundtrip deep)))

(ert-deftest nelisp-sexp-dsl-bytes-truncated-tag ()
  (should-error (nelisp-sexp-read-bytes "" 0)
                :type 'nelisp-sexp-truncated))

(ert-deftest nelisp-sexp-dsl-bytes-truncated-int ()
  ;; tag=Int, but only 3 i64 bytes provided.
  (should-error (nelisp-sexp-read-bytes (unibyte-string 2 0 0 0) 0)
                :type 'nelisp-sexp-truncated))

(ert-deftest nelisp-sexp-dsl-bytes-truncated-float ()
  (should-error (nelisp-sexp-read-bytes (unibyte-string 3 0 0 0) 0)
                :type 'nelisp-sexp-truncated))

(ert-deftest nelisp-sexp-dsl-bytes-truncated-utf8-len ()
  ;; tag=Symbol, 2 length bytes (incomplete u32).
  (should-error (nelisp-sexp-read-bytes (unibyte-string 4 0 0) 0)
                :type 'nelisp-sexp-truncated))

(ert-deftest nelisp-sexp-dsl-bytes-truncated-utf8-content ()
  ;; tag=Str, len=5, but only 1 byte content.
  (should-error
   (nelisp-sexp-read-bytes
    (unibyte-string 5 5 0 0 0 #x61) 0)
   :type 'nelisp-sexp-truncated))

(ert-deftest nelisp-sexp-dsl-bytes-truncated-bool-vector ()
  ;; tag=BoolVector, len=8 → needs 1 byte, none supplied.
  (should-error
   (nelisp-sexp-read-bytes (unibyte-string #x0A 8 0 0 0) 0)
   :type 'nelisp-sexp-truncated))

(ert-deftest nelisp-sexp-dsl-bytes-unknown-tag ()
  (should-error (nelisp-sexp-read-bytes (unibyte-string #xFF) 0)
                :type 'nelisp-sexp-unknown-tag)
  (should-error (nelisp-sexp-read-bytes (unibyte-string #x0D) 0)
                :type 'nelisp-sexp-unknown-tag))

(ert-deftest nelisp-sexp-dsl-bytes-equal-p ()
  (should (nelisp-sexp-bytes-equal-p
           (nelisp-sexp-make-int 42)
           (nelisp-sexp-make-int 42)))
  (should-not (nelisp-sexp-bytes-equal-p
               (nelisp-sexp-make-int 42)
               (nelisp-sexp-make-int 43))))

(ert-deftest nelisp-sexp-dsl-bytes-read-returns-new-pos ()
  ;; Sequential decode via NEW-POS = continue reading from same buffer.
  (let* ((b1 (nelisp-sexp-write-bytes (nelisp-sexp-make-int 7)))
         (b2 (nelisp-sexp-write-bytes (nelisp-sexp-make-int 8)))
         (cat (concat b1 b2))
         (first (nelisp-sexp-read-bytes cat 0))
         (second (nelisp-sexp-read-bytes cat (cdr first))))
    (should (= 7 (nelisp-sexp-int-value (car first))))
    (should (= 8 (nelisp-sexp-int-value (car second))))
    (should (= (length cat) (cdr second)))))

(ert-deftest nelisp-sexp-dsl-bytes-record-malformed-type-sym ()
  ;; tag=Record + tag=Int (= not a Symbol) → malformed signal.
  (let ((b (unibyte-string #x0C #x02 1 0 0 0 0 0 0 0 0 0 0 0)))
    (should-error (nelisp-sexp-read-bytes b 0)
                  :type 'nelisp-sexp-malformed-record)))

(ert-deftest nelisp-sexp-dsl-bytes-13-variant-coverage ()
  ;; Single test that exercises every variant via round-trip in one
  ;; pass — coverage smoke test that nothing was forgotten.
  (let ((cases
         (list (nelisp-sexp-make-nil)
               (nelisp-sexp-make-t)
               (nelisp-sexp-make-int 0)
               (nelisp-sexp-make-float 3.14)
               (nelisp-sexp-make-symbol "sym")
               (nelisp-sexp-make-str "str")
               (nelisp-sexp-make-mut-str "mut")
               (nelisp-sexp-make-cons (nelisp-sexp-make-int 1)
                                      (nelisp-sexp-make-nil))
               (nelisp-sexp-make-vector
                (list (nelisp-sexp-make-int 2)))
               (nelisp-sexp-make-char-table (nelisp-sexp-make-nil) nil)
               (nelisp-sexp-make-bool-vector 3 t)
               (nelisp-sexp-make-cell (nelisp-sexp-make-int 9))
               (nelisp-sexp-make-record 'r nil))))
    (should (= 13 (length cases)))
    (dolist (c cases)
      (should (nelisp-sexp-bytes-round-trip-p c)))))

;;; --- §95.d: Rust ABI fixed-layout JIT bridge ----------------------

(ert-deftest nelisp-sexp-dsl-jit-record-size-nil ()
  ;; Every variant emits a record of exactly 32 bytes.
  (let ((r (nelisp-sexp-jit-write-bytes (nelisp-sexp-make-nil))))
    (should (= 32 (length r)))))

(ert-deftest nelisp-sexp-dsl-jit-record-size-t ()
  (let ((r (nelisp-sexp-jit-write-bytes (nelisp-sexp-make-t))))
    (should (= 32 (length r)))))

(ert-deftest nelisp-sexp-dsl-jit-record-size-int ()
  (let ((r (nelisp-sexp-jit-write-bytes (nelisp-sexp-make-int 42))))
    (should (= 32 (length r)))))

(ert-deftest nelisp-sexp-dsl-jit-record-size-float ()
  (let ((r (nelisp-sexp-jit-write-bytes (nelisp-sexp-make-float 3.14))))
    (should (= 32 (length r)))))

(ert-deftest nelisp-sexp-dsl-jit-record-size-all-13-variants ()
  ;; Layout invariant: every variant produces a 32-byte record.
  (let* ((store (nelisp-sexp-jit-make-heap-store))
         (cases (list (nelisp-sexp-make-nil)
                      (nelisp-sexp-make-t)
                      (nelisp-sexp-make-int 1)
                      (nelisp-sexp-make-float 2.0)
                      (nelisp-sexp-make-symbol "x")
                      (nelisp-sexp-make-str "y")
                      (nelisp-sexp-make-mut-str "z")
                      (nelisp-sexp-make-cons (nelisp-sexp-make-int 1)
                                             (nelisp-sexp-make-nil))
                      (nelisp-sexp-make-vector
                       (list (nelisp-sexp-make-int 2)))
                      (nelisp-sexp-make-char-table
                       (nelisp-sexp-make-nil) nil)
                      (nelisp-sexp-make-bool-vector 8 t)
                      (nelisp-sexp-make-cell (nelisp-sexp-make-int 7))
                      (nelisp-sexp-make-record 'r nil))))
    (should (= 13 (length cases)))
    (dolist (c cases)
      (let ((r (nelisp-sexp-jit-write-bytes c store)))
        (should (= 32 (length r)))))))

(ert-deftest nelisp-sexp-dsl-jit-tag-byte-position ()
  ;; Tag byte is at offset 0 for every variant (mirrors Rust
  ;; `variant_tag' which reads `*(self as *const u8)').
  (let* ((store (nelisp-sexp-jit-make-heap-store)))
    (should (= nelisp-sexp-jit-tag-nil
               (aref (nelisp-sexp-jit-write-bytes
                      (nelisp-sexp-make-nil)) 0)))
    (should (= nelisp-sexp-jit-tag-t
               (aref (nelisp-sexp-jit-write-bytes
                      (nelisp-sexp-make-t)) 0)))
    (should (= nelisp-sexp-jit-tag-int
               (aref (nelisp-sexp-jit-write-bytes
                      (nelisp-sexp-make-int 0)) 0)))
    (should (= nelisp-sexp-jit-tag-float
               (aref (nelisp-sexp-jit-write-bytes
                      (nelisp-sexp-make-float 0.0)) 0)))
    (should (= nelisp-sexp-jit-tag-symbol
               (aref (nelisp-sexp-jit-write-bytes
                      (nelisp-sexp-make-symbol "x") store) 0)))
    (should (= nelisp-sexp-jit-tag-cons
               (aref (nelisp-sexp-jit-write-bytes
                      (nelisp-sexp-make-cons
                       (nelisp-sexp-make-nil)
                       (nelisp-sexp-make-nil)) store) 0)))))

(ert-deftest nelisp-sexp-dsl-jit-tag-byte-rust-stable ()
  ;; Pin tag byte numeric values to the SEXP_TAG_* constants in
  ;; sexp.rs lines 217-229 so JIT decode stays stable.
  (should (= 0 nelisp-sexp-jit-tag-nil))
  (should (= 1 nelisp-sexp-jit-tag-t))
  (should (= 2 nelisp-sexp-jit-tag-int))
  (should (= 3 nelisp-sexp-jit-tag-float))
  (should (= 4 nelisp-sexp-jit-tag-symbol))
  (should (= 5 nelisp-sexp-jit-tag-str))
  (should (= 6 nelisp-sexp-jit-tag-mut-str))
  (should (= 7 nelisp-sexp-jit-tag-cons))
  (should (= 8 nelisp-sexp-jit-tag-vector))
  (should (= 9 nelisp-sexp-jit-tag-char-table))
  (should (= 10 nelisp-sexp-jit-tag-bool-vector))
  (should (= 11 nelisp-sexp-jit-tag-cell))
  (should (= 12 nelisp-sexp-jit-tag-record)))

(ert-deftest nelisp-sexp-dsl-jit-pad-bytes-are-zero ()
  ;; Bytes 1-7 (= align padding) must be zero so the JIT trampoline's
  ;; `mov al, [rdi+0]; movzx eax, al' reads only the tag bits.
  (let ((r (nelisp-sexp-jit-write-bytes (nelisp-sexp-make-int 42))))
    (dotimes (i 7)
      (should (zerop (aref r (1+ i)))))))

(ert-deftest nelisp-sexp-dsl-jit-trail-pad-bytes-zero ()
  ;; Bytes 16-31 (= trailing pad) zero for inline variants — Rust
  ;; `String' / `NonNull' payload-bytes-tail isn't meaningful in our
  ;; placeholder layout, so we emit zeros for predictability.
  (let ((r (nelisp-sexp-jit-write-bytes (nelisp-sexp-make-int 1))))
    (dotimes (i 16)
      (should (zerop (aref r (+ 16 i)))))))

(ert-deftest nelisp-sexp-dsl-jit-int-payload-le-i64 ()
  ;; Bytes 8-15 hold i64 LE payload for `Sexp::Int' — mirrors Rust
  ;; `match self { Sexp::Int(n) => /* load *(self+8) as i64 */ }'.
  (let ((r (nelisp-sexp-jit-write-bytes (nelisp-sexp-make-int 1))))
    (should (= 1 (aref r 8)))
    (dotimes (i 7) (should (zerop (aref r (+ 9 i))))))
  (let ((r (nelisp-sexp-jit-write-bytes (nelisp-sexp-make-int #x102))))
    (should (= #x02 (aref r 8)))
    (should (= #x01 (aref r 9)))))

(ert-deftest nelisp-sexp-dsl-jit-int-payload-negative ()
  ;; Negative i64 → two's-complement LE.  -1 = 0xFF * 8.
  (let ((r (nelisp-sexp-jit-write-bytes (nelisp-sexp-make-int -1))))
    (dotimes (i 8)
      (should (= #xFF (aref r (+ 8 i)))))))

(ert-deftest nelisp-sexp-dsl-jit-int-payload-max-min ()
  ;; i64 MAX / MIN round-trip.
  (let ((maxv (1- (ash 1 63)))
        (minv (- (ash 1 63))))
    (let ((r (nelisp-sexp-jit-write-bytes (nelisp-sexp-make-int maxv))))
      (should (= maxv (nelisp-sexp-int-value
                       (nelisp-sexp-jit-read-bytes r 0 nil)))))
    (let ((r (nelisp-sexp-jit-write-bytes (nelisp-sexp-make-int minv))))
      (should (= minv (nelisp-sexp-int-value
                       (nelisp-sexp-jit-read-bytes r 0 nil)))))))

(ert-deftest nelisp-sexp-dsl-jit-float-payload-le-f64 ()
  ;; Float 1.0 = 0x3FF0000000000000 LE → byte 15 = 0x3F, byte 14 = 0xF0.
  (let ((r (nelisp-sexp-jit-write-bytes (nelisp-sexp-make-float 1.0))))
    (should (= 32 (length r)))
    (should (= #x3F (aref r 15)))
    (should (= #xF0 (aref r 14)))))

(ert-deftest nelisp-sexp-dsl-jit-inline-decode-no-heap ()
  ;; Inline variants (= Nil / T / Int / Float) decode without HEAP-FN.
  (let ((nil-rec (nelisp-sexp-jit-write-bytes (nelisp-sexp-make-nil)))
        (t-rec (nelisp-sexp-jit-write-bytes (nelisp-sexp-make-t)))
        (int-rec (nelisp-sexp-jit-write-bytes
                  (nelisp-sexp-make-int 99)))
        (float-rec (nelisp-sexp-jit-write-bytes
                    (nelisp-sexp-make-float 1.5))))
    (should (nelisp-sexp-nil-p (nelisp-sexp-jit-read-bytes nil-rec 0 nil)))
    (should (nelisp-sexp-t-p (nelisp-sexp-jit-read-bytes t-rec 0 nil)))
    (should (= 99 (nelisp-sexp-int-value
                   (nelisp-sexp-jit-read-bytes int-rec 0 nil))))
    (should (= 1.5 (nelisp-sexp-float-value
                    (nelisp-sexp-jit-read-bytes float-rec 0 nil))))))

(ert-deftest nelisp-sexp-dsl-jit-heap-store-str ()
  ;; Str variant: heap store gets populated, decode round-trips.
  (let* ((store (nelisp-sexp-jit-make-heap-store))
         (s (nelisp-sexp-make-str "hello"))
         (r (nelisp-sexp-jit-write-bytes s store)))
    (should (= 32 (length r)))
    (should (= nelisp-sexp-jit-tag-str (aref r 0)))
    (let ((decoded (nelisp-sexp-jit-read-bytes
                    r 0 (nelisp-sexp-jit-heap-store-fn store))))
      (should (nelisp-sexp-eq s decoded)))))

(ert-deftest nelisp-sexp-dsl-jit-heap-store-symbol-multibyte ()
  ;; Multibyte symbol round-trip through fixed layout + heap-store.
  (let* ((store (nelisp-sexp-jit-make-heap-store))
         (s (nelisp-sexp-make-symbol "日本語"))
         (r (nelisp-sexp-jit-write-bytes s store))
         (decoded (nelisp-sexp-jit-read-bytes
                   r 0 (nelisp-sexp-jit-heap-store-fn store))))
    (should (nelisp-sexp-eq s decoded))))

(ert-deftest nelisp-sexp-dsl-jit-heap-store-cons ()
  ;; Cons round-trip via heap-store.
  (let* ((store (nelisp-sexp-jit-make-heap-store))
         (s (nelisp-sexp-make-cons (nelisp-sexp-make-int 1)
                                   (nelisp-sexp-make-int 2)))
         (r (nelisp-sexp-jit-write-bytes s store))
         (decoded (nelisp-sexp-jit-read-bytes
                   r 0 (nelisp-sexp-jit-heap-store-fn store))))
    (should (nelisp-sexp-eq s decoded))))

(ert-deftest nelisp-sexp-dsl-jit-heap-store-vector ()
  (let* ((store (nelisp-sexp-jit-make-heap-store))
         (s (nelisp-sexp-make-vector
             (list (nelisp-sexp-make-int 1)
                   (nelisp-sexp-make-int 2)
                   (nelisp-sexp-make-int 3))))
         (r (nelisp-sexp-jit-write-bytes s store))
         (decoded (nelisp-sexp-jit-read-bytes
                   r 0 (nelisp-sexp-jit-heap-store-fn store))))
    (should (nelisp-sexp-eq s decoded))))

(ert-deftest nelisp-sexp-dsl-jit-heap-store-record ()
  (let* ((store (nelisp-sexp-jit-make-heap-store))
         (s (nelisp-sexp-make-record 'pt
                                     (list (nelisp-sexp-make-int 3)
                                           (nelisp-sexp-make-int 4))))
         (r (nelisp-sexp-jit-write-bytes s store))
         (decoded (nelisp-sexp-jit-read-bytes
                   r 0 (nelisp-sexp-jit-heap-store-fn store))))
    (should (nelisp-sexp-eq s decoded))))

(ert-deftest nelisp-sexp-dsl-jit-heap-store-cell ()
  (let* ((store (nelisp-sexp-jit-make-heap-store))
         (s (nelisp-sexp-make-cell (nelisp-sexp-make-int 7)))
         (r (nelisp-sexp-jit-write-bytes s store))
         (decoded (nelisp-sexp-jit-read-bytes
                   r 0 (nelisp-sexp-jit-heap-store-fn store))))
    (should (nelisp-sexp-eq s decoded))))

(ert-deftest nelisp-sexp-dsl-jit-round-trip-all-13 ()
  ;; Single round-trip helper covers every variant.
  (let ((cases (list (nelisp-sexp-make-nil)
                     (nelisp-sexp-make-t)
                     (nelisp-sexp-make-int 0)
                     (nelisp-sexp-make-int -42)
                     (nelisp-sexp-make-float 3.14)
                     (nelisp-sexp-make-symbol "x")
                     (nelisp-sexp-make-str "y")
                     (nelisp-sexp-make-mut-str "z")
                     (nelisp-sexp-make-cons
                      (nelisp-sexp-make-int 1)
                      (nelisp-sexp-make-int 2))
                     (nelisp-sexp-make-vector
                      (list (nelisp-sexp-make-int 5)))
                     (nelisp-sexp-make-char-table
                      (nelisp-sexp-make-nil) nil)
                     (nelisp-sexp-make-bool-vector 4 t)
                     (nelisp-sexp-make-cell
                      (nelisp-sexp-make-int 8))
                     (nelisp-sexp-make-record 'tag nil))))
    (dolist (c cases)
      (should (nelisp-sexp-jit-round-trip-p c)))))

(ert-deftest nelisp-sexp-dsl-jit-round-trip-nested ()
  ;; Deeply nested Sexp survives through wire-format heap.
  (let ((sexp (nelisp-sexp-make-cons
               (nelisp-sexp-make-vector
                (list (nelisp-sexp-make-int 1)
                      (nelisp-sexp-make-cell
                       (nelisp-sexp-make-str "deep"))))
               (nelisp-sexp-make-record 'r
                                        (list (nelisp-sexp-make-t))))))
    (should (nelisp-sexp-jit-round-trip-p sexp))))

(ert-deftest nelisp-sexp-dsl-jit-truncated-record ()
  ;; Short input → truncated signal.
  (should-error (nelisp-sexp-jit-read-bytes
                 (make-string 16 0) 0 nil)
                :type 'nelisp-sexp-truncated))

(ert-deftest nelisp-sexp-dsl-jit-unknown-tag ()
  ;; Unknown tag byte → unknown-tag signal.
  (let ((bad (concat (unibyte-string #xFF) (make-string 31 0))))
    (should-error (nelisp-sexp-jit-read-bytes bad 0 nil)
                  :type 'nelisp-sexp-unknown-tag)))

(ert-deftest nelisp-sexp-dsl-jit-boxed-without-heap-fn ()
  ;; Boxed variants require a HEAP-FN; nil HEAP-FN signals.
  (let* ((store (nelisp-sexp-jit-make-heap-store))
         (r (nelisp-sexp-jit-write-bytes
             (nelisp-sexp-make-str "abc") store)))
    (should-error (nelisp-sexp-jit-read-bytes r 0 nil)
                  :type 'wrong-type-argument)))

(ert-deftest nelisp-sexp-dsl-jit-heap-miss ()
  ;; HEAP-FN returning nil for a pointer signals heap-miss.
  (let* ((store (nelisp-sexp-jit-make-heap-store))
         (r (nelisp-sexp-jit-write-bytes
             (nelisp-sexp-make-str "abc") store)))
    (should-error (nelisp-sexp-jit-read-bytes r 0 (lambda (_p _l) nil))
                  :type 'nelisp-sexp-jit-heap-miss)))

(ert-deftest nelisp-sexp-dsl-jit-pos-cursor ()
  ;; Read at non-zero POS advances correctly (records are 32 bytes).
  (let* ((r1 (nelisp-sexp-jit-write-bytes (nelisp-sexp-make-int 1)))
         (r2 (nelisp-sexp-jit-write-bytes (nelisp-sexp-make-int 2)))
         (cat (concat r1 r2)))
    (should (= 1 (nelisp-sexp-int-value
                  (nelisp-sexp-jit-read-bytes cat 0 nil))))
    (should (= 2 (nelisp-sexp-int-value
                  (nelisp-sexp-jit-read-bytes cat 32 nil))))))

(ert-deftest nelisp-sexp-dsl-jit-fresh-store-counter-grows ()
  ;; Each boxed-variant write allocates a fresh ptr.
  (let* ((store (nelisp-sexp-jit-make-heap-store))
         (_a (nelisp-sexp-jit-write-bytes
              (nelisp-sexp-make-str "a") store))
         (_b (nelisp-sexp-jit-write-bytes
              (nelisp-sexp-make-str "b") store)))
    ;; Two distinct keys live in the store.
    (should (= 2 (hash-table-count (cdr store))))))

(ert-deftest nelisp-sexp-dsl-jit-rejects-non-sexp ()
  ;; Writer requires a Sexp DSL plist.
  (should-error (nelisp-sexp-jit-write-bytes 42)
                :type 'wrong-type-argument)
  (should-error (nelisp-sexp-jit-write-bytes "hi")
                :type 'wrong-type-argument))

(ert-deftest nelisp-sexp-dsl-jit-payload-offset-const ()
  ;; Payload offset constant matches Rust SEXP_PAYLOAD_OFFSET = 8.
  (should (= 8 nelisp-sexp-jit-payload-offset)))

(ert-deftest nelisp-sexp-dsl-jit-record-size-const ()
  ;; Record size matches size_of::<Sexp>() = 32.
  (should (= 32 nelisp-sexp-jit-record-size)))

(provide 'nelisp-sexp-dsl-test)

;;; nelisp-sexp-dsl-test.el ends here
