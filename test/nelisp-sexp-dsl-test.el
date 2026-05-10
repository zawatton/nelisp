;;; nelisp-sexp-dsl-test.el --- ERT tests for Doc 95 §95.a Sexp DSL  -*- lexical-binding: t; -*-

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

(provide 'nelisp-sexp-dsl-test)

;;; nelisp-sexp-dsl-test.el ends here
