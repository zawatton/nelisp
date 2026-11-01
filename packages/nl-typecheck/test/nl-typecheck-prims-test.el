;;; nl-typecheck-prims-test.el --- ERT tests for Doc 236 Stage 1 -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nl-typecheck-prims)

(ert-deftest nl-typecheck-prims-table-shape ()
  (should (> (length nl-typecheck-prims-table) 200))
  (should (equal (nl-typecheck-prims-signature 'car) '(function (list) t)))
  (should (null (nl-typecheck-prims-signature 'no-such-primitive-xyz)))
  (should (equal (nl-typecheck-prims-spec-arity
                  (nl-typecheck-prims-signature 'plist-get))
                 '(2 . 3)))
  (should (equal (nl-typecheck-prims-spec-arity
                  (nl-typecheck-prims-signature '+))
                 '(0 . many))))

(ert-deftest nl-typecheck-prims-table-sorted ()
  (let ((names (mapcar (lambda (e) (symbol-name (car e)))
                       nl-typecheck-prims-table)))
    (should (equal names (sort (copy-sequence names) #'string<)))))

(ert-deftest nl-typecheck-prims-crosscheck-runs ()
  (let ((lines (nl-typecheck-prims-crosscheck)))
    (should (listp lines))
    (should (cl-every #'stringp lines))))

(ert-deftest nl-typecheck-prims-negative-control ()
  (let* ((good nl-typecheck-prims-table)
         (bad (mapcar (lambda (e)
                        (if (eq (car e) 'car)
                            (list 'car '(function (list list) t))
                          e))
                      good))
         (clean (nl-typecheck-prims-crosscheck good nil nil))
         (lines (nl-typecheck-prims-crosscheck bad nil nil)))
    ;; The uncorrupted table does not flag car or cdr.
    (should-not (cl-some (lambda (l) (string-match-p "\\`arity-mismatch car " l))
                         clean))
    (should-not (cl-some (lambda (l) (string-match-p "\\`arity-mismatch cdr " l))
                         lines))
    ;; The corrupted car entry is reported as an arity mismatch.
    (should (cl-some (lambda (l) (string-match-p "\\`arity-mismatch car " l))
                     lines))
    ;; The shipped table is untouched by the let-bound copy.
    (should (equal (nl-typecheck-prims-signature 'car) '(function (list) t)))))

(ert-deftest nl-typecheck-prims-missing-spec-and-unknown-type ()
  (let ((lines (nl-typecheck-prims-crosscheck
                '((car (function (list) t))
                  (cdr (function (list) (weird-type))))
                '("car" "no-spec-builtin") nil)))
    (should (member "missing-spec no-spec-builtin no GNU spec for NeLisp builtin"
                    lines))
    (should (cl-some (lambda (l) (string-prefix-p "unknown-type cdr " l)) lines))
    (should-not (cl-some (lambda (l) (string-match-p "car" l)) lines))))

(ert-deftest nl-typecheck-types-subtype ()
  (should (nl-typecheck-subtype-p 'fixnum 'integer))
  (should (nl-typecheck-subtype-p 'integer 'number))
  (should (nl-typecheck-subtype-p 'fixnum 'number))
  (should-not (nl-typecheck-subtype-p 'number 'integer))
  (should (nl-typecheck-subtype-p '(integer 0 10) 'fixnum))
  (should-not (nl-typecheck-subtype-p 'fixnum '(integer 0 10)))
  (should (nl-typecheck-subtype-p '(integer 0 10) '(integer -1 *)))
  (should (nl-typecheck-subtype-p 'null 'list))
  (should (nl-typecheck-subtype-p 'string 'sequence))
  (should (nl-typecheck-subtype-p '(or fixnum float) 'number))
  (should (nl-typecheck-subtype-p '(member 1 2) 'fixnum))
  (should (nl-typecheck-subtype-p '(member a b) 'symbol))
  (should-not (nl-typecheck-subtype-p 'symbol '(member a b)))
  (should (nl-typecheck-subtype-p '(and number (not float)) 'integer))
  (should (nl-typecheck-subtype-p '(list-of fixnum) '(list-of integer)))
  (should-not (nl-typecheck-subtype-p '(list-of integer) '(list-of fixnum)))
  (should (nl-typecheck-subtype-p 'string t))
  (should-not (nl-typecheck-subtype-p t 'string)))

(ert-deftest nl-typecheck-types-disjoint-and-consistent ()
  (should (nl-typecheck-disjoint-p 'string 'integer))
  (should (nl-typecheck-disjoint-p 'string '(or integer float)))
  (should (nl-typecheck-disjoint-p '(integer 0 3) '(integer 5 9)))
  (should-not (nl-typecheck-disjoint-p 'number 'integer))
  (should-not (nl-typecheck-disjoint-p 'list 'symbol))
  (should-not (nl-typecheck-disjoint-p t 'string))
  (should-not (nl-typecheck-disjoint-p 'string t))
  (should (nl-typecheck-consistent-p t 'string))
  (should (nl-typecheck-consistent-p 'string t))
  (should-not (nl-typecheck-consistent-p 'string 'integer))
  ;; Unknown types are never proven clashing.
  (should-not (nl-typecheck-disjoint-p 'string '(satisfies foo)))
  (should (nl-typecheck-consistent-p 'string 'mystery-type)))

(ert-deftest nl-typecheck-types-normalize ()
  (should (equal (nl-typecheck-normalize '(or integer (or integer string)))
                 '(or integer string)))
  (should (eq (nl-typecheck-normalize '(or integer t)) t))
  (should (eq (nl-typecheck-normalize '(and integer nil)) nil))
  (should (eq (nl-typecheck-normalize '(or fixnum integer)) 'integer))
  (should (eq (nl-typecheck-normalize '(not (not string))) 'string))
  (should (eq (nl-typecheck-normalize '(integer * *)) 'integer))
  (should (eq (nl-typecheck-normalize '(or)) nil))
  (should (equal (nl-typecheck-normalize '(or string integer))
                 (nl-typecheck-normalize '(or integer string)))))

(provide 'nl-typecheck-prims-test)
;;; nl-typecheck-prims-test.el ends here
