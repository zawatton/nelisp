;;; nelisp-sys-borrow-test.el --- ERT tests for nelisp-sys borrow checker -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 131.4 / 130.S3-S4 gate tests.  Covers:
;;   P1  Two shared borrows of the same place coexist without error.
;;   P2  A single mutable borrow is accepted.
;;   N1  E-SYS-BORROW-002 — mutable borrow while a shared borrow is active.
;;   N2  E-SYS-BORROW-001 — shared borrow while a mutable borrow is active.
;;   N3  E-SYS-BORROW-001 — mutable borrow while a mutable borrow is active.
;;   N4  E-SYS-BORROW-003 — function declared to return a borrow type.
;;   N5  E-SYS-BOUNDS-001  — negative constant slice index.

;;; Code:

(require 'ert)
(require 'nelisp-sys-frontend)
(require 'nelisp-sys-borrow)

(defun nelisp-sys-borrow-test--code (forms)
  "Parse FORMS into a module and run the borrow checker.
Returns the first diagnostic code (a symbol) when a violation is
detected, or t when the module is clean."
  (let ((d (nelisp-sys-borrow-check-collect
            (nelisp-sys-frontend-parse-module forms))))
    (if d (car (car d)) t)))

;;; Positive tests — expect t (no violation).

(ert-deftest nelisp-sys-borrow-p1-two-shared-coexist ()
  "Two shared borrows of the same place must coexist without error."
  (should
   (eq t
       (nelisp-sys-borrow-test--code
        '((sys:defstruct point (:repr c) (x i32))
          (sys:defun f ((pv point)) i32 ()
            (sys:with-borrow ((a (& point) pv))
              (sys:with-borrow ((b (& point) pv))
                (sys:load-field a x)))))))))

(ert-deftest nelisp-sys-borrow-p2-single-mut ()
  "A single mutable borrow must be accepted."
  (should
   (eq t
       (nelisp-sys-borrow-test--code
        '((sys:defstruct point (:repr c) (x i32))
          (sys:defun f ((pv point)) void ()
            (sys:with-borrow-mut ((m (&mut point) pv))
              (sys:store-field! m x 1))))))))

;;; Negative tests — each asserts a specific stable code.

(ert-deftest nelisp-sys-borrow-n1-mut-while-shared ()
  "E-SYS-BORROW-002: mutable borrow while a shared borrow is active."
  (should
   (eq 'E-SYS-BORROW-002
       (nelisp-sys-borrow-test--code
        '((sys:defstruct point (:repr c) (x i32))
          (sys:defun f ((pv point)) void ()
            (sys:with-borrow ((a (& point) pv))
              (sys:with-borrow-mut ((m (&mut point) pv))
                (seq)))))))))

(ert-deftest nelisp-sys-borrow-n2-shared-while-mut ()
  "E-SYS-BORROW-001: shared borrow while a mutable borrow is active."
  (should
   (eq 'E-SYS-BORROW-001
       (nelisp-sys-borrow-test--code
        '((sys:defstruct point (:repr c) (x i32))
          (sys:defun f ((pv point)) void ()
            (sys:with-borrow-mut ((m (&mut point) pv))
              (sys:with-borrow ((a (& point) pv))
                (seq)))))))))

(ert-deftest nelisp-sys-borrow-n3-mut-while-mut ()
  "E-SYS-BORROW-001: mutable borrow while a mutable borrow is active."
  (should
   (eq 'E-SYS-BORROW-001
       (nelisp-sys-borrow-test--code
        '((sys:defstruct point (:repr c) (x i32))
          (sys:defun f ((pv point)) void ()
            (sys:with-borrow-mut ((m (&mut point) pv))
              (sys:with-borrow-mut ((n (&mut point) pv))
                (seq)))))))))

(ert-deftest nelisp-sys-borrow-n4-return-borrow ()
  "E-SYS-BORROW-003: function declared to return a borrow type."
  (should
   (eq 'E-SYS-BORROW-003
       (nelisp-sys-borrow-test--code
        '((sys:defstruct point (:repr c) (x i32))
          (sys:defun f ((pv point)) (& point) ()
            (seq)))))))

(ert-deftest nelisp-sys-borrow-n5-negative-slice-index ()
  "E-SYS-BOUNDS-001: negative constant slice index."
  (should
   (eq 'E-SYS-BOUNDS-001
       (nelisp-sys-borrow-test--code
        '((sys:defun f ((s (slice u8))) u8 ()
            (sys:slice-ref s -1)))))))

;;; nelisp-sys-borrow-test.el ends here
