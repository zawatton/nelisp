;;; nelisp-sys-check-test.el --- ERT tests for nelisp-sys type checker -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Stage 131.2 gate: positive checks plus negative checks asserting on
;; stable diagnostic codes (E-SYS-TYPE-NNN).

;;; Code:

(require 'ert)
(require 'nelisp-sys-frontend)
(require 'nelisp-sys-check)

(defun nelisp-sys-check-test--code (forms)
  "Parse + type-check FORMS; return the first diagnostic code, or t if clean."
  (let ((diags (nelisp-sys-check-collect
                (nelisp-sys-frontend-parse-module forms))))
    (if diags (car (car diags)) t)))

;;; Positive.

(ert-deftest nelisp-sys-check-add-clean ()
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun add ((a i32) (b i32)) i32
                     (:abi c :alloc none) (+ a b)))))))

(ert-deftest nelisp-sys-check-struct-distance-clean ()
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defstruct point (:repr c) (x i32) (y i32))
                   (sys:defun distance2 ((p (ptr point))) i64
                     (:abi c :alloc none)
                     (let ((x i32 (sys:load-field p x))
                           (y i32 (sys:load-field p y)))
                       (+ (* (sys:cast i64 x) (sys:cast i64 x))
                          (* (sys:cast i64 y) (sys:cast i64 y))))))))))

(ert-deftest nelisp-sys-check-call-clean ()
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun g ((a i32)) i32 () a)
                   (sys:defun f () i32 () (g 5)))))))

(ert-deftest nelisp-sys-check-control-flow-clean ()
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun f ((a i32) (b i32)) i32 ()
                     (if (< a b) a b)))))))

(ert-deftest nelisp-sys-check-slice-and-ptr-clean ()
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun len ((s (slice u8))) usize () (sys:slice-len s))))))
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun deref ((p (ptr i32))) i32 () (sys:load p)))))))

(ert-deftest nelisp-sys-check-while-and-set-clean ()
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun count ((n i32)) void ()
                     (let ((i i32 0))
                       (while (< i n) (set! i (+ i 1)))))))))) ; body void

;;; Negative — each asserts a specific stable code.

(ert-deftest nelisp-sys-check-unknown-variable ()
  (should (eq 'E-SYS-TYPE-001
              (nelisp-sys-check-test--code
               '((sys:defun f () i32 () x))))))

(ert-deftest nelisp-sys-check-let-type-mismatch ()
  (should (eq 'E-SYS-TYPE-002
              (nelisp-sys-check-test--code
               '((sys:defun f ((a i64)) void () (let ((x i32 a)) (seq))))))))

(ert-deftest nelisp-sys-check-unknown-type ()
  (should (eq 'E-SYS-TYPE-003
              (nelisp-sys-check-test--code
               '((sys:defun f ((a (ptr nope))) void () (seq)))))))

(ert-deftest nelisp-sys-check-unknown-field ()
  (should (eq 'E-SYS-TYPE-004
              (nelisp-sys-check-test--code
               '((sys:defstruct p (:repr c) (x i32))
                 (sys:defun f ((q (ptr p))) i32 () (sys:load-field q y)))))))

(ert-deftest nelisp-sys-check-arity-mismatch ()
  (should (eq 'E-SYS-TYPE-005
              (nelisp-sys-check-test--code
               '((sys:defun g ((a i32)) i32 () a)
                 (sys:defun f () i32 () (g 1 2)))))))

(ert-deftest nelisp-sys-check-unknown-function ()
  (should (eq 'E-SYS-TYPE-006
              (nelisp-sys-check-test--code
               '((sys:defun f () i32 () (nope 1)))))))

(ert-deftest nelisp-sys-check-field-on-non-struct ()
  (should (eq 'E-SYS-TYPE-007
              (nelisp-sys-check-test--code
               '((sys:defun f ((a i32)) i32 () (sys:load-field a x)))))))

(ert-deftest nelisp-sys-check-return-mismatch ()
  (should (eq 'E-SYS-TYPE-011
              (nelisp-sys-check-test--code
               '((sys:defun f ((a i64)) i32 () a)))))) ; returns i64, declared i32

(ert-deftest nelisp-sys-check-slice-on-non-slice ()
  (should (eq 'E-SYS-TYPE-012
              (nelisp-sys-check-test--code
               '((sys:defun f ((a i32)) usize () (sys:slice-len a)))))))

(ert-deftest nelisp-sys-check-deref-non-pointer ()
  (should (eq 'E-SYS-TYPE-014
              (nelisp-sys-check-test--code
               '((sys:defun f ((a i32)) i32 () (sys:load a)))))))

(ert-deftest nelisp-sys-check-duplicate-function ()
  (should (eq 'E-SYS-TYPE-015
              (nelisp-sys-check-test--code
               '((sys:defun f () void () (seq))
                 (sys:defun f () void () (seq)))))))

;;; nelisp-sys-check-test.el ends here
