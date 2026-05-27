;;; nelisp-sys-driver-test.el --- ERT tests for the nelisp-sys analysis facade -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Verifies that `nelisp-sys-check-all' runs every pass and surfaces each
;; pass's error type, and that a fully-valid module passes all gates.

;;; Code:

(require 'ert)
(require 'nelisp-sys-driver)

(ert-deftest nelisp-sys-driver-valid-module-passes ()
  (should (eq t (nelisp-sys-analyze
                 '((sys:defun add ((a i32) (b i32)) i32
                     (:abi c :alloc none) (+ a b))))))
  (should (null (nelisp-sys-analyze-collect
                 '((sys:defun add ((a i32) (b i32)) i32
                     (:abi c :alloc none) (+ a b)))))))

(ert-deftest nelisp-sys-driver-valid-struct-module-passes ()
  (should (eq t (nelisp-sys-analyze
                 '((sys:defstruct point (:repr c) (x i32) (y i32))
                   (sys:defun getx ((p (ptr point))) i32 (:alloc none)
                     (sys:load-field p x)))))))

(ert-deftest nelisp-sys-driver-surfaces-type-error ()
  (should-error (nelisp-sys-analyze '((sys:defun f () i32 () x)))
                :type 'nelisp-sys-check-error))

(ert-deftest nelisp-sys-driver-surfaces-ownership-error ()
  ;; `close' is a defined consuming function so type-check passes and the
  ;; double-consume is caught by the ownership pass, not the type pass.
  (should-error (nelisp-sys-analyze
                 '((sys:defun close ((fd (owned i32))) void () (seq))
                   (sys:defun f ((fd (owned i32))) void ()
                     (seq (close fd) (close fd)))))
                :type 'nelisp-sys-ownership-error))

(ert-deftest nelisp-sys-driver-surfaces-borrow-error ()
  ;; Body actually yields the borrow so type-check passes; the borrow pass
  ;; rejects the escaping ref return type.
  (should-error (nelisp-sys-analyze
                 '((sys:defstruct point (:repr c) (x i32))
                   (sys:defun f ((pv point)) (& point) ()
                     (sys:with-borrow ((b (& point) pv)) b))))
                :type 'nelisp-sys-borrow-error))

(ert-deftest nelisp-sys-driver-surfaces-unsafe-error ()
  (should-error (nelisp-sys-analyze
                 '((sys:defun poke ((p (ptr u8)) (v u8)) void (:alloc none)
                     (sys:store! p v))))
                :type 'nelisp-sys-unsafe-error))

(ert-deftest nelisp-sys-driver-collect-reports-parse-error ()
  (let ((diags (nelisp-sys-analyze-collect '((sys:bogus 1)))))
    (should (= 1 (length diags)))
    (should (eq 'E-SYS-PARSE (car (car diags))))))

(ert-deftest nelisp-sys-driver-collect-reports-safety-error ()
  (let ((diags (nelisp-sys-analyze-collect
                '((sys:defun close ((fd (owned i32))) void () (seq))
                  (sys:defun f ((fd (owned i32))) void ()
                    (seq (close fd) (close fd)))))))
    (should (eq 'E-SYS-OWN-001 (car (car diags))))))

;;; nelisp-sys-driver-test.el ends here
