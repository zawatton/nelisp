;;; nelisp-sys-ownership-test.el --- ERT tests for nelisp-sys ownership checker -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Stage 130.S2 / 131.3 gate tests: positive checks (expect t = clean) and
;; negative checks (expect a stable diagnostic code E-SYS-OWN-NNN).

;;; Code:

(require 'ert)
(require 'nelisp-sys-frontend)
(require 'nelisp-sys-ownership)

(defun nelisp-sys-ownership-test--code (forms)
  "Parse FORMS, run the ownership checker, return first code or t if clean."
  (let ((d (nelisp-sys-ownership-check-collect
            (nelisp-sys-frontend-parse-module forms))))
    (if d (car (car d)) t)))

;;; Positive tests (expect t).

(ert-deftest nelisp-sys-ownership-p1-single-consume ()
  "P1: owned param consumed exactly once is clean."
  ;; (sys:close fd) is a call consuming the owned fd once.
  (should (eq t (nelisp-sys-ownership-test--code
                 '((sys:defun sys:close ((fd (owned i32))) void () (seq))
                   (sys:defun f ((fd (owned i32))) void ()
                     (sys:close fd)))))))

(ert-deftest nelisp-sys-ownership-p2-copy-used-twice ()
  "P2: copy-typed param used twice is clean."
  (should (eq t (nelisp-sys-ownership-test--code
                 '((sys:defun f ((a i32)) i32 () (+ a a)))))))

(ert-deftest nelisp-sys-ownership-p3-owned-never-used ()
  "P3: owned param never used (dropped) is fine at this checker stage."
  (should (eq t (nelisp-sys-ownership-test--code
                 '((sys:defun f ((fd (owned i32))) void () (seq)))))))

;;; Negative tests (expect the code).

(ert-deftest nelisp-sys-ownership-e001-double-close ()
  "E-SYS-OWN-001: double-close = double consume."
  (should (eq 'E-SYS-OWN-001
              (nelisp-sys-ownership-test--code
               '((sys:defun sys:close ((fd (owned i32))) void () (seq))
                 (sys:defun f ((fd (owned i32))) void ()
                   (seq (sys:close fd) (sys:close fd))))))))

(ert-deftest nelisp-sys-ownership-e001-use-after-move-into-call ()
  "E-SYS-OWN-001: move into call then use again."
  (should (eq 'E-SYS-OWN-001
              (nelisp-sys-ownership-test--code
               '((sys:defun take ((x (owned i32))) void () (seq))
                 (sys:defun sys:close ((fd (owned i32))) void () (seq))
                 (sys:defun f ((fd (owned i32))) void ()
                   (seq (take fd) (sys:close fd))))))))

(ert-deftest nelisp-sys-ownership-e001-move-in-let-then-use ()
  "E-SYS-OWN-001: move value into let binding init, then use original."
  (should (eq 'E-SYS-OWN-001
              (nelisp-sys-ownership-test--code
               '((sys:defun sys:close ((fd (owned i32))) void () (seq))
                 (sys:defun f ((fd (owned i32))) void ()
                   (let ((g (owned i32) fd))
                     (sys:close fd))))))))

;;; nelisp-sys-ownership-test.el ends here
