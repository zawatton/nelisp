;;; nelisp-sys-unsafe-test.el --- ERT tests for nelisp-sys unsafe/effect checker -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 130.S1 / 131.5 gate: positive checks plus negative checks
;; asserting on stable diagnostic codes (E-SYS-UNSAFE-NNN, E-SYS-ALLOC-NNN).

;;; Code:

(require 'ert)
(require 'nelisp-sys-frontend)
(require 'nelisp-sys-unsafe)

(defun nelisp-sys-unsafe-test--code (forms)
  "Parse FORMS and run the unsafe/effect checker.
Return the first diagnostic code, or t when clean."
  (let ((d (nelisp-sys-unsafe-check-collect
            (nelisp-sys-frontend-parse-module forms))))
    (if d (car (car d)) t)))

;;; Positive — expect t (no violation).

(ert-deftest nelisp-sys-unsafe-p1-raw-store-inside-unsafe ()
  "ptr-store! inside sys:unsafe is allowed."
  (should (eq t (nelisp-sys-unsafe-test--code
                 '((sys:defun poke ((p (ptr u8)) (v u8)) void (:alloc none)
                     (sys:unsafe (:reason "ok") (sys:store! p v))))))))

(ert-deftest nelisp-sys-unsafe-p2-alloc-none-calls-alloc-none ()
  "A :alloc none function calling another :alloc none function is clean."
  (should (eq t (nelisp-sys-unsafe-test--code
                 '((sys:defun g () void (:alloc none) (seq))
                   (sys:defun f () void (:alloc none) (g)))))))

;;; Negative — each asserts a specific stable code.

(ert-deftest nelisp-sys-unsafe-001-ptr-store-outside-unsafe ()
  "ptr-store! outside unsafe => E-SYS-UNSAFE-001."
  (should (eq 'E-SYS-UNSAFE-001
              (nelisp-sys-unsafe-test--code
               '((sys:defun poke ((p (ptr u8)) (v u8)) void (:alloc none)
                   (sys:store! p v)))))))

(ert-deftest nelisp-sys-unsafe-001-ptr-load-outside-unsafe ()
  "ptr-load outside unsafe => E-SYS-UNSAFE-001."
  (should (eq 'E-SYS-UNSAFE-001
              (nelisp-sys-unsafe-test--code
               '((sys:defun rd ((p (ptr u8))) u8 () (sys:load p)))))))

(ert-deftest nelisp-sys-unsafe-002-unsafe-extern-call ()
  "Call to an unsafe extern outside unsafe => E-SYS-UNSAFE-002."
  (should (eq 'E-SYS-UNSAFE-002
              (nelisp-sys-unsafe-test--code
               '((sys:extern memx (:symbol "memx" :abi c :unsafe t)
                   ((d (ptr u8))) (ptr u8) (:alloc none))
                 (sys:defun f ((d (ptr u8))) void ()
                   (memx d)))))))

(ert-deftest nelisp-sys-unsafe-003-raw-slice-ref-outside-unsafe ()
  "slice-ref with :raw t outside unsafe => E-SYS-UNSAFE-003."
  (should (eq 'E-SYS-UNSAFE-003
              (nelisp-sys-unsafe-test--code
               '((sys:defun rr ((s (slice u8))) u8 ()
                   (sys:slice-ref-raw s 0)))))))

(ert-deftest nelisp-sys-unsafe-004-forget-outside-unsafe ()
  "sys:forget outside unsafe => E-SYS-UNSAFE-004."
  (should (eq 'E-SYS-UNSAFE-004
              (nelisp-sys-unsafe-test--code
               '((sys:defun f ((x (owned u8))) void ()
                   (sys:forget x)))))))

(ert-deftest nelisp-sys-alloc-001-alloc-none-calls-heap ()
  ":alloc none function calling :alloc heap callee => E-SYS-ALLOC-001."
  (should (eq 'E-SYS-ALLOC-001
              (nelisp-sys-unsafe-test--code
               '((sys:defun g () void (:alloc heap) (seq))
                 (sys:defun f () void (:alloc none) (g)))))))

;;; nelisp-sys-unsafe-test.el ends here
