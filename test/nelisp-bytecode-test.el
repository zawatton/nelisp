;;; nelisp-bytecode-test.el --- ERT skeleton for Phase 3b bytecode VM  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 3b.1 skeleton: validate the module boundary and data shapes
;; only.  Compile / run tests land in 3b.2 when the first opcodes are
;; defined.  See docs/design/08-bytecode-vm.org.

;;; Code:

(require 'ert)
(require 'nelisp-bytecode)

;;; Data shape -------------------------------------------------------

(ert-deftest nelisp-bc-skeleton-make-and-predicate ()
  (let ((bcl (nelisp-bc-make nil '(x) [] [] 0 0)))
    (should (nelisp-bcl-p bcl))
    (should-not (nelisp-bcl-p '(nelisp-closure nil () nil)))
    (should-not (nelisp-bcl-p nil))
    (should-not (nelisp-bcl-p 42))))

(ert-deftest nelisp-bc-skeleton-accessors ()
  (let* ((env '((a . 1)))
         (params '(x &optional y))
         (consts [foo "bar" 3])
         (code [0 1 2])
         (bcl (nelisp-bc-make env params consts code 4 #b101)))
    (should (equal (nelisp-bc-env bcl) env))
    (should (equal (nelisp-bc-params bcl) params))
    (should (equal (nelisp-bc-consts bcl) consts))
    (should (equal (nelisp-bc-code bcl) code))
    (should (=     (nelisp-bc-stack-depth bcl) 4))
    (should (=     (nelisp-bc-special-mask bcl) 5))))

;;; Opcode table -----------------------------------------------------

(ert-deftest nelisp-bc-skeleton-opcode-table-empty ()
  ;; Phase 3b.1 ships no opcodes; any lookup must raise cleanly.
  (should-error (nelisp-bc-opcode 'CONST) :type 'nelisp-bc-error)
  (should (null nelisp-bc--opcodes)))

;;; Stubs ------------------------------------------------------------

(ert-deftest nelisp-bc-skeleton-compile-stub ()
  (should-error (nelisp-bc-compile '(+ 1 2))
                :type 'nelisp-bc-unimplemented))

(ert-deftest nelisp-bc-skeleton-run-stub ()
  (let ((bcl (nelisp-bc-make nil nil [] [] 0 0)))
    (should-error (nelisp-bc-run bcl)
                  :type 'nelisp-bc-unimplemented)))

(ert-deftest nelisp-bc-skeleton-error-hierarchy ()
  ;; `nelisp-bc-unimplemented' must be a sub-error of `nelisp-bc-error'.
  (should (eq (get 'nelisp-bc-unimplemented 'error-conditions)
              (get 'nelisp-bc-unimplemented 'error-conditions)))
  (should (memq 'nelisp-bc-error
                (get 'nelisp-bc-unimplemented 'error-conditions))))

(provide 'nelisp-bytecode-test)
;;; nelisp-bytecode-test.el ends here
