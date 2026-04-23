;;; nelisp-gc-test.el --- Phase 3c.1 root-set ERTs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 3c.1 scope: root-set enumeration only.  Mark pass,
;; finalizers, and MCP introspection get their own test files as
;; each sub-phase lands (3c.2 onward).  See
;; docs/design/09-phase3c-gc.org §3.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp)
(require 'nelisp-bytecode)
(require 'nelisp-gc)

;;; Globals root ------------------------------------------------------

(ert-deftest nelisp-gc-root-set-includes-four-global-tables ()
  "Root set lists `nelisp--globals' etc. as independent entries."
  (let ((kinds (mapcar (lambda (r) (plist-get r :kind))
                       (nelisp-gc-root-set))))
    (should (memq 'nelisp--globals   kinds))
    (should (memq 'nelisp--functions kinds))
    (should (memq 'nelisp--macros    kinds))
    (should (memq 'nelisp--specials  kinds))))

(ert-deftest nelisp-gc-root-set-global-entries-carry-hash-tables ()
  "Each global-table root entry's :value is the actual hash-table
(identity compare, not a copy)."
  (dolist (r (nelisp-gc-root-set))
    (when (memq (plist-get r :kind)
                '(nelisp--globals nelisp--functions
                  nelisp--macros nelisp--specials))
      (should (hash-table-p (plist-get r :value)))
      (should (eq (plist-get r :value)
                  (symbol-value (plist-get r :kind)))))))

;;; VM stack root -----------------------------------------------------

(ert-deftest nelisp-gc-no-vm-stack-outside-nelisp-bc-run ()
  "With no bytecode execution in flight, `vm-stack' root entries absent."
  (let ((nelisp-gc--active-vms nil))
    (should-not
     (cl-find 'vm-stack (nelisp-gc-root-set)
              :key (lambda (r) (plist-get r :kind))))))

(ert-deftest nelisp-gc-active-vm-surfaces-in-root-set ()
  "A VM bound into `nelisp-gc--active-vms' becomes a `vm-stack' root entry."
  (let* ((fake-vm (vector 'mock 'vm 'state))
         (nelisp-gc--active-vms (list fake-vm))
         (vm-roots (cl-remove-if-not
                    (lambda (r) (eq (plist-get r :kind) 'vm-stack))
                    (nelisp-gc-root-set))))
    (should (= 1 (length vm-roots)))
    (should (eq fake-vm (plist-get (car vm-roots) :value)))))

(ert-deftest nelisp-gc-vm-stack-appears-while-bc-run ()
  "`nelisp-bc-run' pushes its VM state vector onto `nelisp-gc--active-vms'
before entering dispatch.

Intercept `nelisp-bc--dispatch' via `cl-letf' to snapshot the dynamic
binding at the exact moment control reaches dispatch — that frames
the push-site guarantee without relying on CALL-opcode dispatch of a
test-local probe function (which would itself be subject to the
bcl/host-apply routing the test is meant to be orthogonal to)."
  (let* ((snapshot nil)
         (orig (symbol-function 'nelisp-bc--dispatch))
         (probe-bcl (nelisp-bc-compile '(lambda () 42))))
    (cl-letf (((symbol-function 'nelisp-bc--dispatch)
               (lambda (vm nested)
                 (unless snapshot
                   (setq snapshot nelisp-gc--active-vms))
                 (funcall orig vm nested))))
      (nelisp-bc-run probe-bcl))
    (should (consp snapshot))
    (should (vectorp (car snapshot)))
    ;; Pushed VM is the dispatching VM itself (slot 0 = code vector).
    (should (arrayp (aref (car snapshot) 0)))))

(ert-deftest nelisp-gc-vm-stack-pops-on-normal-exit ()
  "After `nelisp-bc-run' returns, `nelisp-gc--active-vms' is empty again."
  (let ((before nelisp-gc--active-vms)
        (probe (nelisp-bc-compile '(lambda () 1))))
    (nelisp-bc-run probe)
    (should (equal before nelisp-gc--active-vms))))

(ert-deftest nelisp-gc-vm-stack-pops-on-non-local-exit ()
  "A `throw' past `nelisp-bc-run' still pops the active-VM entry —
dynamic `let' + `unwind-protect' inside `nelisp-bc-run' guarantees
the cleanup regardless of how control leaves the body."
  (let ((before nelisp-gc--active-vms)
        (thrower (nelisp-bc-compile
                  '(lambda () (throw 'nelisp-gc-test--tag 'ok)))))
    (catch 'nelisp-gc-test--tag (nelisp-bc-run thrower))
    (should (equal before nelisp-gc--active-vms))))

(provide 'nelisp-gc-test)
;;; nelisp-gc-test.el ends here
