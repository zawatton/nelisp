;;; nelisp-bytecode-native-rooted-cfg-constructor-contract.el --- Sealed constructor domain -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Trusted startup loads this adjacent owner after the existing contract
;; validator. It never republishes the validator, loader or producer. A missing
;; startup validator stays unavailable, and replacement is refused before call.

;;; Code:
(require 'cl-lib)

(let ((valid-owner
       (and (fboundp 'nelisp-bytecode-native-rooted-cfg-contract-valid-p)
            (symbol-function 'nelisp-bytecode-native-rooted-cfg-contract-valid-p)))
      (lookup (symbol-function 'symbol-function))
      (same (symbol-function 'eq)))
;;;###autoload
(defun nelisp-bytecode-native-rooted-cfg-contract-constructor-p (contract)
  "Authenticate CONTRACT and its exact straight-line constructor domain.
The normal compiler capability currently certifies protected argument/constant
roots and CONS. Arithmetic, calls, accessors and branches remain outside it."
  (and valid-owner
       (funcall same valid-owner
                (funcall lookup 'nelisp-bytecode-native-rooted-cfg-contract-valid-p))
       (let* ((reconstruction (funcall valid-owner contract :reconstruction))
              (plan (plist-get reconstruction :plan))
              (blocks (plist-get plan :blocks))
              (operations (and (consp blocks)
                               (cl-loop for block in blocks append
                                        (plist-get block :operations))))
              (has-cons (cl-some (lambda (operation)
                                   (eq (plist-get operation :opcode) 'cons))
                                 operations))
              (imports (and has-cons '("nl_native_cons_v2"))))
         (and reconstruction
              (funcall same valid-owner
                       (funcall lookup 'nelisp-bytecode-native-rooted-cfg-contract-valid-p))
              (eq (plist-get plan :status) 'complete)
              (= (length blocks) 1)
              (null (plist-get (car blocks) :successors))
              (null (plist-get plan :phis))
              (eq (plist-get (car (last operations)) :opcode) 'return)
              (not (plist-get plan :exit-root-base))
              (consp operations)
              (cl-every (lambda (operation)
                          (memq (plist-get operation :opcode)
                                '(const stack-ref dup discard cons return)))
                        operations)
              (equal (plist-get plan :gateway-imports) imports)
              (equal (plist-get contract :imports) imports))))))

(require 'nelisp-bytecode-native-rooted-cfg-safe-contract)
(let ((validator (symbol-function 'nelisp-bytecode-native-rooted-cfg-contract-valid-p))
      (safe-validator (symbol-function 'nelisp-bytecode-native-rooted-cfg-safe-contract-valid-p))
      (lookup (symbol-function 'symbol-function)) (same (symbol-function 'eq)))
(defun nelisp-bytecode-native-rooted-cfg-contract-f1-p (contract)
  "Authenticate the separate generic evaluator domain without widening CONS."
  (and (funcall same validator (funcall lookup 'nelisp-bytecode-native-rooted-cfg-contract-valid-p))
       (funcall same safe-validator (funcall lookup 'nelisp-bytecode-native-rooted-cfg-safe-contract-valid-p))
       (if (equal (plist-get contract :version) nelisp-bytecode-native-rooted-cfg-safe-contract-f1-version)
           (funcall safe-validator contract)
         (funcall validator contract))
       (plist-get (plist-get contract :plan) :funcall-version)
       (equal (plist-get contract :funcall-descriptor) (nelisp-native-funcall-v2-descriptor))
       (equal (plist-get contract :funcall-hash) (nelisp-native-funcall-v2-hash))
       (equal (plist-get contract :imports) '("nl_native_funcall_v2" "nl_root_pin_slot_v2")))))
(provide 'nelisp-bytecode-native-rooted-cfg-constructor-contract)
;;; nelisp-bytecode-native-rooted-cfg-constructor-contract.el ends here
