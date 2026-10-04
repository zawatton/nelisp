;;; standalone-native-funcall-v2-pre-f1-driver.el --- Executed old compiler control -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-native-compiler)
(require 'nelisp-bytecode-native-consumer)
(let* ((directory (getenv "F1B_PRE_F1"))
       (function (cdr (assq 'f1-fixture (nelisp-bytecode-native-consumer-read-elc-functions (getenv "F1_FIXTURE"))))))
  ;; The caller supplies retained pre-F1 sources, never fabricated opcodes.
  (load (expand-file-name "nelisp-bytecode-native-rooted-cfg-plan.el" directory) nil t t)
  (load (expand-file-name "nelisp-bytecode-native-compiler.el" directory) nil t t)
  (let ((result (nelisp-bytecode-native-compiler-build function
                 (expand-file-name "refused.nelr" (getenv "NELISP_NATIVE_CACHE")) "f1-pre-change")))
    (unless (and (eq (plist-get result :status) 'unsupported)
                 (string-match-p "opcode 66 is accepted only in the pinned GNU 31.1 two-argument CONS template" (plist-get result :reason)))
      (error "Pre-F1 compiler did not reproduce the unsupported body: %S" result))
    (princ (format "F1B-PRE-F1-REFUSAL=%S\n" (list :status (plist-get result :status) :reason (plist-get result :reason))))))
