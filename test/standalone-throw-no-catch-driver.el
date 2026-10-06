;;; standalone-throw-no-catch-driver.el --- Shared catch registry parity -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-native-consumer)
(load "test/support/throw-no-catch-fixtures.el" nil t t)
(defvar throw-nocatch-head (string-to-number (getenv "THROW_NOCATCH_HEAD")))
(defun throw-nocatch-check-head (phase)
  (unless (= (ptr-read-u64 throw-nocatch-head 0) 0)
    (error "Catch registry leaked after %s" phase)))
(throw-nocatch-check-head 'startup)
(throw-nocatch-observe 'eval)
(throw-nocatch-check-head 'eval)
(dolist (name '(normal nested error missing nil pop))
  (funcall (intern (concat "throw-nocatch-fixture-" (symbol-name name))))
  (throw-nocatch-check-head name))
(dolist (pair (nelisp-bytecode-native-consumer-read-elc-functions
               (getenv "THROW_NOCATCH_FIXTURE")))
  (when (string-prefix-p "throw-nocatch-fixture-" (symbol-name (car pair)))
    (fset (car pair) (cdr pair))))
(throw-nocatch-observe 'vm)
(throw-nocatch-check-head 'vm)
;; Inspect the registry after each normal, throw and error activation.
(dolist (name '(normal nested error missing nil pop))
  (funcall (intern (concat "throw-nocatch-fixture-" (symbol-name name))))
  (throw-nocatch-check-head name))
(unless (= (throw-nocatch-fixture-loop) 0) (error "Catch loop failed"))
(throw-nocatch-check-head 'loop)
(princ "THROW-BALANCED\n")
nil
