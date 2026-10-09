;;; nelisp-native-template-bytecode.el --- Compile Tier 0 Lisp -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'bytecomp)
(require 'nelisp-native-cache)
(let* ((root (expand-file-name ".." (file-name-directory load-file-name)))
       (output (expand-file-name "target/nelisp-template-bytecode.el" root)))
  ;; Compile only ordinary compiler helpers, never the captured runtime gateways
  ;; or validation-owner closures. The same GNU bytecode VM executes these bodies.
  (with-temp-file output
    (insert ";;; Generated Tier 0 helpers -*- lexical-binding: t; -*-\n")
    (dolist (name '(nelisp-native-template-recipe
                    nelisp-native-template-validate nelisp-native-template-frame-states
                    nelisp-native-template-assemble nelisp-native-template-manifest
                    nelisp-native-template-bounded-data-p
                    nelisp-native-template--native-printable-p
                    nelisp-native-template-patch32))
      (let ((compiled (byte-compile (symbol-function name)))
            (print-length nil) (print-level nil) (print-circle nil)
            (print-escape-newlines t) (print-escape-nonascii t))
        (unless (byte-code-function-p compiled) (error "Tier 0 bytecode refused: %s" name))
        (prin1 (list 'fset (list 'quote name) compiled) (current-buffer))
        (insert "\n")))
    (insert "t\n")))
