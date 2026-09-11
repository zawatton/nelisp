;;; nelisp-runtime-reload-compile.el --- Stage the current native runtime -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Host compiler invoked from the persistent development REPL.  The child
;; compiles a source snapshot; only the parent can publish executable code.
;;; Code:
(require 'nelisp-runtime-reload-build)
(require 'nelisp-native-load)

(defun nelisp-runtime-reload-compile-command ()
  "Consume OUTPUT and BINARY-SHA arguments and stage a native runtime unit."
  (let ((output (pop command-line-args-left))
        (binary (pop command-line-args-left)))
    (unless (and (stringp output) (> (length output) 0)
                 (stringp binary)
                 (string-match-p "\\`[0-9a-f]\\{64\\}\\'" binary))
      (error "Usage: -f nelisp-runtime-reload-compile-command OUTPUT BINARY-SHA"))
    (let* ((source (concat output ".el"))
           (_snapshot (nelisp-runtime-reload-export-source source))
           (manifest (nelisp-native-load-raw-v2-compile-file
                      source output "checkout-runtime" binary))
           (problems (nelisp-native-load-raw-v2-check manifest)))
      (when problems (error "Native runtime staging rejected: %S" problems))
      (princ (format "Staged %s\n" output)))))

(provide 'nelisp-runtime-reload-compile)
;;; nelisp-runtime-reload-compile.el ends here
