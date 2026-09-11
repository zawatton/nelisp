;;; nelisp-native-unit-compile.el --- host compiler for replaceable native units -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-load)

(defun nelisp-native-unit-compile-command ()
  "Compile SOURCE OUTPUT BINARY-SHA command arguments without evaluating SOURCE."
  (let ((source (pop command-line-args-left))
        (output (pop command-line-args-left))
        (binary (pop command-line-args-left)))
    (unless (and source output binary (null command-line-args-left))
      (error "Expected SOURCE OUTPUT BINARY-SHA"))
    (let ((manifest (nelisp-native-load-raw-compile-file
                     source output nil "replaceable-native-unit" binary)))
      (when (nelisp-native-load-raw-check manifest)
        (error "Invalid native unit"))
      (princ "Native unit compiled\n"))))

(provide 'nelisp-native-unit-compile)
