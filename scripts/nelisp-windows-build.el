;;; nelisp-windows-build.el --- native Windows x86_64 build entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 138 Stage 1.  Build the smallest native Windows PE32+ executable
;; through the pure-elisp PE writer: import kernel32.dll!ExitProcess and
;; call it with a chosen status code.

;;; Code:

(require 'nelisp-pe-write)

(defun nelisp-windows-build-exitprocess (out-path exit-code)
  "Write OUT-PATH as a PE32+ EXE calling ExitProcess(EXIT-CODE)."
  (nelisp-pe-write-exitprocess-executable out-path exit-code)
  (message "nelisp-windows-build: wrote %s (ExitProcess %d)"
           out-path exit-code)
  out-path)

(defun nelisp-windows-build-exit42 ()
  "Batch entry: build target/nelisp-windows-exit42.exe."
  (nelisp-windows-build-exitprocess "target/nelisp-windows-exit42.exe" 42))

(provide 'nelisp-windows-build)

;;; nelisp-windows-build.el ends here
