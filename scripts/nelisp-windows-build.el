;;; nelisp-windows-build.el --- native Windows x86_64 build entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 138 Stage 1/2.  Build native Windows PE32+ executables through
;; the pure-elisp PE writer, starting with ExitProcess and VirtualAlloc
;; import-table probes.

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

(defun nelisp-windows-build-virtualalloc-probe (out-path)
  "Write OUT-PATH as a PE32+ EXE probing VirtualAlloc.
The program exits 42 on allocation success and 13 on failure."
  (nelisp-pe-write-virtualalloc-executable out-path 42 13)
  (message "nelisp-windows-build: wrote %s (VirtualAlloc probe)" out-path)
  out-path)

(defun nelisp-windows-build-virtualalloc42 ()
  "Batch entry: build target/nelisp-windows-virtualalloc42.exe."
  (nelisp-windows-build-virtualalloc-probe
   "target/nelisp-windows-virtualalloc42.exe"))

(provide 'nelisp-windows-build)

;;; nelisp-windows-build.el ends here
