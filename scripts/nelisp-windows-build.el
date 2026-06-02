;;; nelisp-windows-build.el --- native Windows x86_64 build entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 138 Stage 1/2/3.  Build native Windows PE32+ executables through
;; the pure-elisp PE writer, starting with ExitProcess and VirtualAlloc
;; import-table probes, then wiring Phase47 `(exit ...)' through Win64
;; KERNEL32.dll!ExitProcess.

;;; Code:

(require 'nelisp-pe-write)
(require 'nelisp-phase47-compiler)

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

(defun nelisp-windows-build--phase47-text (sexp text-rva iat-rvas)
  "Return Win64 .text bytes for Phase47 SEXP using TEXT-RVA and IAT-RVAS."
  (let ((exitprocess-iat (cdr (assoc "ExitProcess" iat-rvas))))
    (unless exitprocess-iat
      (error "nelisp-windows-build: missing ExitProcess IAT RVA"))
    (let* ((nelisp-phase47-compiler--label-counter 0)
           (nelisp-phase47-compiler--arch 'x86_64)
           (nelisp-phase47-compiler--os 'windows)
           (nelisp-phase47-compiler--abi 'win64)
           (nelisp-phase47-compiler--windows-text-rva text-rva)
           (nelisp-phase47-compiler--windows-exitprocess-iat-rva exitprocess-iat)
           (ir (nelisp-phase47-compiler--parse sexp nil))
           (collected (nelisp-phase47-compiler--collect-strings ir))
           (str-offsets (car collected))
           (str-rodata-bytes (cdr collected))
           (table-collected (nelisp-phase47-compiler--collect-tables ir))
           (table-offsets (car table-collected))
           (table-bytes (cdr table-collected))
           (defuns (nelisp-phase47-compiler--collect-defuns ir)))
      (when (or (> (length str-rodata-bytes) 0)
                (> (length table-bytes) 0))
        (signal 'nelisp-phase47-compiler-error
                (list :windows-rodata-not-yet-supported)))
      (let* ((pass1-table-vaddrs
              (mapcar (lambda (entry) (cons (car entry) 0)) table-offsets))
             (pass1 (nelisp-phase47-compiler--pass
                     ir defuns str-offsets 0 pass1-table-vaddrs))
             (text-size (nelisp-asm-x86_64-buffer-pos pass1))
             (table-vaddrs
              (mapcar (lambda (entry)
                        (let ((name (car entry)))
                          (cons name 0)))
                      table-offsets))
             (pass2 (nelisp-phase47-compiler--pass
                     ir defuns str-offsets 0 table-vaddrs))
             (text-bytes (nelisp-asm-x86_64-resolve-fixups pass2)))
        (ignore pass1)
        (unless (= (length text-bytes) text-size)
          (signal 'nelisp-phase47-compiler-error
                  (list :pass-length-mismatch
                        :pass1 text-size
                        :pass2 (length text-bytes))))
        text-bytes))))

(defun nelisp-windows-build--phase47-executable-bytes (sexp)
  "Return a PE32+ EXE byte string for Phase47 SEXP."
  (nelisp-pe-write-build-kernel32-executable
   '("ExitProcess")
   (lambda (text-rva iat-rvas)
     (nelisp-windows-build--phase47-text sexp text-rva iat-rvas))))

(defun nelisp-windows-build-phase47-exe (sexp out-path)
  "Write OUT-PATH as a PE32+ EXE compiled from Phase47 SEXP."
  (let ((bytes (nelisp-windows-build--phase47-executable-bytes sexp))
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent))
  (message "nelisp-windows-build: wrote %s (Phase47 %S)" out-path sexp)
  out-path)

(defun nelisp-windows-build-phase47-exit42 ()
  "Batch entry: build target/nelisp-windows-phase47-exit42.exe."
  (nelisp-windows-build-phase47-exe
   '(exit 42)
   "target/nelisp-windows-phase47-exit42.exe"))

(provide 'nelisp-windows-build)

;;; nelisp-windows-build.el ends here
