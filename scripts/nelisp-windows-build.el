;;; nelisp-windows-build.el --- native Windows x86_64 build entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 138 Stage 1/2/3/4.  Build native Windows PE32+ executables through
;; the pure-elisp PE writer, starting with ExitProcess and VirtualAlloc
;; import-table probes, then wiring Phase47 `(exit ...)' through Win64
;; KERNEL32.dll!ExitProcess and `(write ...)' through WriteFile.

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

(defun nelisp-windows-build--phase47-import-names (str-rodata-bytes)
  "Return KERNEL32 import names needed by STR-RODATA-BYTES."
  (if (> (length str-rodata-bytes) 0)
      '("ExitProcess" "GetStdHandle" "WriteFile")
    '("ExitProcess")))

(defun nelisp-windows-build--phase47-rodata-bytes (sexp)
  "Return Phase47 string rodata bytes for SEXP."
  (let* ((nelisp-phase47-compiler--label-counter 0)
         (ir (nelisp-phase47-compiler--parse sexp nil))
         (collected (nelisp-phase47-compiler--collect-strings ir)))
    (cdr collected)))

(defun nelisp-windows-build--phase47-text (sexp text-rva iat-rvas rodata-rva)
  "Return Win64 .text bytes for Phase47 SEXP.
TEXT-RVA is the PE .text RVA, IAT-RVAS maps imported functions to IAT
slot RVAs, and RODATA-RVA is byte 0 of the appended string rodata."
  (let ((exitprocess-iat (cdr (assoc "ExitProcess" iat-rvas)))
        (getstdhandle-iat (cdr (assoc "GetStdHandle" iat-rvas)))
        (writefile-iat (cdr (assoc "WriteFile" iat-rvas))))
    (unless exitprocess-iat
      (error "nelisp-windows-build: missing ExitProcess IAT RVA"))
    (let* ((nelisp-phase47-compiler--label-counter 0)
           (nelisp-phase47-compiler--arch 'x86_64)
           (nelisp-phase47-compiler--os 'windows)
           (nelisp-phase47-compiler--abi 'win64)
           (nelisp-phase47-compiler--windows-text-rva text-rva)
           (nelisp-phase47-compiler--windows-exitprocess-iat-rva exitprocess-iat)
           (nelisp-phase47-compiler--windows-getstdhandle-iat-rva getstdhandle-iat)
           (nelisp-phase47-compiler--windows-writefile-iat-rva writefile-iat)
           (ir (nelisp-phase47-compiler--parse sexp nil))
           (collected (nelisp-phase47-compiler--collect-strings ir))
           (str-offsets (car collected))
           (table-collected (nelisp-phase47-compiler--collect-tables ir))
           (table-offsets (car table-collected))
           (table-bytes (cdr table-collected))
           (defuns (nelisp-phase47-compiler--collect-defuns ir)))
      (when (> (length table-bytes) 0)
        (signal 'nelisp-phase47-compiler-error
                (list :windows-tables-not-yet-supported)))
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
                     ir defuns str-offsets rodata-rva table-vaddrs))
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
  (let* ((rodata-bytes (nelisp-windows-build--phase47-rodata-bytes sexp))
         (imports (nelisp-windows-build--phase47-import-names rodata-bytes)))
    (nelisp-pe-write-build-kernel32-executable
     imports
     (lambda (text-rva iat-rvas rodata-rva)
       (nelisp-windows-build--phase47-text
        sexp text-rva iat-rvas rodata-rva))
     rodata-bytes)))

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

(defun nelisp-windows-build-phase47-hello ()
  "Batch entry: build target/nelisp-windows-phase47-hello.exe."
  (nelisp-windows-build-phase47-exe
   '(seq (write "hello\n") (exit 42))
   "target/nelisp-windows-phase47-hello.exe"))

(provide 'nelisp-windows-build)

;;; nelisp-windows-build.el ends here
