;;; nelisp-windows-build.el --- native Windows x86_64 build entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 138 Stage 1/2/3/4/5/6/7.  Build native Windows PE32+ executables through
;; the pure-elisp PE writer, starting with ExitProcess and VirtualAlloc
;; import-table probes, then wiring Phase47 `(exit ...)' through Win64
;; KERNEL32.dll!ExitProcess, `(write ...)' through WriteFile, and
;; `alloc-bytes' / `dealloc-bytes' through VirtualAlloc / VirtualFree.
;; Stage 7 maps stdio-shaped `syscall-direct' read/write calls to
;; GetStdHandle + ReadFile/WriteFile.

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

(defun nelisp-windows-build--sexp-contains-symbol-p (sexp symbol)
  "Return non-nil when SEXP contains SYMBOL as a list head."
  (cond
   ((atom sexp) nil)
   ((eq (car sexp) symbol) t)
   (t (let ((tail sexp)
            (found nil))
        (while (and tail (not found))
          (setq found (nelisp-windows-build--sexp-contains-symbol-p
                       (car tail) symbol))
          (setq tail (cdr tail)))
        found))))

(defun nelisp-windows-build--sexp-contains-syscall-direct-p (sexp nr fds)
  "Return non-nil when SEXP contains `(syscall-direct NR FD ...)'.
FDS is the list of accepted immediate fd values."
  (cond
   ((atom sexp) nil)
   ((and (eq (car sexp) 'syscall-direct)
         (= (length sexp) 8)
         (equal (nth 1 sexp) nr)
         (member (nth 2 sexp) fds)
         (equal (nth 5 sexp) 0)
         (equal (nth 6 sexp) 0)
         (equal (nth 7 sexp) 0))
    t)
   (t (let ((tail sexp)
            (found nil))
        (while (and tail (not found))
          (setq found
                (nelisp-windows-build--sexp-contains-syscall-direct-p
                 (car tail) nr fds))
          (setq tail (cdr tail)))
        found))))

(defun nelisp-windows-build--append-import-names (names additions)
  "Return NAMES with ADDITIONS appended once, preserving order."
  (let ((out names))
    (dolist (name additions)
      (unless (member name out)
        (setq out (append out (list name)))))
    out))

(defun nelisp-windows-build--phase47-import-names (sexp str-rodata-bytes)
  "Return KERNEL32 import names needed by SEXP and STR-RODATA-BYTES."
  (let ((names '("ExitProcess")))
    (when (> (length str-rodata-bytes) 0)
      (setq names
            (nelisp-windows-build--append-import-names
             names '("GetStdHandle" "WriteFile"))))
    (when (nelisp-windows-build--sexp-contains-syscall-direct-p sexp 0 '(0))
      (setq names
            (nelisp-windows-build--append-import-names
             names '("GetStdHandle" "ReadFile"))))
    (when (nelisp-windows-build--sexp-contains-syscall-direct-p sexp 1 '(1 2))
      (setq names
            (nelisp-windows-build--append-import-names
             names '("GetStdHandle" "WriteFile"))))
    (when (nelisp-windows-build--sexp-contains-symbol-p sexp 'alloc-bytes)
      (setq names
            (nelisp-windows-build--append-import-names
             names '("VirtualAlloc"))))
    (when (nelisp-windows-build--sexp-contains-symbol-p sexp 'dealloc-bytes)
      (setq names
            (nelisp-windows-build--append-import-names
             names '("VirtualFree"))))
    names))

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
        (writefile-iat (cdr (assoc "WriteFile" iat-rvas)))
        (readfile-iat (cdr (assoc "ReadFile" iat-rvas))))
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
           (nelisp-phase47-compiler--windows-readfile-iat-rva readfile-iat)
           (nelisp-phase47-compiler--windows-virtualalloc-iat-rva
            (cdr (assoc "VirtualAlloc" iat-rvas)))
           (nelisp-phase47-compiler--windows-virtualfree-iat-rva
            (cdr (assoc "VirtualFree" iat-rvas)))
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
         (imports (nelisp-windows-build--phase47-import-names sexp rodata-bytes)))
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

(defun nelisp-windows-build-phase47-alloc42 ()
  "Batch entry: build target/nelisp-windows-phase47-alloc42.exe."
  (nelisp-windows-build-phase47-exe
   '(exit (if (= (alloc-bytes 4096 8) 0) 13 42))
   "target/nelisp-windows-phase47-alloc42.exe"))

(defun nelisp-windows-build-phase47-alloc-free42 ()
  "Batch entry: build target/nelisp-windows-phase47-alloc-free42.exe."
  (nelisp-windows-build-phase47-exe
   '(seq
     (defun alloc_free_probe ()
       (let* ((p (alloc-bytes 4096 8)))
         (if (= p 0)
             13
           (seq (dealloc-bytes p 4096 8) 42))))
     (exit (alloc_free_probe)))
   "target/nelisp-windows-phase47-alloc-free42.exe"))

(defun nelisp-windows-build-phase47-syswrite42 ()
  "Batch entry: build target/nelisp-windows-phase47-syswrite42.exe."
  (nelisp-windows-build-phase47-exe
   '(seq
     (defun syswrite_probe ()
       (let* ((p (alloc-bytes 1 1)))
         (if (= p 0)
             13
           (seq
            (ptr-write-u8 p 0 88)
            (syscall-direct 1 1 p 1 0 0 0)
            (dealloc-bytes p 1 1)
            42))))
     (exit (syswrite_probe)))
   "target/nelisp-windows-phase47-syswrite42.exe"))

(defun nelisp-windows-build-phase47-sysread-byte ()
  "Batch entry: build target/nelisp-windows-phase47-sysread-byte.exe."
  (nelisp-windows-build-phase47-exe
   '(seq
     (defun sysread_probe ()
       (let* ((p (alloc-bytes 1 1)))
         (if (= p 0)
             13
           (let* ((n (syscall-direct 0 0 p 1 0 0 0))
                  (b (ptr-read-u8 p 0)))
             (seq
              (dealloc-bytes p 1 1)
              (if (= n 1) b 14))))))
     (exit (sysread_probe)))
   "target/nelisp-windows-phase47-sysread-byte.exe"))

(provide 'nelisp-windows-build)

;;; nelisp-windows-build.el ends here
