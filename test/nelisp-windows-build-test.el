;;; nelisp-windows-build-test.el --- ERT tests for Windows PE build entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 138 Stage 3 — structure tests for Phase47 -> Win64 PE32+ EXE emit.

;;; Code:

(require 'ert)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (root (and test-dir (expand-file-name ".." test-dir))))
  (when root
    (add-to-list 'load-path (expand-file-name "lisp" root))
    (add-to-list 'load-path (expand-file-name "src" root))
    (add-to-list 'load-path (expand-file-name "scripts" root))))

(require 'nelisp-windows-build)

(defun nelisp-windows-build-test--read-le32 (bytes offset)
  "Read unsigned 32-bit little-endian integer from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)
          (ash (aref bytes (+ offset 2)) 16)
          (ash (aref bytes (+ offset 3)) 24)))

(defun nelisp-windows-build-test--read-le64 (bytes offset)
  "Read unsigned 64-bit little-endian integer from BYTES at OFFSET."
  (let ((acc 0)
        (i 7))
    (while (>= i 0)
      (setq acc (logior (ash acc 8) (aref bytes (+ offset i))))
      (setq i (1- i)))
    acc))

(defun nelisp-windows-build-test--read-cstr (bytes offset)
  "Read a NUL-terminated ASCII string from BYTES at OFFSET."
  (let ((end offset))
    (while (and (< end (length bytes))
                (/= (aref bytes end) 0))
      (setq end (1+ end)))
    (substring bytes offset end)))

(defun nelisp-windows-build-test--phase47-exe (&optional sexp)
  "Return PE32+ EXE bytes for SEXP, defaulting to `(exit 42)'."
  (nelisp-windows-build--phase47-executable-bytes (or sexp '(exit 42))))

(ert-deftest nelisp-windows-build-phase47-exit42-imports-exitprocess ()
  "Phase47 Windows EXE imports KERNEL32.dll!ExitProcess."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe))
         (peoff (nelisp-windows-build-test--read-le32 bytes #x3c))
         (opt (+ peoff 24))
         (import-rva (nelisp-windows-build-test--read-le32 bytes (+ opt 120)))
         (rdata-rva #x2000)
         (rdata-raw #x400)
         (import-off (+ rdata-raw (- import-rva rdata-rva)))
         (oft (nelisp-windows-build-test--read-le32 bytes import-off))
         (name-rva (nelisp-windows-build-test--read-le32 bytes (+ import-off 12)))
         (iat-rva (nelisp-windows-build-test--read-le32 bytes (+ import-off 16)))
         (hint-name-rva (nelisp-windows-build-test--read-le64
                         bytes (+ rdata-raw (- oft rdata-rva)))))
    (should (= import-rva #x2000))
    (should (= iat-rva #x2038))
    (should (equal (nelisp-windows-build-test--read-cstr
                    bytes (+ rdata-raw (- name-rva rdata-rva)))
                   "KERNEL32.dll"))
    (should (equal (nelisp-windows-build-test--read-cstr
                    bytes (+ rdata-raw (- hint-name-rva rdata-rva) 2))
                   "ExitProcess"))))

(ert-deftest nelisp-windows-build-phase47-exit42-text-calls-iat ()
  "Phase47 `(exit 42)' emits Win64 ExitProcess call, not a Linux syscall."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe))
         (text-raw #x200)
         (text-rva #x1000)
         (iat-rva #x2038)
         (text (substring bytes text-raw (+ text-raw 16)))
         (disp (nelisp-windows-build-test--read-le32 bytes (+ text-raw 11))))
    (should (equal (substring text 0 4)
                   (unibyte-string #x48 #x83 #xec #x28)))
    (should (= (aref text 4) #xb9))
    (should (= (nelisp-windows-build-test--read-le32 text 5) 42))
    (should (equal (substring text 9 11)
                   (unibyte-string #xff #x15)))
    (should (= (+ text-rva 15 disp) iat-rva))
    (should (= (aref text 15) #xcc))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-phase47-dynamic-exit-moves-rax-to-rcx ()
  "Computed Phase47 exits pass rax to ExitProcess via Win64 rcx."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq (defun id (x) x) (exit (id 42)))))
         (text-raw #x200)
         (text (substring bytes text-raw (+ text-raw 80))))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x89 #xc1))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #xff #x15))
             text))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-phase47-write-rejected ()
  "Windows Phase47 EXE path rejects Linux-only `write' for now."
  (should-error
   (nelisp-windows-build--phase47-executable-bytes
    '(seq (write "x") (exit 0)))
   :type 'nelisp-phase47-compiler-error))

(provide 'nelisp-windows-build-test)

;;; nelisp-windows-build-test.el ends here
