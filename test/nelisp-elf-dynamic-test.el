;;; nelisp-elf-dynamic-test.el --- ERT tests for dynamic-linked ELF (Phase 47.D)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; docs/design/100-phase-47-dynamic-link-elisp.org P1 — pure-elisp tests for the
;; dynamic-linking ELF foundations in `nelisp-elf-write':
;;   (1) byte/int + elf_hash + .hash / .dynstr / Elf64_Dyn builders (unit),
;;   (2) `nelisp-elf-build-dynamic-binary' end-to-end: emit a minimal
;;       dynamically linked ET_EXEC that ld.so loads and runs (exit 42), and
;;       cross-check with `readelf -d' (a DYNAMIC section is present).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-elf-write)

;;;; ---- unit: little-endian + elf_hash ----

(ert-deftest nelisp-elf-dynamic-le-helpers ()
  (should (equal (nelisp-elf--u16le #x0201) (unibyte-string 1 2)))
  (should (equal (nelisp-elf--u32le #x04030201) (unibyte-string 1 2 3 4)))
  (should (equal (nelisp-elf--u64le #x0807060504030201)
                 (unibyte-string 1 2 3 4 5 6 7 8))))

(ert-deftest nelisp-elf-dynamic-elf-hash ()
  ;; Canonical SysV elf_hash values (cross-checked against a reference impl).
  (should (= (nelisp-elf--elf-hash "") 0))
  (should (= (nelisp-elf--elf-hash "a") #x61))
  (should (= (nelisp-elf--elf-hash "ab") #x672))
  (should (= (nelisp-elf--elf-hash "printf") 125371814))
  (should (= (nelisp-elf--elf-hash "aaaaaaaa") 125268225)) ; exercises folding
  (should (= (nelisp-elf--elf-hash "gnutls_handshake") 260464085))
  ;; elf_hash always fits 28 bits (top nibble folded away).
  (should (= 0 (logand (nelisp-elf--elf-hash "anylongsymbolname0123456789")
                       #xf0000000))))

(ert-deftest nelisp-elf-dynamic-builders ()
  ;; Elf64_Dyn = 16 bytes (tag u64 + val u64).
  (let ((b (nelisp-elf-dyn-bytes nelisp-elf--dt-strsz #x1234)))
    (should (= (length b) 16))
    (should (equal b (concat (nelisp-elf--u64le 10) (nelisp-elf--u64le #x1234)))))
  ;; .dynstr begins with NUL; entries are NUL-terminated.
  (let* ((r (nelisp-elf-build-dynstr '("libc.so.6" "getpid"))) (bytes (car r)))
    (should (= (aref bytes 0) 0))
    (should (= (cdr (assoc "libc.so.6" (cdr r))) 1))
    (should (= (aref bytes 10) 0)))
  ;; SysV .hash header = nbucket, nchain (= nsym).
  (let* ((h (nelisp-elf-build-sysv-hash '("" "getpid" "printf"))))
    (should (= (length h) (* 4 8)))            ; 2 hdr + 3 bucket + 3 chain
    (should (= (aref h 0) 3))                  ; nbucket low byte
    (should (= (aref h 4) 3))))                ; nchain low byte

;;;; ---- end-to-end: ld.so loads + runs the dynamic ELF ----

(defun nelisp-elf-dynamic-test--write (bytes)
  "Write unibyte BYTES to a fresh executable temp file; return its path."
  (let ((path (make-temp-file "nelisp-dynelf-"))
        (coding-system-for-write 'binary))
    (with-temp-file path (set-buffer-multibyte nil) (insert bytes))
    (set-file-modes path #o755)
    path))

;; exit(42): mov edi,42 ; mov eax,60 ; syscall
(defconst nelisp-elf-dynamic-test--exit42
  (unibyte-string #xbf #x2a 0 0 0  #xb8 #x3c 0 0 0  #x0f #x05))

(ert-deftest nelisp-elf-dynamic-exec-exit-42 ()
  "A minimal dynamically linked ELF loads via ld.so and runs (exit 42)."
  (skip-unless (memq system-type '(gnu/linux gnu)))
  (skip-unless (string-match-p "x86_64\\|amd64"
                               (or (bound-and-true-p system-configuration) "")))
  (let ((path (nelisp-elf-dynamic-test--write
               (nelisp-elf-build-dynamic-binary
                (list :text nelisp-elf-dynamic-test--exit42)))))
    (unwind-protect
        (should (eq 42 (call-process path nil nil nil)))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-dynamic-readelf-d ()
  "`readelf -d' reports a DYNAMIC section on the emitted binary."
  (skip-unless (executable-find "readelf"))
  (let ((path (nelisp-elf-dynamic-test--write
               (nelisp-elf-build-dynamic-binary
                (list :text nelisp-elf-dynamic-test--exit42)))))
    (unwind-protect
        (let ((out (with-output-to-string
                     (with-current-buffer standard-output
                       (call-process "readelf" nil t nil "-d" path)))))
          (should (string-match-p "Dynamic section" out))
          (should (string-match-p "SYMTAB" out))
          (should (string-match-p "HASH" out)))
      (ignore-errors (delete-file path)))))

(provide 'nelisp-elf-dynamic-test)
;;; nelisp-elf-dynamic-test.el ends here
