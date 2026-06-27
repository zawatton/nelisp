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

;;;; ---- P2: import a libc symbol via GLOB_DAT, call through the GOT ----

(ert-deftest nelisp-elf-dynamic-import-toupper ()
  "Import libc `toupper' via R_X86_64_GLOB_DAT into a .got slot; the entry
loads the ld.so-resolved address and calls it.  toupper(?a=97) => ?A=65, so a
correct dynamic resolve + GOT call exits 65 (a crash/unresolved GOT would
segfault instead)."
  (skip-unless (memq system-type '(gnu/linux gnu)))
  (skip-unless (string-match-p "x86_64\\|amd64"
                               (or (bound-and-true-p system-configuration) "")))
  (skip-unless (file-readable-p "/lib/x86_64-linux-gnu/libc.so.6"))
  (let* ((text-fn
          (lambda (got-map)
            (let ((va (cdr (assoc "toupper" got-map))))
              (concat (unibyte-string #xbf #x61 0 0 0)               ; mov edi,97
                      (unibyte-string #x48 #xb8) (nelisp-elf--u64le va) ; movabs rax,va
                      (unibyte-string #x48 #x8b #x00)                ; mov rax,[rax]
                      (unibyte-string #xff #xd0)                     ; call rax
                      (unibyte-string #x89 #xc7)                     ; mov edi,eax
                      (unibyte-string #xb8 #x3c 0 0 0)               ; mov eax,60
                      (unibyte-string #x0f #x05)))))                 ; syscall
         (path (nelisp-elf-dynamic-test--write
                (nelisp-elf-build-dynamic-binary
                 (list :imports '(("libc.so.6" . "toupper")) :text-fn text-fn)))))
    (unwind-protect
        (should (eq 65 (call-process path nil nil nil)))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-dynamic-import-readelf ()
  "The imported binary declares DT_NEEDED libc.so.6 + a DT_RELA table."
  (skip-unless (executable-find "readelf"))
  (let* ((text-fn (lambda (got-map)
                    (ignore (cdr (assoc "toupper" got-map)))
                    (unibyte-string #xbf #x2a 0 0 0 #xb8 #x3c 0 0 0 #x0f #x05)))
         (path (nelisp-elf-dynamic-test--write
                (nelisp-elf-build-dynamic-binary
                 (list :imports '(("libc.so.6" . "toupper")) :text-fn text-fn)))))
    (unwind-protect
        (let ((out (with-output-to-string
                     (with-current-buffer standard-output
                       (call-process "readelf" nil t nil "-d" path)))))
          (should (string-match-p "NEEDED" out))
          (should (string-match-p "libc\\.so\\.6" out))
          (should (string-match-p "RELA" out)))
      (ignore-errors (delete-file path)))))

;;;; ---- P3: link a compile unit into a dynamic ELF (linker integration) ----

(ert-deftest nelisp-elf-dynamic-link-units-import ()
  "`nelisp-link-units-dynamic' links a compile unit whose .text calls libc
`toupper' via a `__got_toupper' abs64 reloc (resolved by the 2-pass linker to
the GOT slot VA, filled by ld.so).  toupper(97)=>65, so it exits 65."
  (skip-unless (memq system-type '(gnu/linux gnu)))
  (skip-unless (string-match-p "x86_64\\|amd64"
                               (or (bound-and-true-p system-configuration) "")))
  (skip-unless (file-readable-p "/lib/x86_64-linux-gnu/libc.so.6"))
  (require 'nelisp-static-linker)
  (let* ((text (unibyte-string
                #xbf #x61 0 0 0                  ; mov edi,97
                #x48 #xb8 0 0 0 0 0 0 0 0        ; movabs rax,__got_toupper (imm@7)
                #x48 #x8b #x00                   ; mov rax,[rax]
                #xff #xd0                        ; call rax
                #x89 #xc7                        ; mov edi,eax
                #xb8 #x3c 0 0 0                  ; mov eax,60
                #x0f #x05))                      ; syscall
         (unit (nelisp-link-unit-make
                "main.o" (list (cons 'text text))
                (list (nelisp-link-symbol "_start" 0))
                (list (append (nelisp-link-reloc 7 'abs64 "__got_toupper" 0)
                              '(:section text)))))
         (path (make-temp-file "nelisp-p3-")))
    (unwind-protect
        (progn
          (nelisp-link-units-dynamic path (list unit)
                                     '(("libc.so.6" . "toupper")))
          (set-file-modes path #o755)
          (should (eq 65 (call-process path nil nil nil))))
      (ignore-errors (delete-file path)))))

(provide 'nelisp-elf-dynamic-test)
;;; nelisp-elf-dynamic-test.el ends here
