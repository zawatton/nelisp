;;; nelisp-elf-write-test.el --- ERT tests for ELF writer §91.a  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 91 §91.a — pure-elisp ert tests for the `nelisp-elf-write'
;; module.  Exercises (1) byte/int conversion helpers, (2) Ehdr +
;; Phdr serialisers in isolation, and (3) the `minimal-exit-0'
;; orchestrator end-to-end including a `chmod +x' / exec smoke test
;; and a `readelf -h' cross-check when the host has it.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir
                      (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-elf-write)

;; ---------------------------------------------------------------- helpers

(defun nelisp-elf-write-test--collect (writer &rest args)
  "Call WRITER with a unibyte temp-buffer and ARGS.  Return string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (apply writer (current-buffer) args)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun nelisp-elf-write-test--read-file-bytes (path)
  "Return raw unibyte bytes of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally path))
    (buffer-substring-no-properties (point-min) (point-max))))

;; ---------------------------------------------------------------- helpers L0

(ert-deftest nelisp-elf-write-le16-roundtrip ()
  "le16 emits two bytes in LE order and round-trips via the reader."
  (let ((s (nelisp-elf-write-test--collect #'nelisp-elf--write-le16 #x3E)))
    (should (= (length s) 2))
    (should (= (aref s 0) #x3E))
    (should (= (aref s 1) #x00))
    (should (= (nelisp-elf--read-le16 s 0) #x3E)))
  (let ((s (nelisp-elf-write-test--collect #'nelisp-elf--write-le16 #xBEEF)))
    (should (= (aref s 0) #xEF))
    (should (= (aref s 1) #xBE))
    (should (= (nelisp-elf--read-le16 s 0) #xBEEF))))

(ert-deftest nelisp-elf-write-le32-roundtrip ()
  "le32 emits four bytes in LE order."
  (let ((s (nelisp-elf-write-test--collect #'nelisp-elf--write-le32
                                           #x12345678)))
    (should (= (length s) 4))
    (should (= (aref s 0) #x78))
    (should (= (aref s 1) #x56))
    (should (= (aref s 2) #x34))
    (should (= (aref s 3) #x12))
    (should (= (nelisp-elf--read-le32 s 0) #x12345678))))

(ert-deftest nelisp-elf-write-le64-roundtrip ()
  "le64 emits eight bytes in LE order and survives bignum-sized values."
  (let* ((v #x0123456789ABCDEF)
         (s (nelisp-elf-write-test--collect #'nelisp-elf--write-le64 v)))
    (should (= (length s) 8))
    (should (= (aref s 0) #xEF))
    (should (= (aref s 7) #x01))
    (should (= (nelisp-elf--read-le64 s 0) v)))
  ;; zero stays zero
  (let ((s (nelisp-elf-write-test--collect #'nelisp-elf--write-le64 0)))
    (should (= (length s) 8))
    (should (cl-every (lambda (b) (zerop b)) (append s nil)))))

(ert-deftest nelisp-elf-write-bytes-pad-strz ()
  "Raw / pad / strz helpers behave per spec."
  (let ((s (nelisp-elf-write-test--collect
            #'nelisp-elf--write-bytes
            (unibyte-string #xDE #xAD #xBE #xEF))))
    (should (= (length s) 4))
    (should (= (aref s 0) #xDE)))
  ;; default pad value = 0
  (let ((s (nelisp-elf-write-test--collect #'nelisp-elf--write-pad 5)))
    (should (= (length s) 5))
    (should (cl-every (lambda (b) (zerop b)) (append s nil))))
  ;; explicit pad value
  (let ((s (nelisp-elf-write-test--collect #'nelisp-elf--write-pad 3 #xCC)))
    (should (equal s (unibyte-string #xCC #xCC #xCC))))
  ;; strz appends NUL
  (let ((s (nelisp-elf-write-test--collect #'nelisp-elf--write-strz "abc")))
    (should (= (length s) 4))
    (should (= (aref s 3) 0))))

;; ---------------------------------------------------------------- Ehdr L1

(ert-deftest nelisp-elf-write-ehdr-shape ()
  "Ehdr serialises to exactly 64 bytes with the correct ident block."
  (let* ((s (nelisp-elf-write-test--collect
             #'nelisp-elf-write-ehdr
             (list :entry #x401000 :phoff 64 :phnum 1
                   :shoff 0 :shnum 0 :shstrndx 0))))
    (should (= (length s) 64))
    ;; magic
    (should (equal (substring s 0 4) (unibyte-string #x7F #x45 #x4C #x46)))
    (should (= (aref s 4) 2))   ; ELFCLASS64
    (should (= (aref s 5) 1))   ; ELFDATA2LSB
    (should (= (aref s 6) 1))   ; EV_CURRENT
    (should (= (aref s 7) 0))   ; ELFOSABI_NONE
    ;; e_ident[8..15] zero pad
    (dotimes (i 8)
      (should (= (aref s (+ 8 i)) 0)))
    ;; e_type / e_machine
    (should (= (nelisp-elf--read-le16 s 16) 2))    ; ET_EXEC
    (should (= (nelisp-elf--read-le16 s 18) 62))   ; EM_X86_64
    ;; e_version
    (should (= (nelisp-elf--read-le32 s 20) 1))
    ;; e_entry
    (should (= (nelisp-elf--read-le64 s 24) #x401000))
    ;; e_phoff
    (should (= (nelisp-elf--read-le64 s 32) 64))
    ;; e_ehsize / e_phentsize / e_phnum / e_shentsize
    (should (= (nelisp-elf--read-le16 s 52) 64))
    (should (= (nelisp-elf--read-le16 s 54) 56))
    (should (= (nelisp-elf--read-le16 s 56) 1))
    (should (= (nelisp-elf--read-le16 s 58) 64))))

(ert-deftest nelisp-elf-write-ehdr-aarch64 ()
  "Switching :machine to EM_AARCH64 lands at offset 18 as 0xB7."
  (let ((s (nelisp-elf-write-test--collect
            #'nelisp-elf-write-ehdr
            (list :machine 183 :entry 0 :phoff 64
                  :phnum 0 :shnum 0 :shstrndx 0))))
    (should (= (nelisp-elf--read-le16 s 18) 183))))

;; ---------------------------------------------------------------- Phdr L1

(ert-deftest nelisp-elf-write-phdr-shape ()
  "Phdr serialises to exactly 56 bytes; flags / offsets land where expected."
  (let* ((s (nelisp-elf-write-test--collect
             #'nelisp-elf-write-phdr
             (list :type 1
                   :flags 5
                   :offset 0
                   :vaddr  #x400000
                   :paddr  #x400000
                   :filesz #x100
                   :memsz  #x100
                   :align  #x1000))))
    (should (= (length s) 56))
    (should (= (nelisp-elf--read-le32 s 0) 1))         ; PT_LOAD
    (should (= (nelisp-elf--read-le32 s 4) 5))         ; PF_R | PF_X
    (should (= (nelisp-elf--read-le64 s 8) 0))         ; p_offset
    (should (= (nelisp-elf--read-le64 s 16) #x400000)) ; p_vaddr
    (should (= (nelisp-elf--read-le64 s 24) #x400000)) ; p_paddr
    (should (= (nelisp-elf--read-le64 s 32) #x100))    ; p_filesz
    (should (= (nelisp-elf--read-le64 s 40) #x100))    ; p_memsz
    (should (= (nelisp-elf--read-le64 s 48) #x1000)))) ; p_align

(ert-deftest nelisp-elf-write-phdr-defaults ()
  "Phdr defaults: paddr falls back to vaddr, memsz to filesz, align to 4 KiB."
  (let* ((s (nelisp-elf-write-test--collect
             #'nelisp-elf-write-phdr
             (list :vaddr  #xABC000 :filesz 16))))
    (should (= (nelisp-elf--read-le32 s 0) 1))         ; default PT_LOAD
    (should (= (nelisp-elf--read-le32 s 4) 5))         ; default PF_R | PF_X
    (should (= (nelisp-elf--read-le64 s 16) #xABC000)) ; vaddr
    (should (= (nelisp-elf--read-le64 s 24) #xABC000)) ; paddr defaulted
    (should (= (nelisp-elf--read-le64 s 32) 16))       ; filesz
    (should (= (nelisp-elf--read-le64 s 40) 16))       ; memsz defaulted
    (should (= (nelisp-elf--read-le64 s 48) #x1000)))) ; align default

;; ---------------------------------------------------------------- L2 round-trip

(ert-deftest nelisp-elf-write-binary-file-shape ()
  "minimal-exit-0 emits a 127-byte file with valid Ehdr + Phdr."
  (let ((path (make-temp-file "nelisp-elf-test-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary path 'minimal-exit-0)
          (let ((bytes (nelisp-elf-write-test--read-file-bytes path)))
            (should (= (length bytes) (+ 64 56 7)))
            (should (equal (substring bytes 0 4)
                           (unibyte-string #x7F #x45 #x4C #x46)))
            (should (= (aref bytes 4) 2))                  ; ELFCLASS64
            (should (= (aref bytes 5) 1))                  ; ELFDATA2LSB
            (should (= (nelisp-elf--read-le16 bytes 16) 2)) ; ET_EXEC
            (should (= (nelisp-elf--read-le16 bytes 18) 62)) ; EM_X86_64
            (should (= (nelisp-elf--read-le16 bytes 56) 1))  ; e_phnum
            ;; Phdr lives at offset 64; first dword = PT_LOAD
            (should (= (nelisp-elf--read-le32 bytes 64) 1))
            ;; .text begins at offset 120 (= 64 + 56)
            (should (= (aref bytes 120) #xb8))
            (should (= (aref bytes 124) #x00))
            (should (= (aref bytes 125) #x0f))
            (should (= (aref bytes 126) #x05))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-binary-rejects-unknown ()
  "Unknown SECTIONS arguments signal an error."
  (let ((path (make-temp-file "nelisp-elf-test-")))
    (unwind-protect
        (should-error
         (nelisp-elf-write-binary path 'something-else))
      (ignore-errors (delete-file path)))))

;; ---------------------------------------------------------------- L3 exec

(ert-deftest nelisp-elf-write-binary-exec-exit-0 ()
  "The emitted minimal-exit-0 binary runs and returns exit code 0."
  (skip-unless (memq system-type '(gnu/linux gnu)))
  ;; Only meaningful on x86_64 hosts.
  (skip-unless (let ((arch (or (and (boundp 'system-configuration)
                                    system-configuration)
                               "")))
                 (string-match-p "x86_64\\|amd64" arch)))
  (let ((path (make-temp-file "nelisp-elf-exec-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary path 'minimal-exit-0)
          (set-file-modes path #o755)
          (let ((rc (call-process path nil nil nil)))
            (should (eq rc 0))))
      (ignore-errors (delete-file path)))))

;; ---------------------------------------------------------------- L4 readelf

(ert-deftest nelisp-elf-write-binary-readelf-h ()
  "When `readelf' is available, `readelf -h' confirms ELF64 / EXEC / X86-64."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-elf-readelf-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary path 'minimal-exit-0)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-h" path)))))
            (should (string-match-p "ELF64" out))
            (should (string-match-p "EXEC" out))
            (should (string-match-p "X86-64" out))))
      (ignore-errors (delete-file path)))))

(provide 'nelisp-elf-write-test)

;;; nelisp-elf-write-test.el ends here
