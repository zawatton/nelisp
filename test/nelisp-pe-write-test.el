;;; nelisp-pe-write-test.el --- ERT tests for PE32+/COFF object writer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 101 §101.A — byte-structure tests for the PE32+/COFF x86_64
;; object writer.  These parse the emitted .obj directly without
;; requiring a Windows toolchain.

;;; Code:

(require 'ert)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir
                      (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-pe-write)

;; ---- helpers ----

(defun nelisp-pe-write-test--read-file-bytes (path)
  "Return raw unibyte bytes of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally path))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun nelisp-pe-write-test--read-le16 (bytes offset)
  "Read unsigned 16-bit little-endian integer from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)))

(defun nelisp-pe-write-test--read-le32 (bytes offset)
  "Read unsigned 32-bit little-endian integer from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)
          (ash (aref bytes (+ offset 2)) 16)
          (ash (aref bytes (+ offset 3)) 24)))

(defun nelisp-pe-write-test--read-le64 (bytes offset)
  "Read unsigned 64-bit little-endian integer from BYTES at OFFSET."
  (let ((acc 0)
        (i 7))
    (while (>= i 0)
      (setq acc (logior (ash acc 8) (aref bytes (+ offset i))))
      (setq i (1- i)))
    acc))

(defun nelisp-pe-write-test--read-cstr (bytes offset)
  "Read a NUL-terminated ASCII string from BYTES at OFFSET."
  (let ((end offset))
    (while (and (< end (length bytes))
                (/= (aref bytes end) 0))
      (setq end (1+ end)))
    (substring bytes offset end)))

;; ---- sample input ----

(defconst nelisp-pe-write-test--sample-text
  ;; A trivial x86_64 function: mov eax, 42 ; ret
  (unibyte-string #xB8 #x2A #x00 #x00 #x00  ; mov eax, 42
                  #xC3)                       ; ret
  "Minimal x86_64 text payload for round-trip tests (6 bytes).")

(defconst nelisp-pe-write-test--sample-plist
  (list :text    nelisp-pe-write-test--sample-text
        :symbols (list (list :name "answer"
                             :value 0
                             :size  6
                             :section 'text
                             :bind 'global
                             :type 'func))
        :machine 'x86_64)
  "Minimal build-plist exercising global function symbol, no relocs.")

(defun nelisp-pe-write-test--emit-sample ()
  "Emit the sample COFF object to a temp file and return its bytes."
  (let ((path (make-temp-file "nelisp-pe-test-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-pe-write-binary path nelisp-pe-write-test--sample-plist)
          (nelisp-pe-write-test--read-file-bytes path))
      (when (file-exists-p path)
        (delete-file path)))))

(defun nelisp-pe-write-test--emit-exitprocess (&optional code)
  "Emit the sample PE32+ ExitProcess executable and return its bytes."
  (let ((path (make-temp-file "nelisp-pe-exe-test-" nil ".exe")))
    (unwind-protect
        (progn
          (nelisp-pe-write-exitprocess-executable path (or code 42))
          (nelisp-pe-write-test--read-file-bytes path))
      (when (file-exists-p path)
        (delete-file path)))))

;; ---- IMAGE_FILE_HEADER tests ----

(ert-deftest nelisp-pe-write-binary-machine-amd64 ()
  "IMAGE_FILE_HEADER.Machine must be 0x8664 (AMD64)."
  (let ((bytes (nelisp-pe-write-test--emit-sample)))
    (should (= (nelisp-pe-write-test--read-le16 bytes 0) #x8664))))

(ert-deftest nelisp-pe-write-binary-num-sections ()
  "IMAGE_FILE_HEADER.NumberOfSections = 1 for text-only input."
  (let ((bytes (nelisp-pe-write-test--emit-sample)))
    (should (= (nelisp-pe-write-test--read-le16 bytes 2) 1))))

(ert-deftest nelisp-pe-write-binary-optional-header-size-zero ()
  "IMAGE_FILE_HEADER.SizeOfOptionalHeader must be 0 for object files."
  (let ((bytes (nelisp-pe-write-test--emit-sample)))
    ;; SizeOfOptionalHeader is at offset 16 in IMAGE_FILE_HEADER.
    (should (= (nelisp-pe-write-test--read-le16 bytes 16) 0))))

;; ---- IMAGE_SECTION_HEADER tests ----

(ert-deftest nelisp-pe-write-binary-section-name-text ()
  "First IMAGE_SECTION_HEADER.Name must begin with \".text\"."
  (let ((bytes (nelisp-pe-write-test--emit-sample)))
    ;; Section header 0 starts at offset 20 (= sizeof IMAGE_FILE_HEADER).
    (let ((name-bytes (substring bytes 20 28)))
      (should (string-prefix-p ".text" name-bytes)))))

(ert-deftest nelisp-pe-write-binary-section-raw-data-size ()
  "First section SizeOfRawData must equal the :text byte length (6)."
  (let ((bytes (nelisp-pe-write-test--emit-sample)))
    ;; SizeOfRawData is at offset 20+16 = 36.
    (should (= (nelisp-pe-write-test--read-le32 bytes 36)
               (length nelisp-pe-write-test--sample-text)))))

(ert-deftest nelisp-pe-write-binary-section-raw-data-content ()
  "Raw .text bytes must match the input :text payload."
  (let ((bytes (nelisp-pe-write-test--emit-sample)))
    ;; PointerToRawData is at offset 20+20 = 40 (= 4 bytes).
    (let* ((ptr (nelisp-pe-write-test--read-le32 bytes 40))
           (text-in-file (substring bytes ptr (+ ptr 6))))
      (should (equal text-in-file nelisp-pe-write-test--sample-text)))))

;; ---- Relocation test ----

(ert-deftest nelisp-pe-write-binary-reloc-entry ()
  "Relocation entry has correct VirtualAddress, SymbolTableIndex and Type."
  (let* (;; call rel32 to an external function: E8 + 4 bytes of displacement.
         (call-text (unibyte-string #xE8 #x00 #x00 #x00 #x00))
         (plist (list :text    call-text
                      :symbols (list (list :name "callee"
                                           :value 0
                                           :size  0
                                           :section 'undef
                                           :bind 'global
                                           :type 'func))
                      :relocs  (list (list :offset 1
                                           :symbol "callee"
                                           :type   'plt32
                                           :addend -4))
                      :machine 'x86_64))
         (path (make-temp-file "nelisp-pe-reloc-" nil ".obj"))
         (bytes
          (unwind-protect
              (progn
                (nelisp-pe-write-binary path plist)
                (nelisp-pe-write-test--read-file-bytes path))
            (when (file-exists-p path)
              (delete-file path)))))
    ;; IMAGE_FILE_HEADER.NumberOfRelocations for section 0 is read from
    ;; the section header at offset 20+32 = 52 (NumberOfRelocations = le16).
    (should (= (nelisp-pe-write-test--read-le16 bytes 52) 1))
    ;; PointerToRelocations for section 0 is at offset 20+24 = 44.
    (let* ((reloc-ptr (nelisp-pe-write-test--read-le32 bytes 44))
           ;; IMAGE_RELOCATION: VirtualAddress[4] + SymbolTableIndex[4] + Type[2]
           (va    (nelisp-pe-write-test--read-le32 bytes reloc-ptr))
           (type  (nelisp-pe-write-test--read-le16 bytes (+ reloc-ptr 8))))
      ;; VirtualAddress = 1 (= offset of the rel32 displacement field).
      (should (= va 1))
      ;; Type = IMAGE_REL_AMD64_REL32 = 0x0004.
      (should (= type #x0004)))))

;; ---- PE32+ executable tests --------------------------------------

(ert-deftest nelisp-pe-write-executable-mz-pe32plus-headers ()
  "ExitProcess EXE has MZ, PE signature, AMD64 machine and PE32+ optional header."
  (let* ((bytes (nelisp-pe-write-test--emit-exitprocess 42))
         (peoff (nelisp-pe-write-test--read-le32 bytes #x3c))
         (opt (+ peoff 24)))
    (should (= (aref bytes 0) #x4d))
    (should (= (aref bytes 1) #x5a))
    (should (= peoff #x80))
    (should (equal (substring bytes peoff (+ peoff 4))
                   (unibyte-string #x50 #x45 #x00 #x00)))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ peoff 4)) #x8664))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ peoff 6)) 2))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ peoff 20)) 240))
    (should (= (nelisp-pe-write-test--read-le16 bytes opt) #x20b))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ opt 68)) 3))
    (should (= (nelisp-pe-write-test--read-le64 bytes (+ opt 24))
               #x140000000))))

(ert-deftest nelisp-pe-write-executable-sections-and-entry ()
  "ExitProcess EXE has page-aligned .text/.rdata sections and entry RVA."
  (let* ((bytes (nelisp-pe-write-test--emit-exitprocess 42))
         (peoff (nelisp-pe-write-test--read-le32 bytes #x3c))
         (opt (+ peoff 24))
         (sect0 (+ opt 240))
         (sect1 (+ sect0 40)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt 16)) #x1000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt 32)) #x1000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt 36)) #x200))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt 60)) #x200))
    (should (string-prefix-p ".text" (substring bytes sect0 (+ sect0 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect0 12)) #x1000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect0 20)) #x200))
    (should (string-prefix-p ".rdata" (substring bytes sect1 (+ sect1 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 12)) #x2000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 20)) #x400))))

(ert-deftest nelisp-pe-write-executable-imports-exitprocess ()
  "ExitProcess EXE import directory names KERNEL32.dll and ExitProcess."
  (let* ((bytes (nelisp-pe-write-test--emit-exitprocess 42))
         (peoff (nelisp-pe-write-test--read-le32 bytes #x3c))
         (opt (+ peoff 24))
         (import-rva (nelisp-pe-write-test--read-le32 bytes (+ opt 120)))
         (import-size (nelisp-pe-write-test--read-le32 bytes (+ opt 124)))
         (rdata-rva #x2000)
         (rdata-raw #x400)
         (import-off (+ rdata-raw (- import-rva rdata-rva)))
         (oft (nelisp-pe-write-test--read-le32 bytes import-off))
         (name-rva (nelisp-pe-write-test--read-le32 bytes (+ import-off 12)))
         (iat-rva (nelisp-pe-write-test--read-le32 bytes (+ import-off 16)))
         (hint-name-rva (nelisp-pe-write-test--read-le64
                         bytes (+ rdata-raw (- oft rdata-rva))))
         (dll-name-off (+ rdata-raw (- name-rva rdata-rva)))
         (hint-name-off (+ rdata-raw (- hint-name-rva rdata-rva))))
    (should (= import-rva #x2000))
    (should (> import-size 80))
    (should (= oft #x2028))
    (should (= iat-rva #x2038))
    (should (equal (nelisp-pe-write-test--read-cstr bytes dll-name-off)
                   "KERNEL32.dll"))
    (should (= (nelisp-pe-write-test--read-le16 bytes hint-name-off) 0))
    (should (equal (nelisp-pe-write-test--read-cstr bytes (+ hint-name-off 2))
                   "ExitProcess"))))

(ert-deftest nelisp-pe-write-executable-text-calls-iat ()
  "ExitProcess EXE entry code moves 42 into ECX and calls the IAT slot."
  (let* ((bytes (nelisp-pe-write-test--emit-exitprocess 42))
         (text-raw #x200)
         (text-rva #x1000)
         (iat-rva #x2038)
         (disp (nelisp-pe-write-test--read-le32 bytes (+ text-raw 11))))
    (should (equal (substring bytes text-raw (+ text-raw 4))
                   (unibyte-string #x48 #x83 #xec #x28)))
    (should (= (aref bytes (+ text-raw 4)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-raw 5)) 42))
    (should (equal (substring bytes (+ text-raw 9) (+ text-raw 11))
                   (unibyte-string #xff #x15)))
    (should (= (+ text-rva 15 disp) iat-rva))))

(provide 'nelisp-pe-write-test)

;;; nelisp-pe-write-test.el ends here
