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
(require 'cl-lib)

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
        (i 0))
    (while (< i 8)
      (setq acc (logior acc (ash (aref bytes (+ offset i)) (* i 8))))
      (setq i (1+ i)))
    acc))

(defun nelisp-pe-write-test--contains-p (bytes needle)
  "Return non-nil when BYTES contains NEEDLE."
  (not (null (cl-search needle bytes :test #'char-equal))))

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

(defun nelisp-pe-write-test--emit-exe (&optional spec)
  "Emit a PE32+ EXE for SPEC and return its raw bytes."
  (let ((path (make-temp-file "nelisp-pe-exe-test-" nil ".exe")))
    (unwind-protect
        (progn
          (nelisp-pe-write-exe-binary path (or spec 'minimal-exit-42))
          (nelisp-pe-write-test--read-file-bytes path))
      (when (file-exists-p path)
        (delete-file path)))))

(defun nelisp-pe-write-test--emit-minimal-exe ()
  "Emit the minimal ExitProcess PE32+ EXE and return its raw bytes."
  (nelisp-pe-write-test--emit-exe 'minimal-exit-42))

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

;; ---- PE32+ EXE tests (Doc 138 Stage 1) ----

(ert-deftest nelisp-pe-write-exe-binary-dos-and-pe-signature ()
  "The minimal EXE has MZ, e_lfanew and PE\\0\\0 at the expected offset."
  (let* ((bytes (nelisp-pe-write-test--emit-minimal-exe))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c)))
    (should (equal (substring bytes 0 2) (unibyte-string #x4d #x5a)))
    (should (= pe-off #x80))
    (should (equal (substring bytes pe-off (+ pe-off 4))
                   (unibyte-string #x50 #x45 #x00 #x00)))))

(ert-deftest nelisp-pe-write-exe-binary-file-and-optional-header ()
  "The minimal EXE is AMD64 PE32+ console with two sections."
  (let* ((bytes (nelisp-pe-write-test--emit-minimal-exe))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (file-off (+ pe-off 4))
         (opt-off (+ file-off 20)))
    (should (= (nelisp-pe-write-test--read-le16 bytes file-off) #x8664))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ file-off 2)) 2))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ file-off 16)) 240))
    (should (= (nelisp-pe-write-test--read-le16 bytes opt-off) #x020b))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 16)) #x1000))
    (should (= (nelisp-pe-write-test--read-le64 bytes (+ opt-off 24)) #x140000000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 32)) #x1000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 36)) #x200))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 60)) #x200))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ opt-off 68)) 3))))

(ert-deftest nelisp-pe-write-exe-binary-section-table ()
  "The minimal EXE has .text at RVA 0x1000 and .idata at RVA 0x2000."
  (let* ((bytes (nelisp-pe-write-test--emit-minimal-exe))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (sect0 (+ pe-off 4 20 240))
         (sect1 (+ sect0 40)))
    (should (string-prefix-p ".text" (substring bytes sect0 (+ sect0 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect0 8)) 16))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect0 12)) #x1000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect0 16)) #x200))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect0 20)) #x200))
    (should (string-prefix-p ".idata" (substring bytes sect1 (+ sect1 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 12)) #x2000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 16)) #x200))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 20)) #x400))))

(ert-deftest nelisp-pe-write-exe-binary-import-directory ()
  "The import directory names KERNEL32.dll and ExitProcess via ILT/IAT."
  (let* ((bytes (nelisp-pe-write-test--emit-minimal-exe))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (opt-off (+ pe-off 4 20))
         (import-dir-off (+ opt-off 112 8))
         (iat-dir-off (+ opt-off 112 (* 12 8)))
         (idata-raw #x400)
         (idata-rva #x2000)
         (import-rva (nelisp-pe-write-test--read-le32 bytes import-dir-off))
         (import-size (nelisp-pe-write-test--read-le32 bytes (+ import-dir-off 4)))
         (iat-rva (nelisp-pe-write-test--read-le32 bytes iat-dir-off))
         (iat-size (nelisp-pe-write-test--read-le32 bytes (+ iat-dir-off 4)))
         (name-rva (nelisp-pe-write-test--read-le32 bytes (+ idata-raw 12)))
         (first-thunk-rva (nelisp-pe-write-test--read-le32 bytes (+ idata-raw 16)))
         (hint-name-rva (nelisp-pe-write-test--read-le64 bytes (- (+ idata-raw iat-rva)
                                                                  idata-rva)))
         (name-off (+ idata-raw (- name-rva idata-rva)))
         (hint-name-off (+ idata-raw (- hint-name-rva idata-rva))))
    (should (= import-rva #x2000))
    (should (= import-size 40))
    (should (= iat-rva #x2038))
    (should (= iat-size 16))
    (should (= first-thunk-rva iat-rva))
    (should (string-prefix-p "KERNEL32.dll" (substring bytes name-off (+ name-off 13))))
    (should (= (nelisp-pe-write-test--read-le16 bytes hint-name-off) 0))
    (should (string-prefix-p "ExitProcess"
                             (substring bytes (+ hint-name-off 2)
                                        (+ hint-name-off 14))))))

(ert-deftest nelisp-pe-write-exe-binary-entry-code-calls-iat ()
  "The entry code sets RCX=42 and calls the ExitProcess IAT slot."
  (let* ((bytes (nelisp-pe-write-test--emit-minimal-exe))
         (text-off #x200)
         (disp (nelisp-pe-write-test--read-le32 bytes (+ text-off 11))))
    (should (equal (substring bytes text-off (+ text-off 4))
                   (unibyte-string #x48 #x83 #xec #x28)))
    (should (= (aref bytes (+ text-off 4)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 5)) 42))
    (should (equal (substring bytes (+ text-off 9) (+ text-off 11))
                   (unibyte-string #xff #x15)))
    (should (= disp #x1029))
    (should (= (aref bytes (+ text-off 15)) #xcc))))

(ert-deftest nelisp-pe-write-exe-binary-virtualalloc-import-directory ()
  "The VirtualAlloc smoke EXE imports ExitProcess and VirtualAlloc."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'virtualalloc-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (opt-off (+ pe-off 4 20))
         (iat-dir-off (+ opt-off 112 (* 12 8)))
         (idata-raw #x400)
         (idata-rva #x2000)
         (ilt-rva (nelisp-pe-write-test--read-le32 bytes idata-raw))
         (name-rva (nelisp-pe-write-test--read-le32 bytes (+ idata-raw 12)))
         (first-thunk-rva (nelisp-pe-write-test--read-le32 bytes (+ idata-raw 16)))
         (iat-rva (nelisp-pe-write-test--read-le32 bytes iat-dir-off))
         (iat-size (nelisp-pe-write-test--read-le32 bytes (+ iat-dir-off 4)))
         (exit-hint-rva (nelisp-pe-write-test--read-le64 bytes
                                                            (+ idata-raw (- iat-rva idata-rva))))
         (virtualalloc-hint-rva
          (nelisp-pe-write-test--read-le64 bytes
                                           (+ idata-raw (- (+ iat-rva 8) idata-rva))))
         (dll-name-off (+ idata-raw (- name-rva idata-rva)))
         (exit-hint-off (+ idata-raw (- exit-hint-rva idata-rva)))
         (virtualalloc-hint-off (+ idata-raw (- virtualalloc-hint-rva idata-rva))))
    (should (= ilt-rva #x2028))
    (should (= first-thunk-rva #x2040))
    (should (= iat-rva #x2040))
    (should (= iat-size 24))
    (should (string-prefix-p "KERNEL32.dll"
                             (substring bytes dll-name-off (+ dll-name-off 13))))
    (should (string-prefix-p "ExitProcess"
                             (substring bytes (+ exit-hint-off 2)
                                        (+ exit-hint-off 14))))
    (should (string-prefix-p "VirtualAlloc"
                             (substring bytes (+ virtualalloc-hint-off 2)
                                        (+ virtualalloc-hint-off 15))))))

(ert-deftest nelisp-pe-write-exe-binary-virtualalloc-entry-code ()
  "The VirtualAlloc smoke EXE sets Win64 args and exits 42 on success."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'virtualalloc-exit-42))
         (text-off #x200))
    (should (equal (substring bytes text-off (+ text-off 4))
                   (unibyte-string #x48 #x83 #xec #x28)))
    (should (equal (substring bytes (+ text-off 4) (+ text-off 6))
                   (unibyte-string #x31 #xc9))) ; rcx = NULL
    (should (= (aref bytes (+ text-off 6)) #xba))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 7)) #x1000))
    (should (equal (substring bytes (+ text-off 11) (+ text-off 13))
                   (unibyte-string #x41 #xb8)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 13)) #x3000))
    (should (equal (substring bytes (+ text-off 17) (+ text-off 19))
                   (unibyte-string #x41 #xb9)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 19)) #x4))
    (should (equal (substring bytes (+ text-off 23) (+ text-off 25))
                   (unibyte-string #xff #x15)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 25)) #x102b))
    (should (equal (substring bytes (+ text-off 29) (+ text-off 34))
                   (unibyte-string #x48 #x85 #xc0 #x74 #x0b)))
    (should (= (aref bytes (+ text-off 34)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 35)) 42))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 41)) #x1013))
    (should (= (aref bytes (+ text-off 45)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 46)) 1))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 52)) #x1008))
    (should (= (aref bytes (+ text-off 56)) #xcc))))

(ert-deftest nelisp-pe-write-exe-binary-virtualprotect-free-section-table ()
  "The VirtualProtect / VirtualFree smoke EXE has .data oldProtect storage."
  (let* ((bytes (nelisp-pe-write-test--emit-exe
                 'virtualprotect-free-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (file-off (+ pe-off 4))
         (opt-off (+ file-off 20))
         (sect0 (+ pe-off 4 20 240))
         (sect1 (+ sect0 40))
         (sect2 (+ sect1 40))
         (data-raw #x400))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ file-off 2)) 3))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 4)) #x200))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 8)) #x400))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 56)) #x4000))
    (should (string-prefix-p ".text" (substring bytes sect0 (+ sect0 8))))
    (should (string-prefix-p ".data" (substring bytes sect1 (+ sect1 8))))
    (should (string-prefix-p ".idata" (substring bytes sect2 (+ sect2 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 8)) 4))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 12)) #x2000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 16)) #x200))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 20)) data-raw))
    (dotimes (i 4)
      (should (= (aref bytes (+ data-raw i)) 0)))))

(ert-deftest nelisp-pe-write-exe-binary-virtualprotect-free-import-directory ()
  "The VirtualProtect / VirtualFree smoke EXE imports four KERNEL32 APIs."
  (let* ((bytes (nelisp-pe-write-test--emit-exe
                 'virtualprotect-free-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (opt-off (+ pe-off 4 20))
         (iat-dir-off (+ opt-off 112 (* 12 8)))
         (idata-raw #x600)
         (idata-rva #x3000)
         (ilt-rva (nelisp-pe-write-test--read-le32 bytes idata-raw))
         (name-rva (nelisp-pe-write-test--read-le32 bytes (+ idata-raw 12)))
         (first-thunk-rva (nelisp-pe-write-test--read-le32 bytes (+ idata-raw 16)))
         (iat-rva (nelisp-pe-write-test--read-le32 bytes iat-dir-off))
         (iat-size (nelisp-pe-write-test--read-le32 bytes (+ iat-dir-off 4)))
         (exit-hint-rva
          (nelisp-pe-write-test--read-le64 bytes
                                           (+ idata-raw (- iat-rva idata-rva))))
         (virtualalloc-hint-rva
          (nelisp-pe-write-test--read-le64
           bytes (+ idata-raw (- (+ iat-rva 8) idata-rva))))
         (virtualprotect-hint-rva
          (nelisp-pe-write-test--read-le64
           bytes (+ idata-raw (- (+ iat-rva 16) idata-rva))))
         (virtualfree-hint-rva
          (nelisp-pe-write-test--read-le64
           bytes (+ idata-raw (- (+ iat-rva 24) idata-rva))))
         (dll-name-off (+ idata-raw (- name-rva idata-rva)))
         (exit-hint-off (+ idata-raw (- exit-hint-rva idata-rva)))
         (virtualalloc-hint-off
          (+ idata-raw (- virtualalloc-hint-rva idata-rva)))
         (virtualprotect-hint-off
          (+ idata-raw (- virtualprotect-hint-rva idata-rva)))
         (virtualfree-hint-off
          (+ idata-raw (- virtualfree-hint-rva idata-rva))))
    (should (= ilt-rva #x3028))
    (should (= first-thunk-rva #x3050))
    (should (= iat-rva #x3050))
    (should (= iat-size 40))
    (should (string-prefix-p "KERNEL32.dll"
                             (substring bytes dll-name-off (+ dll-name-off 13))))
    (should (string-prefix-p "ExitProcess"
                             (substring bytes (+ exit-hint-off 2)
                                        (+ exit-hint-off 14))))
    (should (string-prefix-p "VirtualAlloc"
                             (substring bytes (+ virtualalloc-hint-off 2)
                                        (+ virtualalloc-hint-off 15))))
    (should (string-prefix-p "VirtualProtect"
                             (substring bytes (+ virtualprotect-hint-off 2)
                                        (+ virtualprotect-hint-off 16))))
    (should (string-prefix-p "VirtualFree"
                             (substring bytes (+ virtualfree-hint-off 2)
                                        (+ virtualfree-hint-off 13))))))

(ert-deftest nelisp-pe-write-exe-binary-virtualprotect-free-entry-code ()
  "The VirtualProtect / VirtualFree smoke EXE uses Win64 argument registers."
  (let* ((bytes (nelisp-pe-write-test--emit-exe
                 'virtualprotect-free-exit-42))
         (text-off #x200))
    (should (equal (substring bytes text-off (+ text-off 4))
                   (unibyte-string #x48 #x83 #xec #x28)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 7)) #x1000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 13)) #x3000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 19)) #x4))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 25)) #x203b))
    (should (equal (substring bytes (+ text-off 29) (+ text-off 34))
                   (unibyte-string #x48 #x85 #xc0 #x74 #x53)))
    (should (equal (substring bytes (+ text-off 34) (+ text-off 40))
                   (unibyte-string #x48 #x89 #xc3 #x48 #x89 #xc1)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 41)) #x1000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 47)) #x2))
    (should (equal (substring bytes (+ text-off 51) (+ text-off 54))
                   (unibyte-string #x4c #x8d #x0d)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 54)) #xfc6))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 60)) #x2020))
    (should (equal (substring bytes (+ text-off 64) (+ text-off 68))
                   (unibyte-string #x85 #xc0 #x74 #x20)))
    (should (equal (substring bytes (+ text-off 68) (+ text-off 73))
                   (unibyte-string #x48 #x89 #xd9 #x31 #xd2)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 75)) #x8000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 81)) #x2013))
    (should (equal (substring bytes (+ text-off 85) (+ text-off 89))
                   (unibyte-string #x85 #xc0 #x74 #x1c)))
    (should (= (aref bytes (+ text-off 89)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 90)) 42))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 96)) #x1fec))
    (should (equal (substring bytes (+ text-off 100) (+ text-off 105))
                   (unibyte-string #x48 #x89 #xd9 #x31 #xd2)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 107)) #x8000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 113)) #x1ff3))
    (should (= (aref bytes (+ text-off 117)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 118)) 1))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 124)) #x1fd0))
    (should (= (aref bytes (+ text-off 128)) #xcc))))

(ert-deftest nelisp-pe-write-exe-binary-getcommandline-import-directory ()
  "The GetCommandLineW smoke EXE imports ExitProcess and GetCommandLineW."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'getcommandline-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (opt-off (+ pe-off 4 20))
         (iat-dir-off (+ opt-off 112 (* 12 8)))
         (idata-raw #x400)
         (idata-rva #x2000)
         (ilt-rva (nelisp-pe-write-test--read-le32 bytes idata-raw))
         (name-rva (nelisp-pe-write-test--read-le32 bytes (+ idata-raw 12)))
         (first-thunk-rva (nelisp-pe-write-test--read-le32 bytes (+ idata-raw 16)))
         (iat-rva (nelisp-pe-write-test--read-le32 bytes iat-dir-off))
         (iat-size (nelisp-pe-write-test--read-le32 bytes (+ iat-dir-off 4)))
         (exit-hint-rva (nelisp-pe-write-test--read-le64
                         bytes (+ idata-raw (- iat-rva idata-rva))))
         (cmdline-hint-rva
          (nelisp-pe-write-test--read-le64
           bytes (+ idata-raw (- (+ iat-rva 8) idata-rva))))
         (dll-name-off (+ idata-raw (- name-rva idata-rva)))
         (exit-hint-off (+ idata-raw (- exit-hint-rva idata-rva)))
         (cmdline-hint-off (+ idata-raw (- cmdline-hint-rva idata-rva))))
    (should (= ilt-rva #x2028))
    (should (= first-thunk-rva #x2040))
    (should (= iat-rva #x2040))
    (should (= iat-size 24))
    (should (string-prefix-p "KERNEL32.dll"
                             (substring bytes dll-name-off (+ dll-name-off 13))))
    (should (string-prefix-p "ExitProcess"
                             (substring bytes (+ exit-hint-off 2)
                                        (+ exit-hint-off 14))))
    (should (string-prefix-p "GetCommandLineW"
                             (substring bytes (+ cmdline-hint-off 2)
                                        (+ cmdline-hint-off 17))))))

(ert-deftest nelisp-pe-write-exe-binary-getcommandline-entry-code ()
  "The GetCommandLineW smoke EXE exits 42 when the command line pointer exists."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'getcommandline-exit-42))
         (text-off #x200))
    (should (equal (substring bytes text-off (+ text-off 4))
                   (unibyte-string #x48 #x83 #xec #x28)))
    (should (equal (substring bytes (+ text-off 4) (+ text-off 6))
                   (unibyte-string #xff #x15)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 6)) #x103e))
    (should (equal (substring bytes (+ text-off 10) (+ text-off 15))
                   (unibyte-string #x48 #x85 #xc0 #x74 #x0b)))
    (should (= (aref bytes (+ text-off 15)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 16)) 42))
    (should (equal (substring bytes (+ text-off 20) (+ text-off 22))
                   (unibyte-string #xff #x15)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 22)) #x1026))
    (should (= (aref bytes (+ text-off 26)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 27)) 1))
    (should (equal (substring bytes (+ text-off 31) (+ text-off 33))
                   (unibyte-string #xff #x15)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 33)) #x101b))
    (should (= (aref bytes (+ text-off 37)) #xcc))))

(ert-deftest nelisp-pe-write-exe-binary-wsastartup-section-table ()
  "The WSAStartup smoke EXE has .data WSADATA bytes and multi-DLL .idata."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'wsastartup-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (file-off (+ pe-off 4))
         (opt-off (+ file-off 20))
         (sect0 (+ pe-off 4 20 240))
         (sect1 (+ sect0 40))
         (sect2 (+ sect1 40))
         (text-raw #x200)
         (data-raw #x400)
         (idata-raw #x600))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ file-off 2)) 3))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 4)) #x200))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 8)) #x400))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 56)) #x4000))
    (should (string-prefix-p ".text" (substring bytes sect0 (+ sect0 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect0 20)) text-raw))
    (should (string-prefix-p ".data" (substring bytes sect1 (+ sect1 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 8)) 512))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 12)) #x2000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 20)) data-raw))
    (should (string-prefix-p ".idata" (substring bytes sect2 (+ sect2 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 12)) #x3000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 20)) idata-raw))
    (dotimes (i 512)
      (should (= (aref bytes (+ data-raw i)) 0)))))

(ert-deftest nelisp-pe-write-exe-binary-wsastartup-import-directory ()
  "The WSAStartup smoke EXE imports KERNEL32 and WS2_32 descriptors."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'wsastartup-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (opt-off (+ pe-off 4 20))
         (import-dir-off (+ opt-off 112 8))
         (iat-dir-off (+ opt-off 112 (* 12 8)))
         (idata-raw #x600)
         (idata-rva #x3000)
         (k-desc idata-raw)
         (w-desc (+ idata-raw 20))
         (null-desc (+ idata-raw 40))
         (import-rva (nelisp-pe-write-test--read-le32 bytes import-dir-off))
         (import-size (nelisp-pe-write-test--read-le32 bytes (+ import-dir-off 4)))
         (iat-rva (nelisp-pe-write-test--read-le32 bytes iat-dir-off))
         (iat-size (nelisp-pe-write-test--read-le32 bytes (+ iat-dir-off 4)))
         (k-ilt-rva (nelisp-pe-write-test--read-le32 bytes k-desc))
         (k-name-rva (nelisp-pe-write-test--read-le32 bytes (+ k-desc 12)))
         (k-first-thunk-rva (nelisp-pe-write-test--read-le32 bytes (+ k-desc 16)))
         (w-ilt-rva (nelisp-pe-write-test--read-le32 bytes w-desc))
         (w-name-rva (nelisp-pe-write-test--read-le32 bytes (+ w-desc 12)))
         (w-first-thunk-rva (nelisp-pe-write-test--read-le32 bytes (+ w-desc 16)))
         (exit-hint-rva (nelisp-pe-write-test--read-le64
                         bytes (+ idata-raw (- k-first-thunk-rva idata-rva))))
         (wsa-hint-rva (nelisp-pe-write-test--read-le64
                        bytes (+ idata-raw (- w-first-thunk-rva idata-rva))))
         (k-name-off (+ idata-raw (- k-name-rva idata-rva)))
         (w-name-off (+ idata-raw (- w-name-rva idata-rva)))
         (exit-hint-off (+ idata-raw (- exit-hint-rva idata-rva)))
         (wsa-hint-off (+ idata-raw (- wsa-hint-rva idata-rva))))
    (should (= import-rva #x3000))
    (should (= import-size 60))
    (should (= iat-rva #x305c))
    (should (= iat-size 32))
    (should (= k-ilt-rva #x303c))
    (should (= w-ilt-rva #x304c))
    (should (= k-first-thunk-rva #x305c))
    (should (= w-first-thunk-rva #x306c))
    (dotimes (i 20)
      (should (= (aref bytes (+ null-desc i)) 0)))
    (should (string-prefix-p "KERNEL32.dll"
                             (substring bytes k-name-off (+ k-name-off 13))))
    (should (string-prefix-p "WS2_32.dll"
                             (substring bytes w-name-off (+ w-name-off 11))))
    (should (string-prefix-p "ExitProcess"
                             (substring bytes (+ exit-hint-off 2)
                                        (+ exit-hint-off 14))))
    (should (string-prefix-p "WSAStartup"
                             (substring bytes (+ wsa-hint-off 2)
                                        (+ wsa-hint-off 12))))))

(ert-deftest nelisp-pe-write-exe-binary-wsastartup-entry-code ()
  "The WSAStartup smoke EXE sets Win64 args and exits 42 on success."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'wsastartup-exit-42))
         (text-off #x200))
    (should (equal (substring bytes text-off (+ text-off 4))
                   (unibyte-string #x48 #x83 #xec #x28)))
    (should (= (aref bytes (+ text-off 4)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 5)) #x0202))
    (should (equal (substring bytes (+ text-off 9) (+ text-off 12))
                   (unibyte-string #x48 #x8d #x15)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 12)) #xff0))
    (should (equal (substring bytes (+ text-off 16) (+ text-off 18))
                   (unibyte-string #xff #x15)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 18)) #x2056))
    (should (equal (substring bytes (+ text-off 22) (+ text-off 26))
                   (unibyte-string #x85 #xc0 #x74 #x0b)))
    (should (= (aref bytes (+ text-off 26)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 27)) 1))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 33)) #x2037))
    (should (= (aref bytes (+ text-off 37)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 38)) 42))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 44)) #x202c))
    (should (= (aref bytes (+ text-off 48)) #xcc))))

(ert-deftest nelisp-pe-write-exe-binary-commandlinetoargv-section-table ()
  "The CommandLineToArgvW smoke EXE has .data argc and multi-DLL .idata."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'commandlinetoargv-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (file-off (+ pe-off 4))
         (opt-off (+ file-off 20))
         (sect0 (+ pe-off 4 20 240))
         (sect1 (+ sect0 40))
         (sect2 (+ sect1 40))
         (text-raw #x200)
         (data-raw #x400)
         (idata-raw #x600))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ file-off 2)) 3))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 4)) #x200))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 8)) #x400))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 56)) #x4000))
    (should (string-prefix-p ".text" (substring bytes sect0 (+ sect0 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect0 20)) text-raw))
    (should (string-prefix-p ".data" (substring bytes sect1 (+ sect1 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 8)) 4))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 12)) #x2000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 20)) data-raw))
    (should (string-prefix-p ".idata" (substring bytes sect2 (+ sect2 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 12)) #x3000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 20)) idata-raw))
    (dotimes (i 4)
      (should (= (aref bytes (+ data-raw i)) 0)))))

(ert-deftest nelisp-pe-write-exe-binary-commandlinetoargv-import-directory ()
  "The CommandLineToArgvW smoke EXE imports KERNEL32 and SHELL32."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'commandlinetoargv-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (opt-off (+ pe-off 4 20))
         (import-dir-off (+ opt-off 112 8))
         (iat-dir-off (+ opt-off 112 (* 12 8)))
         (idata-raw #x600)
         (idata-rva #x3000)
         (k-desc idata-raw)
         (s-desc (+ idata-raw 20))
         (null-desc (+ idata-raw 40))
         (import-rva (nelisp-pe-write-test--read-le32 bytes import-dir-off))
         (import-size (nelisp-pe-write-test--read-le32 bytes (+ import-dir-off 4)))
         (iat-rva (nelisp-pe-write-test--read-le32 bytes iat-dir-off))
         (iat-size (nelisp-pe-write-test--read-le32 bytes (+ iat-dir-off 4)))
         (k-ilt-rva (nelisp-pe-write-test--read-le32 bytes k-desc))
         (k-name-rva (nelisp-pe-write-test--read-le32 bytes (+ k-desc 12)))
         (k-first-thunk-rva (nelisp-pe-write-test--read-le32 bytes (+ k-desc 16)))
         (s-ilt-rva (nelisp-pe-write-test--read-le32 bytes s-desc))
         (s-name-rva (nelisp-pe-write-test--read-le32 bytes (+ s-desc 12)))
         (s-first-thunk-rva (nelisp-pe-write-test--read-le32 bytes (+ s-desc 16)))
         (exit-hint-rva (nelisp-pe-write-test--read-le64
                         bytes (+ idata-raw (- k-first-thunk-rva idata-rva))))
         (cmdline-hint-rva (nelisp-pe-write-test--read-le64
                            bytes (+ idata-raw (- (+ k-first-thunk-rva 8)
                                                  idata-rva))))
         (localfree-hint-rva (nelisp-pe-write-test--read-le64
                              bytes (+ idata-raw (- (+ k-first-thunk-rva 16)
                                                    idata-rva))))
         (argv-hint-rva (nelisp-pe-write-test--read-le64
                         bytes (+ idata-raw (- s-first-thunk-rva idata-rva))))
         (k-name-off (+ idata-raw (- k-name-rva idata-rva)))
         (s-name-off (+ idata-raw (- s-name-rva idata-rva)))
         (exit-hint-off (+ idata-raw (- exit-hint-rva idata-rva)))
         (cmdline-hint-off (+ idata-raw (- cmdline-hint-rva idata-rva)))
         (localfree-hint-off (+ idata-raw (- localfree-hint-rva idata-rva)))
         (argv-hint-off (+ idata-raw (- argv-hint-rva idata-rva))))
    (should (= import-rva #x3000))
    (should (= import-size 60))
    (should (= iat-rva #x306c))
    (should (= iat-size 48))
    (should (= k-ilt-rva #x303c))
    (should (= s-ilt-rva #x305c))
    (should (= k-first-thunk-rva #x306c))
    (should (= s-first-thunk-rva #x308c))
    (dotimes (i 20)
      (should (= (aref bytes (+ null-desc i)) 0)))
    (should (string-prefix-p "KERNEL32.dll"
                             (substring bytes k-name-off (+ k-name-off 13))))
    (should (string-prefix-p "SHELL32.dll"
                             (substring bytes s-name-off (+ s-name-off 12))))
    (should (string-prefix-p "ExitProcess"
                             (substring bytes (+ exit-hint-off 2)
                                        (+ exit-hint-off 14))))
    (should (string-prefix-p "GetCommandLineW"
                             (substring bytes (+ cmdline-hint-off 2)
                                        (+ cmdline-hint-off 17))))
    (should (string-prefix-p "LocalFree"
                             (substring bytes (+ localfree-hint-off 2)
                                        (+ localfree-hint-off 11))))
    (should (string-prefix-p "CommandLineToArgvW"
                             (substring bytes (+ argv-hint-off 2)
                                        (+ argv-hint-off 20))))))

(ert-deftest nelisp-pe-write-exe-binary-commandlinetoargv-entry-code ()
  "The CommandLineToArgvW smoke EXE materializes argc and frees argv."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'commandlinetoargv-exit-42))
         (text-off #x200))
    (should (equal (substring bytes text-off (+ text-off 4))
                   (unibyte-string #x48 #x83 #xec #x28)))
    (should (equal (substring bytes (+ text-off 4) (+ text-off 6))
                   (unibyte-string #xff #x15)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 6)) #x206a))
    (should (equal (substring bytes (+ text-off 10) (+ text-off 18))
                   (unibyte-string #x48 #x85 #xc0 #x74 #x35 #x48 #x89 #xc1)))
    (should (equal (substring bytes (+ text-off 18) (+ text-off 21))
                   (unibyte-string #x48 #x8d #x15)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 21)) #xfe7))
    (should (equal (substring bytes (+ text-off 25) (+ text-off 27))
                   (unibyte-string #xff #x15)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 27)) #x206d))
    (should (equal (substring bytes (+ text-off 31) (+ text-off 39))
                   (unibyte-string #x48 #x85 #xc0 #x74 #x20 #x48 #x89 #xc3)))
    (should (equal (substring bytes (+ text-off 39) (+ text-off 41))
                   (unibyte-string #x83 #x3d)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 41)) #xfd2))
    (should (= (aref bytes (+ text-off 45)) 1))
    (should (equal (substring bytes (+ text-off 46) (+ text-off 51))
                   (unibyte-string #x7c #x14 #x48 #x89 #xd9)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 53)) #x2043))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 64)) #x2028))
    (should (= (aref bytes (+ text-off 68)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 69)) 1))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 75)) #x201d))
    (should (= (aref bytes (+ text-off 79)) #xcc))))

(ert-deftest nelisp-pe-write-exe-binary-createprocess-section-table ()
  "The CreateProcessW smoke EXE has writable command/startup/process data."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'createprocess-wait-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (file-off (+ pe-off 4))
         (opt-off (+ file-off 20))
         (sect0 (+ pe-off 4 20 240))
         (sect1 (+ sect0 40))
         (sect2 (+ sect1 40))
         (text-raw #x200)
         (data-raw #x400)
         (idata-raw #x600)
         (command (nelisp-pe--utf16le-z-bytes "cmd.exe /c exit 42")))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ file-off 2)) 3))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 4)) #x200))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 8)) #x400))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 56)) #x4000))
    (should (string-prefix-p ".text" (substring bytes sect0 (+ sect0 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect0 20)) text-raw))
    (should (string-prefix-p ".data" (substring bytes sect1 (+ sect1 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 8)) #xac))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 12)) #x2000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 20)) data-raw))
    (should (string-prefix-p ".idata" (substring bytes sect2 (+ sect2 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 12)) #x3000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 20)) idata-raw))
    (should (equal (substring bytes data-raw (+ data-raw (length command)))
                   command))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ data-raw #x28)) 104))
    (dotimes (i 24)
      (should (= (aref bytes (+ data-raw #x90 i)) 0)))))

(ert-deftest nelisp-pe-write-exe-binary-createprocess-import-directory ()
  "The CreateProcessW smoke EXE imports the process wait API set."
  (let ((bytes (nelisp-pe-write-test--emit-exe 'createprocess-wait-exit-42)))
    (dolist (name '("KERNEL32.dll"
                    "ExitProcess"
                    "CreateProcessW"
                    "WaitForSingleObject"
                    "GetExitCodeProcess"
                    "CloseHandle"))
      (should (nelisp-pe-write-test--contains-p bytes name)))))

(ert-deftest nelisp-pe-write-exe-binary-createprocess-entry-code ()
  "The CreateProcessW smoke EXE sets Win64 args and checks child exit 42."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'createprocess-wait-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (sect0 (+ pe-off 4 20 240))
         (text-size (nelisp-pe-write-test--read-le32 bytes (+ sect0 8)))
         (text-off #x200))
    (should (equal (substring bytes text-off (+ text-off 4))
                   (unibyte-string #x48 #x83 #xec #x68)))
    (should (equal (substring bytes (+ text-off 4) (+ text-off 13))
                   (unibyte-string #x31 #xc9 #x48 #x8d #x15
                                   #xf3 #x0f #x00 #x00)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x45 #x31 #xc0 #x45 #x31 #xc9)))
    (dolist (slot '(#x20 #x28 #x30 #x38))
      (should (nelisp-pe-write-test--contains-p
               bytes
               (unibyte-string #x48 #xc7 #x44 #x24 slot
                               #x00 #x00 #x00 #x00))))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x48 #x89 #x44 #x24 #x40)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x48 #x89 #x44 #x24 #x48)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #xba #xff #xff #xff #xff)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x83 #x3d)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x2a #x74 #x0b #xb9
                                   #x01 #x00 #x00 #x00)))
    (should (= (aref bytes (+ text-off (1- text-size))) #xcc))))

(ert-deftest nelisp-pe-write-exe-binary-createthread-section-table ()
  "The CreateThread smoke EXE has writable thread id and exit-code data."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'createthread-wait-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (file-off (+ pe-off 4))
         (opt-off (+ file-off 20))
         (sect0 (+ pe-off 4 20 240))
         (sect1 (+ sect0 40))
         (sect2 (+ sect1 40))
         (text-raw #x200)
         (data-raw #x400)
         (idata-raw #x600))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ file-off 2)) 3))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 4)) #x200))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 8)) #x400))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 56)) #x4000))
    (should (string-prefix-p ".text" (substring bytes sect0 (+ sect0 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect0 20)) text-raw))
    (should (string-prefix-p ".data" (substring bytes sect1 (+ sect1 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 8)) 8))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 12)) #x2000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 20)) data-raw))
    (should (string-prefix-p ".idata" (substring bytes sect2 (+ sect2 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 12)) #x3000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 20)) idata-raw))
    (dotimes (i 8)
      (should (= (aref bytes (+ data-raw i)) 0)))))

(ert-deftest nelisp-pe-write-exe-binary-createthread-import-directory ()
  "The CreateThread smoke EXE imports the thread wait API set."
  (let ((bytes (nelisp-pe-write-test--emit-exe 'createthread-wait-exit-42)))
    (dolist (name '("KERNEL32.dll"
                    "ExitProcess"
                    "CreateThread"
                    "WaitForSingleObject"
                    "GetExitCodeThread"
                    "CloseHandle"))
      (should (nelisp-pe-write-test--contains-p bytes name)))))

(ert-deftest nelisp-pe-write-exe-binary-createthread-entry-code ()
  "The CreateThread smoke EXE starts an in-image routine and checks exit 42."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'createthread-wait-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (sect0 (+ pe-off 4 20 240))
         (text-size (nelisp-pe-write-test--read-le32 bytes (+ sect0 8)))
         (text-off #x200)
         (thread-entry-off (+ text-off #xa4)))
    (should (equal (substring bytes text-off (+ text-off 8))
                   (unibyte-string #x48 #x83 #xec #x48 #x31 #xc9 #x31 #xd2)))
    (should (equal (substring bytes (+ text-off 8) (+ text-off 15))
                   (unibyte-string #x4c #x8d #x05 #x95 #x00 #x00 #x00)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x45 #x31 #xc9)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x48 #xc7 #x44 #x24 #x20
                                   #x00 #x00 #x00 #x00)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x48 #x89 #x44 #x24 #x28)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x48 #x85 #xc0 #x75 #x0b)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #xba #xff #xff #xff #xff)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x83 #x3d)))
    (should (equal (substring bytes thread-entry-off (+ thread-entry-off 7))
                   (unibyte-string #xb8 #x2a #x00 #x00 #x00 #xc3 #xcc)))
    (should (= text-size #xab))
    (should (= (aref bytes (+ text-off (1- text-size))) #xcc))))

(ert-deftest nelisp-pe-write-exe-binary-virtualalloc-arena-section-table ()
  "The arena smoke EXE adds a .data section for base/cursor/end metadata."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'virtualalloc-arena-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (file-off (+ pe-off 4))
         (opt-off (+ file-off 20))
         (sect0 (+ pe-off 4 20 240))
         (sect1 (+ sect0 40))
         (sect2 (+ sect1 40))
         (data-raw #x600))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ file-off 2)) 3))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 4)) #x200))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 8)) #x400))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 56)) #x4000))
    (should (string-prefix-p ".text" (substring bytes sect0 (+ sect0 8))))
    (should (string-prefix-p ".idata" (substring bytes sect1 (+ sect1 8))))
    (should (string-prefix-p ".data" (substring bytes sect2 (+ sect2 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 8)) 24))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 12)) #x3000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 16)) #x200))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 20)) data-raw))
    (dotimes (i 24)
      (should (= (aref bytes (+ data-raw i)) 0)))))

(ert-deftest nelisp-pe-write-exe-binary-virtualalloc-arena-entry-code ()
  "The arena smoke EXE stores VirtualAlloc result into .data metadata."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'virtualalloc-arena-exit-42))
         (text-off #x200))
    (should (equal (substring bytes text-off (+ text-off 4))
                   (unibyte-string #x48 #x83 #xec #x28)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 7)) #x10000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 13)) #x3000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 19)) #x4))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 25)) #x102b))
    (should (equal (substring bytes (+ text-off 29) (+ text-off 34))
                   (unibyte-string #x48 #x85 #xc0 #x74 #x2a)))
    (should (equal (substring bytes (+ text-off 34) (+ text-off 37))
                   (unibyte-string #x48 #x89 #x05)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 37)) #x1fd7))
    (should (equal (substring bytes (+ text-off 41) (+ text-off 44))
                   (unibyte-string #x48 #x89 #x05)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 44)) #x1fd8))
    (should (equal (substring bytes (+ text-off 48) (+ text-off 58))
                   (unibyte-string #x48 #x89 #xc2 #x48 #x81 #xc2
                                   #x00 #x00 #x01 #x00)))
    (should (equal (substring bytes (+ text-off 58) (+ text-off 61))
                   (unibyte-string #x48 #x89 #x15)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 61)) #x1fcf))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 72)) #xff4))
    (should (= (aref bytes (+ text-off 76)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 77)) 1))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 83)) #xfe9))
    (should (= (aref bytes (+ text-off 87)) #xcc))))

(ert-deftest nelisp-pe-write-exe-binary-writefile-stdout-section-table ()
  "The WriteFile smoke EXE has .rdata message, .data bytesWritten, and .idata."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'writefile-stdout-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (file-off (+ pe-off 4))
         (opt-off (+ file-off 20))
         (sect0 (+ pe-off 4 20 240))
         (sect1 (+ sect0 40))
         (sect2 (+ sect1 40))
         (sect3 (+ sect2 40))
         (text-raw #x400)
         (rdata-raw #x600)
         (data-raw #x800)
         (idata-raw #xa00)
         (message "hello from nelisp windows\n"))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ file-off 2)) 4))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 4)) #x200))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 8)) #x600))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 56)) #x5000))
    (should (string-prefix-p ".text" (substring bytes sect0 (+ sect0 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect0 20)) text-raw))
    (should (string-prefix-p ".rdata" (substring bytes sect1 (+ sect1 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 12)) #x2000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 20)) rdata-raw))
    (should (string-prefix-p ".data" (substring bytes sect2 (+ sect2 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 12)) #x3000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 20)) data-raw))
    (should (string-prefix-p ".idata" (substring bytes sect3 (+ sect3 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect3 12)) #x4000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect3 20)) idata-raw))
    (should (equal (substring bytes rdata-raw (+ rdata-raw (length message)))
                   message))
    (dotimes (i 4)
      (should (= (aref bytes (+ data-raw i)) 0)))))

(ert-deftest nelisp-pe-write-exe-binary-writefile-stdout-import-directory ()
  "The WriteFile smoke EXE imports ExitProcess, GetStdHandle and WriteFile."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'writefile-stdout-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (opt-off (+ pe-off 4 20))
         (iat-dir-off (+ opt-off 112 (* 12 8)))
         (idata-raw #xa00)
         (idata-rva #x4000)
         (ilt-rva (nelisp-pe-write-test--read-le32 bytes idata-raw))
         (name-rva (nelisp-pe-write-test--read-le32 bytes (+ idata-raw 12)))
         (first-thunk-rva (nelisp-pe-write-test--read-le32 bytes (+ idata-raw 16)))
         (iat-rva (nelisp-pe-write-test--read-le32 bytes iat-dir-off))
         (iat-size (nelisp-pe-write-test--read-le32 bytes (+ iat-dir-off 4)))
         (exit-hint-rva (nelisp-pe-write-test--read-le64
                         bytes (+ idata-raw (- iat-rva idata-rva))))
         (get-hint-rva (nelisp-pe-write-test--read-le64
                        bytes (+ idata-raw (- (+ iat-rva 8) idata-rva))))
         (write-hint-rva (nelisp-pe-write-test--read-le64
                          bytes (+ idata-raw (- (+ iat-rva 16) idata-rva))))
         (dll-name-off (+ idata-raw (- name-rva idata-rva)))
         (exit-hint-off (+ idata-raw (- exit-hint-rva idata-rva)))
         (get-hint-off (+ idata-raw (- get-hint-rva idata-rva)))
         (write-hint-off (+ idata-raw (- write-hint-rva idata-rva))))
    (should (= ilt-rva #x4028))
    (should (= first-thunk-rva #x4048))
    (should (= iat-rva #x4048))
    (should (= iat-size 32))
    (should (string-prefix-p "KERNEL32.dll"
                             (substring bytes dll-name-off (+ dll-name-off 13))))
    (should (string-prefix-p "ExitProcess"
                             (substring bytes (+ exit-hint-off 2)
                                        (+ exit-hint-off 14))))
    (should (string-prefix-p "GetStdHandle"
                             (substring bytes (+ get-hint-off 2)
                                        (+ get-hint-off 15))))
    (should (string-prefix-p "WriteFile"
                             (substring bytes (+ write-hint-off 2)
                                        (+ write-hint-off 11))))))

(ert-deftest nelisp-pe-write-exe-binary-writefile-stdout-entry-code ()
  "The WriteFile smoke EXE sets Win64 args including the 5th stack arg."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'writefile-stdout-exit-42))
         (text-off #x400)
         (message-len (length "hello from nelisp windows\n")))
    (should (equal (substring bytes text-off (+ text-off 4))
                   (unibyte-string #x48 #x83 #xec #x38)))
    (should (= (aref bytes (+ text-off 4)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 5))
               #xfffffff5))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 11)) #x3041))
    (should (equal (substring bytes (+ text-off 15) (+ text-off 18))
                   (unibyte-string #x48 #x89 #xc1)))
    (should (equal (substring bytes (+ text-off 18) (+ text-off 21))
                   (unibyte-string #x48 #x8d #x15)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 21)) #xfe7))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 27))
               message-len))
    (should (equal (substring bytes (+ text-off 31) (+ text-off 34))
                   (unibyte-string #x4c #x8d #x0d)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 34)) #x1fda))
    (should (equal (substring bytes (+ text-off 38) (+ text-off 47))
                   (unibyte-string #x48 #xc7 #x44 #x24 #x20
                                   #x00 #x00 #x00 #x00)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 49)) #x3023))
    (should (equal (substring bytes (+ text-off 53) (+ text-off 57))
                   (unibyte-string #x85 #xc0 #x74 #x0b)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 64)) #x3004))
    (should (= (aref bytes (+ text-off 68)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 69)) 1))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 75)) #x2ff9))
    (should (= (aref bytes (+ text-off 79)) #xcc))))

(ert-deftest nelisp-pe-write-exe-binary-readfile-stdin-section-table ()
  "The ReadFile smoke EXE has .data storage and .idata imports."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'readfile-stdin-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (file-off (+ pe-off 4))
         (opt-off (+ file-off 20))
         (sect0 (+ pe-off 4 20 240))
         (sect1 (+ sect0 40))
         (sect2 (+ sect1 40))
         (text-raw #x200)
         (data-raw #x400)
         (idata-raw #x600))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ file-off 2)) 3))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 4)) #x200))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 8)) #x400))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 56)) #x4000))
    (should (string-prefix-p ".text" (substring bytes sect0 (+ sect0 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect0 20)) text-raw))
    (should (string-prefix-p ".data" (substring bytes sect1 (+ sect1 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 12)) #x2000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 20)) data-raw))
    (should (string-prefix-p ".idata" (substring bytes sect2 (+ sect2 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 12)) #x3000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 20)) idata-raw))
    (dotimes (i 8)
      (should (= (aref bytes (+ data-raw i)) 0)))))

(ert-deftest nelisp-pe-write-exe-binary-readfile-stdin-import-directory ()
  "The ReadFile smoke EXE imports ExitProcess, GetStdHandle and ReadFile."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'readfile-stdin-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (opt-off (+ pe-off 4 20))
         (iat-dir-off (+ opt-off 112 (* 12 8)))
         (idata-raw #x600)
         (idata-rva #x3000)
         (ilt-rva (nelisp-pe-write-test--read-le32 bytes idata-raw))
         (name-rva (nelisp-pe-write-test--read-le32 bytes (+ idata-raw 12)))
         (first-thunk-rva (nelisp-pe-write-test--read-le32 bytes (+ idata-raw 16)))
         (iat-rva (nelisp-pe-write-test--read-le32 bytes iat-dir-off))
         (iat-size (nelisp-pe-write-test--read-le32 bytes (+ iat-dir-off 4)))
         (exit-hint-rva (nelisp-pe-write-test--read-le64
                         bytes (+ idata-raw (- iat-rva idata-rva))))
         (get-hint-rva (nelisp-pe-write-test--read-le64
                        bytes (+ idata-raw (- (+ iat-rva 8) idata-rva))))
         (read-hint-rva (nelisp-pe-write-test--read-le64
                         bytes (+ idata-raw (- (+ iat-rva 16) idata-rva))))
         (dll-name-off (+ idata-raw (- name-rva idata-rva)))
         (exit-hint-off (+ idata-raw (- exit-hint-rva idata-rva)))
         (get-hint-off (+ idata-raw (- get-hint-rva idata-rva)))
         (read-hint-off (+ idata-raw (- read-hint-rva idata-rva))))
    (should (= ilt-rva #x3028))
    (should (= first-thunk-rva #x3048))
    (should (= iat-rva #x3048))
    (should (= iat-size 32))
    (should (string-prefix-p "KERNEL32.dll"
                             (substring bytes dll-name-off (+ dll-name-off 13))))
    (should (string-prefix-p "ExitProcess"
                             (substring bytes (+ exit-hint-off 2)
                                        (+ exit-hint-off 14))))
    (should (string-prefix-p "GetStdHandle"
                             (substring bytes (+ get-hint-off 2)
                                        (+ get-hint-off 15))))
    (should (string-prefix-p "ReadFile"
                             (substring bytes (+ read-hint-off 2)
                                        (+ read-hint-off 10))))))

(ert-deftest nelisp-pe-write-exe-binary-readfile-stdin-entry-code ()
  "The ReadFile smoke EXE sets Win64 args including the 5th stack arg."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'readfile-stdin-exit-42))
         (text-off #x200))
    (should (equal (substring bytes text-off (+ text-off 4))
                   (unibyte-string #x48 #x83 #xec #x38)))
    (should (= (aref bytes (+ text-off 4)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 5))
               #xfffffff6))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 11)) #x2041))
    (should (equal (substring bytes (+ text-off 15) (+ text-off 18))
                   (unibyte-string #x48 #x89 #xc1)))
    (should (equal (substring bytes (+ text-off 18) (+ text-off 21))
                   (unibyte-string #x48 #x8d #x15)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 21)) #xfe7))
    (should (equal (substring bytes (+ text-off 25) (+ text-off 28))
                   (unibyte-string #x45 #x31 #xc0)))
    (should (equal (substring bytes (+ text-off 28) (+ text-off 31))
                   (unibyte-string #x4c #x8d #x0d)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 31)) #xfe1))
    (should (equal (substring bytes (+ text-off 35) (+ text-off 44))
                   (unibyte-string #x48 #xc7 #x44 #x24 #x20
                                   #x00 #x00 #x00 #x00)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 46)) #x2026))
    (should (equal (substring bytes (+ text-off 50) (+ text-off 54))
                   (unibyte-string #x85 #xc0 #x74 #x0b)))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 61)) #x2007))
    (should (= (aref bytes (+ text-off 65)) #xb9))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 66)) 1))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ text-off 72)) #x1ffc))
    (should (= (aref bytes (+ text-off 76)) #xcc))))

(ert-deftest nelisp-pe-write-exe-binary-createfile-write-section-table ()
  "The CreateFileW smoke EXE has path/message rdata, data, and imports."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'createfile-write-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (file-off (+ pe-off 4))
         (opt-off (+ file-off 20))
         (sect0 (+ pe-off 4 20 240))
         (sect1 (+ sect0 40))
         (sect2 (+ sect1 40))
         (sect3 (+ sect2 40))
         (text-raw #x400)
         (rdata-raw #x600)
         (data-raw #x800)
         (idata-raw #xa00)
         (path-bytes
          (nelisp-pe--utf16le-z-bytes
           "target\\windows-smoke\\nelisp-windows-createfile.tmp"))
         (message "createfile smoke\n"))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ file-off 2)) 4))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 4)) #x200))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 8)) #x600))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 56)) #x5000))
    (should (string-prefix-p ".text" (substring bytes sect0 (+ sect0 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect0 20)) text-raw))
    (should (string-prefix-p ".rdata" (substring bytes sect1 (+ sect1 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 12)) #x2000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 20)) rdata-raw))
    (should (string-prefix-p ".data" (substring bytes sect2 (+ sect2 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 8)) 12))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 12)) #x3000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 20)) data-raw))
    (should (string-prefix-p ".idata" (substring bytes sect3 (+ sect3 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect3 12)) #x4000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect3 20)) idata-raw))
    (should (equal (substring bytes rdata-raw (+ rdata-raw (length path-bytes)))
                   path-bytes))
    (should (equal (substring bytes
                              (+ rdata-raw (length path-bytes))
                              (+ rdata-raw (length path-bytes) (length message)))
                   message))
    (dotimes (i 12)
      (should (= (aref bytes (+ data-raw i)) 0)))))

(ert-deftest nelisp-pe-write-exe-binary-createfile-write-import-directory ()
  "The CreateFileW smoke EXE imports file lifecycle APIs."
  (let ((bytes (nelisp-pe-write-test--emit-exe 'createfile-write-exit-42)))
    (dolist (name '("KERNEL32.dll"
                    "ExitProcess"
                    "CreateFileW"
                    "WriteFile"
                    "CloseHandle"
                    "DeleteFileW"))
      (should (nelisp-pe-write-test--contains-p bytes name)))))

(ert-deftest nelisp-pe-write-exe-binary-createfile-write-entry-code ()
  "The CreateFileW smoke EXE sets seven CreateFileW args and cleans up."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'createfile-write-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (sect0 (+ pe-off 4 20 240))
         (text-size (nelisp-pe-write-test--read-le32 bytes (+ sect0 8)))
         (text-off #x400))
    (should (equal (substring bytes text-off (+ text-off 4))
                   (unibyte-string #x48 #x83 #xec #x48)))
    (should (equal (substring bytes (+ text-off 4) (+ text-off 7))
                   (unibyte-string #x48 #x8d #x0d)))
    (should (equal (substring bytes (+ text-off 11) (+ text-off 16))
                   (unibyte-string #xba #x00 #x00 #x00 #x40)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x45 #x31 #xc0 #x45 #x31 #xc9)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #xc7 #x44 #x24 #x20
                                   #x02 #x00 #x00 #x00)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #xc7 #x44 #x24 #x28
                                   #x80 #x00 #x00 #x00)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x48 #xc7 #x44 #x24 #x30
                                   #x00 #x00 #x00 #x00)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x48 #x83 #xf8 #xff #x74 #x5e)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x48 #x89 #x05)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x48 #xc7 #x44 #x24 #x20
                                   #x00 #x00 #x00 #x00)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x85 #xc0 #x74 #x2d)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x85 #xc0 #x74 #x1c)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x85 #xc0 #x74 #x0b)))
    (should (= (aref bytes (+ text-off (1- text-size))) #xcc))))

(ert-deftest nelisp-pe-write-exe-binary-setfilepointer-section-table ()
  "The SetFilePointerEx smoke EXE has path rdata, data, and imports."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'setfilepointer-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (file-off (+ pe-off 4))
         (opt-off (+ file-off 20))
         (sect0 (+ pe-off 4 20 240))
         (sect1 (+ sect0 40))
         (sect2 (+ sect1 40))
         (sect3 (+ sect2 40))
         (text-raw #x400)
         (rdata-raw #x600)
         (data-raw #x800)
         (idata-raw #xa00)
         (path-bytes
          (nelisp-pe--utf16le-z-bytes
           "target\\windows-smoke\\nelisp-windows-setfilepointer.tmp")))
    (should (= (nelisp-pe-write-test--read-le16 bytes (+ file-off 2)) 4))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 4)) #x200))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 8)) #x600))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ opt-off 56)) #x5000))
    (should (string-prefix-p ".text" (substring bytes sect0 (+ sect0 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect0 20)) text-raw))
    (should (string-prefix-p ".rdata" (substring bytes sect1 (+ sect1 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 12)) #x2000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect1 20)) rdata-raw))
    (should (string-prefix-p ".data" (substring bytes sect2 (+ sect2 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 8)) 16))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 12)) #x3000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect2 20)) data-raw))
    (should (string-prefix-p ".idata" (substring bytes sect3 (+ sect3 8))))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect3 12)) #x4000))
    (should (= (nelisp-pe-write-test--read-le32 bytes (+ sect3 20)) idata-raw))
    (should (equal (substring bytes rdata-raw (+ rdata-raw (length path-bytes)))
                   path-bytes))
    (dotimes (i 16)
      (should (= (aref bytes (+ data-raw i)) 0)))))

(ert-deftest nelisp-pe-write-exe-binary-setfilepointer-import-directory ()
  "The SetFilePointerEx smoke EXE imports file seek lifecycle APIs."
  (let ((bytes (nelisp-pe-write-test--emit-exe 'setfilepointer-exit-42)))
    (dolist (name '("KERNEL32.dll"
                    "ExitProcess"
                    "CreateFileW"
                    "SetFilePointerEx"
                    "CloseHandle"
                    "DeleteFileW"))
      (should (nelisp-pe-write-test--contains-p bytes name)))))

(ert-deftest nelisp-pe-write-exe-binary-setfilepointer-entry-code ()
  "The SetFilePointerEx smoke EXE seeks to FILE_BEGIN and cleans up."
  (let* ((bytes (nelisp-pe-write-test--emit-exe 'setfilepointer-exit-42))
         (pe-off (nelisp-pe-write-test--read-le32 bytes #x3c))
         (sect0 (+ pe-off 4 20 240))
         (text-size (nelisp-pe-write-test--read-le32 bytes (+ sect0 8)))
         (text-off #x400))
    (should (equal (substring bytes text-off (+ text-off 4))
                   (unibyte-string #x48 #x83 #xec #x48)))
    (should (equal (substring bytes (+ text-off 4) (+ text-off 7))
                   (unibyte-string #x48 #x8d #x0d)))
    (should (equal (substring bytes (+ text-off 11) (+ text-off 16))
                   (unibyte-string #xba #x00 #x00 #x00 #x40)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x45 #x31 #xc0 #x45 #x31 #xc9)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #xc7 #x44 #x24 #x20
                                   #x02 #x00 #x00 #x00)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #xc7 #x44 #x24 #x28
                                   #x80 #x00 #x00 #x00)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x48 #xc7 #x44 #x24 #x30
                                   #x00 #x00 #x00 #x00)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x48 #x83 #xf8 #xff #x74 #x4d)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x48 #x89 #x05)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x31 #xd2)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x4c #x8d #x05)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x45 #x31 #xc9)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x85 #xc0 #x74 #x2d)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x85 #xc0 #x74 #x1c)))
    (should (nelisp-pe-write-test--contains-p
             bytes (unibyte-string #x85 #xc0 #x74 #x0b)))
    (should (= (aref bytes (+ text-off (1- text-size))) #xcc))))

(provide 'nelisp-pe-write-test)

;;; nelisp-pe-write-test.el ends here
