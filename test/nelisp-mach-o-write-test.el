;;; nelisp-mach-o-write-test.el --- ERT tests for Mach-O writer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 100 §100.D Stage 2/3 — byte-structure tests for the minimal
;; Mach-O arm64 ET_REL writer.  These parse the emitted .o directly on
;; Linux, without requiring macOS toolchain binaries.

;;; Code:

(require 'ert)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir
                      (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-mach-o-write)

(defun nelisp-mach-o-write-test--read-file-bytes (path)
  "Return raw unibyte bytes of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally path))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun nelisp-mach-o-write-test--read-le16 (bytes offset)
  "Read an unsigned 16-bit little-endian integer from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)))

(defun nelisp-mach-o-write-test--read-le32 (bytes offset)
  "Read an unsigned 32-bit little-endian integer from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)
          (ash (aref bytes (+ offset 2)) 16)
          (ash (aref bytes (+ offset 3)) 24)))

(defun nelisp-mach-o-write-test--read-le64 (bytes offset)
  "Read an unsigned 64-bit little-endian integer from BYTES at OFFSET."
  (let ((acc 0)
        (i 0))
    (while (< i 8)
      (setq acc (logior acc (ash (aref bytes (+ offset i)) (* i 8))))
      (setq i (1+ i)))
    acc))

(defconst nelisp-mach-o-write-test--sample-text
  (unibyte-string #x20 #x00 #x80 #xD2 #xC0 #x03 #x5F #xD6)
  "A tiny arm64 text payload used by the round-trip tests.")

(defconst nelisp-mach-o-write-test--sample-sections
  (list :text nelisp-mach-o-write-test--sample-text
        :symbols (list (list :name "nelisp_jit_add2"
                             :value 4
                             :size 4
                             :section 'text
                             :bind 'global
                             :type 'func))
        :machine 'aarch64
        :entry-sym "nelisp_jit_add2")
  "Sample Mach-O input plist.")

(defun nelisp-mach-o-write-test--emit-sample ()
  "Emit the sample object and return its raw bytes."
  (let ((path (make-temp-file "nelisp-mach-o-test-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-mach-o-write-binary path nelisp-mach-o-write-test--sample-sections)
          (nelisp-mach-o-write-test--read-file-bytes path))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-mach-o-write-binary-magic-bytes ()
  "The output starts with the little-endian MH_MAGIC_64 bytes."
  (let ((bytes (nelisp-mach-o-write-test--emit-sample)))
    (should (equal (substring bytes 0 4)
                   (unibyte-string #xCF #xFA #xED #xFE)))))

(ert-deftest nelisp-mach-o-write-binary-arm64-cputype ()
  "The header encodes CPU_TYPE_ARM64 at offset 4."
  (let ((bytes (nelisp-mach-o-write-test--emit-sample)))
    (should (= (nelisp-mach-o-write-test--read-le32 bytes 4) #x0100000C))))

(ert-deftest nelisp-mach-o-write-binary-ncmds-and-filetype ()
  "The header reports MH_OBJECT with two load commands."
  (let ((bytes (nelisp-mach-o-write-test--emit-sample)))
    (should (= (nelisp-mach-o-write-test--read-le32 bytes 12) 1))
    (should (= (nelisp-mach-o-write-test--read-le32 bytes 16) 2))))

(ert-deftest nelisp-mach-o-write-binary-symbol-entry-shape ()
  "The first nlist_64 entry is a global __text symbol with the right value."
  (let* ((bytes (nelisp-mach-o-write-test--emit-sample))
         (symoff (nelisp-mach-o-write-test--read-le32 bytes 192))
         (nsyms (nelisp-mach-o-write-test--read-le32 bytes 196)))
    (should (= nsyms 1))
    (should (= (aref bytes (+ symoff 4)) #x0F))
    (should (= (aref bytes (+ symoff 5)) 1))
    (should (= (nelisp-mach-o-write-test--read-le16 bytes (+ symoff 6)) 0))
    (should (= (nelisp-mach-o-write-test--read-le64 bytes (+ symoff 8)) 4))))

(ert-deftest nelisp-mach-o-write-binary-string-table-prefixes-underscore ()
  "The string table contains the leading-underscore symbol spelling."
  (let* ((bytes (nelisp-mach-o-write-test--emit-sample))
         (stroff (nelisp-mach-o-write-test--read-le32 bytes 200))
         (strsize (nelisp-mach-o-write-test--read-le32 bytes 204))
         (strtab (substring bytes stroff (+ stroff strsize))))
    (should (= (aref strtab 0) 0))
    (should (string-match-p "_nelisp_jit_add2\0" strtab))))

(ert-deftest nelisp-mach-o-write-binary-text-size-matches-input ()
  "The __text section size field and raw payload length match the input."
  (let* ((bytes (nelisp-mach-o-write-test--emit-sample))
         (section-off 104)
         (text-size (nelisp-mach-o-write-test--read-le64 bytes (+ section-off 40)))
         (text-offset (nelisp-mach-o-write-test--read-le32 bytes (+ section-off 48)))
         (raw (substring bytes text-offset (+ text-offset text-size))))
    (should (= text-size (length nelisp-mach-o-write-test--sample-text)))
    (should (equal raw nelisp-mach-o-write-test--sample-text))))

;;; ---- v2: relocation records + undefined imports ----

(defconst nelisp-mach-o-write-test--reloc-sections
  (list :text (unibyte-string
               #x00 #x00 #x00 #x94   ; bl 0 (placeholder)
               #xC0 #x03 #x5F #xD6)  ; ret
        :symbols (list (list :name "caller" :value 0 :size 8
                             :section 'text :bind 'global :type 'func)
                       (list :name "callee" :value 0 :size 0
                             :section 'undef :bind 'global :type 'notype))
        :relocs (list (list :offset 0 :type 'b26-pc
                            :symbol "callee" :addend 0))
        :machine 'aarch64)
  "Sample input carrying one BRANCH26 reloc against an undef import.")

(defun nelisp-mach-o-write-test--emit (sections)
  "Emit SECTIONS and return the object's raw bytes."
  (let ((path (make-temp-file "nelisp-mach-o-test-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-mach-o-write-binary path sections)
          (nelisp-mach-o-write-test--read-file-bytes path))
      (ignore-errors (delete-file path)))))

(defun nelisp-mach-o-write-test--reloc-record (bytes index)
  "Decode relocation_info INDEX via the __text section header in BYTES.
Returns (ADDR SYMBOLNUM PCREL LENGTH EXTERN TYPE)."
  (let* ((section-off 104)
         (reloff (nelisp-mach-o-write-test--read-le32 bytes (+ section-off 56)))
         (ro (+ reloff (* index 8)))
         (addr (nelisp-mach-o-write-test--read-le32 bytes ro))
         (packed (nelisp-mach-o-write-test--read-le32 bytes (+ ro 4))))
    (list addr
          (logand packed #xFFFFFF)
          (logand (ash packed -24) 1)
          (logand (ash packed -25) 3)
          (logand (ash packed -27) 1)
          (logand (ash packed -28) #xF))))

(ert-deftest nelisp-mach-o-write-binary-reloc-header-fields ()
  "reloff / nreloc land in the __text section_64 header."
  (let* ((bytes (nelisp-mach-o-write-test--emit
                 nelisp-mach-o-write-test--reloc-sections))
         (section-off 104)
         (reloff (nelisp-mach-o-write-test--read-le32 bytes (+ section-off 56)))
         (nreloc (nelisp-mach-o-write-test--read-le32 bytes (+ section-off 60)))
         (symoff (nelisp-mach-o-write-test--read-le32 bytes 192)))
    (should (= nreloc 1))
    (should (> reloff 0))
    ;; The symtab sits directly after the reloc records.
    (should (= symoff (+ reloff 8)))))

(ert-deftest nelisp-mach-o-write-binary-reloc-branch26-record ()
  "A b26-pc reloc becomes an extern pcrel ARM64_RELOC_BRANCH26."
  (let ((bytes (nelisp-mach-o-write-test--emit
                nelisp-mach-o-write-test--reloc-sections)))
    ;; symbolnum 1 = the undef `callee' nlist index.
    (should (equal (nelisp-mach-o-write-test--reloc-record bytes 0)
                   '(0 1 1 2 1 2)))))

(ert-deftest nelisp-mach-o-write-binary-undef-symbol-nlist ()
  "An `undef' symbol is written as N_UNDF|N_EXT with n_sect 0."
  (let* ((bytes (nelisp-mach-o-write-test--emit
                 nelisp-mach-o-write-test--reloc-sections))
         (symoff (nelisp-mach-o-write-test--read-le32 bytes 192))
         (undef-off (+ symoff 16)))
    (should (= (nelisp-mach-o-write-test--read-le32 bytes 196) 2))
    (should (= (aref bytes (+ undef-off 4)) #x01))
    (should (= (aref bytes (+ undef-off 5)) 0))
    (should (= (nelisp-mach-o-write-test--read-le64 bytes (+ undef-off 8)) 0))))

(ert-deftest nelisp-mach-o-write-binary-reloc-addend-record-pair ()
  "A non-zero addend expands into ADDEND + target record adjacency."
  (let* ((sections (copy-sequence nelisp-mach-o-write-test--reloc-sections))
         (sections (plist-put sections :relocs
                              (list (list :offset 0 :type 'b26-pc
                                          :symbol "callee" :addend 8))))
         (bytes (nelisp-mach-o-write-test--emit sections))
         (section-off 104)
         (nreloc (nelisp-mach-o-write-test--read-le32 bytes (+ section-off 60))))
    (should (= nreloc 2))
    ;; ARM64_RELOC_ADDEND: symbolnum carries the addend, extern 0, type 10.
    (should (equal (nelisp-mach-o-write-test--reloc-record bytes 0)
                   '(0 8 0 2 0 10)))
    (should (equal (nelisp-mach-o-write-test--reloc-record bytes 1)
                   '(0 1 1 2 1 2)))))

(ert-deftest nelisp-mach-o-write-binary-reloc-rejects-x86-64 ()
  "x86_64 reloc emission is not implemented and must signal."
  (let* ((sections (copy-sequence nelisp-mach-o-write-test--reloc-sections))
         (sections (plist-put sections :machine 'x86_64)))
    (should-error (nelisp-mach-o-write-test--emit sections))))

(ert-deftest nelisp-mach-o-write-binary-reloc-rejects-unknown-symbol ()
  "A reloc whose symbol is absent from :symbols must signal."
  (let* ((sections (copy-sequence nelisp-mach-o-write-test--reloc-sections))
         (sections (plist-put sections :relocs
                              (list (list :offset 0 :type 'b26-pc
                                          :symbol "nosuch" :addend 0)))))
    (should-error (nelisp-mach-o-write-test--emit sections))))

;;; ---- v3: multi-section (__const / __data / __bss) ----

(defconst nelisp-mach-o-write-test--multi-sections
  (list :text (unibyte-string #xC0 #x03 #x5F #xD6) ; ret
        :rodata (unibyte-string #x2A 0 0 0 0 0 0 0) ; magic = 42
        :data (unibyte-string 1 2 3 4)
        :bss-size 16
        :symbols (list (list :name "f" :value 0 :size 4
                             :section 'text :bind 'global :type 'func)
                       (list :name "magic" :value 0 :size 8
                             :section 'rodata :bind 'global :type 'object)
                       (list :name "state" :value 0 :size 4
                             :section 'data :bind 'global :type 'object))
        :machine 'aarch64)
  "Sample input with all four section kinds populated.")

(defun nelisp-mach-o-write-test--section-64 (bytes index)
  "Decode section_64 INDEX (0-based) from BYTES.
Returns (SECTNAME SEGNAME ADDR SIZE OFFSET RELOFF NRELOC FLAGS)."
  (let* ((off (+ 104 (* index 80)))
         (sectname (substring bytes off (+ off 16)))
         (segname (substring bytes (+ off 16) (+ off 32))))
    (list (substring sectname 0 (string-match "\0" sectname))
          (substring segname 0 (string-match "\0" segname))
          (nelisp-mach-o-write-test--read-le64 bytes (+ off 32))
          (nelisp-mach-o-write-test--read-le64 bytes (+ off 40))
          (nelisp-mach-o-write-test--read-le32 bytes (+ off 48))
          (nelisp-mach-o-write-test--read-le32 bytes (+ off 56))
          (nelisp-mach-o-write-test--read-le32 bytes (+ off 60))
          (nelisp-mach-o-write-test--read-le32 bytes (+ off 64)))))

(ert-deftest nelisp-mach-o-write-binary-multi-section-headers ()
  "__text/__const/__data/__bss headers carry a contiguous address space."
  (let* ((bytes (nelisp-mach-o-write-test--emit
                 nelisp-mach-o-write-test--multi-sections))
         (text (nelisp-mach-o-write-test--section-64 bytes 0))
         (const (nelisp-mach-o-write-test--section-64 bytes 1))
         (data (nelisp-mach-o-write-test--section-64 bytes 2))
         (bss (nelisp-mach-o-write-test--section-64 bytes 3)))
    ;; nsects = 4 in the segment header (offset 32+64 = 96).
    (should (= (nelisp-mach-o-write-test--read-le32 bytes 96) 4))
    (should (equal (car text) "__text"))
    (should (equal (car const) "__const"))
    (should (equal (cadr const) "__TEXT"))
    (should (equal (car data) "__data"))
    (should (equal (cadr data) "__DATA"))
    (should (equal (car bss) "__bss"))
    ;; text addr 0 size 4; const aligned to 8 -> addr 8; data addr 16;
    ;; bss addr 24 with S_ZEROFILL and file offset 0.
    (should (= (nth 2 text) 0))
    (should (= (nth 2 const) 8))
    (should (= (nth 2 data) 16))
    (should (= (nth 2 bss) 24))
    (should (= (nth 7 bss) #x1))
    (should (= (nth 4 bss) 0))
    ;; File offsets mirror addresses relative to the content base.
    (let ((base (nth 4 text)))
      (should (= (nth 4 const) (+ base 8)))
      (should (= (nth 4 data) (+ base 16))))))

(ert-deftest nelisp-mach-o-write-binary-multi-section-payloads ()
  "__const / __data payload bytes land at their header offsets."
  (let* ((bytes (nelisp-mach-o-write-test--emit
                 nelisp-mach-o-write-test--multi-sections))
         (const (nelisp-mach-o-write-test--section-64 bytes 1))
         (data (nelisp-mach-o-write-test--section-64 bytes 2)))
    (should (equal (substring bytes (nth 4 const) (+ (nth 4 const) 8))
                   (unibyte-string #x2A 0 0 0 0 0 0 0)))
    (should (equal (substring bytes (nth 4 data) (+ (nth 4 data) 4))
                   (unibyte-string 1 2 3 4)))))

(defun nelisp-mach-o-write-test--symoff (bytes)
  "Read symoff from LC_SYMTAB, whose position depends on nsects."
  (let* ((nsects (nelisp-mach-o-write-test--read-le32 bytes 96))
         (symtab-cmd (+ 32 72 (* nsects 80))))
    (nelisp-mach-o-write-test--read-le32 bytes (+ symtab-cmd 8))))

(ert-deftest nelisp-mach-o-write-binary-multi-section-symbol-values ()
  "Section symbols get ordinal n_sect and object-space n_value."
  (let* ((bytes (nelisp-mach-o-write-test--emit
                 nelisp-mach-o-write-test--multi-sections))
         (symoff (nelisp-mach-o-write-test--symoff bytes))
         (magic-off (+ symoff 16))
         (state-off (+ symoff 32)))
    ;; magic: n_sect 2 (__const), n_value 8.
    (should (= (aref bytes (+ magic-off 5)) 2))
    (should (= (nelisp-mach-o-write-test--read-le64 bytes (+ magic-off 8)) 8))
    ;; state: n_sect 3 (__data), n_value 16.
    (should (= (aref bytes (+ state-off 5)) 3))
    (should (= (nelisp-mach-o-write-test--read-le64 bytes (+ state-off 8)) 16))))

(ert-deftest nelisp-mach-o-write-binary-page-reloc-records ()
  "PAGE21/PAGEOFF12 relocs against a rodata symbol are emitted."
  (let* ((sections (copy-sequence nelisp-mach-o-write-test--multi-sections))
         (sections (plist-put sections :relocs
                              (list (list :offset 0 :type 'adr-prel-pg-hi21
                                          :symbol "magic" :addend 0)
                                    (list :offset 4 :type 'add-abs-lo12-nc
                                          :symbol "magic" :addend 0))))
         (bytes (nelisp-mach-o-write-test--emit sections)))
    ;; magic is nlist index 1; PAGE21 pcrel, PAGEOFF12 not.
    (should (equal (nelisp-mach-o-write-test--reloc-record bytes 0)
                   '(0 1 1 2 1 3)))
    (should (equal (nelisp-mach-o-write-test--reloc-record bytes 1)
                   '(4 1 0 2 1 4)))))

;;; nelisp-mach-o-write-test.el ends here
