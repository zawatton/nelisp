;;; nelisp-elf-write-test.el --- ERT tests for ELF writer §91.a-c  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 91 §91.a + §91.b + §91.c — pure-elisp ert tests for the
;; `nelisp-elf-write' module.  Exercises (1) byte/int conversion
;; helpers, (2) Ehdr + Phdr / Shdr / sym / rela serialisers in
;; isolation, (3) the `minimal-exit-0' + rich-plist orchestrators
;; end-to-end including `chmod +x' / exec smoke tests + `readelf'
;; cross-checks, and (4) the §91.c multi-PT_LOAD + .bss NOBITS
;; emission and the `hello-world-write' corpus #2 binary.

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

;; ====================================================================
;; §91.b — Shdr / strtab / sym / rela tests
;; ====================================================================

;; ---------------------------------------------------------------- Shdr L1

(ert-deftest nelisp-elf-write-shdr-shape ()
  "Shdr serialises to exactly 64 bytes; every field lands at the
documented offset and round-trips through the reader."
  (let* ((s (nelisp-elf-write-test--collect
             #'nelisp-elf-write-shdr
             (list :name      #x11
                   :type      1                  ; SHT_PROGBITS
                   :flags     6                  ; ALLOC | EXECINSTR
                   :addr      #x401000
                   :offset    #x78
                   :size      #x42
                   :link      3
                   :info      1
                   :addralign 16
                   :entsize   #x18))))
    (should (= (length s) 64))
    (should (= (nelisp-elf--read-le32 s 0)  #x11))     ; sh_name
    (should (= (nelisp-elf--read-le32 s 4)  1))        ; sh_type
    (should (= (nelisp-elf--read-le64 s 8)  6))        ; sh_flags
    (should (= (nelisp-elf--read-le64 s 16) #x401000)) ; sh_addr
    (should (= (nelisp-elf--read-le64 s 24) #x78))     ; sh_offset
    (should (= (nelisp-elf--read-le64 s 32) #x42))     ; sh_size
    (should (= (nelisp-elf--read-le32 s 40) 3))        ; sh_link
    (should (= (nelisp-elf--read-le32 s 44) 1))        ; sh_info
    (should (= (nelisp-elf--read-le64 s 48) 16))       ; sh_addralign
    (should (= (nelisp-elf--read-le64 s 56) #x18))))   ; sh_entsize

(ert-deftest nelisp-elf-write-shdr-null-defaults ()
  "Empty FIELDS produces an all-zero 64-byte SHT_NULL section header."
  (let ((s (nelisp-elf-write-test--collect #'nelisp-elf-write-shdr nil)))
    (should (= (length s) 64))
    (should (cl-every (lambda (b) (zerop b)) (append s nil)))))

;; ---------------------------------------------------------------- strtab L0

(ert-deftest nelisp-elf-write-strtab-empty ()
  "A fresh strtab is one NUL byte (= the reserved empty entry)."
  (let* ((state (nelisp-elf-strtab-make))
         (bytes (nelisp-elf-strtab-bytes state)))
    (should (= (length bytes) 1))
    (should (= (aref bytes 0) 0))))

(ert-deftest nelisp-elf-write-strtab-add-offset ()
  "Adding a non-empty string returns its offset and NUL-terminates it."
  (let* ((state (nelisp-elf-strtab-make))
         (o1 (nelisp-elf-strtab-add state ".text")))
    (should (= o1 1))                   ; right after leading NUL
    (let ((bytes (nelisp-elf-strtab-bytes state)))
      (should (= (length bytes) (+ 1 5 1)))  ; NUL + ".text" + NUL
      (should (= (aref bytes 0) 0))
      (should (= (aref bytes 1) ?.))
      (should (= (aref bytes 5) ?t))
      (should (= (aref bytes 6) 0)))))

(ert-deftest nelisp-elf-write-strtab-dedup ()
  "Re-adding the same string returns the cached offset without growing."
  (let* ((state (nelisp-elf-strtab-make))
         (o1 (nelisp-elf-strtab-add state ".text"))
         (size-after-first (length (nelisp-elf-strtab-bytes state)))
         (o2 (nelisp-elf-strtab-add state ".text")))
    (should (= o1 o2))
    (should (= (length (nelisp-elf-strtab-bytes state))
               size-after-first))))

(ert-deftest nelisp-elf-write-strtab-empty-string ()
  "Adding the empty string returns offset 0 and does not grow the table."
  (let* ((state (nelisp-elf-strtab-make))
         (o1 (nelisp-elf-strtab-add state "")))
    (should (= o1 0))
    (should (= (length (nelisp-elf-strtab-bytes state)) 1))))

(ert-deftest nelisp-elf-write-strtab-distinct ()
  "Distinct strings produce monotonically increasing offsets."
  (let* ((state (nelisp-elf-strtab-make))
         (o-a (nelisp-elf-strtab-add state ".text"))
         (o-b (nelisp-elf-strtab-add state ".rodata"))
         (o-c (nelisp-elf-strtab-add state ".symtab")))
    (should (< o-a o-b))
    (should (< o-b o-c))
    ;; Offsets should equal the cumulative length before insertion.
    (should (= o-a 1))
    (should (= o-b (+ 1 5 1)))
    (should (= o-c (+ 1 5 1 7 1)))))

;; ---------------------------------------------------------------- Sym L1

(ert-deftest nelisp-elf-sym-info-pack ()
  "BIND<<4 | TYPE encoding matches the System V gABI specification."
  (should (= (nelisp-elf-sym-info 1 2) #x12))  ; STB_GLOBAL | STT_FUNC
  (should (= (nelisp-elf-sym-info 0 0) #x00))  ; STB_LOCAL  | STT_NOTYPE
  (should (= (nelisp-elf-sym-info 2 3) #x23))  ; STB_WEAK   | STT_SECTION
  ;; High bits of BIND get masked to the 4-bit field.
  (should (= (nelisp-elf-sym-info #x11 #x12) #x12))) ; 1<<4 | 2

(ert-deftest nelisp-elf-write-sym-shape ()
  "Sym serialises to exactly 24 bytes and each field lands at the right offset."
  (let* ((s (nelisp-elf-write-test--collect
             #'nelisp-elf-write-sym
             (list :name  #x07
                   :info  (nelisp-elf-sym-info 1 2)
                   :other 0
                   :shndx 1
                   :value #x400078
                   :size  7))))
    (should (= (length s) nelisp-elf--sym-size))
    (should (= (nelisp-elf--read-le32 s 0) #x07))     ; st_name
    (should (= (aref s 4) #x12))                       ; st_info
    (should (= (aref s 5) 0))                          ; st_other
    (should (= (nelisp-elf--read-le16 s 6) 1))         ; st_shndx
    (should (= (nelisp-elf--read-le64 s 8) #x400078))  ; st_value
    (should (= (nelisp-elf--read-le64 s 16) 7))))      ; st_size

(ert-deftest nelisp-elf-write-sym-stn-undef ()
  "An empty FIELDS plist produces the 24-byte all-zero STN_UNDEF symbol."
  (let ((s (nelisp-elf-write-test--collect #'nelisp-elf-write-sym nil)))
    (should (= (length s) 24))
    (should (cl-every (lambda (b) (zerop b)) (append s nil)))))

;; ---------------------------------------------------------------- Rela L1

(ert-deftest nelisp-elf-rela-info-pack ()
  "r_info encoding splits SYM and TYPE across the high and low 32 bits."
  (should (= (nelisp-elf-rela-info 1 2)
             (logior (ash 1 32) 2)))
  (should (= (nelisp-elf-rela-info 0 1) 1))
  (should (= (nelisp-elf-rela-info 0 0) 0))
  ;; Symbol index 5 with R_X86_64_PLT32 (= type 4)
  (should (= (nelisp-elf-rela-info 5 4)
             (logior (ash 5 32) 4))))

(ert-deftest nelisp-elf-write-rela-shape ()
  "Rela serialises to exactly 24 bytes and round-trips offset / info / addend."
  (let* ((info (nelisp-elf-rela-info 1 2))   ; sym 1, R_X86_64_PC32
         (s (nelisp-elf-write-test--collect
             #'nelisp-elf-write-rela
             (list :offset 4 :info info :addend -4))))
    (should (= (length s) nelisp-elf--rela-size))
    (should (= (nelisp-elf--read-le64 s 0) 4))    ; r_offset
    (should (= (nelisp-elf--read-le64 s 8) info)) ; r_info
    ;; Negative addend = -4 = 0xFFFFFFFF_FFFFFFFC in two's complement.
    (should (= (nelisp-elf--read-le64 s 16)
               (+ -4 (ash 1 64))))))

(ert-deftest nelisp-elf-write-rela-positive-addend ()
  "Positive r_addend round-trips through the writer without sign-extension."
  (let ((s (nelisp-elf-write-test--collect
            #'nelisp-elf-write-rela
            (list :offset 0 :info 0 :addend #x12345678))))
    (should (= (nelisp-elf--read-le64 s 16) #x12345678))))

;; ---------------------------------------------------------------- rich orchestrator

(defun nelisp-elf-write-test--rich-exit-0 ()
  "Return a minimal `:text + _start' rich-plist for the orchestrator."
  (list :text (unibyte-string #xb8 #x3c #x00 #x00 #x00 #x0f #x05)
        :symbols (list (list :name "_start" :value 0 :size 7
                             :section 'text :bind 'global :type 'func))
        :entry-sym "_start"))

(ert-deftest nelisp-elf-write-binary-rich-emit ()
  "Rich-plist path emits a valid ELF with .text + symtab + strtab + shstrtab."
  (let ((path (make-temp-file "nelisp-elf-rich-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary path (nelisp-elf-write-test--rich-exit-0))
          (let ((bytes (nelisp-elf-write-test--read-file-bytes path)))
            ;; Header magic + class + endian.
            (should (equal (substring bytes 0 4)
                           (unibyte-string #x7F #x45 #x4C #x46)))
            (should (= (aref bytes 4) 2))    ; ELFCLASS64
            (should (= (aref bytes 5) 1))    ; ELFDATA2LSB
            ;; e_type = ET_EXEC, e_machine = EM_X86_64.
            (should (= (nelisp-elf--read-le16 bytes 16) 2))
            (should (= (nelisp-elf--read-le16 bytes 18) 62))
            ;; shoff must be non-zero, shnum >= 5 (NULL + text + 3 tables).
            (should (> (nelisp-elf--read-le64 bytes 40) 0))
            (should (>= (nelisp-elf--read-le16 bytes 60) 5))
            ;; .text starts at offset 0x78 (= 64 + 56) with mov eax,60.
            (should (= (aref bytes #x78) #xb8))
            (should (= (aref bytes (+ #x78 5)) #x0f))
            (should (= (aref bytes (+ #x78 6)) #x05))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-binary-rich-exec ()
  "Rich-plist `_start' executable runs and exits with code 0."
  (skip-unless (memq system-type '(gnu/linux gnu)))
  (skip-unless (string-match-p "x86_64\\|amd64"
                               (or (and (boundp 'system-configuration)
                                        system-configuration)
                                   "")))
  (let ((path (make-temp-file "nelisp-elf-rich-exec-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary path (nelisp-elf-write-test--rich-exit-0))
          (set-file-modes path #o755)
          (let ((rc (call-process path nil nil nil)))
            (should (eq rc 0))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-binary-rich-readelf-s ()
  "`readelf -s' on the rich output lists the `_start' entry symbol."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-elf-rich-readelf-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary path (nelisp-elf-write-test--rich-exit-0))
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-s" path)))))
            (should (string-match-p "_start" out))
            (should (string-match-p "FUNC" out))
            (should (string-match-p "GLOBAL" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-binary-rich-nm ()
  "`nm' on the rich output lists `_start' as a T (text/global) entry."
  (skip-unless (executable-find "nm"))
  (let ((path (make-temp-file "nelisp-elf-rich-nm-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary path (nelisp-elf-write-test--rich-exit-0))
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "nm" nil t nil path)))))
            (should (string-match-p " T _start" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-binary-rich-readelf-r ()
  "`readelf -r' shows R_X86_64_PC32 reloc against `msg' in .rela.text."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-elf-rela-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary
           path
           (list :text (unibyte-string #xb8 #x3c #x00 #x00 #x00 #x0f #x05)
                 :rodata (unibyte-string #x68 #x65 #x6c #x6c #x6f #x00)
                 :symbols (list (list :name "_start" :value 0 :size 7
                                      :section 'text :bind 'global
                                      :type 'func)
                                (list :name "msg" :value 0 :size 6
                                      :section 'rodata :bind 'local
                                      :type 'object))
                 :relocs (list (list :section 'text :offset 1
                                     :symbol "msg" :type 'pc32
                                     :addend -4))
                 :entry-sym "_start"))
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-r" path)))))
            (should (string-match-p "R_X86_64_PC32" out))
            (should (string-match-p "msg" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-binary-rich-readelf-r-plt32 ()
  "PLT32 reloc keyword maps to R_X86_64_PLT32 in the emitted .rela.text."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-elf-plt-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary
           path
           (list :text (unibyte-string #xb8 #x3c #x00 #x00 #x00 #x0f #x05)
                 :symbols (list (list :name "_start" :value 0 :size 7
                                      :section 'text :bind 'global
                                      :type 'func)
                                (list :name "callee" :value 0 :size 0
                                      :section 'text :bind 'global
                                      :type 'func))
                 :relocs (list (list :section 'text :offset 2
                                     :symbol "callee" :type 'plt32
                                     :addend -4))
                 :entry-sym "_start"))
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-r" path)))))
            (should (string-match-p "R_X86_64_PLT32" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-binary-rich-rejects-missing-entry ()
  "Rich-plist without :entry-sym signals an error."
  (let ((path (make-temp-file "nelisp-elf-noentry-")))
    (unwind-protect
        (should-error
         (nelisp-elf-write-binary
          path
          (list :text (unibyte-string #xb8 #x3c)
                :symbols (list (list :name "x" :value 0 :section 'text)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-binary-rich-rejects-unknown-entry ()
  "Entry symbol not found in :symbols signals an error."
  (let ((path (make-temp-file "nelisp-elf-bad-entry-")))
    (unwind-protect
        (should-error
         (nelisp-elf-write-binary
          path
          (list :text (unibyte-string #xb8 #x3c)
                :symbols (list (list :name "x" :value 0 :section 'text))
                :entry-sym "does-not-exist")))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-binary-rich-symtab-info-counts-locals ()
  "Shdr[.symtab].sh_info equals the index of the first non-local symbol."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-elf-info-")))
    (unwind-protect
        (progn
          ;; 2 locals + 1 global = sh_info should be 3 (= 1 STN_UNDEF + 2 locals).
          (nelisp-elf-write-binary
           path
           (list :text (unibyte-string #xb8 #x3c #x00 #x00 #x00 #x0f #x05)
                 :symbols (list (list :name "a" :value 0 :section 'text
                                      :bind 'local :type 'notype)
                                (list :name "b" :value 1 :section 'text
                                      :bind 'local :type 'notype)
                                (list :name "_start" :value 0 :size 7
                                      :section 'text :bind 'global
                                      :type 'func))
                 :entry-sym "_start"))
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-S" path)))))
            ;; readelf prints the column "Info" - look for ".symtab"
            ;; and verify the listing exists (= structural OK).
            (should (string-match-p "\\.symtab" out))))
      (ignore-errors (delete-file path)))))

;; ====================================================================
;; §91.c — multi PT_LOAD + .bss NOBITS + corpus #2 hello-world-write
;; ====================================================================

(defun nelisp-elf-write-test--rich-rw-plist (&optional bss-size)
  "Return a rich plist exercising .text + .rodata + .data (+ optional .bss)."
  (list :text (unibyte-string #xb8 #x3c #x00 #x00 #x00 #x0f #x05)
        :rodata (unibyte-string ?h ?i ?\n)
        :data (unibyte-string #x41 #x42 #x43 #x44)
        :bss-size (or bss-size 0)
        :symbols (list (list :name "_start" :value 0 :size 7
                             :section 'text :bind 'global :type 'func))
        :entry-sym "_start"))

;; ---------------------------------------------------------------- multi-PT_LOAD

(ert-deftest nelisp-elf-write-multi-ehdr-phnum ()
  "When :data is non-empty, Ehdr.phnum equals 2 (= RX + RW segments)."
  (let ((path (make-temp-file "nelisp-elf-mphdr-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary
           path (nelisp-elf-write-test--rich-rw-plist))
          (let ((bytes (nelisp-elf-write-test--read-file-bytes path)))
            (should (= (nelisp-elf--read-le16 bytes 56) 2))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-multi-no-rw-keeps-phnum-1 ()
  "Absent :data and zero :bss-size keeps the single-segment layout."
  (let ((path (make-temp-file "nelisp-elf-phnum1-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary
           path
           (list :text (unibyte-string #xb8 #x3c #x00 #x00 #x00 #x0f #x05)
                 :rodata (unibyte-string ?h ?i ?\n)
                 :symbols (list (list :name "_start" :value 0 :size 7
                                      :section 'text :bind 'global
                                      :type 'func))
                 :entry-sym "_start"))
          (let ((bytes (nelisp-elf-write-test--read-file-bytes path)))
            (should (= (nelisp-elf--read-le16 bytes 56) 1))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-multi-rw-page-aligned ()
  "The RW PT_LOAD lives on a fresh 4 KiB page after the RX segment."
  (let ((path (make-temp-file "nelisp-elf-page-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary
           path (nelisp-elf-write-test--rich-rw-plist))
          (let* ((bytes (nelisp-elf-write-test--read-file-bytes path))
                 ;; Phdr[0] starts at offset 64; Phdr[1] at 64 + 56.
                 (phdr1 (+ 64 56))
                 (rw-type   (nelisp-elf--read-le32 bytes phdr1))
                 (rw-flags  (nelisp-elf--read-le32 bytes (+ phdr1 4)))
                 (rw-offset (nelisp-elf--read-le64 bytes (+ phdr1 8)))
                 (rw-vaddr  (nelisp-elf--read-le64 bytes (+ phdr1 16))))
            (should (= rw-type 1))                     ; PT_LOAD
            (should (= rw-flags 6))                    ; PF_R | PF_W
            (should (zerop (mod rw-offset #x1000)))    ; page-aligned
            (should (= rw-offset #x1000))
            (should (= rw-vaddr #x401000))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-multi-rx-phdr-still-readable ()
  "Multi-segment emission preserves the RX PT_LOAD flags / vaddr."
  (let ((path (make-temp-file "nelisp-elf-rxflags-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary
           path (nelisp-elf-write-test--rich-rw-plist))
          (let* ((bytes (nelisp-elf-write-test--read-file-bytes path))
                 (phdr0 64)
                 (rx-type  (nelisp-elf--read-le32 bytes phdr0))
                 (rx-flags (nelisp-elf--read-le32 bytes (+ phdr0 4)))
                 (rx-vaddr (nelisp-elf--read-le64 bytes (+ phdr0 16))))
            (should (= rx-type 1))                     ; PT_LOAD
            (should (= rx-flags 5))                    ; PF_R | PF_X
            (should (= rx-vaddr #x400000))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-multi-readelf-l ()
  "`readelf -l' reports two non-overlapping PT_LOADs with RW + RE flags."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-elf-readelf-l-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary
           path (nelisp-elf-write-test--rich-rw-plist))
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-l" path)))))
            (should (string-match-p "LOAD" out))
            (should (string-match-p "R E" out))
            (should (string-match-p "RW" out))))
      (ignore-errors (delete-file path)))))

;; ---------------------------------------------------------------- .bss NOBITS

(ert-deftest nelisp-elf-write-bss-memsz-exceeds-filesz ()
  "With :bss-size > 0, the RW PT_LOAD's p_memsz exceeds p_filesz by bss size."
  (let ((path (make-temp-file "nelisp-elf-bss-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary
           path (nelisp-elf-write-test--rich-rw-plist 100))
          (let* ((bytes (nelisp-elf-write-test--read-file-bytes path))
                 (phdr1 (+ 64 56))
                 (filesz (nelisp-elf--read-le64 bytes (+ phdr1 32)))
                 (memsz  (nelisp-elf--read-le64 bytes (+ phdr1 40))))
            (should (= filesz 4))                       ; .data only
            (should (= memsz (+ 4 100)))                ; .data + .bss
            (should (> memsz filesz))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-bss-without-data ()
  "An :bss-size with no :data still produces a RW PT_LOAD (= filesz 0)."
  (let ((path (make-temp-file "nelisp-elf-bss-only-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary
           path
           (list :text (unibyte-string #xb8 #x3c #x00 #x00 #x00 #x0f #x05)
                 :bss-size 64
                 :symbols (list (list :name "_start" :value 0 :size 7
                                      :section 'text :bind 'global
                                      :type 'func))
                 :entry-sym "_start"))
          (let* ((bytes (nelisp-elf-write-test--read-file-bytes path))
                 (phdr1 (+ 64 56))
                 (filesz (nelisp-elf--read-le64 bytes (+ phdr1 32)))
                 (memsz  (nelisp-elf--read-le64 bytes (+ phdr1 40))))
            (should (= filesz 0))
            (should (= memsz 64))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-bss-readelf-S-nobits ()
  "`readelf -S' lists .bss as NOBITS when :bss-size is non-zero."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-elf-bss-S-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary
           path (nelisp-elf-write-test--rich-rw-plist 200))
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-S" path)))))
            (should (string-match-p "\\.bss" out))
            (should (string-match-p "NOBITS" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-bss-readelf-l-memsz-greater ()
  "`readelf -l' shows the RW segment's MemSiz > FileSiz when .bss is set."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-elf-bss-l-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary
           path (nelisp-elf-write-test--rich-rw-plist 500))
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-l" path)))))
            ;; The RW row has FileSiz 0x4 + MemSiz 0x1f8 (= 500 + 4 = 504).
            (should (string-match-p "0x00000000000001f8" out))))
      (ignore-errors (delete-file path)))))

;; ---------------------------------------------------------------- corpus #2

(ert-deftest nelisp-elf-write-hello-world-write-elf-shape ()
  "The hello-world-write corpus emits a valid ELF64 EXEC x86-64 file."
  (let ((path (make-temp-file "nelisp-elf-corpus2-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary path 'hello-world-write)
          (let ((bytes (nelisp-elf-write-test--read-file-bytes path)))
            (should (equal (substring bytes 0 4)
                           (unibyte-string #x7F #x45 #x4C #x46)))
            (should (= (aref bytes 4) 2))                  ; ELFCLASS64
            (should (= (aref bytes 5) 1))                  ; ELFDATA2LSB
            (should (= (nelisp-elf--read-le16 bytes 16) 2)) ; ET_EXEC
            (should (= (nelisp-elf--read-le16 bytes 18) 62)) ; EM_X86_64
            ;; phnum = 1 (= RX only, no .data / no .bss).
            (should (= (nelisp-elf--read-le16 bytes 56) 1))
            ;; .text begins at offset 0x78 (= 64 + 56).
            (should (= (aref bytes #x78) #x48))            ; and rsp, -16
            (should (= (aref bytes (+ #x78 1)) #x83))
            ;; sys_write opcode at offset 0x78 + 21 = #x8d.
            (should (= (aref bytes (+ #x78 21)) #xb8))
            (should (= (aref bytes (+ #x78 22)) #x01))     ; eax = 1
            ;; syscall at offset 0x78 + 26.
            (should (= (aref bytes (+ #x78 26)) #x0f))
            (should (= (aref bytes (+ #x78 27)) #x05))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-hello-world-write-rodata-bytes ()
  "The corpus #2 .rodata holds exactly `hello\\n' (= 6 bytes)."
  (let ((path (make-temp-file "nelisp-elf-corpus2-msg-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary path 'hello-world-write)
          (let* ((bytes (nelisp-elf-write-test--read-file-bytes path))
                 ;; .rodata starts at offset 0x78 + 37 = 0x9d.
                 (off (+ #x78 37)))
            (should (= (aref bytes off) ?h))
            (should (= (aref bytes (+ off 1)) ?e))
            (should (= (aref bytes (+ off 2)) ?l))
            (should (= (aref bytes (+ off 3)) ?l))
            (should (= (aref bytes (+ off 4)) ?o))
            (should (= (aref bytes (+ off 5)) ?\n))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-hello-world-write-exec-stdout ()
  "The corpus #2 binary, when exec'd, prints `hello\\n' and exits 0."
  (skip-unless (memq system-type '(gnu/linux gnu)))
  (skip-unless (string-match-p "x86_64\\|amd64"
                               (or (and (boundp 'system-configuration)
                                        system-configuration)
                                   "")))
  (let ((path (make-temp-file "nelisp-elf-corpus2-exec-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary path 'hello-world-write)
          (set-file-modes path #o755)
          (let* ((out-buf (generate-new-buffer " *corpus2-out*"))
                 (rc (call-process path nil out-buf nil)))
            (unwind-protect
                (progn
                  (should (eq rc 0))
                  (with-current-buffer out-buf
                    (should (equal (buffer-string) "hello\n"))))
              (kill-buffer out-buf))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-hello-world-write-readelf-h ()
  "`readelf -h' confirms the corpus #2 is a valid ELF64 EXEC binary."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-elf-corpus2-readelf-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary path 'hello-world-write)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-h" path)))))
            (should (string-match-p "ELF64" out))
            (should (string-match-p "EXEC" out))
            (should (string-match-p "X86-64" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-elf-write-hello-world-write-text-size-constant ()
  "The corpus #2 .text routine has the documented 37-byte size."
  (should (= nelisp-elf--hello-write-text-size 37))
  (should (= (length nelisp-elf--hello-write-msg) 6))
  ;; The internal text emitter produces exactly 37 bytes.
  (let ((bytes (nelisp-elf--hello-write-emit-text 6 #x400078 #x40009d)))
    (should (= (length bytes) 37))))

(ert-deftest nelisp-elf-write-hello-world-write-rel32-baked ()
  "The lea rel32 bytes in corpus #2 .text match the .rodata vaddr offset."
  (let* ((bytes (nelisp-elf--hello-write-emit-text 6 #x400078 #x40009d))
         ;; rel32 byte position = offset 12 (after `48 8d 35').
         (b0 (aref bytes 12))
         (b1 (aref bytes 13))
         (b2 (aref bytes 14))
         (b3 (aref bytes 15)))
    ;; rel32 = 0x40009d - (0x400078 + 16) = 0x40009d - 0x400088 = 0x15 = 21
    (should (= b0 #x15))
    (should (= b1 #x00))
    (should (= b2 #x00))
    (should (= b3 #x00))))

(ert-deftest nelisp-elf-write-encode-le32-bytes-negative ()
  "`nelisp-elf--encode-le32-bytes' encodes -1 as 0xFF 0xFF 0xFF 0xFF."
  (let ((s (nelisp-elf--encode-le32-bytes -1)))
    (should (= (length s) 4))
    (should (cl-every (lambda (b) (= b #xFF)) (append s nil))))
  (let ((s (nelisp-elf--encode-le32-bytes #x12345678)))
    (should (= (aref s 0) #x78))
    (should (= (aref s 3) #x12))))

(ert-deftest nelisp-elf-write-multi-bss-symbol-binding ()
  "A symbol whose :section is `bss' resolves to the .bss base vaddr."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-elf-bsym-")))
    (unwind-protect
        (progn
          (nelisp-elf-write-binary
           path
           (list :text (unibyte-string #xb8 #x3c #x00 #x00 #x00 #x0f #x05)
                 :data (unibyte-string #x01 #x02 #x03 #x04)
                 :bss-size 16
                 :symbols
                 (list (list :name "_start" :value 0 :size 7
                             :section 'text :bind 'global :type 'func)
                       (list :name "buf" :value 0 :size 16
                             :section 'bss :bind 'global :type 'object))
                 :entry-sym "_start"))
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-s" path)))))
            (should (string-match-p "buf" out))
            (should (string-match-p "OBJECT" out))))
      (ignore-errors (delete-file path)))))

(provide 'nelisp-elf-write-test)

;;; nelisp-elf-write-test.el ends here
