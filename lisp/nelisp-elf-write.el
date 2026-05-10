;;; nelisp-elf-write.el --- ELF64 binary writer (Phase 47 spike)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 91 §91.a — header-skeleton ELF writer (= spike implementation).
;;
;; Pure-elisp emitter for ELF64 binaries.  Stage 1 scope = byte-int
;; helpers + Ehdr + Phdr writers + a minimal-binary orchestrator that
;; produces an `exit(0)' executable on x86_64-linux.  Sections (= Shdr,
;; symtab, strtab, rela) are deferred to §91.b / §91.c.
;;
;; Layout produced by `nelisp-elf-write-binary' for the
;; `minimal-exit-0' preset (see Brian Raiter's "Teensy ELF" tutorial):
;;
;;   offset 0x0000  Ehdr           (64 bytes)
;;   offset 0x0040  Phdr[0]        (56 bytes, PT_LOAD R+X)
;;   offset 0x0078  .text          (7 bytes: mov eax,60; syscall)
;;
;; The whole file is mapped into a single PT_LOAD segment at
;; vaddr = 0x400000 with file offset 0; the entry point lands at
;; vaddr + 0x78 (= start of .text after Ehdr + Phdr).
;;
;; The module is freestanding — not yet wired into nelisp-baker
;; STDLIB_FILES (= integration deferred per Doc 91 §8.1).

;;; Code:

;; ---- ELF format constants (= magic numbers from §4.4) ----

(defconst nelisp-elf--ei-class-64 2 "ELFCLASS64.")
(defconst nelisp-elf--ei-data-lsb 1 "ELFDATA2LSB (= little-endian).")
(defconst nelisp-elf--ev-current 1 "ELF version EV_CURRENT.")
(defconst nelisp-elf--ei-osabi-sysv 0 "ELFOSABI_NONE / SYSV.")

(defconst nelisp-elf--et-exec 2 "ET_EXEC.")
(defconst nelisp-elf--et-dyn 3 "ET_DYN.")

(defconst nelisp-elf--em-x86-64 62 "EM_X86_64 (= 0x3E).")
(defconst nelisp-elf--em-aarch64 183 "EM_AARCH64 (= 0xB7).")

(defconst nelisp-elf--pt-load 1 "PT_LOAD.")
(defconst nelisp-elf--pt-phdr 6 "PT_PHDR.")

(defconst nelisp-elf--pf-x 1 "PF_X (= executable).")
(defconst nelisp-elf--pf-w 2 "PF_W (= writable).")
(defconst nelisp-elf--pf-r 4 "PF_R (= readable).")

(defconst nelisp-elf--ehdr-size 64 "sizeof(Elf64_Ehdr).")
(defconst nelisp-elf--phdr-size 56 "sizeof(Elf64_Phdr).")
(defconst nelisp-elf--shdr-size 64 "sizeof(Elf64_Shdr).")

;; ---- byte / int conversion helpers (= §3.2) ----

(defsubst nelisp-elf--write-u8 (buf v)
  "Append byte V (0..255) to BUF (= unibyte buffer).  Ignored arg BUF
is the current buffer; the parameter exists so callers self-document."
  (ignore buf)
  (insert (unibyte-string (logand v #xff))))

(defun nelisp-elf--write-le16 (buf v)
  "Append unsigned 16-bit V to BUF in little-endian order (= 2 bytes)."
  (ignore buf)
  (insert (unibyte-string (logand v #xff)
                          (logand (ash v -8) #xff))))

(defun nelisp-elf--write-le32 (buf v)
  "Append unsigned 32-bit V to BUF in little-endian order (= 4 bytes)."
  (ignore buf)
  (insert (unibyte-string (logand v #xff)
                          (logand (ash v -8) #xff)
                          (logand (ash v -16) #xff)
                          (logand (ash v -24) #xff))))

(defun nelisp-elf--write-le64 (buf v)
  "Append unsigned 64-bit V to BUF in little-endian order (= 8 bytes).
Bignum-safe: shifts one byte at a time so values above 2^29 work on
32-bit Emacs as well (= §7.2 mitigation)."
  (ignore buf)
  (let ((bytes (make-vector 8 0))
        (i 0))
    (while (< i 8)
      (aset bytes i (logand (ash v (- (* i 8))) #xff))
      (setq i (1+ i)))
    (insert (apply #'unibyte-string (append bytes nil)))))

(defun nelisp-elf--write-bytes (buf bytes)
  "Append unibyte string BYTES to BUF verbatim."
  (ignore buf)
  (insert bytes))

(defun nelisp-elf--write-strz (buf s)
  "Append S to BUF followed by a NUL byte (= for .strtab entries)."
  (ignore buf)
  (insert s)
  (insert (unibyte-string 0)))

(defun nelisp-elf--write-pad (buf nbytes &optional value)
  "Append NBYTES copies of VALUE (default 0) to BUF."
  (ignore buf)
  (insert (make-string nbytes (or value 0))))

;; ---- Ehdr writer (= §3.3 §2.2) ----

(defun nelisp-elf-write-ehdr (buf fields)
  "Write a 64-byte ELF64 Ehdr to BUF using FIELDS plist.
FIELDS recognises:
  :type        ET_EXEC (2) or ET_DYN (3); default ET_EXEC.
  :machine     EM_X86_64 (62) or EM_AARCH64 (183); default EM_X86_64.
  :entry       e_entry virtual address (default 0).
  :phoff       Phdr table file offset (default 0).
  :shoff       Shdr table file offset (default 0).
  :flags       e_flags (default 0).
  :phentsize   sizeof(Phdr) (default 56).
  :phnum       Phdr count (default 0).
  :shentsize   sizeof(Shdr) (default 64).
  :shnum       Shdr count (default 0).
  :shstrndx    .shstrtab section index (default 0).
Returns the number of bytes written (= 64)."
  (let ((start (point))
        (type      (or (plist-get fields :type)      nelisp-elf--et-exec))
        (machine   (or (plist-get fields :machine)   nelisp-elf--em-x86-64))
        (entry     (or (plist-get fields :entry)     0))
        (phoff     (or (plist-get fields :phoff)     0))
        (shoff     (or (plist-get fields :shoff)     0))
        (flags     (or (plist-get fields :flags)     0))
        (phentsize (or (plist-get fields :phentsize) nelisp-elf--phdr-size))
        (phnum     (or (plist-get fields :phnum)     0))
        (shentsize (or (plist-get fields :shentsize) nelisp-elf--shdr-size))
        (shnum     (or (plist-get fields :shnum)     0))
        (shstrndx  (or (plist-get fields :shstrndx)  0)))
    ;; e_ident[0..15]
    ;; NB: cannot use the literal "\x7FELF" here because elisp parses
    ;; `\x7FE' as a single hex escape (= U+07FE) — must spell the four
    ;; magic bytes out individually.
    (nelisp-elf--write-bytes buf (unibyte-string #x7F #x45 #x4C #x46))
    (nelisp-elf--write-u8    buf nelisp-elf--ei-class-64)
    (nelisp-elf--write-u8    buf nelisp-elf--ei-data-lsb)
    (nelisp-elf--write-u8    buf nelisp-elf--ev-current)
    (nelisp-elf--write-u8    buf nelisp-elf--ei-osabi-sysv)
    (nelisp-elf--write-pad   buf 8) ; e_ident[8..15] = abiversion + reserved
    ;; remaining fields
    (nelisp-elf--write-le16  buf type)
    (nelisp-elf--write-le16  buf machine)
    (nelisp-elf--write-le32  buf nelisp-elf--ev-current)
    (nelisp-elf--write-le64  buf entry)
    (nelisp-elf--write-le64  buf phoff)
    (nelisp-elf--write-le64  buf shoff)
    (nelisp-elf--write-le32  buf flags)
    (nelisp-elf--write-le16  buf nelisp-elf--ehdr-size)
    (nelisp-elf--write-le16  buf phentsize)
    (nelisp-elf--write-le16  buf phnum)
    (nelisp-elf--write-le16  buf shentsize)
    (nelisp-elf--write-le16  buf shnum)
    (nelisp-elf--write-le16  buf shstrndx)
    (- (point) start)))

;; ---- Phdr writer (= §3.3 §2.3) ----

(defun nelisp-elf-write-phdr (buf fields)
  "Write a 56-byte ELF64 Phdr to BUF using FIELDS plist.
FIELDS recognises:
  :type     PT_LOAD (1), PT_PHDR (6), etc.  Default PT_LOAD.
  :flags    bitwise OR of PF_R / PF_W / PF_X (default PF_R | PF_X).
  :offset   p_offset (file offset).
  :vaddr    p_vaddr (mapped virtual address).
  :paddr    p_paddr (default = :vaddr).
  :filesz   p_filesz (file image size).
  :memsz    p_memsz (memory image size, default = :filesz).
  :align    p_align (default 0x1000).
Returns the number of bytes written (= 56)."
  (let* ((start (point))
         (vaddr  (or (plist-get fields :vaddr)  0))
         (filesz (or (plist-get fields :filesz) 0))
         (type   (or (plist-get fields :type)   nelisp-elf--pt-load))
         (flags  (or (plist-get fields :flags)
                     (logior nelisp-elf--pf-r nelisp-elf--pf-x)))
         (offset (or (plist-get fields :offset) 0))
         (paddr  (or (plist-get fields :paddr)  vaddr))
         (memsz  (or (plist-get fields :memsz)  filesz))
         (align  (or (plist-get fields :align)  #x1000)))
    (nelisp-elf--write-le32 buf type)
    (nelisp-elf--write-le32 buf flags)
    (nelisp-elf--write-le64 buf offset)
    (nelisp-elf--write-le64 buf vaddr)
    (nelisp-elf--write-le64 buf paddr)
    (nelisp-elf--write-le64 buf filesz)
    (nelisp-elf--write-le64 buf memsz)
    (nelisp-elf--write-le64 buf align)
    (- (point) start)))

;; ---- Top-level orchestrator (= §3.4, minimal §91.a slice) ----

(defconst nelisp-elf--minimal-exit-0-text
  ;; mov eax, 60 ; syscall  (= sys_exit(0) on x86_64-linux)
  ;;   B8 3C 00 00 00      mov eax, 60
  ;;   0F 05               syscall
  (unibyte-string #xb8 #x3c #x00 #x00 #x00 #x0f #x05)
  "Minimal x86_64 machine code calling `exit(0)' via syscall (= 7 bytes).")

(defconst nelisp-elf--minimal-vaddr-base #x400000
  "Base virtual address used by the minimal-exit-0 preset (= 4 MiB).")

(defun nelisp-elf--build-minimal-exit-0 ()
  "Build a minimal valid ELF64 x86_64 `exit(0)' binary, return unibyte string.
Layout: Ehdr (64) + Phdr (56) + .text (7) = 127 bytes total.  The
single PT_LOAD segment maps offset 0 at virtual address #x400000 and
includes both headers in the loaded image (= Teensy ELF pattern)."
  (let* ((text nelisp-elf--minimal-exit-0-text)
         (text-size (length text))
         (text-off (+ nelisp-elf--ehdr-size nelisp-elf--phdr-size))
         (filesz (+ text-off text-size))
         (vaddr-base nelisp-elf--minimal-vaddr-base)
         (entry (+ vaddr-base text-off)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (nelisp-elf-write-ehdr
       (current-buffer)
       (list :type      nelisp-elf--et-exec
             :machine   nelisp-elf--em-x86-64
             :entry     entry
             :phoff     nelisp-elf--ehdr-size
             :shoff     0
             :phnum     1
             :shnum     0
             :shstrndx  0))
      (nelisp-elf-write-phdr
       (current-buffer)
       (list :type   nelisp-elf--pt-load
             :flags  (logior nelisp-elf--pf-r nelisp-elf--pf-x)
             :offset 0
             :vaddr  vaddr-base
             :paddr  vaddr-base
             :filesz filesz
             :memsz  filesz
             :align  #x1000))
      (nelisp-elf--write-bytes (current-buffer) text)
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun nelisp-elf-write-binary (file-path sections)
  "Emit a static-linked ELF64 executable to FILE-PATH.
SECTIONS is currently restricted to the symbol `minimal-exit-0' (=
§91.a slice).  Future stages (§91.b / §91.c) will accept a plist of
`(:text :rodata :data :bss-size :symbols :relocs :entry-sym)' and
emit a fully-formed binary; passing such a plist now signals an
error.

The file is written with mode #o755 (= +x bit set)."
  (let ((bytes
         (cond
          ((eq sections 'minimal-exit-0)
           (nelisp-elf--build-minimal-exit-0))
          (t
           (error
            "nelisp-elf-write-binary: §91.a only supports `minimal-exit-0' (got %S)"
            sections)))))
    (let ((coding-system-for-write 'no-conversion))
      (write-region bytes nil file-path nil 'silent))
    (set-file-modes file-path #o755)
    file-path))

;; ---- read-back helpers (= used by the ert round-trip + future tooling) ----

(defun nelisp-elf--read-le16 (bytes offset)
  "Read an unsigned 16-bit little-endian int from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)))

(defun nelisp-elf--read-le32 (bytes offset)
  "Read an unsigned 32-bit little-endian int from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)
          (ash (aref bytes (+ offset 2)) 16)
          (ash (aref bytes (+ offset 3)) 24)))

(defun nelisp-elf--read-le64 (bytes offset)
  "Read an unsigned 64-bit little-endian int from BYTES at OFFSET."
  (let ((acc 0)
        (i 0))
    (while (< i 8)
      (setq acc (logior acc (ash (aref bytes (+ offset i)) (* i 8))))
      (setq i (1+ i)))
    acc))

(provide 'nelisp-elf-write)

;;; nelisp-elf-write.el ends here
