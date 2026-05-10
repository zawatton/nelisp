;;; nelisp-elf-write.el --- ELF64 binary writer (Phase 47 spike)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 91 §91.a + §91.b — pure-elisp ELF writer.
;;
;; Pure-elisp emitter for ELF64 binaries.  §91.a shipped the
;; byte-int helpers + Ehdr + Phdr writers + a minimal `exit(0)'
;; orchestrator.  §91.b adds section headers (Shdr), string tables
;; (.shstrtab / .strtab), the symbol table (.symtab) and relocation
;; entries (.rela.text) — everything needed to emit an ELF that
;; `readelf -s' and `nm' can introspect.
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
;; Layout produced by the §91.b rich-plist path (= when SECTIONS is a
;; plist rather than the `minimal-exit-0' sentinel) — a single PT_LOAD
;; covers Ehdr + Phdr + .text + .rodata, then the non-alloc sections
;; live after the segment:
;;
;;   offset 0x0000  Ehdr
;;   offset 0x0040  Phdr[0]        (PT_LOAD R+X covering up through .rodata)
;;   offset 0x0078  .text
;;   ...            .rodata        (if present)
;;   ...            .shstrtab      (not in segment)
;;   ...            .strtab        (not in segment)
;;   ...            .symtab        (not in segment)
;;   ...            .rela.text     (optional, not in segment)
;;   ...            Shdr[0..n]     (section header table)
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
(defconst nelisp-elf--sym-size  24 "sizeof(Elf64_Sym).")
(defconst nelisp-elf--rela-size 24 "sizeof(Elf64_Rela).")

;; ---- §91.b constants — section / symbol / relocation tags ----

(defconst nelisp-elf--sht-null     0 "SHT_NULL (= reserved section 0).")
(defconst nelisp-elf--sht-progbits 1 "SHT_PROGBITS.")
(defconst nelisp-elf--sht-symtab   2 "SHT_SYMTAB.")
(defconst nelisp-elf--sht-strtab   3 "SHT_STRTAB.")
(defconst nelisp-elf--sht-rela     4 "SHT_RELA.")
(defconst nelisp-elf--sht-nobits   8 "SHT_NOBITS.")

(defconst nelisp-elf--shf-write     1 "SHF_WRITE.")
(defconst nelisp-elf--shf-alloc     2 "SHF_ALLOC.")
(defconst nelisp-elf--shf-execinstr 4 "SHF_EXECINSTR.")

(defconst nelisp-elf--stb-local  0 "STB_LOCAL.")
(defconst nelisp-elf--stb-global 1 "STB_GLOBAL.")
(defconst nelisp-elf--stb-weak   2 "STB_WEAK.")

(defconst nelisp-elf--stt-notype  0 "STT_NOTYPE.")
(defconst nelisp-elf--stt-object  1 "STT_OBJECT.")
(defconst nelisp-elf--stt-func    2 "STT_FUNC.")
(defconst nelisp-elf--stt-section 3 "STT_SECTION.")

(defconst nelisp-elf--r-x86-64-64    1 "R_X86_64_64 (= 64-bit absolute).")
(defconst nelisp-elf--r-x86-64-pc32  2 "R_X86_64_PC32 (= 32-bit PC-relative).")
(defconst nelisp-elf--r-x86-64-plt32 4 "R_X86_64_PLT32 (= 32-bit PC-rel via PLT).")

(defconst nelisp-elf--shn-undef 0 "SHN_UNDEF (= unresolved symbol).")
(defconst nelisp-elf--shn-abs   #xFFF1 "SHN_ABS (= absolute value, no relocation).")

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

;; ---- §91.b Shdr writer (= §2.4) ----

(defun nelisp-elf-write-shdr (buf fields)
  "Write a 64-byte ELF64 Shdr to BUF using FIELDS plist.
FIELDS recognises every Elf64_Shdr field directly (all default 0):
  :name       sh_name      (= offset into .shstrtab).
  :type       sh_type      (SHT_PROGBITS / SYMTAB / STRTAB / RELA / NOBITS).
  :flags      sh_flags     (SHF_WRITE / SHF_ALLOC / SHF_EXECINSTR bit-OR).
  :addr       sh_addr      (= mapped virtual address; ALLOC sections only).
  :offset     sh_offset    (= file offset).
  :size       sh_size      (= section size in bytes).
  :link       sh_link      (RELA -> SYMTAB index, SYMTAB -> STRTAB index).
  :info       sh_info      (RELA -> target section index; SYMTAB -> last
                            local symbol index + 1).
  :addralign  sh_addralign (= section alignment, 0 or power-of-two).
  :entsize    sh_entsize   (= fixed entry size for table sections).
Returns the number of bytes written (= 64)."
  (let ((start     (point))
        (name      (or (plist-get fields :name)      0))
        (type      (or (plist-get fields :type)      nelisp-elf--sht-null))
        (flags     (or (plist-get fields :flags)     0))
        (addr      (or (plist-get fields :addr)      0))
        (offset    (or (plist-get fields :offset)    0))
        (size      (or (plist-get fields :size)      0))
        (link      (or (plist-get fields :link)      0))
        (info      (or (plist-get fields :info)      0))
        (addralign (or (plist-get fields :addralign) 0))
        (entsize   (or (plist-get fields :entsize)   0)))
    (nelisp-elf--write-le32 buf name)
    (nelisp-elf--write-le32 buf type)
    (nelisp-elf--write-le64 buf flags)
    (nelisp-elf--write-le64 buf addr)
    (nelisp-elf--write-le64 buf offset)
    (nelisp-elf--write-le64 buf size)
    (nelisp-elf--write-le32 buf link)
    (nelisp-elf--write-le32 buf info)
    (nelisp-elf--write-le64 buf addralign)
    (nelisp-elf--write-le64 buf entsize)
    (- (point) start)))

;; ---- §91.b string table (= .shstrtab / .strtab builder) ----

(defun nelisp-elf-strtab-make ()
  "Create a fresh string-table accumulator.
The result is a plist holding `:bytes' (= unibyte string seeded with
one leading NUL, matching ELF's reserved zero-offset empty entry) and
`:index' (= hash-table mapping every string already added to its
byte offset, used for dedup)."
  (list :bytes (unibyte-string 0)
        :index (let ((h (make-hash-table :test 'equal)))
                 (puthash "" 0 h)
                 h)))

(defun nelisp-elf-strtab-add (state str)
  "Add STR to STATE (= a `nelisp-elf-strtab-make' plist) and return its offset.
If STR is already present, the existing offset is returned and STATE
is left unchanged (= dedup)."
  (let* ((bytes (plist-get state :bytes))
         (index (plist-get state :index))
         (cached (gethash str index)))
    (cond
     (cached cached)
     ((string-empty-p str) 0)
     (t
      (let ((offset (length bytes)))
        (plist-put state :bytes
                   (concat bytes
                           (encode-coding-string str 'utf-8 t)
                           (unibyte-string 0)))
        (puthash str offset index)
        offset)))))

(defun nelisp-elf-strtab-bytes (state)
  "Return raw unibyte bytes of STATE (= concatenated NUL-terminated entries)."
  (plist-get state :bytes))

;; ---- §91.b Sym writer (= §2.5 Elf64_Sym, 24 bytes) ----

(defun nelisp-elf-sym-info (bind type)
  "Pack BIND (4 bits) and TYPE (4 bits) into a single Elf64_Sym st_info byte."
  (logand (logior (ash (logand bind #xF) 4) (logand type #xF)) #xFF))

(defun nelisp-elf-write-sym (buf fields)
  "Write a 24-byte Elf64_Sym to BUF using FIELDS plist.
FIELDS recognises (all default 0 unless noted):
  :name   st_name  (= offset into .strtab).
  :info   st_info  (= bind<<4 | type; use `nelisp-elf-sym-info').
  :other  st_other (= visibility, default 0).
  :shndx  st_shndx (= section index; 0 = SHN_UNDEF).
  :value  st_value (= symbol address / section offset).
  :size   st_size  (= symbol size in bytes).
Returns the number of bytes written (= 24)."
  (let ((start (point))
        (name  (or (plist-get fields :name)  0))
        (info  (or (plist-get fields :info)  0))
        (other (or (plist-get fields :other) 0))
        (shndx (or (plist-get fields :shndx) 0))
        (value (or (plist-get fields :value) 0))
        (size  (or (plist-get fields :size)  0)))
    (nelisp-elf--write-le32 buf name)
    (nelisp-elf--write-u8   buf info)
    (nelisp-elf--write-u8   buf other)
    (nelisp-elf--write-le16 buf shndx)
    (nelisp-elf--write-le64 buf value)
    (nelisp-elf--write-le64 buf size)
    (- (point) start)))

;; ---- §91.b Rela writer (= §2.6 Elf64_Rela, 24 bytes) ----

(defun nelisp-elf-rela-info (sym type)
  "Pack SYM (= symtab index, 32 bits) and TYPE (32 bits) into r_info (u64)."
  (logior (ash (logand sym #xFFFFFFFF) 32) (logand type #xFFFFFFFF)))

(defun nelisp-elf--write-le64-signed (buf v)
  "Append signed 64-bit V to BUF in two's-complement little-endian (= 8 bytes)."
  (let ((u (if (< v 0)
               (+ v (ash 1 64))
             v)))
    (nelisp-elf--write-le64 buf u)))

(defun nelisp-elf-write-rela (buf fields)
  "Write a 24-byte Elf64_Rela entry to BUF using FIELDS plist.
FIELDS recognises (all default 0):
  :offset  r_offset (= relocation target offset in the section).
  :info    r_info   (= sym<<32 | type; use `nelisp-elf-rela-info').
  :addend  r_addend (= signed addend; negative values are accepted and
                     encoded as two's-complement in 8 bytes).
Returns the number of bytes written (= 24)."
  (let ((start  (point))
        (offset (or (plist-get fields :offset) 0))
        (info   (or (plist-get fields :info)   0))
        (addend (or (plist-get fields :addend) 0)))
    (nelisp-elf--write-le64        buf offset)
    (nelisp-elf--write-le64        buf info)
    (nelisp-elf--write-le64-signed buf addend)
    (- (point) start)))

;; ---- §91.b reloc-type symbol → ELF constant mapping ----

(defun nelisp-elf--reloc-type-code (sym)
  "Translate the user-facing reloc TYPE symbol SYM to its ELF constant.
Supported: `pc32' (= R_X86_64_PC32), `abs64' (= R_X86_64_64),
`plt32' (= R_X86_64_PLT32).  Raw integers pass through unchanged."
  (cond
   ((integerp sym) sym)
   ((eq sym 'pc32)  nelisp-elf--r-x86-64-pc32)
   ((eq sym 'abs64) nelisp-elf--r-x86-64-64)
   ((eq sym 'plt32) nelisp-elf--r-x86-64-plt32)
   (t (error "nelisp-elf: unknown relocation type %S" sym))))

(defun nelisp-elf--sym-bind-code (bind)
  "Translate BIND keyword to STB_* constant (`local'/`global'/`weak')."
  (cond
   ((integerp bind) bind)
   ((eq bind 'local)  nelisp-elf--stb-local)
   ((eq bind 'global) nelisp-elf--stb-global)
   ((eq bind 'weak)   nelisp-elf--stb-weak)
   (t (error "nelisp-elf: unknown symbol bind %S" bind))))

(defun nelisp-elf--sym-type-code (type)
  "Translate TYPE keyword to STT_* code (`notype'/`object'/`func'/`section')."
  (cond
   ((integerp type) type)
   ((or (null type) (eq type 'notype))   nelisp-elf--stt-notype)
   ((eq type 'object)   nelisp-elf--stt-object)
   ((eq type 'func)     nelisp-elf--stt-func)
   ((eq type 'section)  nelisp-elf--stt-section)
   (t (error "nelisp-elf: unknown symbol type %S" type))))

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

;; ---- §91.b rich-plist orchestrator ----

(defun nelisp-elf--align-up (n align)
  "Round N up to the next multiple of ALIGN (ALIGN must be > 0)."
  (if (or (zerop align) (= align 1))
      n
    (* align (/ (+ n align -1) align))))

(defun nelisp-elf--symbol-key (sym)
  "Return the lookup string for SYM (= a :symbols list entry)."
  (plist-get sym :name))

(defun nelisp-elf--lookup-symbol (symbols name)
  "Return the first entry in SYMBOLS whose :name equals NAME, or nil."
  (let ((found nil))
    (dolist (sym symbols)
      (when (and (null found) (equal (nelisp-elf--symbol-key sym) name))
        (setq found sym)))
    found))

(defun nelisp-elf--section-vaddr (sec text-vaddr rodata-vaddr)
  "Map section keyword SEC to its loaded virtual address.
SEC = `text' / `rodata'.  TEXT-VADDR and RODATA-VADDR are the
respective loaded addresses computed by the layout pass."
  (cond
   ((eq sec 'text)   text-vaddr)
   ((eq sec 'rodata) rodata-vaddr)
   (t (error "nelisp-elf: cannot map section %S to a vaddr" sec))))

(defun nelisp-elf--section-shndx (sec text-shndx rodata-shndx)
  "Map section keyword SEC to its Shdr table index."
  (cond
   ((eq sec 'text)   text-shndx)
   ((eq sec 'rodata)
    (or rodata-shndx
        (error "nelisp-elf: symbol references rodata but :rodata is empty")))
   (t (error "nelisp-elf: cannot map section %S to an shndx" sec))))

(defun nelisp-elf--build-rich (plist)
  "Build an ET_EXEC ELF64 binary from PLIST, return unibyte string.
PLIST keys: :text (= unibyte bytes, required), :rodata (= bytes),
:symbols (= list of plists with :name :value :size :section :bind
:type), :relocs (= list of plists with :section :offset :symbol :type
:addend), :entry-sym (= entry-point symbol name, required).  Sections
.symtab / .strtab / .shstrtab are always emitted; .rela.text is
emitted only when :relocs is non-empty."
  (let* ((text     (or (plist-get plist :text)
                       (error "nelisp-elf: :text is required")))
         (rodata   (plist-get plist :rodata))
         (symbols  (plist-get plist :symbols))
         (relocs   (plist-get plist :relocs))
         (entry-sym (or (plist-get plist :entry-sym)
                        (error "nelisp-elf: :entry-sym is required")))
         (have-rodata (and rodata (> (length rodata) 0)))
         (have-rela   (and relocs (> (length relocs) 0)))
         (text-size   (length text))
         (rodata-size (if have-rodata (length rodata) 0))
         (vaddr-base  nelisp-elf--minimal-vaddr-base)
         ;; Layout: Ehdr + Phdr at offset 0, then .text, then .rodata
         ;; (both in the PT_LOAD segment).  Non-alloc sections follow.
         (phdr-off   nelisp-elf--ehdr-size)
         (text-off   (+ phdr-off nelisp-elf--phdr-size))
         (rodata-off (+ text-off text-size))
         (text-vaddr   (+ vaddr-base text-off))
         (rodata-vaddr (+ vaddr-base rodata-off))
         (segment-end (+ rodata-off rodata-size))
         ;; Non-alloc sections (= 8-byte aligned for SYMTAB / RELA).
         (shstrtab-off (nelisp-elf--align-up segment-end 1))
         ;; Build .shstrtab + indices.
         (shstrtab (nelisp-elf-strtab-make))
         (sh-name-text     (nelisp-elf-strtab-add shstrtab ".text"))
         (sh-name-rodata   (when have-rodata
                             (nelisp-elf-strtab-add shstrtab ".rodata")))
         (sh-name-shstrtab (nelisp-elf-strtab-add shstrtab ".shstrtab"))
         (sh-name-strtab   (nelisp-elf-strtab-add shstrtab ".strtab"))
         (sh-name-symtab   (nelisp-elf-strtab-add shstrtab ".symtab"))
         (sh-name-rela     (when have-rela
                             (nelisp-elf-strtab-add shstrtab ".rela.text")))
         (shstrtab-bytes (nelisp-elf-strtab-bytes shstrtab))
         (shstrtab-size  (length shstrtab-bytes))
         (strtab-off (nelisp-elf--align-up
                      (+ shstrtab-off shstrtab-size) 1))
         ;; Build .strtab from the user-provided symbol names.
         (strtab (nelisp-elf-strtab-make))
         (sym-name-offsets
          (mapcar (lambda (sym)
                    (cons (plist-get sym :name)
                          (nelisp-elf-strtab-add
                           strtab (plist-get sym :name))))
                  symbols))
         (strtab-bytes (nelisp-elf-strtab-bytes strtab))
         (strtab-size  (length strtab-bytes))
         (symtab-off (nelisp-elf--align-up
                      (+ strtab-off strtab-size) 8))
         ;; Symbols: index 0 is the always-zero STN_UNDEF entry,
         ;; followed by user symbols in declaration order.
         (sym-count (1+ (length symbols)))
         (symtab-size (* nelisp-elf--sym-size sym-count))
         ;; Partition by bind for sh_info (= first non-local).
         (local-count
          (let ((c 1))                  ; STN_UNDEF entry counts as local
            (dolist (sym symbols)
              (when (memq (or (plist-get sym :bind) 'local)
                          '(local 0))
                (setq c (1+ c))))
            c))
         ;; Order symbols: locals first, then non-locals (= preserves
         ;; declaration order within each group).  This is required by
         ;; ELF gABI: STB_LOCAL must precede STB_GLOBAL in .symtab.
         (ordered-symbols
          (let (locals globals)
            (dolist (sym symbols)
              (if (memq (or (plist-get sym :bind) 'local) '(local 0))
                  (push sym locals)
                (push sym globals)))
            (append (nreverse locals) (nreverse globals))))
         (rela-off (when have-rela
                     (nelisp-elf--align-up
                      (+ symtab-off symtab-size) 8)))
         (rela-size (when have-rela
                      (* nelisp-elf--rela-size (length relocs))))
         (after-rela (if have-rela
                         (+ rela-off rela-size)
                       (+ symtab-off symtab-size)))
         (shoff (nelisp-elf--align-up after-rela 8))
         ;; Section indices: 0 = NULL, then in emit order.
         (idx 1)
         (text-shndx idx)
         (rodata-shndx (and have-rodata (setq idx (1+ idx)) idx))
         (shstrtab-shndx (progn (setq idx (1+ idx)) idx))
         (strtab-shndx (progn (setq idx (1+ idx)) idx))
         (symtab-shndx (progn (setq idx (1+ idx)) idx))
         (_rela-shndx (and have-rela (setq idx (1+ idx)) idx))
         (shnum (1+ idx))
         ;; Resolve entry point.
         (entry-sym-entry
          (or (nelisp-elf--lookup-symbol symbols entry-sym)
              (error "nelisp-elf: :entry-sym %S not found in :symbols"
                     entry-sym)))
         (entry-section (or (plist-get entry-sym-entry :section) 'text))
         (entry-offset  (or (plist-get entry-sym-entry :value)  0))
         (entry (+ (nelisp-elf--section-vaddr
                    entry-section text-vaddr rodata-vaddr)
                   entry-offset))
         (segment-filesz segment-end)
         (segment-memsz  segment-end))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      ;; ---- Ehdr ----
      (nelisp-elf-write-ehdr
       (current-buffer)
       (list :type      nelisp-elf--et-exec
             :machine   nelisp-elf--em-x86-64
             :entry     entry
             :phoff     phdr-off
             :shoff     shoff
             :phnum     1
             :shnum     shnum
             :shstrndx  shstrtab-shndx))
      ;; ---- Phdr[0]: PT_LOAD covering everything up to .rodata ----
      (nelisp-elf-write-phdr
       (current-buffer)
       (list :type   nelisp-elf--pt-load
             :flags  (logior nelisp-elf--pf-r nelisp-elf--pf-x)
             :offset 0
             :vaddr  vaddr-base
             :paddr  vaddr-base
             :filesz segment-filesz
             :memsz  segment-memsz
             :align  #x1000))
      ;; ---- .text ----
      (unless (= (point) (1+ text-off))
        (error "nelisp-elf: .text offset drift (point=%d expected=%d)"
               (1- (point)) text-off))
      (nelisp-elf--write-bytes (current-buffer) text)
      ;; ---- .rodata ----
      (when have-rodata
        (nelisp-elf--write-bytes (current-buffer) rodata))
      ;; ---- .shstrtab ----
      (let ((pad (- shstrtab-off (1- (point)))))
        (when (> pad 0) (nelisp-elf--write-pad (current-buffer) pad)))
      (nelisp-elf--write-bytes (current-buffer) shstrtab-bytes)
      ;; ---- .strtab ----
      (let ((pad (- strtab-off (1- (point)))))
        (when (> pad 0) (nelisp-elf--write-pad (current-buffer) pad)))
      (nelisp-elf--write-bytes (current-buffer) strtab-bytes)
      ;; ---- .symtab ----
      (let ((pad (- symtab-off (1- (point)))))
        (when (> pad 0) (nelisp-elf--write-pad (current-buffer) pad)))
      ;; symbol 0 = STN_UNDEF (all zeros).
      (nelisp-elf-write-sym (current-buffer) '(:name 0 :info 0))
      (dolist (sym ordered-symbols)
        (let* ((nm    (plist-get sym :name))
               (sect  (or (plist-get sym :section) 'text))
               (bind  (or (plist-get sym :bind) 'local))
               (type  (or (plist-get sym :type) 'notype))
               (name-off (cdr (assoc nm sym-name-offsets)))
               (shndx (nelisp-elf--section-shndx
                       sect text-shndx rodata-shndx))
               (vaddr (nelisp-elf--section-vaddr
                       sect text-vaddr rodata-vaddr))
               (value (+ vaddr (or (plist-get sym :value) 0))))
          (nelisp-elf-write-sym
           (current-buffer)
           (list :name  name-off
                 :info  (nelisp-elf-sym-info
                         (nelisp-elf--sym-bind-code bind)
                         (nelisp-elf--sym-type-code type))
                 :other 0
                 :shndx shndx
                 :value value
                 :size  (or (plist-get sym :size) 0)))))
      ;; ---- .rela.text (optional) ----
      (when have-rela
        (let ((pad (- rela-off (1- (point)))))
          (when (> pad 0) (nelisp-elf--write-pad (current-buffer) pad)))
        (dolist (rel relocs)
          (let* ((rsym (plist-get rel :symbol))
                 (rtype (plist-get rel :type))
                 (sym-idx
                  (or (let ((i 1) found)
                        (dolist (s ordered-symbols)
                          (when (and (not found)
                                     (equal (plist-get s :name) rsym))
                            (setq found i))
                          (setq i (1+ i)))
                        found)
                      (error "nelisp-elf: relocation references unknown symbol %S"
                             rsym))))
            (nelisp-elf-write-rela
             (current-buffer)
             (list :offset (or (plist-get rel :offset) 0)
                   :info   (nelisp-elf-rela-info
                            sym-idx
                            (nelisp-elf--reloc-type-code rtype))
                   :addend (or (plist-get rel :addend) 0))))))
      ;; ---- Shdr table ----
      (let ((pad (- shoff (1- (point)))))
        (when (> pad 0) (nelisp-elf--write-pad (current-buffer) pad)))
      ;; Shdr[0] = SHT_NULL.
      (nelisp-elf-write-shdr (current-buffer) '(:type 0))
      ;; Shdr[text].
      (nelisp-elf-write-shdr
       (current-buffer)
       (list :name      sh-name-text
             :type      nelisp-elf--sht-progbits
             :flags     (logior nelisp-elf--shf-alloc
                                nelisp-elf--shf-execinstr)
             :addr      text-vaddr
             :offset    text-off
             :size      text-size
             :addralign 16))
      ;; Shdr[rodata] (if present).
      (when have-rodata
        (nelisp-elf-write-shdr
         (current-buffer)
         (list :name      sh-name-rodata
               :type      nelisp-elf--sht-progbits
               :flags     nelisp-elf--shf-alloc
               :addr      rodata-vaddr
               :offset    rodata-off
               :size      rodata-size
               :addralign 8)))
      ;; Shdr[shstrtab].
      (nelisp-elf-write-shdr
       (current-buffer)
       (list :name      sh-name-shstrtab
             :type      nelisp-elf--sht-strtab
             :offset    shstrtab-off
             :size      shstrtab-size
             :addralign 1))
      ;; Shdr[strtab].
      (nelisp-elf-write-shdr
       (current-buffer)
       (list :name      sh-name-strtab
             :type      nelisp-elf--sht-strtab
             :offset    strtab-off
             :size      strtab-size
             :addralign 1))
      ;; Shdr[symtab].
      (nelisp-elf-write-shdr
       (current-buffer)
       (list :name      sh-name-symtab
             :type      nelisp-elf--sht-symtab
             :offset    symtab-off
             :size      symtab-size
             :link      strtab-shndx
             :info      local-count
             :addralign 8
             :entsize   nelisp-elf--sym-size))
      ;; Shdr[rela.text] (if present).
      (when have-rela
        (nelisp-elf-write-shdr
         (current-buffer)
         (list :name      sh-name-rela
               :type      nelisp-elf--sht-rela
               :offset    rela-off
               :size      rela-size
               :link      symtab-shndx
               :info      text-shndx
               :addralign 8
               :entsize   nelisp-elf--rela-size)))
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun nelisp-elf-write-binary (file-path sections)
  "Emit a static-linked ELF64 executable to FILE-PATH.
SECTIONS is either the symbol `minimal-exit-0' (= §91.a Teensy-ELF
shortcut) or a plist that drives the §91.b rich path:
  :text       unibyte bytes (= machine code; required).
  :rodata     unibyte bytes (= constants).
  :symbols    list of plists with keys :name :value :size :section
              :bind :type (= STB_LOCAL/GLOBAL/WEAK keyword,
              STT_NOTYPE/OBJECT/FUNC/SECTION keyword).
  :relocs     list of plists with keys :section :offset :symbol :type
              :addend (= pc32 / abs64 / plt32 keyword).
  :entry-sym  symbol name resolved to e_entry (required for rich path).

The file is written with mode #o755 (= +x bit set)."
  (let ((bytes
         (cond
          ((eq sections 'minimal-exit-0)
           (nelisp-elf--build-minimal-exit-0))
          ((listp sections)
           (nelisp-elf--build-rich sections))
          (t
           (error
            "nelisp-elf-write-binary: invalid SECTIONS arg %S"
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
