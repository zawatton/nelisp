;;; nelisp-elf-write.el --- ELF64 binary writer (Phase 47 spike)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 91 §91.a + §91.b + §91.c — pure-elisp ELF writer.
;;
;; Pure-elisp emitter for ELF64 binaries.  §91.a shipped the
;; byte-int helpers + Ehdr + Phdr writers + a minimal `exit(0)'
;; orchestrator.  §91.b adds section headers (Shdr), string tables
;; (.shstrtab / .strtab), the symbol table (.symtab) and relocation
;; entries (.rela.text) — everything needed to emit an ELF that
;; `readelf -s' and `nm' can introspect.  §91.c adds multi-PT_LOAD
;; emission (RX + RW segments on separate pages), `.bss' NOBITS
;; handling (= `p_filesz < p_memsz') and a second corpus binary
;; (`hello-world-write') that exercises `write(2)' + `exit(2)' via
;; two raw syscalls.
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

;; ---- §91.d chunk-build buffer abstraction (= §7.1 perf mitigation) ----

;; A `cbuf' (= chunk-buffer) is a plist holding a reverse-order list of
;; unibyte string chunks plus a running length.  Finalising via
;; `nelisp-elf-buffer-bytes' joins them in one `apply' + `concat' call,
;; converting the O(N) per-write `insert' + buffer-position update
;; pattern into O(1) push + O(N) one-shot finalize (= O(N) total instead
;; of the gap-buffer O(N) per write that dominates large-binary emits).
;;
;; The public writer signatures (= `nelisp-elf-write-ehdr' / `-phdr' /
;; `-shdr' / `-sym' / `-rela') stay byte-for-byte identical: the first
;; arg accepts either a `cbuf' plist OR a live Emacs buffer (= for
;; backward compat with the ert helper `nelisp-elf-write-test--collect',
;; which does `with-temp-buffer' + `(current-buffer)' + writer).

(defun nelisp-elf-make-buffer ()
  "Make a new chunk-buffer accumulator.
Returns a plist (:chunks REVERSE-LIST :length N) — chunks are
pushed in emit order with `cons' so the head holds the most recent
write; `nelisp-elf-buffer-bytes' reverses and concatenates."
  (list :chunks nil :length 0))

(defun nelisp-elf--coerce-unibyte (str)
  "Return a unibyte copy of STR.
Idempotent for already-unibyte strings.  For multibyte strings that
hold raw bytes (= chars in 0..255), each char is collapsed to its
8-bit byte via a temporary unibyte buffer (= safe across emacs
versions, unlike the obsolete `string-make-unibyte')."
  (if (multibyte-string-p str)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert str)
        (buffer-substring-no-properties (point-min) (point-max)))
    str))

(defun nelisp-elf-buffer-bytes (cbuf)
  "Finalize CBUF and return its accumulated unibyte string.
Joins the reverse-order chunk list with one `apply' + `concat' after
`nreverse', so the cost is O(total-bytes) — not O(N²) like
incremental `concat'.  Coerced to unibyte via
`nelisp-elf--coerce-unibyte' so callers can safely pass the result
straight to `write-region' under
`coding-system-for-write' = no-conversion."
  (nelisp-elf--coerce-unibyte
   (apply #'concat (nreverse (plist-get cbuf :chunks)))))

(defsubst nelisp-elf-buffer-length (cbuf)
  "Return current cumulative byte length of CBUF (= O(1))."
  (plist-get cbuf :length))

(defsubst nelisp-elf--cbuf-push (cbuf bytes)
  "Push unibyte BYTES onto CBUF in place and update :length.  Returns CBUF.
Uses `setcar' against the plist value cells so the original `cbuf'
cons survives — callers that hold a reference (= every writer
helper) see the new length without needing to rebind."
  (let ((len (length bytes))
        ;; cbuf = (:chunks CHUNKS :length LEN)
        ;;        car=:chunks cdr=(CHUNKS :length LEN)
        ;;                        car=CHUNKS cdr=(:length LEN)
        ;;                                       car=:length cdr=(LEN)
        ;;                                                       car=LEN
        (chunks-cell (cdr cbuf))             ; (CHUNKS :length LEN)
        (length-cell (cdr (cdr (cdr cbuf))))) ; (LEN)
    (setcar chunks-cell (cons bytes (car chunks-cell)))
    (setcar length-cell (+ (car length-cell) len))
    cbuf))

(defsubst nelisp-elf--buf-emit (buf bytes)
  "Append unibyte BYTES to BUF (= either a chunk-buffer or an Emacs buffer)."
  (if (bufferp buf)
      (insert bytes)
    (nelisp-elf--cbuf-push buf bytes)))

(defsubst nelisp-elf--buf-position (buf)
  "Return current byte-count position of BUF.
For an Emacs buffer this is `(1- (point))' (= byte offset from start);
for a chunk-buffer it is the cached `:length' field."
  (if (bufferp buf)
      (1- (point))
    (plist-get buf :length)))

(defsubst nelisp-elf--buf-mark (buf)
  "Return a marker (= byte offset) suitable for delta math against BUF.
Companion to `nelisp-elf--buf-position' — kept distinct for symmetry
with the legacy `(point)' baseline."
  (nelisp-elf--buf-position buf))

;; ---- byte / int conversion helpers (= §3.2) ----

(defsubst nelisp-elf--write-u8 (buf v)
  "Append byte V (0..255) to BUF (= chunk-buf or Emacs buffer)."
  (nelisp-elf--buf-emit buf (unibyte-string (logand v #xff))))

(defun nelisp-elf--write-le16 (buf v)
  "Append unsigned 16-bit V to BUF in little-endian order (= 2 bytes)."
  (nelisp-elf--buf-emit
   buf
   (unibyte-string (logand v #xff)
                   (logand (ash v -8) #xff))))

(defun nelisp-elf--write-le32 (buf v)
  "Append unsigned 32-bit V to BUF in little-endian order (= 4 bytes)."
  (nelisp-elf--buf-emit
   buf
   (unibyte-string (logand v #xff)
                   (logand (ash v -8) #xff)
                   (logand (ash v -16) #xff)
                   (logand (ash v -24) #xff))))

(defun nelisp-elf--write-le64 (buf v)
  "Append unsigned 64-bit V to BUF in little-endian order (= 8 bytes).
Bignum-safe: shifts one byte at a time so values above 2^29 work on
32-bit Emacs as well (= §7.2 mitigation)."
  (let ((bytes (make-vector 8 0))
        (i 0))
    (while (< i 8)
      (aset bytes i (logand (ash v (- (* i 8))) #xff))
      (setq i (1+ i)))
    (nelisp-elf--buf-emit buf (apply #'unibyte-string (append bytes nil)))))

(defun nelisp-elf--write-bytes (buf bytes)
  "Append unibyte string BYTES to BUF verbatim.
Coerces BYTES to unibyte via `nelisp-elf--coerce-unibyte' (=
round-trips multibyte → unibyte by collapsing each U+00XX char
to one byte) so user-supplied `:text' / `:rodata' built via
`make-string' do not poison the chunk-buffer with UTF-8-expanded
chunks."
  (nelisp-elf--buf-emit buf (nelisp-elf--coerce-unibyte bytes)))

(defun nelisp-elf--write-strz (buf s)
  "Append S to BUF followed by a NUL byte (= for .strtab entries).
Encodes S as utf-8 (= already unibyte) + a single NUL byte."
  (nelisp-elf--buf-emit
   buf
   (concat (encode-coding-string s 'utf-8 t) (unibyte-string 0))))

(defun nelisp-elf--write-pad (buf nbytes &optional value)
  "Append NBYTES copies of VALUE (default 0) to BUF.
Forced unibyte (= `make-string' returns a multibyte string for
values >= #x80, which would poison the chunk-buffer).  VALUE = 0
is the hot path (= section alignment fillers) and stays unibyte
without conversion (= `make-string n 0' is unibyte by default)."
  (let ((v (or value 0)))
    (nelisp-elf--buf-emit
     buf
     (if (< v #x80)
         (make-string nbytes v)
       (nelisp-elf--coerce-unibyte (make-string nbytes v))))))

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
  (let ((start (nelisp-elf--buf-position buf))
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
    (- (nelisp-elf--buf-position buf) start)))

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
  (let* ((start (nelisp-elf--buf-position buf))
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
    (- (nelisp-elf--buf-position buf) start)))

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
  (let ((start     (nelisp-elf--buf-position buf))
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
    (- (nelisp-elf--buf-position buf) start)))

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
  (let ((start (nelisp-elf--buf-position buf))
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
    (- (nelisp-elf--buf-position buf) start)))

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
  (let ((start  (nelisp-elf--buf-position buf))
        (offset (or (plist-get fields :offset) 0))
        (info   (or (plist-get fields :info)   0))
        (addend (or (plist-get fields :addend) 0)))
    (nelisp-elf--write-le64        buf offset)
    (nelisp-elf--write-le64        buf info)
    (nelisp-elf--write-le64-signed buf addend)
    (- (nelisp-elf--buf-position buf) start)))

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
         (entry (+ vaddr-base text-off))
         (cbuf (nelisp-elf-make-buffer)))
    (nelisp-elf-write-ehdr
     cbuf
     (list :type      nelisp-elf--et-exec
           :machine   nelisp-elf--em-x86-64
           :entry     entry
           :phoff     nelisp-elf--ehdr-size
           :shoff     0
           :phnum     1
           :shnum     0
           :shstrndx  0))
    (nelisp-elf-write-phdr
     cbuf
     (list :type   nelisp-elf--pt-load
           :flags  (logior nelisp-elf--pf-r nelisp-elf--pf-x)
           :offset 0
           :vaddr  vaddr-base
           :paddr  vaddr-base
           :filesz filesz
           :memsz  filesz
           :align  #x1000))
    (nelisp-elf--write-bytes cbuf text)
    (nelisp-elf-buffer-bytes cbuf)))

;; ---- §91.c hello-world-write corpus #2 ----

(defconst nelisp-elf--hello-write-msg
  (unibyte-string ?h ?e ?l ?l ?o ?\n)
  "The `hello\\n' message bytes written by the corpus #2 binary.
Length = 6 (= `h' `e' `l' `l' `o' `\\n').")

(defconst nelisp-elf--hello-write-text-size 37
  "Length in bytes of the corpus #2 `_start' routine (= 37).
Computed so the orchestrator can place .rodata immediately after
.text without any relocation pass.")

(defun nelisp-elf--encode-le32-bytes (v)
  "Return the unsigned 32-bit value V as a 4-byte little-endian string.
Negative values are sign-extended into the low 32 bits and emitted
as their two's-complement representation."
  (let ((u (if (< v 0) (logand (+ v (ash 1 32)) #xFFFFFFFF) v)))
    (unibyte-string (logand u #xff)
                    (logand (ash u -8) #xff)
                    (logand (ash u -16) #xff)
                    (logand (ash u -24) #xff))))

(defun nelisp-elf--hello-write-emit-text (msg-len text-vaddr rodata-vaddr)
  "Return self-contained corpus #2 `_start' bytes with a baked rel32.
MSG-LEN is the .rodata buffer length.  TEXT-VADDR is the runtime
virtual address of `_start' (= start of .text).  RODATA-VADDR is
the runtime virtual address of the message buffer (= start of
.rodata).  The rel32 is computed relative to the byte after the
`lea' instruction (= TEXT-VADDR + 16) per the AMD64 ABI.

Layout returned (= 37 bytes total = 4+5+3+4+5+5+2+2+5+2):
  off  0  48 83 e4 f0          and  rsp, -16
  off  4  bf 01 00 00 00       mov  edi, 1
  off  9  48 8d 35 RR RR RR RR lea  rsi, [rip+REL]
  off 16  ba LL 00 00 00       mov  edx, LEN
  off 21  b8 01 00 00 00       mov  eax, 1
  off 26  0f 05                syscall
  off 28  31 ff                xor  edi, edi
  off 30  b8 3c 00 00 00       mov  eax, 60
  off 35  0f 05                syscall

Doc 91 §91.c writes this corpus directly (= no Doc 92 / Doc 94
runtime dependency) so the §91.c smoke test can prove the full
ELF chain on its own."
  (let* ((next-after-lea (+ text-vaddr 16))
         (rel32 (- rodata-vaddr next-after-lea))
         (len-imm (logand msg-len #xffffffff)))
    (concat
     (unibyte-string #x48 #x83 #xe4 #xf0)         ; and rsp, -16
     (unibyte-string #xbf #x01 #x00 #x00 #x00)    ; mov edi, 1
     (unibyte-string #x48 #x8d #x35)              ; lea rsi, [rip+
     (nelisp-elf--encode-le32-bytes rel32)        ;   rel32]
     (unibyte-string #xba)                        ; mov edx, imm32
     (nelisp-elf--encode-le32-bytes len-imm)
     (unibyte-string #xb8 #x01 #x00 #x00 #x00)    ; mov eax, 1
     (unibyte-string #x0f #x05)                   ; syscall
     (unibyte-string #x31 #xff)                   ; xor edi, edi
     (unibyte-string #xb8 #x3c #x00 #x00 #x00)    ; mov eax, 60
     (unibyte-string #x0f #x05))))                ; syscall

(defun nelisp-elf--build-hello-world-write ()
  "Build the corpus #2 hello-world-write binary, return unibyte string.
Uses the rich-plist path with a single PT_LOAD (RX) — no .data /
no .bss — so the binary is the minimum that exercises a non-trivial
two-syscall program on x86_64 Linux."
  (let* ((vaddr-base nelisp-elf--minimal-vaddr-base)
         (text-off   (+ nelisp-elf--ehdr-size nelisp-elf--phdr-size))
         (text-vaddr (+ vaddr-base text-off))
         (msg-len    (length nelisp-elf--hello-write-msg))
         (rodata-off (+ text-off nelisp-elf--hello-write-text-size))
         (rodata-vaddr (+ vaddr-base rodata-off))
         (text-bytes (nelisp-elf--hello-write-emit-text
                      msg-len text-vaddr rodata-vaddr)))
    (unless (= (length text-bytes) nelisp-elf--hello-write-text-size)
      (error "nelisp-elf: hello-world-write .text drift (got %d expected %d)"
             (length text-bytes) nelisp-elf--hello-write-text-size))
    (nelisp-elf--build-rich
     (list :text text-bytes
           :rodata nelisp-elf--hello-write-msg
           :symbols
           (list (list :name "_start" :value 0
                       :size nelisp-elf--hello-write-text-size
                       :section 'text :bind 'global :type 'func)
                 (list :name "msg" :value 0
                       :size msg-len :section 'rodata
                       :bind 'local :type 'object))
           :entry-sym "_start"))))

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

(defun nelisp-elf--section-vaddr (sec text-vaddr rodata-vaddr
                                      &optional data-vaddr bss-vaddr)
  "Map section keyword SEC to its loaded virtual address.
SEC = `text' / `rodata' / `data' / `bss'.  Each of TEXT-VADDR,
RODATA-VADDR, DATA-VADDR, BSS-VADDR is the runtime virtual address
chosen for that section by the layout pass.  DATA-VADDR and
BSS-VADDR may be nil when the respective sections are absent."
  (cond
   ((eq sec 'text)   text-vaddr)
   ((eq sec 'rodata) rodata-vaddr)
   ((eq sec 'data)
    (or data-vaddr
        (error "nelisp-elf: symbol references data but :data is empty")))
   ((eq sec 'bss)
    (or bss-vaddr
        (error "nelisp-elf: symbol references bss but :bss-size is nil")))
   (t (error "nelisp-elf: cannot map section %S to a vaddr" sec))))

(defun nelisp-elf--section-shndx (sec text-shndx rodata-shndx
                                      &optional data-shndx bss-shndx)
  "Map section keyword SEC to its Shdr table index.
DATA-SHNDX and BSS-SHNDX may be nil when the respective sections
are absent from the rich-plist input."
  (cond
   ((eq sec 'text)   text-shndx)
   ((eq sec 'rodata)
    (or rodata-shndx
        (error "nelisp-elf: symbol references rodata but :rodata is empty")))
   ((eq sec 'data)
    (or data-shndx
        (error "nelisp-elf: symbol references data but :data is empty")))
   ((eq sec 'bss)
    (or bss-shndx
        (error "nelisp-elf: symbol references bss but :bss-size is nil")))
   (t (error "nelisp-elf: cannot map section %S to an shndx" sec))))

(defun nelisp-elf--build-rich (plist)
  "Build an ET_EXEC ELF64 binary from PLIST, return unibyte string.
PLIST keys: :text (= unibyte bytes, required), :rodata (= bytes),
:data (= bytes, §91.c), :bss-size (= integer, §91.c),
:symbols (= list of plists with :name :value :size :section :bind
:type), :relocs (= list of plists with :section :offset :symbol :type
:addend), :entry-sym (= entry-point symbol name, required).  Sections
.symtab / .strtab / .shstrtab are always emitted; .rela.text is
emitted only when :relocs is non-empty.  When :data and/or :bss-size
is present, the loader image is split into an RX PT_LOAD (= .text +
.rodata) and a separate page-aligned RW PT_LOAD (= .data + .bss) per
Doc 91 §91.c."
  (let* ((text     (or (plist-get plist :text)
                       (error "nelisp-elf: :text is required")))
         (rodata   (plist-get plist :rodata))
         (data     (plist-get plist :data))
         (bss-size (or (plist-get plist :bss-size) 0))
         (symbols  (plist-get plist :symbols))
         (relocs   (plist-get plist :relocs))
         (entry-sym (or (plist-get plist :entry-sym)
                        (error "nelisp-elf: :entry-sym is required")))
         ;; §94.b cleanup: :machine arg (= 'x86_64 / 'aarch64 / int).
         ;; Default x86_64.  Replaces §94.b post-emit patch hack.
         (machine-arg (or (plist-get plist :machine) 'x86_64))
         (machine-em
          (cond
           ((or (eq machine-arg 'x86_64) (eq machine-arg 'x86-64))
            nelisp-elf--em-x86-64)
           ((or (eq machine-arg 'aarch64) (eq machine-arg 'arm64))
            nelisp-elf--em-aarch64)
           ((integerp machine-arg) machine-arg)
           (t (error "nelisp-elf: invalid :machine %S" machine-arg))))
         (have-rodata (and rodata (> (length rodata) 0)))
         (have-data   (and data (> (length data) 0)))
         (have-bss    (> bss-size 0))
         (have-rw     (or have-data have-bss))
         (have-rela   (and relocs (> (length relocs) 0)))
         (text-size   (length text))
         (rodata-size (if have-rodata (length rodata) 0))
         (data-size   (if have-data (length data) 0))
         (vaddr-base  nelisp-elf--minimal-vaddr-base)
         (page-size   #x1000)
         ;; ---- RX segment layout (= Ehdr + Phdrs + .text + .rodata).
         (phnum      (if have-rw 2 1))
         (phdr-off   nelisp-elf--ehdr-size)
         (text-off   (+ phdr-off (* nelisp-elf--phdr-size phnum)))
         (rodata-off (+ text-off text-size))
         (text-vaddr   (+ vaddr-base text-off))
         (rodata-vaddr (+ vaddr-base rodata-off))
         (rx-segment-end (+ rodata-off rodata-size))
         ;; ---- RW segment layout (= .data + .bss on a fresh page).
         (data-off   (and have-rw
                          (nelisp-elf--align-up rx-segment-end page-size)))
         (data-vaddr (and have-rw (+ vaddr-base data-off)))
         (bss-off    (and have-bss (+ data-off data-size)))
         (bss-vaddr  (and have-bss (+ data-vaddr data-size)))
         (rw-filesz  (and have-rw data-size))
         (rw-memsz   (and have-rw (+ data-size bss-size)))
         ;; Non-alloc sections start after .data bytes on disk (the
         ;; .bss section is NOBITS so it contributes 0 file bytes).
         (non-alloc-base (if have-rw
                             (+ data-off data-size)
                           rx-segment-end))
         (shstrtab-off (nelisp-elf--align-up non-alloc-base 1))
         ;; Build .shstrtab + indices.
         (shstrtab (nelisp-elf-strtab-make))
         (sh-name-text     (nelisp-elf-strtab-add shstrtab ".text"))
         (sh-name-rodata   (when have-rodata
                             (nelisp-elf-strtab-add shstrtab ".rodata")))
         (sh-name-data     (when have-data
                             (nelisp-elf-strtab-add shstrtab ".data")))
         (sh-name-bss      (when have-bss
                             (nelisp-elf-strtab-add shstrtab ".bss")))
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
         (data-shndx   (and have-data   (setq idx (1+ idx)) idx))
         (bss-shndx    (and have-bss    (setq idx (1+ idx)) idx))
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
                    entry-section text-vaddr rodata-vaddr
                    data-vaddr bss-vaddr)
                   entry-offset))
         ;; RX segment image covers everything up to end of rodata.
         (rx-segment-filesz rx-segment-end)
         (rx-segment-memsz  rx-segment-end)
         ;; §91.d: chunk-build accumulator — `nelisp-elf-buffer-bytes'
         ;; is called once at the end to fuse all chunks via
         ;; `apply' + `concat' (= O(N), vs gap-buffer O(N) per write).
         (cbuf (nelisp-elf-make-buffer)))
    ;; ---- Ehdr ----
    (nelisp-elf-write-ehdr
     cbuf
     (list :type      nelisp-elf--et-exec
           :machine   machine-em
           :entry     entry
           :phoff     phdr-off
           :shoff     shoff
           :phnum     phnum
           :shnum     shnum
           :shstrndx  shstrtab-shndx))
    ;; ---- Phdr[0]: PT_LOAD R+X covering Ehdr + Phdrs + .text + .rodata
    (nelisp-elf-write-phdr
     cbuf
     (list :type   nelisp-elf--pt-load
           :flags  (logior nelisp-elf--pf-r nelisp-elf--pf-x)
           :offset 0
           :vaddr  vaddr-base
           :paddr  vaddr-base
           :filesz rx-segment-filesz
           :memsz  rx-segment-memsz
           :align  page-size))
    ;; ---- Phdr[1]: PT_LOAD R+W for .data + .bss (NOBITS) ----
    (when have-rw
      (nelisp-elf-write-phdr
       cbuf
       (list :type   nelisp-elf--pt-load
             :flags  (logior nelisp-elf--pf-r nelisp-elf--pf-w)
             :offset data-off
             :vaddr  data-vaddr
             :paddr  data-vaddr
             :filesz rw-filesz
             :memsz  rw-memsz
             :align  page-size)))
    ;; ---- .text ----
    (unless (= (nelisp-elf-buffer-length cbuf) text-off)
      (error "nelisp-elf: .text offset drift (length=%d expected=%d)"
             (nelisp-elf-buffer-length cbuf) text-off))
    (nelisp-elf--write-bytes cbuf text)
    ;; ---- .rodata ----
    (when have-rodata
      (nelisp-elf--write-bytes cbuf rodata))
    ;; ---- .data (= RW segment, on a fresh page) ----
    (when have-data
      (let ((pad (- data-off (nelisp-elf-buffer-length cbuf))))
        (when (> pad 0) (nelisp-elf--write-pad cbuf pad)))
      (nelisp-elf--write-bytes cbuf data))
    ;; .bss contributes no file bytes — it is NOBITS.
    ;; ---- .shstrtab ----
    (let ((pad (- shstrtab-off (nelisp-elf-buffer-length cbuf))))
      (when (> pad 0) (nelisp-elf--write-pad cbuf pad)))
    (nelisp-elf--write-bytes cbuf shstrtab-bytes)
    ;; ---- .strtab ----
    (let ((pad (- strtab-off (nelisp-elf-buffer-length cbuf))))
      (when (> pad 0) (nelisp-elf--write-pad cbuf pad)))
    (nelisp-elf--write-bytes cbuf strtab-bytes)
    ;; ---- .symtab ----
    (let ((pad (- symtab-off (nelisp-elf-buffer-length cbuf))))
      (when (> pad 0) (nelisp-elf--write-pad cbuf pad)))
    ;; symbol 0 = STN_UNDEF (all zeros).
    (nelisp-elf-write-sym cbuf '(:name 0 :info 0))
    (dolist (sym ordered-symbols)
      (let* ((nm    (plist-get sym :name))
             (sect  (or (plist-get sym :section) 'text))
             (bind  (or (plist-get sym :bind) 'local))
             (type  (or (plist-get sym :type) 'notype))
             (name-off (cdr (assoc nm sym-name-offsets)))
             (shndx (nelisp-elf--section-shndx
                     sect text-shndx rodata-shndx
                     data-shndx bss-shndx))
             (vaddr (nelisp-elf--section-vaddr
                     sect text-vaddr rodata-vaddr
                     data-vaddr bss-vaddr))
             (value (+ vaddr (or (plist-get sym :value) 0))))
        (nelisp-elf-write-sym
         cbuf
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
      (let ((pad (- rela-off (nelisp-elf-buffer-length cbuf))))
        (when (> pad 0) (nelisp-elf--write-pad cbuf pad)))
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
           cbuf
           (list :offset (or (plist-get rel :offset) 0)
                 :info   (nelisp-elf-rela-info
                          sym-idx
                          (nelisp-elf--reloc-type-code rtype))
                 :addend (or (plist-get rel :addend) 0))))))
    ;; ---- Shdr table ----
    (let ((pad (- shoff (nelisp-elf-buffer-length cbuf))))
      (when (> pad 0) (nelisp-elf--write-pad cbuf pad)))
    ;; Shdr[0] = SHT_NULL.
    (nelisp-elf-write-shdr cbuf '(:type 0))
    ;; Shdr[text].
    (nelisp-elf-write-shdr
     cbuf
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
       cbuf
       (list :name      sh-name-rodata
             :type      nelisp-elf--sht-progbits
             :flags     nelisp-elf--shf-alloc
             :addr      rodata-vaddr
             :offset    rodata-off
             :size      rodata-size
             :addralign 8)))
    ;; Shdr[data] (if present).
    (when have-data
      (nelisp-elf-write-shdr
       cbuf
       (list :name      sh-name-data
             :type      nelisp-elf--sht-progbits
             :flags     (logior nelisp-elf--shf-write
                                nelisp-elf--shf-alloc)
             :addr      data-vaddr
             :offset    data-off
             :size      data-size
             :addralign 8)))
    ;; Shdr[bss] (if present) — SHT_NOBITS, no file footprint.
    (when have-bss
      (nelisp-elf-write-shdr
       cbuf
       (list :name      sh-name-bss
             :type      nelisp-elf--sht-nobits
             :flags     (logior nelisp-elf--shf-write
                                nelisp-elf--shf-alloc)
             :addr      bss-vaddr
             :offset    bss-off
             :size      bss-size
             :addralign 8)))
    ;; Shdr[shstrtab].
    (nelisp-elf-write-shdr
     cbuf
     (list :name      sh-name-shstrtab
           :type      nelisp-elf--sht-strtab
           :offset    shstrtab-off
           :size      shstrtab-size
           :addralign 1))
    ;; Shdr[strtab].
    (nelisp-elf-write-shdr
     cbuf
     (list :name      sh-name-strtab
           :type      nelisp-elf--sht-strtab
           :offset    strtab-off
           :size      strtab-size
           :addralign 1))
    ;; Shdr[symtab].
    (nelisp-elf-write-shdr
     cbuf
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
       cbuf
       (list :name      sh-name-rela
             :type      nelisp-elf--sht-rela
             :offset    rela-off
             :size      rela-size
             :link      symtab-shndx
             :info      text-shndx
             :addralign 8
             :entsize   nelisp-elf--rela-size)))
    (nelisp-elf-buffer-bytes cbuf)))

;;;###autoload
(defun nelisp-elf-write-binary (file-path sections)
  "Emit a static-linked ELF64 executable to FILE-PATH.
SECTIONS is one of the named sentinels (`minimal-exit-0' for the
§91.a Teensy-ELF shortcut, `hello-world-write' for the §91.c
corpus #2 sample that prints `hello\\n' via `write(2)' and exits)
or a plist that drives the §91.b/§91.c rich path:
  :text       unibyte bytes (= machine code; required).
  :rodata     unibyte bytes (= constants).
  :data       unibyte bytes (= initialised writable data, §91.c).
  :bss-size   integer (= NOBITS zero-fill size in bytes, §91.c).
  :symbols    list of plists with keys :name :value :size :section
              :bind :type (= STB_LOCAL/GLOBAL/WEAK keyword,
              STT_NOTYPE/OBJECT/FUNC/SECTION keyword).
              :section is `text' / `rodata' / `data' / `bss'.
  :relocs     list of plists with keys :section :offset :symbol :type
              :addend (= pc32 / abs64 / plt32 keyword).
  :entry-sym  symbol name resolved to e_entry (required for rich path).
  :machine    arch tag, `x86_64' (default) / `aarch64' / integer EM_* code.

When :data or :bss-size is present the loader image splits into two
PT_LOAD segments — one RX (= .text + .rodata) and one RW page-aligned
(= .data + .bss, with .bss declared NOBITS).

The file is written with mode #o755 (= +x bit set)."
  (let ((bytes
         (cond
          ((eq sections 'minimal-exit-0)
           (nelisp-elf--build-minimal-exit-0))
          ((eq sections 'hello-world-write)
           (nelisp-elf--build-hello-world-write))
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

;; ---- §91.d benchmark helper (= chunk-build perf gate) ----

(defun nelisp-elf-benchmark-write-binary (file-path size-kb)
  "Emit a synthetic ELF executable of approximately SIZE-KB to FILE-PATH.
Used by §91.d perf gate (= 1 MB / 1000 KB must emit in < 5 sec).
Generates a filler `.text' buffer of SIZE-KB * 1024 bytes (= filler
byte = #x90, the x86_64 NOP opcode; the resulting binary is not
expected to execute meaningfully, only to exercise the chunk-build
emit path against realistic payload sizes).  Returns FILE-PATH."
  (let* ((nbytes (* size-kb 1024))
         (filler (make-string nbytes #x90))
         (msg-len 6)
         (symbols
          (list (list :name "_start" :value 0 :size nbytes
                      :section 'text :bind 'global :type 'func)
                (list :name "msg" :value 0 :size msg-len
                      :section 'rodata :bind 'local :type 'object)))
         (plist (list :text filler
                      :rodata (unibyte-string ?h ?e ?l ?l ?o ?\n)
                      :symbols symbols
                      :entry-sym "_start")))
    (nelisp-elf-write-binary file-path plist)))

(provide 'nelisp-elf-write)

;;; nelisp-elf-write.el ends here
