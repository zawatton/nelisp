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
;; The module is freestanding — not yet wired into the production
;; STDLIB load (= integration deferred per Doc 91 §8.1).

;;; Code:

;; ---- ELF format constants (= magic numbers from §4.4) ----

(defconst nelisp-elf--ei-class-64 2 "ELFCLASS64.")
(defconst nelisp-elf--ei-data-lsb 1 "ELFDATA2LSB (= little-endian).")
(defconst nelisp-elf--ev-current 1 "ELF version EV_CURRENT.")
(defconst nelisp-elf--ei-osabi-sysv 0 "ELFOSABI_NONE / SYSV.")

(defconst nelisp-elf--et-rel  1 "ET_REL (= relocatable object, Doc 99 §99.A).")
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
versions, unlike the obsolete `string-make-unibyte').

NeLisp standalone has no multibyte/unibyte distinction at the
string level (= all strings are internally UTF-8) and the buffer
stubs are no-ops, so the `with-temp-buffer' trick would silently
return \"\" and starve all downstream writers.  Detect the stub
environment by checking whether the buffer trick preserved length
and fall back to returning STR as-is when it did not (= NeLisp
standalone) — char codes 0..255 stored via `unibyte-string' are
already byte-equivalent for the ASCII range that the ELF
infrastructure relies on."
  (if (multibyte-string-p str)
      (let ((converted
             (with-temp-buffer
               (set-buffer-multibyte nil)
               (insert str)
               (buffer-substring-no-properties (point-min) (point-max)))))
        ;; NeLisp standalone: buffer ops are stubs, conversion produces
        ;; an empty string regardless of input — keep STR so downstream
        ;; writers see the original char sequence (char-value == byte
        ;; for chars 0..127, which covers all ELF header literals).
        (if (= (length converted) (length str))
            converted
          str))
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

;; ---- Wave 16 inline writers (= per-record fast path) ----
;;
;; Wave 14a diagnosis: under standalone NeLisp (= interpreted, no
;; defsubst inlining) every helper call costs ~150-200 ms because the
;; runtime walks the global symbol table on each invocation.  The
;; original writer chain (`write-leN' → `--buf-emit' → `--cbuf-push')
;; therefore pays 3× per byte field × ~25 fields per record, which is
;; what pushed a 127-byte ELF to ~25 sec and a 672-byte ELF past 113
;; sec on standalone NeLisp.
;;
;; The Wave 16 mitigation transforms each writer (`write-ehdr',
;; `write-phdr', `write-shdr', `write-sym', `write-rela') into a
;; single-pass builder: the entire fixed-size record is constructed
;; as a flat byte list inline (= no per-field helper calls, no
;; `append'), then emitted via ONE `apply unibyte-string' + ONE
;; `--buf-emit'.  Total defun-call count for the minimal-exit-0 path
;; drops from ~70 to ~5; for the hello-world-write path it drops
;; from ~650 to ~25.  Measured speedups (standalone NeLisp, this
;; worktree):
;;
;;   minimal-exit-0     25.3 s → 4.3 s  (~6x)
;;   hello-world-write  113  s → 26  s  (~4.3x)
;;   ET_REL 480 byte    >60  s → 18  s  (>3x)
;;
;; Host Emacs sees no behaviour change because the new builders feed
;; identical bytes to the existing chunk-buffer; the byte sequence
;; is identical to what the helper chain produced (= ELF64 gABI
;; §2.2-§2.6 layout, verified by `cmp -l' against host Emacs output
;; for both `minimal-exit-0' and `hello-world-write').

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
  (let ((type      (or (plist-get fields :type)      nelisp-elf--et-exec))
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
    ;; Wave 16: build the entire 64-byte Ehdr as a single flat byte list
    ;; (= NO per-field helper calls, NO `append'), then emit ONE
    ;; `apply unibyte-string' + ONE buf-emit.  Total defun-call count
    ;; drops from ~17 nested helper calls (per record) to 2.  The byte
    ;; sequence is identical to what the helper chain produced
    ;; (= ELF64 gABI §2.2 layout).
    ;;
    ;; NB: cannot use the literal "\x7FELF" here because elisp parses
    ;; `\x7FE' as a single hex escape (= U+07FE) — must spell the four
    ;; magic bytes out individually.
    (let ((record
           (apply
            #'unibyte-string
            (list
             ;; e_ident[0..15] = magic + class + data + version + osabi + pad[8]
             #x7F #x45 #x4C #x46
             nelisp-elf--ei-class-64
             nelisp-elf--ei-data-lsb
             nelisp-elf--ev-current
             nelisp-elf--ei-osabi-sysv
             0 0 0 0 0 0 0 0
             ;; e_type u16
             (logand type #xff) (logand (ash type -8) #xff)
             ;; e_machine u16
             (logand machine #xff) (logand (ash machine -8) #xff)
             ;; e_version u32 (= EV_CURRENT)
             (logand nelisp-elf--ev-current #xff)
             (logand (ash nelisp-elf--ev-current -8)  #xff)
             (logand (ash nelisp-elf--ev-current -16) #xff)
             (logand (ash nelisp-elf--ev-current -24) #xff)
             ;; e_entry u64
             (logand entry #xff)
             (logand (ash entry -8)  #xff)
             (logand (ash entry -16) #xff)
             (logand (ash entry -24) #xff)
             (logand (ash entry -32) #xff)
             (logand (ash entry -40) #xff)
             (logand (ash entry -48) #xff)
             (logand (ash entry -56) #xff)
             ;; e_phoff u64
             (logand phoff #xff)
             (logand (ash phoff -8)  #xff)
             (logand (ash phoff -16) #xff)
             (logand (ash phoff -24) #xff)
             (logand (ash phoff -32) #xff)
             (logand (ash phoff -40) #xff)
             (logand (ash phoff -48) #xff)
             (logand (ash phoff -56) #xff)
             ;; e_shoff u64
             (logand shoff #xff)
             (logand (ash shoff -8)  #xff)
             (logand (ash shoff -16) #xff)
             (logand (ash shoff -24) #xff)
             (logand (ash shoff -32) #xff)
             (logand (ash shoff -40) #xff)
             (logand (ash shoff -48) #xff)
             (logand (ash shoff -56) #xff)
             ;; e_flags u32
             (logand flags #xff)
             (logand (ash flags -8)  #xff)
             (logand (ash flags -16) #xff)
             (logand (ash flags -24) #xff)
             ;; e_ehsize u16
             (logand nelisp-elf--ehdr-size #xff)
             (logand (ash nelisp-elf--ehdr-size -8) #xff)
             ;; e_phentsize u16
             (logand phentsize #xff) (logand (ash phentsize -8) #xff)
             ;; e_phnum u16
             (logand phnum #xff) (logand (ash phnum -8) #xff)
             ;; e_shentsize u16
             (logand shentsize #xff) (logand (ash shentsize -8) #xff)
             ;; e_shnum u16
             (logand shnum #xff) (logand (ash shnum -8) #xff)
             ;; e_shstrndx u16
             (logand shstrndx #xff) (logand (ash shstrndx -8) #xff)))))
      (nelisp-elf--buf-emit buf record)
      nelisp-elf--ehdr-size)))

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
  (let* ((vaddr  (or (plist-get fields :vaddr)  0))
         (filesz (or (plist-get fields :filesz) 0))
         (type   (or (plist-get fields :type)   nelisp-elf--pt-load))
         (flags  (or (plist-get fields :flags)
                     (logior nelisp-elf--pf-r nelisp-elf--pf-x)))
         (offset (or (plist-get fields :offset) 0))
         (paddr  (or (plist-get fields :paddr)  vaddr))
         (memsz  (or (plist-get fields :memsz)  filesz))
         (align  (or (plist-get fields :align)  #x1000)))
    ;; Wave 16: single-call Phdr emit (= 56 bytes, fully inlined).
    (let ((record
           (apply
            #'unibyte-string
            (list
             ;; p_type u32
             (logand type #xff) (logand (ash type -8) #xff)
             (logand (ash type -16) #xff) (logand (ash type -24) #xff)
             ;; p_flags u32
             (logand flags #xff) (logand (ash flags -8) #xff)
             (logand (ash flags -16) #xff) (logand (ash flags -24) #xff)
             ;; p_offset u64
             (logand offset #xff)
             (logand (ash offset -8)  #xff)
             (logand (ash offset -16) #xff)
             (logand (ash offset -24) #xff)
             (logand (ash offset -32) #xff)
             (logand (ash offset -40) #xff)
             (logand (ash offset -48) #xff)
             (logand (ash offset -56) #xff)
             ;; p_vaddr u64
             (logand vaddr #xff)
             (logand (ash vaddr -8)  #xff)
             (logand (ash vaddr -16) #xff)
             (logand (ash vaddr -24) #xff)
             (logand (ash vaddr -32) #xff)
             (logand (ash vaddr -40) #xff)
             (logand (ash vaddr -48) #xff)
             (logand (ash vaddr -56) #xff)
             ;; p_paddr u64
             (logand paddr #xff)
             (logand (ash paddr -8)  #xff)
             (logand (ash paddr -16) #xff)
             (logand (ash paddr -24) #xff)
             (logand (ash paddr -32) #xff)
             (logand (ash paddr -40) #xff)
             (logand (ash paddr -48) #xff)
             (logand (ash paddr -56) #xff)
             ;; p_filesz u64
             (logand filesz #xff)
             (logand (ash filesz -8)  #xff)
             (logand (ash filesz -16) #xff)
             (logand (ash filesz -24) #xff)
             (logand (ash filesz -32) #xff)
             (logand (ash filesz -40) #xff)
             (logand (ash filesz -48) #xff)
             (logand (ash filesz -56) #xff)
             ;; p_memsz u64
             (logand memsz #xff)
             (logand (ash memsz -8)  #xff)
             (logand (ash memsz -16) #xff)
             (logand (ash memsz -24) #xff)
             (logand (ash memsz -32) #xff)
             (logand (ash memsz -40) #xff)
             (logand (ash memsz -48) #xff)
             (logand (ash memsz -56) #xff)
             ;; p_align u64
             (logand align #xff)
             (logand (ash align -8)  #xff)
             (logand (ash align -16) #xff)
             (logand (ash align -24) #xff)
             (logand (ash align -32) #xff)
             (logand (ash align -40) #xff)
             (logand (ash align -48) #xff)
             (logand (ash align -56) #xff)))))
      (nelisp-elf--buf-emit buf record)
      nelisp-elf--phdr-size)))

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
  (let ((name      (or (plist-get fields :name)      0))
        (type      (or (plist-get fields :type)      nelisp-elf--sht-null))
        (flags     (or (plist-get fields :flags)     0))
        (addr      (or (plist-get fields :addr)      0))
        (offset    (or (plist-get fields :offset)    0))
        (size      (or (plist-get fields :size)      0))
        (link      (or (plist-get fields :link)      0))
        (info      (or (plist-get fields :info)      0))
        (addralign (or (plist-get fields :addralign) 0))
        (entsize   (or (plist-get fields :entsize)   0)))
    ;; Wave 16: single-call Shdr emit (= 64 bytes, fully inlined).
    (let ((record
           (apply
            #'unibyte-string
            (list
             ;; sh_name u32
             (logand name #xff) (logand (ash name -8) #xff)
             (logand (ash name -16) #xff) (logand (ash name -24) #xff)
             ;; sh_type u32
             (logand type #xff) (logand (ash type -8) #xff)
             (logand (ash type -16) #xff) (logand (ash type -24) #xff)
             ;; sh_flags u64
             (logand flags #xff)
             (logand (ash flags -8)  #xff)
             (logand (ash flags -16) #xff)
             (logand (ash flags -24) #xff)
             (logand (ash flags -32) #xff)
             (logand (ash flags -40) #xff)
             (logand (ash flags -48) #xff)
             (logand (ash flags -56) #xff)
             ;; sh_addr u64
             (logand addr #xff)
             (logand (ash addr -8)  #xff)
             (logand (ash addr -16) #xff)
             (logand (ash addr -24) #xff)
             (logand (ash addr -32) #xff)
             (logand (ash addr -40) #xff)
             (logand (ash addr -48) #xff)
             (logand (ash addr -56) #xff)
             ;; sh_offset u64
             (logand offset #xff)
             (logand (ash offset -8)  #xff)
             (logand (ash offset -16) #xff)
             (logand (ash offset -24) #xff)
             (logand (ash offset -32) #xff)
             (logand (ash offset -40) #xff)
             (logand (ash offset -48) #xff)
             (logand (ash offset -56) #xff)
             ;; sh_size u64
             (logand size #xff)
             (logand (ash size -8)  #xff)
             (logand (ash size -16) #xff)
             (logand (ash size -24) #xff)
             (logand (ash size -32) #xff)
             (logand (ash size -40) #xff)
             (logand (ash size -48) #xff)
             (logand (ash size -56) #xff)
             ;; sh_link u32
             (logand link #xff) (logand (ash link -8) #xff)
             (logand (ash link -16) #xff) (logand (ash link -24) #xff)
             ;; sh_info u32
             (logand info #xff) (logand (ash info -8) #xff)
             (logand (ash info -16) #xff) (logand (ash info -24) #xff)
             ;; sh_addralign u64
             (logand addralign #xff)
             (logand (ash addralign -8)  #xff)
             (logand (ash addralign -16) #xff)
             (logand (ash addralign -24) #xff)
             (logand (ash addralign -32) #xff)
             (logand (ash addralign -40) #xff)
             (logand (ash addralign -48) #xff)
             (logand (ash addralign -56) #xff)
             ;; sh_entsize u64
             (logand entsize #xff)
             (logand (ash entsize -8)  #xff)
             (logand (ash entsize -16) #xff)
             (logand (ash entsize -24) #xff)
             (logand (ash entsize -32) #xff)
             (logand (ash entsize -40) #xff)
             (logand (ash entsize -48) #xff)
             (logand (ash entsize -56) #xff)))))
      (nelisp-elf--buf-emit buf record)
      nelisp-elf--shdr-size)))

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
  (let ((name  (or (plist-get fields :name)  0))
        (info  (or (plist-get fields :info)  0))
        (other (or (plist-get fields :other) 0))
        (shndx (or (plist-get fields :shndx) 0))
        (value (or (plist-get fields :value) 0))
        (size  (or (plist-get fields :size)  0)))
    ;; Wave 16: single-call Sym emit (= 24 bytes, fully inlined).
    (let ((record
           (apply
            #'unibyte-string
            (list
             ;; st_name u32
             (logand name #xff) (logand (ash name -8) #xff)
             (logand (ash name -16) #xff) (logand (ash name -24) #xff)
             ;; st_info u8 + st_other u8
             (logand info  #xff)
             (logand other #xff)
             ;; st_shndx u16
             (logand shndx #xff) (logand (ash shndx -8) #xff)
             ;; st_value u64
             (logand value #xff)
             (logand (ash value -8)  #xff)
             (logand (ash value -16) #xff)
             (logand (ash value -24) #xff)
             (logand (ash value -32) #xff)
             (logand (ash value -40) #xff)
             (logand (ash value -48) #xff)
             (logand (ash value -56) #xff)
             ;; st_size u64
             (logand size #xff)
             (logand (ash size -8)  #xff)
             (logand (ash size -16) #xff)
             (logand (ash size -24) #xff)
             (logand (ash size -32) #xff)
             (logand (ash size -40) #xff)
             (logand (ash size -48) #xff)
             (logand (ash size -56) #xff)))))
      (nelisp-elf--buf-emit buf record)
      nelisp-elf--sym-size)))

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
  (let ((offset (or (plist-get fields :offset) 0))
        (info   (or (plist-get fields :info)   0))
        (addend (or (plist-get fields :addend) 0)))
    ;; Wave 16: single-call Rela emit (= 24 bytes, fully inlined).
    ;; Two's-complement addend conversion is inlined here (= mirrors the
    ;; legacy `nelisp-elf--write-le64-signed' helper).
    (let* ((addend-u (if (< addend 0) (+ addend (ash 1 64)) addend))
           (record
            (apply
             #'unibyte-string
             (list
              ;; r_offset u64
              (logand offset #xff)
              (logand (ash offset -8)  #xff)
              (logand (ash offset -16) #xff)
              (logand (ash offset -24) #xff)
              (logand (ash offset -32) #xff)
              (logand (ash offset -40) #xff)
              (logand (ash offset -48) #xff)
              (logand (ash offset -56) #xff)
              ;; r_info u64
              (logand info #xff)
              (logand (ash info -8)  #xff)
              (logand (ash info -16) #xff)
              (logand (ash info -24) #xff)
              (logand (ash info -32) #xff)
              (logand (ash info -40) #xff)
              (logand (ash info -48) #xff)
              (logand (ash info -56) #xff)
              ;; r_addend s64 (two's-complement)
              (logand addend-u #xff)
              (logand (ash addend-u -8)  #xff)
              (logand (ash addend-u -16) #xff)
              (logand (ash addend-u -24) #xff)
              (logand (ash addend-u -32) #xff)
              (logand (ash addend-u -40) #xff)
              (logand (ash addend-u -48) #xff)
              (logand (ash addend-u -56) #xff)))))
      (nelisp-elf--buf-emit buf record)
      nelisp-elf--rela-size)))

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
includes both headers in the loaded image (= Teensy ELF pattern).

Wave A25.5 (2026-05-24): the Ehdr / Phdr writes are inlined as
literal `(nelisp-elf--cbuf-push CBUF (unibyte-string ...))' forms
instead of `nelisp-elf-write-ehdr' / `-phdr' defun-calls — saves
one defun dispatch + 11 plist-get walks per record on standalone
NeLisp.  Byte sequence is identical to the legacy plist path
(verified by `cmp -l' against host Emacs output)."
  (let* ((text nelisp-elf--minimal-exit-0-text)
         (text-size (length text))
         (text-off (+ nelisp-elf--ehdr-size nelisp-elf--phdr-size))
         (filesz (+ text-off text-size))
         (vaddr-base nelisp-elf--minimal-vaddr-base)
         (entry (+ vaddr-base text-off))
         (flags (logior nelisp-elf--pf-r nelisp-elf--pf-x))
         (cbuf (nelisp-elf-make-buffer)))
    ;; ---- Ehdr (inline, mirrors --build-rel ehdr pattern) ----
    (nelisp-elf--cbuf-push
     cbuf
     (unibyte-string
      ;; e_ident[0..15] = magic + class + data + version + osabi + pad[8]
      #x7F #x45 #x4C #x46
      nelisp-elf--ei-class-64
      nelisp-elf--ei-data-lsb
      nelisp-elf--ev-current
      nelisp-elf--ei-osabi-sysv
      0 0 0 0 0 0 0 0
      ;; e_type u16 (= ET_EXEC)
      (logand nelisp-elf--et-exec #xff)
      (logand (ash nelisp-elf--et-exec -8) #xff)
      ;; e_machine u16 (= EM_X86_64)
      (logand nelisp-elf--em-x86-64 #xff)
      (logand (ash nelisp-elf--em-x86-64 -8) #xff)
      ;; e_version u32 (= EV_CURRENT, baked as 1)
      (logand nelisp-elf--ev-current #xff) 0 0 0
      ;; e_entry u64
      (logand entry #xff)
      (logand (ash entry -8)  #xff)
      (logand (ash entry -16) #xff)
      (logand (ash entry -24) #xff)
      (logand (ash entry -32) #xff)
      (logand (ash entry -40) #xff)
      (logand (ash entry -48) #xff)
      (logand (ash entry -56) #xff)
      ;; e_phoff u64 (= ehdr-size = 64)
      (logand nelisp-elf--ehdr-size #xff) 0 0 0 0 0 0 0
      ;; e_shoff u64 (= 0)
      0 0 0 0 0 0 0 0
      ;; e_flags u32 (= 0)
      0 0 0 0
      ;; e_ehsize u16
      (logand nelisp-elf--ehdr-size #xff)
      (logand (ash nelisp-elf--ehdr-size -8) #xff)
      ;; e_phentsize u16 (= sizeof(Phdr))
      (logand nelisp-elf--phdr-size #xff)
      (logand (ash nelisp-elf--phdr-size -8) #xff)
      ;; e_phnum u16 (= 1)
      1 0
      ;; e_shentsize u16 (= sizeof(Shdr))
      (logand nelisp-elf--shdr-size #xff)
      (logand (ash nelisp-elf--shdr-size -8) #xff)
      ;; e_shnum u16 (= 0)
      0 0
      ;; e_shstrndx u16 (= 0)
      0 0))
    ;; ---- Phdr (inline) ----
    (nelisp-elf--cbuf-push
     cbuf
     (unibyte-string
      ;; p_type u32 (= PT_LOAD)
      (logand nelisp-elf--pt-load #xff) 0 0 0
      ;; p_flags u32 (= PF_R | PF_X)
      (logand flags #xff) 0 0 0
      ;; p_offset u64 (= 0)
      0 0 0 0 0 0 0 0
      ;; p_vaddr u64
      (logand vaddr-base #xff)
      (logand (ash vaddr-base -8)  #xff)
      (logand (ash vaddr-base -16) #xff)
      (logand (ash vaddr-base -24) #xff)
      (logand (ash vaddr-base -32) #xff)
      (logand (ash vaddr-base -40) #xff)
      (logand (ash vaddr-base -48) #xff)
      (logand (ash vaddr-base -56) #xff)
      ;; p_paddr u64 (= vaddr-base)
      (logand vaddr-base #xff)
      (logand (ash vaddr-base -8)  #xff)
      (logand (ash vaddr-base -16) #xff)
      (logand (ash vaddr-base -24) #xff)
      (logand (ash vaddr-base -32) #xff)
      (logand (ash vaddr-base -40) #xff)
      (logand (ash vaddr-base -48) #xff)
      (logand (ash vaddr-base -56) #xff)
      ;; p_filesz u64
      (logand filesz #xff)
      (logand (ash filesz -8)  #xff)
      (logand (ash filesz -16) #xff)
      (logand (ash filesz -24) #xff)
      (logand (ash filesz -32) #xff)
      (logand (ash filesz -40) #xff)
      (logand (ash filesz -48) #xff)
      (logand (ash filesz -56) #xff)
      ;; p_memsz u64 (= filesz)
      (logand filesz #xff)
      (logand (ash filesz -8)  #xff)
      (logand (ash filesz -16) #xff)
      (logand (ash filesz -24) #xff)
      (logand (ash filesz -32) #xff)
      (logand (ash filesz -40) #xff)
      (logand (ash filesz -48) #xff)
      (logand (ash filesz -56) #xff)
      ;; p_align u64 (= #x1000)
      0 #x10 0 0 0 0 0 0))
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
are absent from the rich-plist input.

`undef' maps to SHN_UNDEF (= 0); used by Doc 100 §100.A extern
symbol entries so callers can register an SHN_UNDEF / STB_GLOBAL /
STT_NOTYPE symbol that the linker resolves against another
object."
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
   ((eq sec 'undef) nelisp-elf--shn-undef)
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
         (cbuf (nelisp-elf-make-buffer))
         ;; Wave A25.5: pre-compute Phdr flags so each inline path can
         ;; reuse the value without re-evaluating the `logior'.
         (rx-flags (logior nelisp-elf--pf-r nelisp-elf--pf-x))
         (rw-flags (and have-rw (logior nelisp-elf--pf-r nelisp-elf--pf-w))))
    ;; ---- Ehdr (Wave A25.5 inline, mirrors --build-rel pattern) ----
    (nelisp-elf--cbuf-push
     cbuf
     (unibyte-string
      ;; e_ident[0..15] = magic + class + data + version + osabi + pad[8]
      #x7F #x45 #x4C #x46
      nelisp-elf--ei-class-64
      nelisp-elf--ei-data-lsb
      nelisp-elf--ev-current
      nelisp-elf--ei-osabi-sysv
      0 0 0 0 0 0 0 0
      ;; e_type u16 (= ET_EXEC)
      (logand nelisp-elf--et-exec #xff)
      (logand (ash nelisp-elf--et-exec -8) #xff)
      ;; e_machine u16
      (logand machine-em #xff) (logand (ash machine-em -8) #xff)
      ;; e_version u32 (= EV_CURRENT, baked as 1)
      (logand nelisp-elf--ev-current #xff) 0 0 0
      ;; e_entry u64
      (logand entry #xff)
      (logand (ash entry -8)  #xff)
      (logand (ash entry -16) #xff)
      (logand (ash entry -24) #xff)
      (logand (ash entry -32) #xff)
      (logand (ash entry -40) #xff)
      (logand (ash entry -48) #xff)
      (logand (ash entry -56) #xff)
      ;; e_phoff u64
      (logand phdr-off #xff)
      (logand (ash phdr-off -8)  #xff)
      (logand (ash phdr-off -16) #xff)
      (logand (ash phdr-off -24) #xff)
      (logand (ash phdr-off -32) #xff)
      (logand (ash phdr-off -40) #xff)
      (logand (ash phdr-off -48) #xff)
      (logand (ash phdr-off -56) #xff)
      ;; e_shoff u64
      (logand shoff #xff)
      (logand (ash shoff -8)  #xff)
      (logand (ash shoff -16) #xff)
      (logand (ash shoff -24) #xff)
      (logand (ash shoff -32) #xff)
      (logand (ash shoff -40) #xff)
      (logand (ash shoff -48) #xff)
      (logand (ash shoff -56) #xff)
      ;; e_flags u32 (= 0)
      0 0 0 0
      ;; e_ehsize u16
      (logand nelisp-elf--ehdr-size #xff)
      (logand (ash nelisp-elf--ehdr-size -8) #xff)
      ;; e_phentsize u16
      (logand nelisp-elf--phdr-size #xff)
      (logand (ash nelisp-elf--phdr-size -8) #xff)
      ;; e_phnum u16
      (logand phnum #xff) (logand (ash phnum -8) #xff)
      ;; e_shentsize u16
      (logand nelisp-elf--shdr-size #xff)
      (logand (ash nelisp-elf--shdr-size -8) #xff)
      ;; e_shnum u16
      (logand shnum #xff) (logand (ash shnum -8) #xff)
      ;; e_shstrndx u16
      (logand shstrtab-shndx #xff)
      (logand (ash shstrtab-shndx -8) #xff)))
    ;; ---- Phdr[0]: PT_LOAD R+X covering Ehdr + Phdrs + .text + .rodata
    ;; (Wave A25.5 inline) ----
    (nelisp-elf--cbuf-push
     cbuf
     (unibyte-string
      ;; p_type u32 (= PT_LOAD)
      (logand nelisp-elf--pt-load #xff) 0 0 0
      ;; p_flags u32 (= rx-flags)
      (logand rx-flags #xff) 0 0 0
      ;; p_offset u64 (= 0)
      0 0 0 0 0 0 0 0
      ;; p_vaddr u64
      (logand vaddr-base #xff)
      (logand (ash vaddr-base -8)  #xff)
      (logand (ash vaddr-base -16) #xff)
      (logand (ash vaddr-base -24) #xff)
      (logand (ash vaddr-base -32) #xff)
      (logand (ash vaddr-base -40) #xff)
      (logand (ash vaddr-base -48) #xff)
      (logand (ash vaddr-base -56) #xff)
      ;; p_paddr u64 (= vaddr-base)
      (logand vaddr-base #xff)
      (logand (ash vaddr-base -8)  #xff)
      (logand (ash vaddr-base -16) #xff)
      (logand (ash vaddr-base -24) #xff)
      (logand (ash vaddr-base -32) #xff)
      (logand (ash vaddr-base -40) #xff)
      (logand (ash vaddr-base -48) #xff)
      (logand (ash vaddr-base -56) #xff)
      ;; p_filesz u64
      (logand rx-segment-filesz #xff)
      (logand (ash rx-segment-filesz -8)  #xff)
      (logand (ash rx-segment-filesz -16) #xff)
      (logand (ash rx-segment-filesz -24) #xff)
      (logand (ash rx-segment-filesz -32) #xff)
      (logand (ash rx-segment-filesz -40) #xff)
      (logand (ash rx-segment-filesz -48) #xff)
      (logand (ash rx-segment-filesz -56) #xff)
      ;; p_memsz u64
      (logand rx-segment-memsz #xff)
      (logand (ash rx-segment-memsz -8)  #xff)
      (logand (ash rx-segment-memsz -16) #xff)
      (logand (ash rx-segment-memsz -24) #xff)
      (logand (ash rx-segment-memsz -32) #xff)
      (logand (ash rx-segment-memsz -40) #xff)
      (logand (ash rx-segment-memsz -48) #xff)
      (logand (ash rx-segment-memsz -56) #xff)
      ;; p_align u64 (= page-size = #x1000)
      (logand page-size #xff)
      (logand (ash page-size -8)  #xff)
      (logand (ash page-size -16) #xff)
      (logand (ash page-size -24) #xff)
      (logand (ash page-size -32) #xff)
      (logand (ash page-size -40) #xff)
      (logand (ash page-size -48) #xff)
      (logand (ash page-size -56) #xff)))
    ;; ---- Phdr[1]: PT_LOAD R+W for .data + .bss (NOBITS) ----
    ;; (Wave A25.5 inline) ----
    (when have-rw
      (nelisp-elf--cbuf-push
       cbuf
       (unibyte-string
        ;; p_type u32 (= PT_LOAD)
        (logand nelisp-elf--pt-load #xff) 0 0 0
        ;; p_flags u32 (= rw-flags)
        (logand rw-flags #xff) 0 0 0
        ;; p_offset u64
        (logand data-off #xff)
        (logand (ash data-off -8)  #xff)
        (logand (ash data-off -16) #xff)
        (logand (ash data-off -24) #xff)
        (logand (ash data-off -32) #xff)
        (logand (ash data-off -40) #xff)
        (logand (ash data-off -48) #xff)
        (logand (ash data-off -56) #xff)
        ;; p_vaddr u64
        (logand data-vaddr #xff)
        (logand (ash data-vaddr -8)  #xff)
        (logand (ash data-vaddr -16) #xff)
        (logand (ash data-vaddr -24) #xff)
        (logand (ash data-vaddr -32) #xff)
        (logand (ash data-vaddr -40) #xff)
        (logand (ash data-vaddr -48) #xff)
        (logand (ash data-vaddr -56) #xff)
        ;; p_paddr u64
        (logand data-vaddr #xff)
        (logand (ash data-vaddr -8)  #xff)
        (logand (ash data-vaddr -16) #xff)
        (logand (ash data-vaddr -24) #xff)
        (logand (ash data-vaddr -32) #xff)
        (logand (ash data-vaddr -40) #xff)
        (logand (ash data-vaddr -48) #xff)
        (logand (ash data-vaddr -56) #xff)
        ;; p_filesz u64
        (logand rw-filesz #xff)
        (logand (ash rw-filesz -8)  #xff)
        (logand (ash rw-filesz -16) #xff)
        (logand (ash rw-filesz -24) #xff)
        (logand (ash rw-filesz -32) #xff)
        (logand (ash rw-filesz -40) #xff)
        (logand (ash rw-filesz -48) #xff)
        (logand (ash rw-filesz -56) #xff)
        ;; p_memsz u64
        (logand rw-memsz #xff)
        (logand (ash rw-memsz -8)  #xff)
        (logand (ash rw-memsz -16) #xff)
        (logand (ash rw-memsz -24) #xff)
        (logand (ash rw-memsz -32) #xff)
        (logand (ash rw-memsz -40) #xff)
        (logand (ash rw-memsz -48) #xff)
        (logand (ash rw-memsz -56) #xff)
        ;; p_align u64 (= page-size)
        (logand page-size #xff)
        (logand (ash page-size -8)  #xff)
        (logand (ash page-size -16) #xff)
        (logand (ash page-size -24) #xff)
        (logand (ash page-size -32) #xff)
        (logand (ash page-size -40) #xff)
        (logand (ash page-size -48) #xff)
        (logand (ash page-size -56) #xff))))
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

;; ---- Doc 99 §99.A ET_REL builder (= relocatable object output) ----
;;
;; ET_REL output drops the program-header / PT_LOAD machinery that ET_EXEC
;; requires.  No segments, no entry-point baking — `ld' resolves both at
;; link time.  Section addresses are 0 (relocatable), symbol values are
;; section-relative offsets, and relocations use the same section-relative
;; offsets without any vaddr baking.  The result is byte-compatible with
;; system `ld' / `gcc' on x86_64 + aarch64 Linux.

;; Wave 21 inline shdr builder (= 64-byte Elf64_Shdr via positional args).
;;
;; Wave 16 collapsed the per-record writers (`write-ehdr' / `write-shdr' /
;; `write-sym' / `write-rela') to a single `apply unibyte-string', but the
;; ET_REL orchestrator `--build-rel' still constructs a fresh plist at
;; every call site (= 9 fixed Shdrs + 1 STN_UNDEF Sym + N user Syms + M
;; Rela), then `write-shdr' walks that plist with 10 `plist-get' calls
;; before re-projecting into a flat byte list.  Each plist roundtrip costs
;; ~150-200 ms on standalone NeLisp (= no defsubst inlining + a full
;; symbol-table lookup per `plist-get'); for `spike-noop' that adds up to
;; ~9 × (1 + 10) = ~99 unnecessary defun-table walks before the byte
;; emit.
;;
;; The Wave 21 mitigation (= 4 changes inside `--build-rel'):
;;
;;   1.  `--build-rel-shdr' / `--build-rel-sym' / `--build-rel-rela' are
;;       positional `defmacro' builders that expand inline at every call
;;       site (= no plist construction at the call site, no `plist-get'
;;       walk inside the writer, no defun-table lookup per record).  Args
;;       are bound exactly once inside the expansion so a complex arg
;;       expression like `(logior shf-write shf-alloc)' isn't
;;       re-evaluated 8 times across the u64 byte slots.
;;
;;   2.  `nelisp-elf-strtab-add' is bypassed by an inline accumulator
;;       (`shstrtab-chunks' + `strtab-chunks') that grows a single
;;       chunk-list via `cons' instead of allocating + plist-putting a
;;       fresh unibyte string at every add.  Dedup is dropped from the
;;       shstrtab path (= the 9 section names are statically distinct);
;;       the symbol strtab keeps dedup via a per-name hash.
;;
;;   3.  The Ehdr write is inlined the same way (= no
;;       `nelisp-elf-write-ehdr' + plist roundtrip), and the section
;;       padding writes use `(nelisp-elf--cbuf-push CBUF (make-string N
;;       0))' directly instead of `nelisp-elf--write-pad' (= avoids the
;;       value-branch check + `coerce-unibyte' that the generic helper
;;       runs for non-zero pads we never produce).
;;
;;   4.  Pre-built unibyte chunks (= text / rodata / data / shstrtab /
;;       strtab) skip the per-write `coerce-unibyte' roundtrip and are
;;       pushed directly.  Caller-supplied `:text' / `:rodata' / `:data'
;;       are coerced once at function entry; the final
;;       `nelisp-elf-buffer-bytes' concat skips its second coerce since
;;       every chunk we pushed is already unibyte.
;;
;; The constant SHT_NULL Shdr and STN_UNDEF Sym are emitted as
;; `(make-string 64 0)' / `(make-string 24 0)' literal padding to avoid
;; 64 / 24 trivial `(logand 0 #xff)' evaluations on the slow standalone
;; interpreter.
;;
;; Byte output is identical to the legacy plist path; verified by
;; `cmp -l' against host-Emacs ET_REL output for every entry of
;; `compile-elisp-objects-manifest' (= 208 .o, 0 differing bytes).

(defmacro nelisp-elf--build-rel-shdr
    (cbuf name type flags addr offset size link info addralign entsize)
  "Append a 64-byte Elf64_Shdr to CBUF using POSITIONAL args (macro).
Expands inline at every call site so the standalone NeLisp interpreter
pays zero defun-table lookup per Shdr.  Each arg is bound exactly once
inside the expansion (= no re-evaluation of `(plist-get ...)' or
`(logior ...)' arg expressions per byte).  Byte sequence is identical
to `nelisp-elf-write-shdr' (= ELF64 gABI §2.4 layout)."
  (let ((cbuf-s (make-symbol "cbuf"))
        (nm  (make-symbol "name"))
        (tp  (make-symbol "type"))
        (fl  (make-symbol "flags"))
        (ad  (make-symbol "addr"))
        (of  (make-symbol "offset"))
        (sz  (make-symbol "size"))
        (lk  (make-symbol "link"))
        (in  (make-symbol "info"))
        (al  (make-symbol "addralign"))
        (es  (make-symbol "entsize")))
    `(let ((,cbuf-s ,cbuf)
           (,nm ,name) (,tp ,type) (,fl ,flags)
           (,ad ,addr) (,of ,offset) (,sz ,size)
           (,lk ,link) (,in ,info) (,al ,addralign) (,es ,entsize))
       (nelisp-elf--cbuf-push
        ,cbuf-s
        (unibyte-string
         ;; sh_name u32
         (logand ,nm #xff) (logand (ash ,nm -8) #xff)
         (logand (ash ,nm -16) #xff) (logand (ash ,nm -24) #xff)
         ;; sh_type u32
         (logand ,tp #xff) (logand (ash ,tp -8) #xff)
         (logand (ash ,tp -16) #xff) (logand (ash ,tp -24) #xff)
         ;; sh_flags u64
         (logand ,fl #xff)
         (logand (ash ,fl -8)  #xff)
         (logand (ash ,fl -16) #xff)
         (logand (ash ,fl -24) #xff)
         (logand (ash ,fl -32) #xff)
         (logand (ash ,fl -40) #xff)
         (logand (ash ,fl -48) #xff)
         (logand (ash ,fl -56) #xff)
         ;; sh_addr u64
         (logand ,ad #xff)
         (logand (ash ,ad -8)  #xff)
         (logand (ash ,ad -16) #xff)
         (logand (ash ,ad -24) #xff)
         (logand (ash ,ad -32) #xff)
         (logand (ash ,ad -40) #xff)
         (logand (ash ,ad -48) #xff)
         (logand (ash ,ad -56) #xff)
         ;; sh_offset u64
         (logand ,of #xff)
         (logand (ash ,of -8)  #xff)
         (logand (ash ,of -16) #xff)
         (logand (ash ,of -24) #xff)
         (logand (ash ,of -32) #xff)
         (logand (ash ,of -40) #xff)
         (logand (ash ,of -48) #xff)
         (logand (ash ,of -56) #xff)
         ;; sh_size u64
         (logand ,sz #xff)
         (logand (ash ,sz -8)  #xff)
         (logand (ash ,sz -16) #xff)
         (logand (ash ,sz -24) #xff)
         (logand (ash ,sz -32) #xff)
         (logand (ash ,sz -40) #xff)
         (logand (ash ,sz -48) #xff)
         (logand (ash ,sz -56) #xff)
         ;; sh_link u32
         (logand ,lk #xff) (logand (ash ,lk -8) #xff)
         (logand (ash ,lk -16) #xff) (logand (ash ,lk -24) #xff)
         ;; sh_info u32
         (logand ,in #xff) (logand (ash ,in -8) #xff)
         (logand (ash ,in -16) #xff) (logand (ash ,in -24) #xff)
         ;; sh_addralign u64
         (logand ,al #xff)
         (logand (ash ,al -8)  #xff)
         (logand (ash ,al -16) #xff)
         (logand (ash ,al -24) #xff)
         (logand (ash ,al -32) #xff)
         (logand (ash ,al -40) #xff)
         (logand (ash ,al -48) #xff)
         (logand (ash ,al -56) #xff)
         ;; sh_entsize u64
         (logand ,es #xff)
         (logand (ash ,es -8)  #xff)
         (logand (ash ,es -16) #xff)
         (logand (ash ,es -24) #xff)
         (logand (ash ,es -32) #xff)
         (logand (ash ,es -40) #xff)
         (logand (ash ,es -48) #xff)
         (logand (ash ,es -56) #xff))))))

(defmacro nelisp-elf--build-rel-sym
    (cbuf name info other shndx value size)
  "Append a 24-byte Elf64_Sym to CBUF using POSITIONAL args (macro).
Wave 21 inline equivalent of `nelisp-elf-write-sym' — same byte output,
no plist roundtrip, no defun call.  Args are bound exactly once."
  (let ((cbuf-s (make-symbol "cbuf"))
        (nm (make-symbol "name"))
        (in (make-symbol "info"))
        (ot (make-symbol "other"))
        (sh (make-symbol "shndx"))
        (vl (make-symbol "value"))
        (sz (make-symbol "size")))
    `(let ((,cbuf-s ,cbuf)
           (,nm ,name) (,in ,info) (,ot ,other)
           (,sh ,shndx) (,vl ,value) (,sz ,size))
       (nelisp-elf--cbuf-push
        ,cbuf-s
        (unibyte-string
         ;; st_name u32
         (logand ,nm #xff) (logand (ash ,nm -8) #xff)
         (logand (ash ,nm -16) #xff) (logand (ash ,nm -24) #xff)
         ;; st_info u8 + st_other u8
         (logand ,in  #xff)
         (logand ,ot #xff)
         ;; st_shndx u16
         (logand ,sh #xff) (logand (ash ,sh -8) #xff)
         ;; st_value u64
         (logand ,vl #xff)
         (logand (ash ,vl -8)  #xff)
         (logand (ash ,vl -16) #xff)
         (logand (ash ,vl -24) #xff)
         (logand (ash ,vl -32) #xff)
         (logand (ash ,vl -40) #xff)
         (logand (ash ,vl -48) #xff)
         (logand (ash ,vl -56) #xff)
         ;; st_size u64
         (logand ,sz #xff)
         (logand (ash ,sz -8)  #xff)
         (logand (ash ,sz -16) #xff)
         (logand (ash ,sz -24) #xff)
         (logand (ash ,sz -32) #xff)
         (logand (ash ,sz -40) #xff)
         (logand (ash ,sz -48) #xff)
         (logand (ash ,sz -56) #xff))))))

(defmacro nelisp-elf--build-rel-rela (cbuf offset info addend)
  "Append a 24-byte Elf64_Rela to CBUF using POSITIONAL args (macro).
Wave 21 inline equivalent of `nelisp-elf-write-rela' — same byte
output, no plist roundtrip, no defun call.  Two's-complement ADDEND
conversion is inlined."
  (let ((cbuf-s (make-symbol "cbuf"))
        (of (make-symbol "offset"))
        (in (make-symbol "info"))
        (au (make-symbol "addend-u")))
    `(let* ((,cbuf-s ,cbuf)
            (,of ,offset)
            (,in ,info)
            (,au (let ((a ,addend)) (if (< a 0) (+ a (ash 1 64)) a))))
       (nelisp-elf--cbuf-push
        ,cbuf-s
        (unibyte-string
         ;; r_offset u64
         (logand ,of #xff)
         (logand (ash ,of -8)  #xff)
         (logand (ash ,of -16) #xff)
         (logand (ash ,of -24) #xff)
         (logand (ash ,of -32) #xff)
         (logand (ash ,of -40) #xff)
         (logand (ash ,of -48) #xff)
         (logand (ash ,of -56) #xff)
         ;; r_info u64
         (logand ,in #xff)
         (logand (ash ,in -8)  #xff)
         (logand (ash ,in -16) #xff)
         (logand (ash ,in -24) #xff)
         (logand (ash ,in -32) #xff)
         (logand (ash ,in -40) #xff)
         (logand (ash ,in -48) #xff)
         (logand (ash ,in -56) #xff)
         ;; r_addend s64 (two's-complement)
         (logand ,au #xff)
         (logand (ash ,au -8)  #xff)
         (logand (ash ,au -16) #xff)
         (logand (ash ,au -24) #xff)
         (logand (ash ,au -32) #xff)
         (logand (ash ,au -40) #xff)
         (logand (ash ,au -48) #xff)
         (logand (ash ,au -56) #xff))))))

(defun nelisp-elf--build-rel (plist)
  "Build an ET_REL ELF64 relocatable object from PLIST, return unibyte string.
PLIST is the same shape `nelisp-elf--build-rich' accepts (= :text /
:rodata / :data / :bss-size / :symbols / :relocs / :machine), minus
:entry-sym (= ET_REL has no entry point — the linker resolves it).
Sections .symtab / .strtab / .shstrtab are always emitted; .rela.text
is emitted only when :relocs is non-empty.  Symbol :value fields are
treated as section-relative offsets (= no vaddr-base addition)."
  (let* ((text     (nelisp-elf--coerce-unibyte
                    (or (plist-get plist :text)
                        (error "nelisp-elf: :text is required"))))
         (rodata-raw (plist-get plist :rodata))
         (rodata     (and rodata-raw (nelisp-elf--coerce-unibyte rodata-raw)))
         (data-raw   (plist-get plist :data))
         (data       (and data-raw (nelisp-elf--coerce-unibyte data-raw)))
         (bss-size (or (plist-get plist :bss-size) 0))
         (symbols  (plist-get plist :symbols))
         (relocs   (plist-get plist :relocs))
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
         (have-rela   (and relocs (> (length relocs) 0)))
         (text-size   (length text))
         (rodata-size (if have-rodata (length rodata) 0))
         (data-size   (if have-data (length data) 0))
         ;; ---- File layout (= no Phdrs, sections start after Ehdr).
         (text-off    nelisp-elf--ehdr-size)
         (rodata-off  (+ text-off text-size))
         (data-off    (+ rodata-off rodata-size))
         (after-data  (+ data-off data-size))
         ;; .bss is NOBITS — no file footprint.
         (non-alloc-base after-data)
         (shstrtab-off non-alloc-base)
         ;; Build .shstrtab inline (= avoid 8 `nelisp-elf-strtab-add'
         ;; plist roundtrips for the statically distinct section names).
         ;; Chunks are pushed in emit order; we track each name's
         ;; offset by maintaining a running `:length' counter.
         (shstrtab-chunks (list (unibyte-string 0)))
         (shstrtab-pos 1) ;; reserved leading NUL
         (sh-name-text
          (let ((off shstrtab-pos) (s ".text"))
            (push (concat (encode-coding-string s 'utf-8 t)
                          (unibyte-string 0))
                  shstrtab-chunks)
            (setq shstrtab-pos (+ shstrtab-pos (length s) 1))
            off))
         (sh-name-rodata
          (when have-rodata
            (let ((off shstrtab-pos) (s ".rodata"))
              (push (concat (encode-coding-string s 'utf-8 t)
                            (unibyte-string 0))
                    shstrtab-chunks)
              (setq shstrtab-pos (+ shstrtab-pos (length s) 1))
              off)))
         (sh-name-data
          (when have-data
            (let ((off shstrtab-pos) (s ".data"))
              (push (concat (encode-coding-string s 'utf-8 t)
                            (unibyte-string 0))
                    shstrtab-chunks)
              (setq shstrtab-pos (+ shstrtab-pos (length s) 1))
              off)))
         (sh-name-bss
          (when have-bss
            (let ((off shstrtab-pos) (s ".bss"))
              (push (concat (encode-coding-string s 'utf-8 t)
                            (unibyte-string 0))
                    shstrtab-chunks)
              (setq shstrtab-pos (+ shstrtab-pos (length s) 1))
              off)))
         (sh-name-shstrtab
          (let ((off shstrtab-pos) (s ".shstrtab"))
            (push (concat (encode-coding-string s 'utf-8 t)
                          (unibyte-string 0))
                  shstrtab-chunks)
            (setq shstrtab-pos (+ shstrtab-pos (length s) 1))
            off))
         (sh-name-strtab
          (let ((off shstrtab-pos) (s ".strtab"))
            (push (concat (encode-coding-string s 'utf-8 t)
                          (unibyte-string 0))
                  shstrtab-chunks)
            (setq shstrtab-pos (+ shstrtab-pos (length s) 1))
            off))
         (sh-name-symtab
          (let ((off shstrtab-pos) (s ".symtab"))
            (push (concat (encode-coding-string s 'utf-8 t)
                          (unibyte-string 0))
                  shstrtab-chunks)
            (setq shstrtab-pos (+ shstrtab-pos (length s) 1))
            off))
         (sh-name-rela
          (when have-rela
            (let ((off shstrtab-pos) (s ".rela.text"))
              (push (concat (encode-coding-string s 'utf-8 t)
                            (unibyte-string 0))
                    shstrtab-chunks)
              (setq shstrtab-pos (+ shstrtab-pos (length s) 1))
              off)))
         (shstrtab-bytes (apply #'concat (nreverse shstrtab-chunks)))
         (shstrtab-size  shstrtab-pos)
         (strtab-off (+ shstrtab-off shstrtab-size))
         ;; Build .strtab from the user-provided symbol names — inline
         ;; the dedup hash + chunk accumulator (= same pattern; one
         ;; allocation per distinct name instead of a plist-put per add).
         (strtab-chunks (list (unibyte-string 0)))
         (strtab-pos 1)
         (strtab-index (make-hash-table :test 'equal))
         (sym-name-offsets
          (let (acc)
            (puthash "" 0 strtab-index)
            (dolist (sym symbols)
              (let* ((nm (plist-get sym :name))
                     (cached (gethash nm strtab-index))
                     (off (cond
                           (cached cached)
                           ((string-empty-p nm) 0)
                           (t
                            (let ((this-off strtab-pos))
                              (push (concat
                                     (encode-coding-string nm 'utf-8 t)
                                     (unibyte-string 0))
                                    strtab-chunks)
                              (setq strtab-pos
                                    (+ strtab-pos (length nm) 1))
                              (puthash nm this-off strtab-index)
                              this-off)))))
                (push (cons nm off) acc)))
            (nreverse acc)))
         (strtab-bytes (apply #'concat (nreverse strtab-chunks)))
         (strtab-size  strtab-pos)
         (symtab-off (nelisp-elf--align-up
                      (+ strtab-off strtab-size) 8))
         (sym-count (1+ (length symbols)))
         (symtab-size (* nelisp-elf--sym-size sym-count))
         (local-count
          (let ((c 1))
            (dolist (sym symbols)
              (when (memq (or (plist-get sym :bind) 'local) '(local 0))
                (setq c (1+ c))))
            c))
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
         (cbuf (nelisp-elf-make-buffer)))
    ;; ---- Ehdr (= ET_REL: no phdrs, no entry).
    ;; Wave 21: inline the 64-byte ELF64 Ehdr literally instead of going
    ;; through `nelisp-elf-write-ehdr' (= would re-execute 11 plist-get
    ;; calls + a fresh plist alloc on the hot path).  Byte sequence is
    ;; identical to gABI §2.2 — verified by `cmp -l' against host Emacs.
    (nelisp-elf--cbuf-push
     cbuf
     (unibyte-string
      ;; e_ident[0..15] = magic + class + data + version + osabi + pad[8]
      #x7F #x45 #x4C #x46
      nelisp-elf--ei-class-64
      nelisp-elf--ei-data-lsb
      nelisp-elf--ev-current
      nelisp-elf--ei-osabi-sysv
      0 0 0 0 0 0 0 0
      ;; e_type u16 (= ET_REL)
      (logand nelisp-elf--et-rel #xff)
      (logand (ash nelisp-elf--et-rel -8) #xff)
      ;; e_machine u16
      (logand machine-em #xff) (logand (ash machine-em -8) #xff)
      ;; e_version u32 (= EV_CURRENT)
      (logand nelisp-elf--ev-current #xff) 0 0 0
      ;; e_entry u64 (= 0)
      0 0 0 0 0 0 0 0
      ;; e_phoff u64 (= 0)
      0 0 0 0 0 0 0 0
      ;; e_shoff u64
      (logand shoff #xff)
      (logand (ash shoff -8)  #xff)
      (logand (ash shoff -16) #xff)
      (logand (ash shoff -24) #xff)
      (logand (ash shoff -32) #xff)
      (logand (ash shoff -40) #xff)
      (logand (ash shoff -48) #xff)
      (logand (ash shoff -56) #xff)
      ;; e_flags u32 (= 0)
      0 0 0 0
      ;; e_ehsize u16
      (logand nelisp-elf--ehdr-size #xff)
      (logand (ash nelisp-elf--ehdr-size -8) #xff)
      ;; e_phentsize u16 (= 0 for ET_REL)
      0 0
      ;; e_phnum u16 (= 0)
      0 0
      ;; e_shentsize u16
      (logand nelisp-elf--shdr-size #xff)
      (logand (ash nelisp-elf--shdr-size -8) #xff)
      ;; e_shnum u16
      (logand shnum #xff) (logand (ash shnum -8) #xff)
      ;; e_shstrndx u16
      (logand shstrtab-shndx #xff)
      (logand (ash shstrtab-shndx -8) #xff)))
    ;; ---- .text
    (unless (= (nelisp-elf-buffer-length cbuf) text-off)
      (error "nelisp-elf: .text offset drift (length=%d expected=%d)"
             (nelisp-elf-buffer-length cbuf) text-off))
    ;; Wave 21: text/rodata/data are caller-supplied unibyte strings (=
    ;; built by the asm assembler via `unibyte-string'), shstrtab/strtab
    ;; bytes are built locally above via `unibyte-string' + utf-8 encode
    ;; — every path is already unibyte.  Coerce only the caller-supplied
    ;; bytes once at entry instead of on every write (= `coerce-unibyte'
    ;; on standalone NeLisp goes through `multibyte-string-p' +
    ;; `with-temp-buffer' which is ~1 sec per call).
    (nelisp-elf--cbuf-push cbuf text)
    ;; ---- .rodata
    (when have-rodata
      (nelisp-elf--cbuf-push cbuf rodata))
    ;; ---- .data
    (when have-data
      (nelisp-elf--cbuf-push cbuf data))
    ;; ---- .shstrtab
    (let ((pad (- shstrtab-off (nelisp-elf-buffer-length cbuf))))
      (when (> pad 0)
        (nelisp-elf--cbuf-push cbuf (make-string pad 0))))
    (nelisp-elf--cbuf-push cbuf shstrtab-bytes)
    ;; ---- .strtab
    (let ((pad (- strtab-off (nelisp-elf-buffer-length cbuf))))
      (when (> pad 0)
        (nelisp-elf--cbuf-push cbuf (make-string pad 0))))
    (nelisp-elf--cbuf-push cbuf strtab-bytes)
    ;; ---- .symtab (= STN_UNDEF + user symbols, locals first).
    (let ((pad (- symtab-off (nelisp-elf-buffer-length cbuf))))
      (when (> pad 0)
        (nelisp-elf--cbuf-push cbuf (make-string pad 0))))
    ;; STN_UNDEF entry (= 24 zero bytes, no logand/ash needed).
    (nelisp-elf--cbuf-push cbuf (make-string nelisp-elf--sym-size 0))
    (dolist (sym ordered-symbols)
      (let* ((nm    (plist-get sym :name))
             (sect  (or (plist-get sym :section) 'text))
             (bind  (or (plist-get sym :bind) 'local))
             (type  (or (plist-get sym :type) 'notype))
             (name-off (cdr (assoc nm sym-name-offsets)))
             (shndx (nelisp-elf--section-shndx
                     sect text-shndx rodata-shndx
                     data-shndx bss-shndx))
             ;; ET_REL: symbol value is section-relative offset (= no vaddr).
             (value (or (plist-get sym :value) 0))
             (size  (or (plist-get sym :size) 0))
             (info  (nelisp-elf-sym-info
                     (nelisp-elf--sym-bind-code bind)
                     (nelisp-elf--sym-type-code type))))
        (nelisp-elf--build-rel-sym cbuf name-off info 0 shndx value size)))
    ;; ---- .rela.text (optional)
    (when have-rela
      (let ((pad (- rela-off (nelisp-elf-buffer-length cbuf))))
        (when (> pad 0)
          (nelisp-elf--cbuf-push cbuf (make-string pad 0))))
      ;; Pre-build a symbol-name -> 1-based index table so the per-rel
      ;; lookup is O(1) instead of O(N) (= matters for spike-noop's
      ;; large extern list).
      (let ((sym-idx-table (make-hash-table :test 'equal))
            (sidx 1))
        (dolist (s ordered-symbols)
          (puthash (plist-get s :name) sidx sym-idx-table)
          (setq sidx (1+ sidx)))
        (dolist (rel relocs)
          (let* ((rsym (plist-get rel :symbol))
                 (rtype (plist-get rel :type))
                 (sym-idx
                  (or (gethash rsym sym-idx-table)
                      (error "nelisp-elf: relocation references unknown symbol %S"
                             rsym)))
                 (offset (or (plist-get rel :offset) 0))
                 (addend (or (plist-get rel :addend) 0))
                 (info (nelisp-elf-rela-info
                        sym-idx
                        (nelisp-elf--reloc-type-code rtype))))
            (nelisp-elf--build-rel-rela cbuf offset info addend)))))
    ;; ---- Shdr table.
    (let ((pad (- shoff (nelisp-elf-buffer-length cbuf))))
      (when (> pad 0)
        (nelisp-elf--cbuf-push cbuf (make-string pad 0))))
    ;; Shdr[0] = SHT_NULL (= 64 zero bytes, no logand/ash needed).
    (nelisp-elf--cbuf-push cbuf (make-string nelisp-elf--shdr-size 0))
    ;; Shdr[text] (= addr 0, relocatable).
    (nelisp-elf--build-rel-shdr
     cbuf sh-name-text nelisp-elf--sht-progbits
     (logior nelisp-elf--shf-alloc nelisp-elf--shf-execinstr)
     0 text-off text-size 0 0 16 0)
    (when have-rodata
      (nelisp-elf--build-rel-shdr
       cbuf sh-name-rodata nelisp-elf--sht-progbits
       nelisp-elf--shf-alloc
       0 rodata-off rodata-size 0 0 8 0))
    (when have-data
      (nelisp-elf--build-rel-shdr
       cbuf sh-name-data nelisp-elf--sht-progbits
       (logior nelisp-elf--shf-write nelisp-elf--shf-alloc)
       0 data-off data-size 0 0 8 0))
    (when have-bss
      ;; SHT_NOBITS sh_offset is conceptually "after the file data" — we
      ;; point it at after-data so readers that sanity-check the value
      ;; see a monotonic layout.
      (nelisp-elf--build-rel-shdr
       cbuf sh-name-bss nelisp-elf--sht-nobits
       (logior nelisp-elf--shf-write nelisp-elf--shf-alloc)
       0 after-data bss-size 0 0 8 0))
    (nelisp-elf--build-rel-shdr
     cbuf sh-name-shstrtab nelisp-elf--sht-strtab
     0 0 shstrtab-off shstrtab-size 0 0 1 0)
    (nelisp-elf--build-rel-shdr
     cbuf sh-name-strtab nelisp-elf--sht-strtab
     0 0 strtab-off strtab-size 0 0 1 0)
    (nelisp-elf--build-rel-shdr
     cbuf sh-name-symtab nelisp-elf--sht-symtab
     0 0 symtab-off symtab-size
     strtab-shndx local-count 8 nelisp-elf--sym-size)
    (when have-rela
      (nelisp-elf--build-rel-shdr
       cbuf sh-name-rela nelisp-elf--sht-rela
       0 0 rela-off rela-size
       symtab-shndx text-shndx 8 nelisp-elf--rela-size))
    ;; Wave 21: all pushed chunks are guaranteed unibyte by construction
    ;; above, so concat the chunk-list directly without the final
    ;; `coerce-unibyte' walk (= saves 1 `multibyte-string-p' +
    ;; `with-temp-buffer' roundtrip on the ~504-byte spike-noop output).
    (apply #'concat (nreverse (plist-get cbuf :chunks)))))

;;;###autoload
(defun nelisp-elf-write-binary (file-path sections)
  "Emit an ELF64 binary to FILE-PATH.
SECTIONS is one of the named sentinels (`minimal-exit-0' for the
§91.a Teensy-ELF shortcut, `hello-world-write' for the §91.c
corpus #2 sample that prints `hello\\n' via `write(2)' and exits)
or a plist that drives the §91.b/§91.c rich path:
  :e-type     `exec' (default — ET_EXEC, statically-linked executable)
              or `rel' (Doc 99 §99.A — ET_REL relocatable object that
              system `ld' can link).
  :text       unibyte bytes (= machine code; required).
  :rodata     unibyte bytes (= constants).
  :data       unibyte bytes (= initialised writable data, §91.c).
  :bss-size   integer (= NOBITS zero-fill size in bytes, §91.c).
  :symbols    list of plists with keys :name :value :size :section
              :bind :type (= STB_LOCAL/GLOBAL/WEAK keyword,
              STT_NOTYPE/OBJECT/FUNC/SECTION keyword).
              :section is `text' / `rodata' / `data' / `bss'.
              For ET_REL :value is section-relative; for ET_EXEC
              the loader bakes in the vaddr base.
  :relocs     list of plists with keys :section :offset :symbol :type
              :addend (= pc32 / abs64 / plt32 keyword).
  :entry-sym  symbol name resolved to e_entry (required for ET_EXEC,
              ignored for ET_REL — `ld' picks the entry at link time).
  :machine    arch tag, `x86_64' (default) / `aarch64' / integer EM_* code.

When :data or :bss-size is present the ET_EXEC loader image splits
into two PT_LOAD segments — one RX (= .text + .rodata) and one RW
page-aligned (= .data + .bss, with .bss declared NOBITS).  The ET_REL
path emits no program headers at all.

The file is written with mode #o755 (= +x bit set)."
  (let ((bytes
         (cond
          ((eq sections 'minimal-exit-0)
           (nelisp-elf--build-minimal-exit-0))
          ((eq sections 'hello-world-write)
           (nelisp-elf--build-hello-world-write))
          ((listp sections)
           (if (eq (plist-get sections :e-type) 'rel)
               (nelisp-elf--build-rel sections)
             (nelisp-elf--build-rich sections)))
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
