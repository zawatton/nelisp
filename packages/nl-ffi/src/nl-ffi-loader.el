;;; nl-ffi-loader.el --- pure-elisp ELF loader for the static reader -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; FFI step 3, increment 1.  Steps 1-2 (nl-ffi.el) reach a foreign symbol
;; through `nl-ffi-call' (a fixed, build-time extern table) or `dlopen'/
;; `dlsym' -- both only exist in the DYNAMIC reader
;; (`NELISP_READER_DYNAMIC=1').  The default, STATICALLY linked reader has
;; neither: it has no PLT/GOT of its own to route a runtime `extern-call'
;; through, and `nl-ffi-call' there is `fboundp' but signals the reader's
;; own catchable `nelisp-unsupported-primitive' on every call.  This file
;; closes that gap with a small, pure-elisp ELF loader built entirely on
;; primitives already `fboundp' in the static reader -- no new Rust, no
;; reader rebuild, nothing behind an opt-in build flag.
;;
;; Confirmed present and working in the DEFAULT `make standalone-reader'
;; binary (measured 2026-09-16 on this repository's Linux x86_64 dev host;
;; see target/ai/ffi-loader-report.md "Step 1" for the exact probes):
;;   `syscall-direct'  (openat/fstat/mmap/mprotect/munmap/close -- confirmed
;;                      via a real reservation-then-MAP_FIXED-carve mmap
;;                      sequence, an mprotect call, and a munmap call, all
;;                      returning 0/a real address, not merely `fboundp')
;;   `ptr-read-u64' / `ptr-write-u64' / `ptr-read-u8' / `ptr-write-u8'
;;   `alloc-bytes'
;;   `ptr-call'        (confirmed by mmap-ing a 5-byte hand-assembled
;;                      function and calling it: `(lea rax, [rdi+1]; ret)'
;;                      returned 42 for input 41)
;; Also confirmed present (not in the brief's list, used here as a
;; convenience -- everything above alone would suffice): `ptr-read-u32'/
;; `ptr-write-u32'.  All of the above are UNCONDITIONAL dispatch arms in
;; `nelisp-standalone--applyfn-bf-builtins' (scripts/nelisp-standalone-
;; build.el) -- unlike `nl-ffi-call' and `dlopen'/`dlsym', they are never
;; gated behind `NELISP_READER_DYNAMIC'.
;;
;; IMPORTANT finding, not in the brief, discovered while making this
;; loader copy real file content: `ptr-read-u64'/`ptr-write-u64' do NOT
;; round-trip an arbitrary 64-bit bit pattern above `most-positive-fixnum'
;; (2^61-1; measured: (ash 1 61) reads back negative, (ash 1 62) as 0) --
;; measured directly against this repository's own dynstr bytes
;; ("nl_ffi_loader_fixture_double" packed 8 bytes at a time), a word whose
;; true value was 6875138896031411712 read back as 2263452877604023808,
;; off by EXACTLY 2^62 (4611686018427387904).  `ptr-read-u32' of the same
;; bytes (both halves, independently) matched exactly, and plain `elisp'
;; arithmetic has the identical ceiling -- `(ash 1 61)' already reads back
;; negative and `(ash 1 62)'/`(ash 1 63)' both evaluate to 0 in this
;; reader, so this is not specific to the two pointer primitives; it is
;; this interpreter path's own integer representation losing bits beyond
;; roughly 2^61-2^62, not a `ptr-read-u64'/`ptr-write-u64'-specific bug.
;; Every OTHER field this file reads with `ptr-read-u64' (a vaddr, a file
;; offset, a size, a packed symbol-index+relocation-type, a small
;; relocation addend) is realistically always far below that ceiling for
;; any real object this loader can otherwise handle at all, so those call
;; sites are left as `ptr-read-u64'/`ptr-write-u64' with this note as
;; their justification; the two places that read genuinely ARBITRARY bit
;; patterns -- bulk segment/string content (`nl-ffi-loader--copy-bytes')
;; and a GNU hash bloom word (`nl-ffi-loader--u64-bit-set-p') -- use
;; `ptr-read-u32'/`ptr-write-u32' (confirmed exact across the full 32-bit
;; unsigned range, nowhere near the ceiling) instead, never combining two
;; halves into one 64-bit Lisp value.
;;
;; Design (increment 1 scope -- see the package README's Roadmap step 3
;; entry and this file's own "Out of scope" list below for what is
;; deliberately NOT here):
;;
;;   1. MAP each PT_LOAD at its own virtual address, with its own final
;;      protection, preserving the gaps between segments.  This loader
;;      reserves the object's whole address span with one PROT_NONE
;;      anonymous `mmap' (so every segment lands at a fixed, consistent
;;      bias relative to the vaddrs the file itself uses -- required for
;;      RIP-relative references between segments to resolve correctly),
;;      then re-maps each PT_LOAD's own page-aligned range with
;;      MAP_FIXED|MAP_ANONYMOUS|MAP_PRIVATE (mmap zero-fills a fresh
;;      anonymous page, which is exactly `.bss' zeroing -- no separate
;;      zeroing pass is needed) and copies exactly `p_filesz' bytes from
;;      the file into it; the `p_memsz - p_filesz' tail stays the zero an
;;      anonymous mmap already produced.  Gaps between segments' aligned
;;      ranges are never re-mapped past the initial PROT_NONE reservation,
;;      so they stay inaccessible rather than silently becoming part of
;;      an adjacent segment (the flat single-mmap shape
;;      `dev/nelisp-ffi/spike-loader-full.el' used, and the pitfall this
;;      file's own header calls out as still open).  `mprotect' to each
;;      segment's REAL final protection (from `p_flags') runs only AFTER
;;      every relocation has been applied, so a read-execute text segment
;;      is never left writable at the point relocations that target it
;;      (if any) actually happen to run -- see
;;      `nl-ffi-loader--protect-segments'.
;;
;;      Simplification: if two PT_LOAD segments' page-aligned ranges
;;      overlap (share a page), this loader REFUSES the object
;;      (`nl-ffi-loader-unsupported' with reason `:overlapping-segments')
;;      rather than working out the correct per-page union of their
;;      protections.  Measured empirically (see the report): this
;;      repository's toolchain (binutils 2.45, `-z separate-code' default)
;;      never produces this shape -- every PT_LOAD's page-rounded range
;;      starts exactly where the previous one's ends, even when the raw,
;;      unrounded `p_vaddr' values are not themselves page-aligned -- so
;;      the refusal is not expected to fire against normally linked
;;      objects; it exists so a toolchain that DOES produce shared pages
;;      is refused cleanly instead of mis-protected silently.
;;
;;   2. RELOCATE.  `R_X86_64_RELATIVE' (base + addend, no symbol lookup)
;;      always applies.  `R_X86_64_GLOB_DAT'/`R_X86_64_JUMP_SLOT' apply
;;      when the referenced `.dynsym' entry is DEFINED in this same
;;      object (`st_shndx' <> SHN_UNDEF); when it is undefined (the
;;      object needs another, un-loaded object to supply it -- dependency
;;      loading is out of scope, see below) this refuses with
;;      `nl-ffi-loader-unsupported' naming the symbol.  `.rela.dyn' and
;;      `.rela.plt' (PLT relocations) are processed the same way -- both
;;      are Elf64_Rela on x86-64; there is no Elf64_Rel PLT variant to
;;      support (checked against `DT_PLTREL' and refused if it ever
;;      claims otherwise).  Every relocation is applied EAGERLY, at load
;;      time (there is no lazy-PLT trampoline here -- equivalent to real
;;      `dlopen(..., RTLD_NOW)').  Anything else -- `R_X86_64_IRELATIVE'
;;      (IFUNC), any TLS relocation type, or any other numeric type
;;      (`R_X86_64_64', `R_X86_64_PC32', `R_X86_64_COPY', ...) -- is
;;      refused with `nl-ffi-loader-unsupported' naming the relocation
;;      type and, when the relocation carries one, the symbol.  A PT_TLS
;;      program header is rejected up front, before any relocation is
;;      even inspected: this loader never runs `__tls_get_addr'-style
;;      machinery, so an object that NEEDS a TLS block allocated is
;;      refused outright rather than partially mapped.
;;
;;   3. RESOLVE a symbol by name through `.dynsym', preferring
;;      `.gnu.hash' (the only style this repository's own toolchain
;;      emits -- `--hash-style=gnu' is binutils's default and there is no
;;      classic `.hash' section in any object this loader has been
;;      exercised against), falling back to the classic SysV `.hash' when
;;      `.gnu.hash' is absent but `.hash' is present, and to a linear
;;      `.dynsym' scan (bounded by `.dynstr - .dynsym / sizeof(Elf64_Sym)'
;;      -- fragile in general, since nothing GUARANTEES `.dynstr'
;;      immediately follows `.dynsym' in address order, but it is the
;;      layout every object this loader has been tried against uses, and
;;      it is a documented last resort, never the primary path) when
;;      NEITHER hash table exists.  Returns the resolved runtime address,
;;      or 0 -- the same "not found" sentinel `dlsym' itself would return,
;;      and the same one `nl-ffi--resolve-via-dlsym' (nl-ffi.el) already
;;      expects -- never nil.
;;
;;   4. WIRING (see nl-ffi.el): `ffi:library' tries the existing `dlopen'
;;      path first; when that signals `nl-ffi-unavailable' (the static
;;      reader has no working `nl-ffi-call' at all -- see
;;      `nl-ffi--call-checked'), it falls back to `nl-ffi-loader-open'
;;      instead of propagating the failure.  `nl-ffi--resolve-via-dlsym'
;;      dispatches a loader-produced handle (`nl-ffi-loader-handle-p') to
;;      `nl-ffi-loader-symbol' instead of a real `dlsym' call, so
;;      `ffi:defun'-generated functions keep their existing call shape and
;;      error contract (`nl-ffi-unresolved-symbol' for a name this loader
;;      cannot find either) with no changes to `ffi:defun' itself.
;;
;; Out of scope for this increment -- each refuses with a named condition
;; rather than doing partial work:
;;   - Dependency loading (`DT_NEEDED').  A relocation whose symbol is
;;     undefined in this object signals `nl-ffi-loader-unsupported'
;;     (reason `:undefined-symbol') rather than searching for, or
;;     silently ignoring, a dependency.  `DT_NEEDED' itself is also
;;     checked directly and refused (reason `:needs-dependency') even for
;;     an object with no relocation that would otherwise expose it.
;;   - Symbol interposition (no search across multiple loaded objects;
;;     each `nl-ffi-loader-open' result only ever resolves within its own
;;     object).
;;   - TLS (`:tls-segment' for a PT_TLS header; `:tls-relocation' for a
;;     TLS-classed relocation type even without one, belt and braces).
;;   - IFUNC / `R_X86_64_IRELATIVE' (reason `:ifunc') -- would require
;;     CALLING the resolver function during relocation, which this loader
;;     does not do.
;;   - Initializers, `DT_INIT'/`DT_INIT_ARRAY' (reason `:has-initializers')
;;     -- never invoked, and an object that has them is refused rather
;;     than silently loaded-but-uninitialized.
;;   - Unloading (no `nl-ffi-loader-close'; a loaded object's mappings
;;     live for the rest of the process, exactly like every other
;;     caller-built buffer in this runtime -- see `nl-ffi--string-to-
;;     cstring''s own docstring for the same policy elsewhere in this
;;     package).
;;   - Anything the six-integer `ptr-call' cannot express: a float
;;     argument/return, or more than six arguments -- unchanged from step
;;     2, still enforced by `nl-ffi--ptr-call-invoke' (`nl-ffi-dlsym-
;;     float-unsupported'/`nl-ffi-too-many-arguments'), not duplicated
;;     here.
;;   - `DT_RELR' (the compact relative-relocation encoding some newer
;;     linker defaults can emit with `-z pack-relative-relocs').  Not
;;     decoded; an object using it is refused (reason `:relr-relocations')
;;     rather than silently leaving those relocations unapplied.  This
;;     repository's own toolchain does not default to it (confirmed
;;     absent from every probe object in the report), and the gate's own
;;     fixture is linked with `-Wl,-z,nopack-relative-relocs' so a future
;;     toolchain default change cannot silently break it.
;;   - `PT_GNU_RELRO' hardening (the RELRO segment's own read-only
;;     re-protection after relocation).  Not applied: a RELRO range keeps
;;     whichever protection its OWN PT_LOAD gives it (normally already
;;     read-write, since RELRO is a sub-range of the RW segment), rather
;;     than being tightened to read-only post-relocation the way a real
;;     `ld.so' does.  A correctness simplification, not a security
;;     property this loader claims.

;;; Code:

(declare-function syscall-direct "ext:nelisp-runtime" (nr a0 a1 a2 a3 a4 a5))
(declare-function alloc-bytes "ext:nelisp-runtime" (nbytes align))
(declare-function ptr-read-u8 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u8 "ext:nelisp-runtime" (ptr offset value))
(declare-function ptr-read-u32 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u32 "ext:nelisp-runtime" (ptr offset value))
(declare-function ptr-read-u64 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u64 "ext:nelisp-runtime" (ptr offset value))
(declare-function ptr-call "ext:nelisp-runtime" (address a b c d e f))
;; Defined earlier in nl-ffi.el, which loads this file after it -- see
;; that file's Commentary on load order.  Declared here so byte-compiling
;; this file alone (its own gate's `nl-ffi.el' load never having run) does
;; not warn that it is undefined.  `nl-ffi--string-to-cstring' is NOT
;; reused here even though it does the same thing -- it is `nl-ffi.el's
;; own private (`--') helper, and `nl-ns-inventory' polices exactly that
;; boundary; see `nl-ffi-loader--cstring' below instead.
(declare-function nl-ffi-get-string "nl-ffi" (ptr))

;;;; --- error conditions ---------------------------------------------------
;;
;; Both are children of `nl-ffi-error' (defined earlier in nl-ffi.el, which
;; loads this file -- see that file's Commentary for the load order this
;; relies on), so an existing broad `nl-ffi-error' handler keeps working
;; unchanged; a caller that wants to tell "the file/mapping step failed"
;; from "the object needs something this loader refuses to half-do" can
;; still catch these two by name.

(define-error 'nl-ffi-loader-error
  "pure-elisp ELF loader error" 'nl-ffi-error)

(define-error 'nl-ffi-loader-open-failed
  "opening or mapping the shared object failed (openat/fstat/mmap)"
  'nl-ffi-loader-error)

(define-error 'nl-ffi-loader-unsupported
  "the shared object needs something this increment's loader refuses \
rather than half-does -- see the DATA reason and this file's Commentary \
\"Out of scope\" list"
  'nl-ffi-loader-error)

;;;; --- syscall / mmap / ELF constants (x86-64 Linux) ------------------------

(defconst nl-ffi-loader--sys-close 3)
(defconst nl-ffi-loader--sys-fstat 5)
(defconst nl-ffi-loader--sys-mmap 9)
(defconst nl-ffi-loader--sys-mprotect 10)
(defconst nl-ffi-loader--sys-munmap 11)
(defconst nl-ffi-loader--sys-openat 257)

(defconst nl-ffi-loader--at-fdcwd -100)
(defconst nl-ffi-loader--o-rdonly 0)

(defconst nl-ffi-loader--prot-none 0)
(defconst nl-ffi-loader--prot-read 1)
(defconst nl-ffi-loader--prot-write 2)
(defconst nl-ffi-loader--prot-exec 4)

(defconst nl-ffi-loader--map-shared 1)
(defconst nl-ffi-loader--map-private 2)
(defconst nl-ffi-loader--map-fixed 16)
(defconst nl-ffi-loader--map-anonymous 32)

(defconst nl-ffi-loader--page-size 4096)

(defconst nl-ffi-loader--pt-load 1)
(defconst nl-ffi-loader--pt-dynamic 2)
(defconst nl-ffi-loader--pt-tls 7)

(defconst nl-ffi-loader--pf-x 1)
(defconst nl-ffi-loader--pf-w 2)
(defconst nl-ffi-loader--pf-r 4)

(defconst nl-ffi-loader--dt-null 0)
(defconst nl-ffi-loader--dt-needed 1)
(defconst nl-ffi-loader--dt-pltrelsz 2)
(defconst nl-ffi-loader--dt-hash 4)
(defconst nl-ffi-loader--dt-strtab 5)
(defconst nl-ffi-loader--dt-symtab 6)
(defconst nl-ffi-loader--dt-rela 7)
(defconst nl-ffi-loader--dt-relasz 8)
(defconst nl-ffi-loader--dt-relaent 9)
(defconst nl-ffi-loader--dt-syment 11)
(defconst nl-ffi-loader--dt-init 12)
(defconst nl-ffi-loader--dt-pltrel 20)
(defconst nl-ffi-loader--dt-jmprel 23)
(defconst nl-ffi-loader--dt-init-array 25)
(defconst nl-ffi-loader--dt-relr #x6fffefff
  "Sentinel this file treats as \"any DT_RELR-family tag\" -- see the
actual check in `nl-ffi-loader--parse-dynamic', which matches the three
real tag values (DT_RELR/DT_RELRSZ/DT_RELRENT = 0x6fffe000+35/36/37)
directly rather than through this constant; kept only as a documented
name for the Commentary to point at.")
(defconst nl-ffi-loader--dt-relr-lo #x6fffe035)
(defconst nl-ffi-loader--dt-relr-hi #x6fffe037)
(defconst nl-ffi-loader--dt-gnu-hash #x6ffffef5)

(defconst nl-ffi-loader--reloc-relative 8)
(defconst nl-ffi-loader--reloc-glob-dat 6)
(defconst nl-ffi-loader--reloc-jump-slot 7)
(defconst nl-ffi-loader--reloc-irelative 37)

(defconst nl-ffi-loader--tls-relocation-types
  '(16 17 18 19 20 21 22 23 34 35 36)
  "R_X86_64_{DTPMOD64,DTPOFF64,TPOFF64,TLSGD,TLSLD,DTPOFF32,GOTTPOFF,
TPOFF32,GOTPC32_TLSDESC,TLSDESC_CALL,TLSDESC} -- refused even though this
file's PT_TLS check (`nl-ffi-loader--open-1') already rejects any object
that needs one of these before a single relocation is inspected; kept as
a second, independent check on the relocation type itself.")

;;;; --- small numeric helpers -------------------------------------------------

(defun nl-ffi-loader--align-down (n align)
  (- n (mod n align)))

(defun nl-ffi-loader--align-up (n align)
  (nl-ffi-loader--align-down (+ n (1- align)) align))

(defun nl-ffi-loader--u16 (ptr offset)
  "Read the 2-byte little-endian field at PTR+OFFSET.
There is no `ptr-read-u16' primitive; every ELF64 u16 field this loader
reads (`e_type'/`e_machine'/`e_phentsize'/`e_phnum', `st_shndx') is
immediately followed, within the same already-mapped struct, by at least
2 more bytes belonging to an adjacent field -- so reading 4 bytes with
`ptr-read-u32' and masking the low 16 bits never reads unmapped memory;
those adjacent bytes (the high 16 bits of the 4-byte read) are simply not
looked at."
  (logand (ptr-read-u32 ptr offset) #xffff))

(defun nl-ffi-loader--syscall-error-p (result)
  "Return non-nil when RESULT is a raw Linux syscall's -errno failure.
A real mmap/openat/fstat/mprotect/munmap/close success is never negative
on this ABI (user-space addresses never set the top bit as a signed
64-bit value); an error is `-errno', always in [-4095, -1] by convention
-- checked as a range rather than plain `(< result 0)' so a caller
mis-reading a real (impossibly huge) address as negative would not be
misclassified as \"a small errno\" and vice versa."
  (and (< result 0) (> result -4096)))

;;;; --- reading the file: open + fstat + read-only mmap -----------------------

(defun nl-ffi-loader--cstring (string)
  "Copy STRING into a fresh NUL-terminated buffer; return its address.
A local copy of `nl-ffi.el's own `nl-ffi--string-to-cstring' (same
`alloc-bytes'/`ptr-write-u8' shape) rather than a cross-file call to it:
that name is `nl-ffi.el's private (`--') helper, and `nl-ns-inventory'
polices exactly that file-privacy boundary.  STRING is treated as a
sequence of bytes, exactly as the original does; see its docstring for
the same caveats (multibyte text, the buffer never being freed)."
  (let* ((n (length string))
         (buf (alloc-bytes (1+ n) 1))
         (i 0))
    (while (< i n)
      (ptr-write-u8 buf i (aref string i))
      (setq i (1+ i)))
    (ptr-write-u8 buf n 0)
    buf))

(defun nl-ffi-loader--open-ro (path)
  "openat(AT_FDCWD, PATH, O_RDONLY); return the fd, or signal.
Signals `nl-ffi-loader-open-failed' (DATA: PATH, the raw -errno) on
failure.  PATH is used exactly as given -- see this file's Commentary and
`nl-ffi-loader-open''s docstring for why this loader does no SONAME
search-path resolution the way real `dlopen' does for a bare library
name; a caller wants a real, openable path here."
  (let* ((path-c (nl-ffi-loader--cstring path))
         (fd (syscall-direct nl-ffi-loader--sys-openat
                              nl-ffi-loader--at-fdcwd path-c
                              nl-ffi-loader--o-rdonly 0 0 0)))
    (if (nl-ffi-loader--syscall-error-p fd)
        (signal 'nl-ffi-loader-open-failed (list path fd))
      fd)))

(defun nl-ffi-loader--fstat-size (fd)
  "Return FD's `st_size' via `fstat(2)'.
`struct stat''s `st_size' field sits at byte offset 48 on this platform's
layout (confirmed against `struct stat' -- see the report's Step 1 for
the field-offset derivation this matches)."
  (let* ((statbuf (alloc-bytes 144 8))
         (rc (syscall-direct nl-ffi-loader--sys-fstat fd statbuf 0 0 0 0)))
    (if (nl-ffi-loader--syscall-error-p rc)
        (signal 'nl-ffi-loader-open-failed (list :fstat-failed fd rc))
      (ptr-read-u64 statbuf 48))))

(defun nl-ffi-loader--map-file-readonly (path)
  "Open, fstat, and read-only `mmap' PATH's whole contents.
Returns (FD-CLOSED . (BASE . SIZE)); the fd itself is closed immediately
after the `mmap' call succeeds (the mapping persists independently of the
fd, exactly like every other file-backed mapping on Linux)."
  (let* ((fd (nl-ffi-loader--open-ro path))
         (size (nl-ffi-loader--fstat-size fd)))
    (when (<= size 0)
      (syscall-direct nl-ffi-loader--sys-close fd 0 0 0 0 0)
      (signal 'nl-ffi-loader-open-failed (list :empty-or-unreadable path size)))
    (let ((base (syscall-direct nl-ffi-loader--sys-mmap 0 size
                                 nl-ffi-loader--prot-read
                                 nl-ffi-loader--map-private fd 0)))
      (syscall-direct nl-ffi-loader--sys-close fd 0 0 0 0 0)
      (when (nl-ffi-loader--syscall-error-p base)
        (signal 'nl-ffi-loader-open-failed (list :mmap-file-failed path base)))
      (cons base size))))

;;;; --- ELF header / program header parsing -----------------------------------

(defconst nl-ffi-loader--elf-magic '(#x7f #x45 #x4c #x46)
  "The four ELF magic bytes: 0x7f, then ASCII \"ELF\".")

(defun nl-ffi-loader--check-elf-header (file-base path)
  "Validate FILE-BASE is a little-endian ELFCLASS64 ET_DYN x86-64 image.
Signals `nl-ffi-loader-unsupported' (reason `:not-elf64-shared-object')
otherwise -- this loader only ever targets a shared object built for the
same architecture/ABI it is itself running on."
  (let ((ok
         (and (= (ptr-read-u8 file-base 0) (nth 0 nl-ffi-loader--elf-magic))
              (= (ptr-read-u8 file-base 1) (nth 1 nl-ffi-loader--elf-magic))
              (= (ptr-read-u8 file-base 2) (nth 2 nl-ffi-loader--elf-magic))
              (= (ptr-read-u8 file-base 3) (nth 3 nl-ffi-loader--elf-magic))
              (= (ptr-read-u8 file-base 4) 2)   ; EI_CLASS = ELFCLASS64
              (= (ptr-read-u8 file-base 5) 1)   ; EI_DATA = little-endian
              (= (nl-ffi-loader--u16 file-base 16) 3)   ; e_type = ET_DYN
              (= (nl-ffi-loader--u16 file-base 18) 62)))) ; e_machine = EM_X86_64
    (unless ok
      (signal 'nl-ffi-loader-unsupported
              (list :not-elf64-shared-object path)))))

(defun nl-ffi-loader--program-headers (file-base)
  "Parse FILE-BASE's program headers.
Returns a plist: `:loads' (a list of (P_VADDR P_OFFSET P_FILESZ P_MEMSZ
P_FLAGS), sorted by P_VADDR ascending), `:dyn-vaddr' (PT_DYNAMIC's
P_VADDR, or nil), `:tls-p' (non-nil when a PT_TLS header is present)."
  (let* ((phoff (ptr-read-u64 file-base 32))
         (phentsize (nl-ffi-loader--u16 file-base 54))
         (phnum (nl-ffi-loader--u16 file-base 56))
         (loads nil) (dyn-vaddr nil) (tls-p nil)
         (i 0))
    (while (< i phnum)
      (let* ((ph (+ file-base phoff (* i phentsize)))
             (p-type (ptr-read-u32 ph 0))
             (p-flags (ptr-read-u32 ph 4))
             (p-offset (ptr-read-u64 ph 8))
             (p-vaddr (ptr-read-u64 ph 16))
             (p-filesz (ptr-read-u64 ph 32))
             (p-memsz (ptr-read-u64 ph 40)))
        (cond
         ((= p-type nl-ffi-loader--pt-load)
          (push (list p-vaddr p-offset p-filesz p-memsz p-flags) loads))
         ((= p-type nl-ffi-loader--pt-dynamic)
          (setq dyn-vaddr p-vaddr))
         ((= p-type nl-ffi-loader--pt-tls)
          (setq tls-p t))))
      (setq i (1+ i)))
    (list :loads (sort (nreverse loads) (lambda (a b) (< (car a) (car b))))
          :dyn-vaddr dyn-vaddr
          :tls-p tls-p)))

;;;; --- mapping the image: reserve, carve, copy, (later) protect --------------

(defun nl-ffi-loader--copy-bytes (src dst n)
  "Copy N bytes from SRC to DST, 4 at a time with a byte-wise tail.
Both SRC and DST may be unaligned -- `ptr-read-u32'/`ptr-write-u32' do
not require alignment on this reader (measured; see the report).  Uses
the 32-bit primitives rather than `ptr-read-u64'/`ptr-write-u64': this
copies ARBITRARY segment content (code, string tables, data), which can
carry any bit pattern, and this file's Commentary documents that the
64-bit primitives do not round-trip a value above `most-positive-fixnum' --
common in ordinary text/data, confirmed by this exact copy corrupting a
symbol name before this fix.  A 32-bit word is always far below that
ceiling, so this is exact for any content."
  (let ((i 0) (full (* 4 (/ n 4))))
    (while (< i full)
      (ptr-write-u32 dst i (ptr-read-u32 src i))
      (setq i (+ i 4)))
    (while (< i n)
      (ptr-write-u8 dst i (ptr-read-u8 src i))
      (setq i (1+ i)))))

(defun nl-ffi-loader--prot-of-pflags (flags)
  "Translate an Elf64_Phdr P_FLAGS bitfield to a Linux PROT_* bitfield.
The two use different bit positions for the same three permissions."
  (logior (if (/= (logand flags nl-ffi-loader--pf-r) 0) nl-ffi-loader--prot-read 0)
          (if (/= (logand flags nl-ffi-loader--pf-w) 0) nl-ffi-loader--prot-write 0)
          (if (/= (logand flags nl-ffi-loader--pf-x) 0) nl-ffi-loader--prot-exec 0)))

(defun nl-ffi-loader--reserve (loads)
  "Reserve one contiguous, currently-inaccessible span covering LOADS.
Returns (BIAS . SPAN-SIZE).  BIAS is added to every P_VADDR in LOADS (and
to every other vaddr-shaped field this loader later reads -- symbol
values, DT_* pointers, relocation offsets) to get a real runtime address;
it is whatever the kernel actually placed the reservation `mmap' at minus
the lowest P_VADDR (normally 0 for a PIE shared object, so BIAS usually
equals the reservation's own address, but this does not assume that)."
  (let* ((lo (nl-ffi-loader--align-down (caar loads) nl-ffi-loader--page-size))
         (hi (let ((m 0))
               (dolist (seg loads)
                 (let ((end (+ (nth 0 seg) (nth 3 seg))))
                   (when (> end m) (setq m end))))
               (nl-ffi-loader--align-up m nl-ffi-loader--page-size)))
         (span (- hi lo))
         (reservation (syscall-direct nl-ffi-loader--sys-mmap 0 span
                                       nl-ffi-loader--prot-none
                                       (logior nl-ffi-loader--map-private
                                               nl-ffi-loader--map-anonymous)
                                       -1 0)))
    (when (nl-ffi-loader--syscall-error-p reservation)
      (signal 'nl-ffi-loader-open-failed (list :mmap-reservation-failed reservation)))
    (cons (- reservation lo) span)))

(defun nl-ffi-loader--map-and-copy-segments (bias file-base loads)
  "Carve out and populate every PT_LOAD segment in LOADS at BIAS.
Signals `nl-ffi-loader-unsupported' (reason `:overlapping-segments') if
two segments' page-aligned ranges overlap -- see this file's Commentary
for why that is refused rather than resolved via a per-page protection
union."
  (let ((prev-hi nil))
    (dolist (seg loads)
      (let* ((vaddr (nth 0 seg)) (offset (nth 1 seg))
             (filesz (nth 2 seg)) (memsz (nth 3 seg))
             (lo (nl-ffi-loader--align-down vaddr nl-ffi-loader--page-size))
             (hi (nl-ffi-loader--align-up (+ vaddr memsz) nl-ffi-loader--page-size)))
        (when (and prev-hi (< lo prev-hi))
          (signal 'nl-ffi-loader-unsupported
                  (list :overlapping-segments lo prev-hi)))
        (let ((m (syscall-direct nl-ffi-loader--sys-mmap (+ bias lo) (- hi lo)
                                  (logior nl-ffi-loader--prot-read nl-ffi-loader--prot-write)
                                  (logior nl-ffi-loader--map-private
                                          nl-ffi-loader--map-anonymous
                                          nl-ffi-loader--map-fixed)
                                  -1 0)))
          (when (or (nl-ffi-loader--syscall-error-p m) (/= m (+ bias lo)))
            (signal 'nl-ffi-loader-open-failed (list :mmap-segment-failed lo m))))
        (when (> filesz 0)
          (nl-ffi-loader--copy-bytes (+ file-base offset) (+ bias vaddr) filesz))
        (setq prev-hi hi)))))

(defun nl-ffi-loader--protect-segments (bias loads)
  "Apply each PT_LOAD's real final protection, one `mprotect' call each.
Safe only because `nl-ffi-loader--map-and-copy-segments' already refused
any pair of segments whose page-aligned ranges overlap; see its
docstring."
  (dolist (seg loads)
    (let* ((vaddr (nth 0 seg)) (memsz (nth 3 seg)) (flags (nth 4 seg))
           (lo (nl-ffi-loader--align-down vaddr nl-ffi-loader--page-size))
           (hi (nl-ffi-loader--align-up (+ vaddr memsz) nl-ffi-loader--page-size))
           (prot (nl-ffi-loader--prot-of-pflags flags))
           (rc (syscall-direct nl-ffi-loader--sys-mprotect (+ bias lo) (- hi lo)
                                prot 0 0 0)))
      (when (nl-ffi-loader--syscall-error-p rc)
        (signal 'nl-ffi-loader-open-failed (list :mprotect-failed lo rc))))))

;;;; --- .dynamic parsing -------------------------------------------------------

(defun nl-ffi-loader--parse-dynamic (bias dyn-vaddr)
  "Read the Elf64_Dyn array at BIAS+DYN-VADDR into a plist of runtime facts.
Every vaddr-shaped DT_* value (DT_STRTAB/DT_SYMTAB/DT_HASH/DT_GNU_HASH/
DT_RELA/DT_JMPREL) is returned already biased into a real runtime
address; every size/count/flag value (DT_STRSZ and friends, DT_NEEDED's
presence, DT_PLTREL) is returned as-is.  Also detects DT_RELR/DT_RELRSZ/
DT_RELRENT (the 0x6fffe035..0x6fffe037 tag range) so an object using the
compact relative-relocation encoding is refused rather than silently
under-relocated -- see this file's Commentary."
  (let ((strtab nil) (symtab nil) (syment nil)
        (gnu-hash nil) (sysv-hash nil)
        (rela nil) (relasz 0) (relaent 0)
        (jmprel nil) (pltrelsz 0) (pltrel nil)
        (needed-p nil) (init-p nil) (relr-p nil)
        (i 0) (go t))
    (while go
      (let* ((entry (+ bias dyn-vaddr (* i 16)))
             (tag (ptr-read-u64 entry 0))
             (val (ptr-read-u64 entry 8)))
        (cond
         ((= tag nl-ffi-loader--dt-null) (setq go nil))
         ((= tag nl-ffi-loader--dt-needed) (setq needed-p t))
         ((= tag nl-ffi-loader--dt-hash) (setq sysv-hash (+ bias val)))
         ((= tag nl-ffi-loader--dt-strtab) (setq strtab (+ bias val)))
         ((= tag nl-ffi-loader--dt-symtab) (setq symtab (+ bias val)))
         ((= tag nl-ffi-loader--dt-rela) (setq rela (+ bias val)))
         ((= tag nl-ffi-loader--dt-relasz) (setq relasz val))
         ((= tag nl-ffi-loader--dt-relaent) (setq relaent val))
         ((= tag nl-ffi-loader--dt-syment) (setq syment val))
         ((= tag nl-ffi-loader--dt-init) (setq init-p t))
         ((= tag nl-ffi-loader--dt-init-array) (setq init-p t))
         ((= tag nl-ffi-loader--dt-pltrelsz) (setq pltrelsz val))
         ((= tag nl-ffi-loader--dt-pltrel) (setq pltrel val))
         ((= tag nl-ffi-loader--dt-jmprel) (setq jmprel (+ bias val)))
         ((= tag nl-ffi-loader--dt-gnu-hash) (setq gnu-hash (+ bias val)))
         ((and (>= tag nl-ffi-loader--dt-relr-lo) (<= tag nl-ffi-loader--dt-relr-hi))
          (setq relr-p t))))
      (setq i (1+ i)))
    (list :strtab strtab :symtab symtab :syment (or syment 24)
          :gnu-hash gnu-hash :sysv-hash sysv-hash
          :rela rela :relasz relasz :relaent (if (> relaent 0) relaent 24)
          :jmprel jmprel :pltrelsz pltrelsz :pltrel pltrel
          :needed-p needed-p :init-p init-p :relr-p relr-p)))

;;;; --- relocations -------------------------------------------------------------

(defun nl-ffi-loader--dynsym-name (dyn symtab-index)
  "Return the symbol name (a string) at SYMTAB-INDEX in DYN's `.dynsym'."
  (let* ((sym (+ (plist-get dyn :symtab) (* symtab-index (plist-get dyn :syment))))
         (name-off (ptr-read-u32 sym 0)))
    (nl-ffi-get-string (+ (plist-get dyn :strtab) name-off))))

(defun nl-ffi-loader--dynsym-value-and-shndx (dyn symtab-index)
  "Return (ST_VALUE . ST_SHNDX) at SYMTAB-INDEX in DYN's `.dynsym'."
  (let* ((sym (+ (plist-get dyn :symtab) (* symtab-index (plist-get dyn :syment))))
         (st-value (ptr-read-u64 sym 8))
         (st-shndx (nl-ffi-loader--u16 sym 6)))
    (cons st-value st-shndx)))

(defun nl-ffi-loader--apply-one-relocation (path bias dyn rela-addr)
  "Apply the single Elf64_Rela relocation at RELA-ADDR.
Signals `nl-ffi-loader-unsupported' for anything outside RELATIVE/
GLOB_DAT/JUMP_SLOT -- see this file's Commentary."
  (let* ((r-offset (ptr-read-u64 rela-addr 0))
         (r-info (ptr-read-u64 rela-addr 8))
         (r-addend (ptr-read-u64 rela-addr 16))
         (r-type (logand r-info #xffffffff))
         (r-sym (ash r-info -32))
         (target (+ bias r-offset)))
    (cond
     ((= r-type nl-ffi-loader--reloc-relative)
      (ptr-write-u64 target 0 (+ bias r-addend)))
     ((or (= r-type nl-ffi-loader--reloc-glob-dat)
          (= r-type nl-ffi-loader--reloc-jump-slot))
      (let* ((vs (nl-ffi-loader--dynsym-value-and-shndx dyn r-sym))
             (st-value (car vs)) (st-shndx (cdr vs)))
        (when (= st-shndx 0) ; SHN_UNDEF
          (signal 'nl-ffi-loader-unsupported
                  (list :undefined-symbol path (nl-ffi-loader--dynsym-name dyn r-sym))))
        (ptr-write-u64 target 0 (+ bias st-value r-addend))))
     ((= r-type nl-ffi-loader--reloc-irelative)
      (signal 'nl-ffi-loader-unsupported (list :ifunc path r-offset)))
     ((memq r-type nl-ffi-loader--tls-relocation-types)
      (signal 'nl-ffi-loader-unsupported (list :tls-relocation path r-type)))
     (t
      (signal 'nl-ffi-loader-unsupported
              (list :relocation-type path r-type
                    (condition-case nil (nl-ffi-loader--dynsym-name dyn r-sym)
                      (error nil))))))))

(defun nl-ffi-loader--apply-relocation-table (path bias dyn addr size entsize)
  (when (and addr (> size 0))
    (let ((i 0) (n (/ size entsize)))
      (while (< i n)
        (nl-ffi-loader--apply-one-relocation path bias dyn (+ addr (* i entsize)))
        (setq i (1+ i))))))

(defconst nl-ffi-loader--dt-pltrel-rela 7
  "DT_PLTREL's value when `.rela.plt' entries are Elf64_Rela (the only
form that exists on x86-64 -- Elf64_Rel, without an inline addend, is not
part of this ABI at all).")

(defun nl-ffi-loader--apply-relocations (path bias dyn)
  "Apply every relocation `.rela.dyn' and `.rela.plt' (if present) carry."
  (when (and (plist-get dyn :jmprel) (plist-get dyn :pltrel)
             (/= (plist-get dyn :pltrel) nl-ffi-loader--dt-pltrel-rela))
    (signal 'nl-ffi-loader-unsupported (list :rel-style-plt-not-rela path)))
  (nl-ffi-loader--apply-relocation-table
   path bias dyn (plist-get dyn :rela) (plist-get dyn :relasz) (plist-get dyn :relaent))
  (nl-ffi-loader--apply-relocation-table
   path bias dyn (plist-get dyn :jmprel) (plist-get dyn :pltrelsz)
   ;; .rela.plt entries are always Elf64_Rela (24 bytes) on x86-64.
   24))

;;;; --- symbol resolution: .gnu.hash / .hash / linear scan ---------------------

(defun nl-ffi-loader--gnu-hash (name)
  "The GNU hash function (uint32_t wraparound) over NAME's raw bytes."
  (let ((h 5381) (i 0) (n (length name)))
    (while (< i n)
      (setq h (logand (+ (* h 33) (aref name i)) #xffffffff))
      (setq i (1+ i)))
    h))

(defun nl-ffi-loader--streq-cstring (addr name)
  "Return non-nil when the NUL-terminated C string at ADDR equals NAME."
  (let ((i 0) (n (length name)) (ok t))
    (while (and ok (< i n))
      (if (= (ptr-read-u8 addr i) (aref name i)) (setq i (1+ i)) (setq ok nil)))
    (and ok (= (ptr-read-u8 addr n) 0))))

(defun nl-ffi-loader--symbol-name-matches-p (dyn symtab-index name)
  (let* ((sym (+ (plist-get dyn :symtab) (* symtab-index (plist-get dyn :syment))))
         (name-off (ptr-read-u32 sym 0)))
    (nl-ffi-loader--streq-cstring (+ (plist-get dyn :strtab) name-off) name)))

(defun nl-ffi-loader--u64-bit-set-p (addr bit)
  "Return non-nil when bit BIT (0-63) of the u64 at ADDR is set.
Reads only the relevant 32-bit half with `ptr-read-u32' and tests the
bit within it -- never combines both halves into one 64-bit Lisp value.
A GNU hash bloom word is an arbitrary bitmask (any of its 64 bits can be
set), which is exactly the case this file's Commentary documents
`ptr-read-u64' as unsafe for."
  (if (< bit 32)
      (/= (logand (ptr-read-u32 addr 0) (ash 1 bit)) 0)
    (/= (logand (ptr-read-u32 addr 4) (ash 1 (- bit 32))) 0)))

(defun nl-ffi-loader--lookup-gnu-hash (dyn name)
  "Resolve NAME via DYN's `.gnu.hash'.  Returns ST_VALUE+bias, or nil."
  (let* ((base (plist-get dyn :gnu-hash))
         (nbuckets (ptr-read-u32 base 0))
         (symoffset (ptr-read-u32 base 4))
         (bloom-size (ptr-read-u32 base 8))
         (bloom-shift (ptr-read-u32 base 12))
         (bloom-base (+ base 16))
         (buckets-base (+ bloom-base (* bloom-size 8)))
         (chain-base (+ buckets-base (* nbuckets 4)))
         (h (nl-ffi-loader--gnu-hash name)))
    (when (and (> nbuckets 0) (> bloom-size 0))
      (let* ((word-index (mod (/ h 64) bloom-size))
             (word-addr (+ bloom-base (* word-index 8)))
             (bit1 (mod h 64))
             (bit2 (mod (ash h (- bloom-shift)) 64)))
        (when (and (nl-ffi-loader--u64-bit-set-p word-addr bit1)
                   (nl-ffi-loader--u64-bit-set-p word-addr bit2))
          (let ((sym-index (ptr-read-u32 buckets-base (* (mod h nbuckets) 4))))
            (when (> sym-index 0)
              (let ((found nil) (done nil))
                (while (not done)
                  (let ((chain-val (ptr-read-u32 chain-base (* (- sym-index symoffset) 4))))
                    (when (= (logior chain-val 1) (logior h 1))
                      (when (nl-ffi-loader--symbol-name-matches-p dyn sym-index name)
                        (setq found sym-index) (setq done t)))
                    (when (and (not done) (/= (logand chain-val 1) 0))
                      (setq done t))
                    (setq sym-index (1+ sym-index))))
                found))))))))

(defun nl-ffi-loader--elf-hash (name)
  "The classic SysV ELF hash function over NAME's raw bytes."
  (let ((h 0) (i 0) (n (length name)))
    (while (< i n)
      (setq h (logand (+ (ash h 4) (aref name i)) #xffffffff))
      (let ((g (logand h #xf0000000)))
        (when (/= g 0) (setq h (logxor h (ash g -24))))
        (setq h (logand h (lognot g))))
      (setq i (1+ i)))
    h))

(defun nl-ffi-loader--lookup-sysv-hash (dyn name)
  "Resolve NAME via DYN's classic `.hash'.  Returns a symtab index, or nil."
  (let* ((base (plist-get dyn :sysv-hash))
         (nbucket (ptr-read-u32 base 0))
         (buckets-base (+ base 8))
         (chain-base (+ buckets-base (* nbucket 4)))
         (i (ptr-read-u32 buckets-base (* (mod (nl-ffi-loader--elf-hash name) nbucket) 4))))
    (let ((found nil))
      (while (and (> i 0) (not found))
        (if (nl-ffi-loader--symbol-name-matches-p dyn i name)
            (setq found i)
          (setq i (ptr-read-u32 chain-base (* i 4)))))
      found)))

(defun nl-ffi-loader--lookup-linear-scan (dyn name)
  "Resolve NAME by scanning every `.dynsym' entry between `.symtab' and
`.strtab'.  Only reached when the object has neither `.gnu.hash' nor
`.hash' -- see this file's Commentary for why the symbol count this
derives is a fragile, last-resort guess rather than a real field."
  (let* ((syment (plist-get dyn :syment))
         (nsyms (max 0 (/ (- (plist-get dyn :strtab) (plist-get dyn :symtab)) syment)))
         (i 1) (found nil)) ; index 0 is always STN_UNDEF
    (while (and (< i nsyms) (not found))
      (when (nl-ffi-loader--symbol-name-matches-p dyn i name)
        (setq found i))
      (setq i (1+ i)))
    found))

(defun nl-ffi-loader-symbol (handle name)
  "Resolve NAME (a string) to its runtime address within HANDLE.
HANDLE is a value `nl-ffi-loader-open' returned.  Returns the resolved
address, or 0 -- the same \"not found\" sentinel a real `dlsym' would
return, and what `nl-ffi--resolve-via-dlsym' (nl-ffi.el) already expects
-- never nil."
  (let* ((dyn (plist-get handle :dyn))
         (bias (plist-get handle :bias))
         (index
          (cond
           ((plist-get dyn :gnu-hash) (nl-ffi-loader--lookup-gnu-hash dyn name))
           ((plist-get dyn :sysv-hash) (nl-ffi-loader--lookup-sysv-hash dyn name))
           (t (nl-ffi-loader--lookup-linear-scan dyn name)))))
    (if (not index)
        0
      (+ bias (car (nl-ffi-loader--dynsym-value-and-shndx dyn index))))))

;;;; --- public entry point ------------------------------------------------------

(defconst nl-ffi-loader--magic 'nl-ffi-loader)

(defun nl-ffi-loader-handle-p (handle)
  "Return non-nil when HANDLE was produced by `nl-ffi-loader-open'.
Used by nl-ffi.el (`nl-ffi--resolve-via-dlsym') to tell a loader object
apart from a real, positive-integer `dlopen' handle before deciding
whether to resolve a symbol through `nl-ffi-loader-symbol' or `dlsym'."
  (and (consp handle) (eq (plist-get handle :nl-ffi-loader-magic)
                           nl-ffi-loader--magic)))

(defun nl-ffi-loader--open-1 (path file-base)
  "Do the real work of `nl-ffi-loader-open' once PATH's file is mapped."
  (nl-ffi-loader--check-elf-header file-base path)
  (let* ((ph (nl-ffi-loader--program-headers file-base))
         (loads (plist-get ph :loads)))
    (unless loads
      (signal 'nl-ffi-loader-unsupported (list :no-load-segments path)))
    (when (plist-get ph :tls-p)
      (signal 'nl-ffi-loader-unsupported (list :tls-segment path)))
    (unless (plist-get ph :dyn-vaddr)
      (signal 'nl-ffi-loader-unsupported (list :no-dynamic-section path)))
    (let* ((reservation (nl-ffi-loader--reserve loads))
           (bias (car reservation)) (span (cdr reservation)))
      (nl-ffi-loader--map-and-copy-segments bias file-base loads)
      (let ((dyn (nl-ffi-loader--parse-dynamic bias (plist-get ph :dyn-vaddr))))
        (when (plist-get dyn :needed-p)
          (signal 'nl-ffi-loader-unsupported (list :needs-dependency path)))
        (when (plist-get dyn :init-p)
          (signal 'nl-ffi-loader-unsupported (list :has-initializers path)))
        (when (plist-get dyn :relr-p)
          (signal 'nl-ffi-loader-unsupported (list :relr-relocations path)))
        (unless (or (plist-get dyn :symtab) (plist-get dyn :strtab))
          (signal 'nl-ffi-loader-unsupported (list :no-dynamic-symbols path)))
        (nl-ffi-loader--apply-relocations path bias dyn)
        (nl-ffi-loader--protect-segments bias loads)
        (list :nl-ffi-loader-magic nl-ffi-loader--magic
              :path path :bias bias :reserved-size span :dyn dyn)))))

(defun nl-ffi-loader-open (path)
  "Map, relocate, and return a handle for the shared object at PATH.
PATH is used exactly as given -- opened with `openat(AT_FDCWD, PATH,
O_RDONLY)' -- unlike a real `dlopen', this loader does no SONAME
search-path resolution (`LD_LIBRARY_PATH', `/etc/ld.so.cache', the
standard library directories): give it a real, openable path.

Returns an opaque handle (`nl-ffi-loader-handle-p' recognizes it) that
`nl-ffi-loader-symbol' resolves names against.  Signals
`nl-ffi-loader-open-failed' for any I/O/mapping failure, and
`nl-ffi-loader-unsupported' for anything out of this increment's scope
-- see this file's Commentary."
  (let* ((fm (nl-ffi-loader--map-file-readonly path))
         (file-base (car fm)) (file-size (cdr fm)))
    (unwind-protect
        (nl-ffi-loader--open-1 path file-base)
      (syscall-direct nl-ffi-loader--sys-munmap file-base file-size 0 0 0 0))))

(provide 'nl-ffi-loader)

;;; nl-ffi-loader.el ends here
