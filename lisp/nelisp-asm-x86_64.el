;;; nelisp-asm-x86_64.el --- x86_64 macro assembler (Doc 92 §92.a)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 92 §92.a — freestanding pure-elisp x86_64 macro assembler for
;; the Phase 47 AOT compile chain.
;;
;; This is intentionally separate from `src/nelisp-cc-x86_64.el' (=
;; the Phase 7.x JIT backend with cl-defstruct + reversed-list byte
;; accumulator + SSA register allocator hooks).  The JIT side patches
;; bytes into a mmap'd page; the AOT side builds a byte buffer that
;; is later handed to Doc 91 ELF writer / Doc 93 static linker.  The
;; two use cases share no live state — keeping them physically split
;; avoids namespace pollution and makes the §92.a spike self-
;; contained.
;;
;; Buffer abstraction (= §92.a (1) per Doc 92 §3):
;;
;;   (nelisp-asm-x86_64-make-buffer)          ; -> opaque state
;;   (nelisp-asm-x86_64-buffer-bytes BUF)     ; -> unibyte-string
;;   (nelisp-asm-x86_64-buffer-pos   BUF)     ; -> integer
;;   (nelisp-asm-x86_64-define-label BUF NM)
;;   (nelisp-asm-x86_64-emit-fixup   BUF SLOT-OFFSET LABEL)
;;   (nelisp-asm-x86_64-resolve-fixups BUF)   ; -> patched unibyte-string
;;   (nelisp-asm-x86_64-emit-reloc   BUF TYPE SYM &optional ADDEND)
;;
;; State shape: `(:bytes "..." :pos N :labels ((NAME . POS) ...)
;;               :fixups ((SLOT . LABEL) ...) :relocs (RELOC ...))'
;; — a plist held in a single-element vector so mutators can
;; rebind the bytes string without losing identity.
;;
;; Instruction emitters (= §92.a (5)):
;;
;;   mov-imm64   mov-reg-reg   mov-imm32
;;   add-reg-reg add-imm32     sub-reg-reg cmp-imm32
;;   push        pop           syscall     ret
;;   call-rel32  jmp-rel32     nop         int3   hlt
;;
;; Relocation marker API (= §92.a (6)) records `:pc32' / `:abs64' /
;; `:plt32' entries for Doc 93 linker handoff — placeholder bytes
;; (4 or 8 zero) are emitted at the recorded offset.
;;
;; §92.c — formalized reloc shape + Doc 93 handshake helpers.  Each
;; recorded entry is a plist of canonical shape
;;
;;   (:offset N :type TYPE :symbol NAME :addend A :section SECTION)
;;
;; matching `nelisp-link-apply-reloc' (= Doc 93 §93.a) consumer
;; expectations.  The legacy §92.a `:sym' key is kept as an alias in
;; the stored plist (= §93.a accepts both) so old callers do not
;; break.  SECTION defaults to `text' but can be overridden via
;; `nelisp-asm-x86_64-emit-reloc' `:section' keyword for §93.b's
;; multi-section linking.  Three higher-level helpers stamp the
;; placeholder + record the reloc in one call:
;;
;;   (nelisp-asm-x86_64-reloc-pc32-here  BUF SYM ADDEND)
;;   (nelisp-asm-x86_64-reloc-abs64-here BUF SYM ADDEND)
;;   (nelisp-asm-x86_64-reloc-plt32-here BUF SYM ADDEND)
;;
;; Buffer-to-linker bridge:
;;
;;   (nelisp-asm-x86_64-extract-relocs BUF)
;;   (nelisp-asm-x86_64-buffer-to-unit BUF NAME &rest SECTION-EXTRAS)
;;
;; — `extract-relocs' returns the canonical §93-compatible plist
;; list; `buffer-to-unit' assembles a `nelisp-link-unit-make' shape
;; for direct hand-off to Doc 93 §93.b's combine/2pass pipeline.
;;
;; Not wired into baker — freestanding spike per Doc 92 §0.2 + §8.1.

;;; Code:

(require 'cl-lib)

(define-error 'nelisp-asm-x86_64-error
  "nelisp-asm-x86_64 invariant violated")

;; ---- register table (= §92.a (2)) ----

(defconst nelisp-asm-x86_64--reg
  '((rax . 0) (rcx . 1) (rdx . 2) (rbx . 3)
    (rsp . 4) (rbp . 5) (rsi . 6) (rdi . 7)
    (r8  . 8) (r9  . 9) (r10 . 10) (r11 . 11)
    (r12 . 12) (r13 . 13) (r14 . 14) (r15 . 15))
  "x86_64 64-bit GPR encoding map.
Each cell is `(NAME . N)' where N is the 4-bit register number
used by the AMD64 encoding: low 3 bits land in ModR/M.reg /
ModR/M.rm / opcode +rd; bit 3 lights up the matching REX.R /
REX.B / REX.X extension.  Covers only integer GPRs — xmm/ymm/
flags are out of scope per Doc 92 OOS list.")

(defun nelisp-asm-x86_64--reg-num (reg)
  "Return the 4-bit register number for REG.
Signals `nelisp-asm-x86_64-error' if REG is unknown."
  (let ((cell (assq reg nelisp-asm-x86_64--reg)))
    (unless cell
      (signal 'nelisp-asm-x86_64-error
              (list :unknown-register reg)))
    (cdr cell)))

(defsubst nelisp-asm-x86_64--reg-low3 (reg)
  "Return REG's low 3 bits (= ModR/M.reg / .rm / opcode +rd field)."
  (logand (nelisp-asm-x86_64--reg-num reg) 7))

(defsubst nelisp-asm-x86_64--reg-ext (reg)
  "Return 1 when REG is r8-r15 (= REX.R / REX.B set), else 0."
  (if (>= (nelisp-asm-x86_64--reg-num reg) 8) 1 0))

;; ---- REX prefix + ModR/M (= §92.a (3) + (4)) ----

(defsubst nelisp-asm-x86_64--rex (w r x b)
  "Compose a REX prefix byte from bits W R X B (each 0 or 1).
Returns 0x40 | (W<<3) | (R<<2) | (X<<1) | B.  Always returns a
valid byte — elision (= drop the prefix when WRXB = 0000) is the
caller's responsibility."
  (logior #x40 (ash w 3) (ash r 2) (ash x 1) b))

(defsubst nelisp-asm-x86_64--modrm (mod reg rm)
  "Compose a ModR/M byte from MOD (2 bits), REG (3 bits), RM (3 bits)."
  (logior (ash mod 6) (ash (logand reg 7) 3) (logand rm 7)))

(defun nelisp-asm-x86_64--imm32-bytes (imm)
  "Encode IMM as a 4-byte little-endian unibyte-string (low byte first).
Accepts signed [-2^31, 2^31-1] or unsigned [0, 2^32-1]; out-of-
range signals `nelisp-asm-x86_64-error'."
  (unless (integerp imm)
    (signal 'nelisp-asm-x86_64-error (list :imm-not-integer imm)))
  (when (or (< imm (- (ash 1 31))) (>= imm (ash 1 32)))
    (signal 'nelisp-asm-x86_64-error (list :imm32-out-of-range imm)))
  (let ((u (logand imm #xFFFFFFFF)))
    (unibyte-string (logand u #xFF)
                    (logand (ash u  -8) #xFF)
                    (logand (ash u -16) #xFF)
                    (logand (ash u -24) #xFF))))

(defun nelisp-asm-x86_64--imm64-bytes (imm)
  "Encode IMM as an 8-byte little-endian unibyte-string.
Accepts signed [-2^63, 2^63-1] or unsigned [0, 2^64-1].  Bignum-
safe — shifts one byte at a time."
  (unless (integerp imm)
    (signal 'nelisp-asm-x86_64-error (list :imm-not-integer imm)))
  (let ((bytes (make-vector 8 0))
        (i 0))
    (while (< i 8)
      (aset bytes i (logand (ash imm (- (* i 8))) #xFF))
      (setq i (1+ i)))
    (apply #'unibyte-string (append bytes nil))))

;; ---- buffer abstraction (= §92.a (1)) ----
;;
;; The buffer is a single-cell vector wrapping a plist.  This gives
;; us reference semantics without cl-defstruct's accessor cost — the
;; spike API surface is small enough to keep direct plist access.

(defsubst nelisp-asm-x86_64--unwrap (buf)
  "Return the plist held inside BUF (= buffer state vector)."
  (aref buf 0))

(defsubst nelisp-asm-x86_64--rewrap (buf plist)
  "Replace BUF's backing plist with PLIST.  Mutates BUF in place."
  (aset buf 0 plist)
  buf)

(defun nelisp-asm-x86_64-make-buffer ()
  "Return a fresh empty x86_64 assembler buffer.
The buffer is opaque; use the accessors below to inspect or
extend it."
  (vector (list :bytes "" :pos 0 :labels nil
                :fixups nil :relocs nil)))

(defun nelisp-asm-x86_64-buffer-bytes (buf)
  "Return BUF's accumulated bytes as a unibyte-string.
The string is not patched — call `nelisp-asm-x86_64-resolve-
fixups' first if any `emit-fixup' entries are pending."
  (plist-get (nelisp-asm-x86_64--unwrap buf) :bytes))

(defun nelisp-asm-x86_64-buffer-pos (buf)
  "Return BUF's current byte offset (= number of bytes written)."
  (plist-get (nelisp-asm-x86_64--unwrap buf) :pos))

(defun nelisp-asm-x86_64-buffer-labels (buf)
  "Return BUF's labels alist `((NAME . POS) ...)' (reverse-defn order)."
  (plist-get (nelisp-asm-x86_64--unwrap buf) :labels))

(defun nelisp-asm-x86_64-buffer-fixups (buf)
  "Return BUF's pending fixups alist `((SLOT-OFFSET . LABEL) ...)'."
  (plist-get (nelisp-asm-x86_64--unwrap buf) :fixups))

(defun nelisp-asm-x86_64-buffer-relocs (buf)
  "Return BUF's pending relocations as a list of plists.
Each entry is `(:type TYPE :sym SYM :offset OFFSET :addend N)' —
order matches emit order, suitable for Doc 93 linker handoff."
  (plist-get (nelisp-asm-x86_64--unwrap buf) :relocs))

(defun nelisp-asm-x86_64--append-bytes (buf bs)
  "Append unibyte-string BS to BUF's byte stream and advance pos.
Internal mutator — call sites are the per-instruction emitters."
  (let* ((plist (nelisp-asm-x86_64--unwrap buf))
         (old (plist-get plist :bytes))
         (pos (plist-get plist :pos))
         (new (concat old bs)))
    (setq plist (plist-put plist :bytes new))
    (setq plist (plist-put plist :pos (+ pos (length bs))))
    (nelisp-asm-x86_64--rewrap buf plist)))

(defun nelisp-asm-x86_64-define-label (buf name)
  "Mark NAME as resolved at BUF's current byte position.
Signals `nelisp-asm-x86_64-error' on duplicate label — silent
shadow would mask codegen bugs."
  (let* ((plist (nelisp-asm-x86_64--unwrap buf))
         (labels (plist-get plist :labels)))
    (when (assq name labels)
      (signal 'nelisp-asm-x86_64-error
              (list :duplicate-label name)))
    (setq plist (plist-put plist :labels
                           (cons (cons name (plist-get plist :pos))
                                 labels)))
    (nelisp-asm-x86_64--rewrap buf plist)))

(defun nelisp-asm-x86_64-emit-fixup (buf slot-offset label)
  "Record a 4-byte rel32 fixup at SLOT-OFFSET against LABEL.
SLOT-OFFSET is the absolute byte position in BUF where the 4-
byte little-endian displacement begins.  Resolution computes
`(label-pos - (slot-offset + 4))' at finalize time.  This helper
records only — the placeholder bytes themselves must already
have been emitted by the caller (= `call-rel32' / `jmp-rel32'
write 4 zero bytes before recording the fixup)."
  (let* ((plist (nelisp-asm-x86_64--unwrap buf))
         (fixups (plist-get plist :fixups)))
    (setq plist (plist-put plist :fixups
                           (cons (cons slot-offset label) fixups)))
    (nelisp-asm-x86_64--rewrap buf plist)))

(defun nelisp-asm-x86_64-emit-reloc (buf type sym &optional addend
                                         &rest keyword-args)
  "Record a pending relocation entry against external symbol SYM.
TYPE is one of `'pc32' / `'abs64' / `'plt32' — names match
R_X86_64_PC32 / R_X86_64_64 / R_X86_64_PLT32 from the ELF64
psABI.  This helper records only; the caller is responsible for
emitting the placeholder bytes (4 zeros for pc32/plt32, 8 zeros
for abs64) at the recorded offset.  ADDEND defaults to 0 (=
matches ELF64 r_addend); negative addends are allowed (e.g. -4
for PC-relative call sites).

KEYWORD-ARGS accepts `:section SECTION-SYM' (= one of `text' /
`rodata' / `data', default `text') to tag which section the patch
site lives in.  Section tagging is required by §93.b's multi-
section reloc dispatch — the §92.a default of `text' covers the
common path so existing callers keep working."
  (unless (memq type '(pc32 abs64 plt32))
    (signal 'nelisp-asm-x86_64-error
            (list :unknown-reloc-type type)))
  (let* ((section (or (plist-get keyword-args :section) 'text))
         (plist (nelisp-asm-x86_64--unwrap buf))
         (offset (plist-get plist :pos))
         (relocs (plist-get plist :relocs))
         (entry (list :offset offset
                      :type type
                      :symbol sym
                      :sym sym
                      :addend (or addend 0)
                      :section section)))
    (setq plist (plist-put plist :relocs (append relocs (list entry))))
    (nelisp-asm-x86_64--rewrap buf plist)))

(defun nelisp-asm-x86_64-extract-relocs (buf)
  "Return BUF's relocs in Doc 93 §93.a-compatible plist form.
Each entry is `(:offset N :type TYPE :symbol NAME :addend A
:section SEC)' — the canonical shape consumed by
`nelisp-link-apply-reloc'.  Emission order is preserved.  The
returned list is a fresh shallow copy of each entry with internal-
only fields elided (= currently the `:sym' alias is dropped, the
public `:symbol' key is kept)."
  (mapcar (lambda (r)
            (list :offset (plist-get r :offset)
                  :type (plist-get r :type)
                  :symbol (or (plist-get r :symbol)
                              (plist-get r :sym))
                  :addend (or (plist-get r :addend) 0)
                  :section (or (plist-get r :section) 'text)))
          (plist-get (nelisp-asm-x86_64--unwrap buf) :relocs)))

(defun nelisp-asm-x86_64-reloc-pc32-here (buf sym-name addend
                                              &optional section)
  "Emit 4 zero bytes + record a `pc32' reloc at the placeholder.
BUF is mutated; SYM-NAME and ADDEND are forwarded to
`nelisp-asm-x86_64-emit-reloc'.  SECTION defaults to `text'.
Returns the offset where the placeholder begins (= caller
bookkeeping for `(call sym)' / `(lea reg, [rip+sym])').  Typical
usage:

  (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
  (nelisp-asm-x86_64-reloc-pc32-here buf \"main\" -4)"
  (let ((offset (nelisp-asm-x86_64-buffer-pos buf)))
    (nelisp-asm-x86_64--append-bytes buf (unibyte-string 0 0 0 0))
    (nelisp-asm-x86_64-emit-reloc buf 'pc32 sym-name addend
                                  :section (or section 'text))
    ;; The reloc was recorded at the *new* pos (= post-append) so
    ;; rewrite the last entry's `:offset' to the placeholder start.
    (let* ((plist (nelisp-asm-x86_64--unwrap buf))
           (relocs (plist-get plist :relocs))
           (last (car (last relocs))))
      (setq last (plist-put last :offset offset))
      (setq plist (plist-put plist :relocs
                             (append (butlast relocs) (list last))))
      (nelisp-asm-x86_64--rewrap buf plist))
    offset))

(defun nelisp-asm-x86_64-reloc-abs64-here (buf sym-name addend
                                               &optional section)
  "Emit 8 zero bytes + record an `abs64' reloc at the placeholder.
BUF is mutated; SYM-NAME and ADDEND are forwarded to
`nelisp-asm-x86_64-emit-reloc'.  SECTION defaults to `text'.
Returns the offset where the placeholder begins."
  (let ((offset (nelisp-asm-x86_64-buffer-pos buf)))
    (nelisp-asm-x86_64--append-bytes
     buf (unibyte-string 0 0 0 0 0 0 0 0))
    (nelisp-asm-x86_64-emit-reloc buf 'abs64 sym-name addend
                                  :section (or section 'text))
    (let* ((plist (nelisp-asm-x86_64--unwrap buf))
           (relocs (plist-get plist :relocs))
           (last (car (last relocs))))
      (setq last (plist-put last :offset offset))
      (setq plist (plist-put plist :relocs
                             (append (butlast relocs) (list last))))
      (nelisp-asm-x86_64--rewrap buf plist))
    offset))

(defun nelisp-asm-x86_64-reloc-plt32-here (buf sym-name addend
                                               &optional section)
  "Emit 4 zero bytes + record a `plt32' reloc at the placeholder.
Semantically identical to `nelisp-asm-x86_64-reloc-pc32-here' for
ET_EXEC static linking (= no PLT trampoline needed) but emits
R_X86_64_PLT32 so the linker can distinguish PIC-aware call
sites.  SECTION defaults to `text'.  Returns the placeholder
offset."
  (let ((offset (nelisp-asm-x86_64-buffer-pos buf)))
    (nelisp-asm-x86_64--append-bytes buf (unibyte-string 0 0 0 0))
    (nelisp-asm-x86_64-emit-reloc buf 'plt32 sym-name addend
                                  :section (or section 'text))
    (let* ((plist (nelisp-asm-x86_64--unwrap buf))
           (relocs (plist-get plist :relocs))
           (last (car (last relocs))))
      (setq last (plist-put last :offset offset))
      (setq plist (plist-put plist :relocs
                             (append (butlast relocs) (list last))))
      (nelisp-asm-x86_64--rewrap buf plist))
    offset))

(defun nelisp-asm-x86_64-buffer-to-unit (buf name &rest section-extras)
  "Bundle BUF as a Doc 93 §93.b compile-unit plist named NAME.
The returned plist has shape
`(:name NAME :sections ((text . BYTES) [...]) :symbols SYMS
  :relocs RELOCS)' suitable for `nelisp-link-unit-make' / direct
input into `nelisp-link-combine-sections'.

The buffer's labels become global function symbols whose `:value'
is the byte offset within `text' (= caller resolves to a VA via
the linker's 2-pass).  RELOCS come from
`nelisp-asm-x86_64-extract-relocs'.  SECTION-EXTRAS is an
optional plist tail of extra section payloads, e.g.
`(:rodata BYTES :data BYTES)' — each `(KEY . VALUE)' pair lands
in the `:sections' alist with KEY's leading colon stripped (so
`:rodata' becomes `rodata').  `:bss' may carry an integer size
instead of a byte string (= matches §93.b's bss convention)."
  (let* ((text-bytes (nelisp-asm-x86_64-buffer-bytes buf))
         (sections (list (cons 'text text-bytes)))
         (labels (nelisp-asm-x86_64-buffer-labels buf))
         (symbols nil)
         (relocs (nelisp-asm-x86_64-extract-relocs buf)))
    ;; Walk SECTION-EXTRAS as a keyword plist and append to sections.
    (let ((tail section-extras))
      (while tail
        (let* ((k (car tail))
               (v (cadr tail))
               (sym (intern (substring (symbol-name k) 1))))
          (push (cons sym v) sections)
          (setq tail (cddr tail)))))
    ;; Each label -> a global func symbol; convert symbol-names to
    ;; strings (= Doc 93 §93.b symbol-table keys are strings).
    (dolist (cell labels)
      (let ((label-name (car cell))
            (label-pos  (cdr cell)))
        (push (list :name (if (stringp label-name)
                              label-name
                            (symbol-name label-name))
                    :value label-pos
                    :size 0
                    :section 'text
                    :bind 'global
                    :type 'func)
              symbols)))
    (list :name name
          :sections (nreverse sections)
          :symbols (nreverse symbols)
          :relocs relocs)))

(defun nelisp-asm-x86_64-emit-bytes (buf bs)
  "Append unibyte-string BS verbatim to BUF.
Public alias of the internal byte-appender — needed because the
`reloc-*-here' helpers' docstring example uses an `emit-bytes'
spelling.  Returns BUF for chaining."
  (nelisp-asm-x86_64--append-bytes buf bs))

(defun nelisp-asm-x86_64-resolve-fixups (buf)
  "Apply every pending fixup in BUF, returning the patched bytes.
Each fixup `(SLOT-OFFSET . LABEL)' is resolved to
`(label-pos - (slot-offset + 4))' and written little-endian into
the 4-byte slot at SLOT-OFFSET.  Signals
`nelisp-asm-x86_64-error' on a fixup whose LABEL was never
defined — silent zero-fill would jump to itself and the bug
would not surface until execution.  Returns the patched
unibyte-string; BUF is mutated in place."
  (let* ((plist  (nelisp-asm-x86_64--unwrap buf))
         (bytes  (plist-get plist :bytes))
         (labels (plist-get plist :labels))
         (fixups (plist-get plist :fixups))
         ;; Build mutable vector so we can aset.
         (n (length bytes))
         (vec (make-vector n 0))
         (i 0))
    (while (< i n)
      (aset vec i (aref bytes i))
      (setq i (1+ i)))
    (dolist (fix fixups)
      (let* ((slot (car fix))
             (label (cdr fix))
             (cell (assq label labels)))
        (unless cell
          (signal 'nelisp-asm-x86_64-error
                  (list :unresolved-label label :at-slot slot)))
        (let* ((rel32 (- (cdr cell) (+ slot 4)))
               (u (logand rel32 #xFFFFFFFF)))
          (aset vec    slot      (logand u #xFF))
          (aset vec (+ slot 1)   (logand (ash u  -8) #xFF))
          (aset vec (+ slot 2)   (logand (ash u -16) #xFF))
          (aset vec (+ slot 3)   (logand (ash u -24) #xFF)))))
    (let ((patched (apply #'unibyte-string (append vec nil))))
      (setq plist (plist-put plist :bytes patched))
      (nelisp-asm-x86_64--rewrap buf plist)
      patched)))

;; ---- instruction emitters (= §92.a (5)) ----
;;
;; All emit-* helpers MUTATE BUF and return BUF (= chainable).

(defun nelisp-asm-x86_64-mov-imm64 (buf reg imm)
  "Emit `MOV REG, IMM64' = REX.W + 0xB8+rd + imm64 (10 bytes total)."
  (let* ((rex (nelisp-asm-x86_64--rex 1 0 0
                                      (nelisp-asm-x86_64--reg-ext reg)))
         (op  (+ #xB8 (nelisp-asm-x86_64--reg-low3 reg))))
    (nelisp-asm-x86_64--append-bytes
     buf (concat (unibyte-string rex op)
                 (nelisp-asm-x86_64--imm64-bytes imm)))))

(defun nelisp-asm-x86_64-mov-imm32 (buf reg imm)
  "Emit `MOV REG, IMM32' = REX.W + 0xC7 /0 + imm32 (7 bytes).
IMM is sign-extended to 64 bits by the CPU."
  (let* ((rex (nelisp-asm-x86_64--rex 1 0 0
                                      (nelisp-asm-x86_64--reg-ext reg)))
         (modrm (nelisp-asm-x86_64--modrm 3 0
                                          (nelisp-asm-x86_64--reg-low3 reg))))
    (nelisp-asm-x86_64--append-bytes
     buf (concat (unibyte-string rex #xC7 modrm)
                 (nelisp-asm-x86_64--imm32-bytes imm)))))

(defun nelisp-asm-x86_64--emit-mr (buf opcode dst src)
  "Internal: emit a 64-bit MR-form binop = REX.W + OPCODE + ModR/M.
DST is the ModR/M.rm operand, SRC is the ModR/M.reg operand."
  (let* ((rex (nelisp-asm-x86_64--rex
               1
               (nelisp-asm-x86_64--reg-ext src)
               0
               (nelisp-asm-x86_64--reg-ext dst)))
         (modrm (nelisp-asm-x86_64--modrm
                 3
                 (nelisp-asm-x86_64--reg-low3 src)
                 (nelisp-asm-x86_64--reg-low3 dst))))
    (nelisp-asm-x86_64--append-bytes
     buf (unibyte-string rex opcode modrm))))

(defun nelisp-asm-x86_64-mov-reg-reg (buf dst src)
  "Emit `MOV DST, SRC' (MR form, 64-bit, opcode 0x89)."
  (nelisp-asm-x86_64--emit-mr buf #x89 dst src))

(defun nelisp-asm-x86_64-add-reg-reg (buf dst src)
  "Emit `ADD DST, SRC' (MR form, 64-bit, opcode 0x01)."
  (nelisp-asm-x86_64--emit-mr buf #x01 dst src))

(defun nelisp-asm-x86_64-sub-reg-reg (buf dst src)
  "Emit `SUB DST, SRC' (MR form, 64-bit, opcode 0x29)."
  (nelisp-asm-x86_64--emit-mr buf #x29 dst src))

(defun nelisp-asm-x86_64--emit-imm32-binop (buf reg imm subop)
  "Internal: emit 64-bit `OP REG, IMM32' (opcode 0x81 with /SUBOP).
SUBOP selects the operation: 0=ADD, 5=SUB, 7=CMP, 1=OR, 4=AND."
  (let* ((rex (nelisp-asm-x86_64--rex 1 0 0
                                      (nelisp-asm-x86_64--reg-ext reg)))
         (modrm (nelisp-asm-x86_64--modrm 3 subop
                                          (nelisp-asm-x86_64--reg-low3 reg))))
    (nelisp-asm-x86_64--append-bytes
     buf (concat (unibyte-string rex #x81 modrm)
                 (nelisp-asm-x86_64--imm32-bytes imm)))))

(defun nelisp-asm-x86_64-add-imm32 (buf reg imm)
  "Emit `ADD REG, IMM32' = REX.W + 0x81 /0 + imm32 (7 bytes)."
  (nelisp-asm-x86_64--emit-imm32-binop buf reg imm 0))

(defun nelisp-asm-x86_64-sub-imm32 (buf reg imm)
  "Emit `SUB REG, IMM32' = REX.W + 0x81 /5 + imm32 (7 bytes)."
  (nelisp-asm-x86_64--emit-imm32-binop buf reg imm 5))

(defun nelisp-asm-x86_64-cmp-imm32 (buf reg imm)
  "Emit `CMP REG, IMM32' = REX.W + 0x81 /7 + imm32 (7 bytes)."
  (nelisp-asm-x86_64--emit-imm32-binop buf reg imm 7))

(defun nelisp-asm-x86_64-push (buf reg)
  "Emit `PUSH REG' = [REX.B] + 0x50+rd (1 or 2 bytes)."
  (let ((ext (nelisp-asm-x86_64--reg-ext reg))
        (low (nelisp-asm-x86_64--reg-low3 reg)))
    (if (zerop ext)
        (nelisp-asm-x86_64--append-bytes
         buf (unibyte-string (+ #x50 low)))
      (nelisp-asm-x86_64--append-bytes
       buf (unibyte-string (nelisp-asm-x86_64--rex 0 0 0 1)
                           (+ #x50 low))))))

(defun nelisp-asm-x86_64-pop (buf reg)
  "Emit `POP REG' = [REX.B] + 0x58+rd (1 or 2 bytes)."
  (let ((ext (nelisp-asm-x86_64--reg-ext reg))
        (low (nelisp-asm-x86_64--reg-low3 reg)))
    (if (zerop ext)
        (nelisp-asm-x86_64--append-bytes
         buf (unibyte-string (+ #x58 low)))
      (nelisp-asm-x86_64--append-bytes
       buf (unibyte-string (nelisp-asm-x86_64--rex 0 0 0 1)
                           (+ #x58 low))))))

(defun nelisp-asm-x86_64-syscall (buf)
  "Emit `SYSCALL' (= 0x0F 0x05, 2 bytes)."
  (nelisp-asm-x86_64--append-bytes buf (unibyte-string #x0F #x05)))

(defun nelisp-asm-x86_64-ret (buf)
  "Emit `RET' (near return, = 0xC3, 1 byte)."
  (nelisp-asm-x86_64--append-bytes buf (unibyte-string #xC3)))

(defun nelisp-asm-x86_64-nop (buf)
  "Emit `NOP' (= 0x90, 1 byte)."
  (nelisp-asm-x86_64--append-bytes buf (unibyte-string #x90)))

(defun nelisp-asm-x86_64-int3 (buf)
  "Emit `INT3' (= 0xCC, 1 byte trap/breakpoint)."
  (nelisp-asm-x86_64--append-bytes buf (unibyte-string #xCC)))

(defun nelisp-asm-x86_64-hlt (buf)
  "Emit `HLT' (= 0xF4, 1 byte halt)."
  (nelisp-asm-x86_64--append-bytes buf (unibyte-string #xF4)))

(defun nelisp-asm-x86_64-call-rel32 (buf label)
  "Emit `CALL rel32' (opcode 0xE8) with a fixup against LABEL.
Writes 0xE8 + 4 zero placeholder bytes (5 bytes total), then
records a fixup at the placeholder offset.  `resolve-fixups'
patches the rel32 = `(label-pos - (slot + 4))'."
  (let ((slot (1+ (nelisp-asm-x86_64-buffer-pos buf))))
    (nelisp-asm-x86_64--append-bytes
     buf (unibyte-string #xE8 0 0 0 0))
    (nelisp-asm-x86_64-emit-fixup buf slot label)))

(defun nelisp-asm-x86_64-jmp-rel32 (buf label)
  "Emit `JMP rel32' (opcode 0xE9) with a fixup against LABEL.
Writes 0xE9 + 4 zero placeholder bytes (5 bytes total), then
records a fixup at the placeholder offset."
  (let ((slot (1+ (nelisp-asm-x86_64-buffer-pos buf))))
    (nelisp-asm-x86_64--append-bytes
     buf (unibyte-string #xE9 0 0 0 0))
    (nelisp-asm-x86_64-emit-fixup buf slot label)))

(provide 'nelisp-asm-x86_64)

;;; nelisp-asm-x86_64.el ends here
