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
;; State shape (§92.d chunk-build, mirroring Doc 91 §91.d):
;;   `(:chunks (CHUNK_N ... CHUNK_2 CHUNK_1) :length N
;;     :labels ((NAME . POS) ...)
;;     :fixups ((SLOT . LABEL) ...) :relocs (RELOC ...))'
;; — a plist held in a single-element vector.  Internally each
;; emitter conses a fresh unibyte-string onto :chunks (= reverse
;; order, O(1) per write) and bumps :length; finalize via
;; `nelisp-asm-x86_64-buffer-bytes' joins the list with one
;; `(apply #'concat (nreverse ...))' call (= O(total-bytes)).  This
;; replaces the §92.a `(concat old new)` accumulator that was
;; quadratic for long buffers.  Public emitter signatures are
;; unchanged.
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

;; ---- Doc 110 §110.A xmm register table (= SSE2 / f64 ABI) ----
;;
;; Kept as a SEPARATE table from the GP `--reg' alist so that the
;; type discipline holds: passing an xmm symbol to a GP helper (or
;; vice versa) signals `:unknown-register' immediately instead of
;; emitting silently-wrong encoding bytes.  Encoding is identical to
;; the GP scheme — low 3 bits land in ModR/M.reg / .rm, bit 3 lights
;; up REX.R / REX.B.  REX.W is irrelevant for xmm ops (the xmm
;; register width is fixed at 128 bits regardless of REX.W).

(defconst nelisp-asm-x86_64--xmm-reg
  '((xmm0 . 0)  (xmm1 . 1)  (xmm2 . 2)  (xmm3 . 3)
    (xmm4 . 4)  (xmm5 . 5)  (xmm6 . 6)  (xmm7 . 7)
    (xmm8 . 8)  (xmm9 . 9)  (xmm10 . 10) (xmm11 . 11)
    (xmm12 . 12) (xmm13 . 13) (xmm14 . 14) (xmm15 . 15))
  "x86_64 SSE / xmm 128-bit register encoding map.
Doc 110 §110.A f64 ABI groundwork.  Each cell is `(NAME . N)' where
N is the 4-bit xmm register number: low 3 bits go in ModR/M.reg /
.rm, bit 3 in REX.R / REX.B.")

(defun nelisp-asm-x86_64--xmm-reg-num (reg)
  "Return the 4-bit xmm register number for REG.
Signals `nelisp-asm-x86_64-error' with key `:unknown-xmm-register'
when REG is not in `nelisp-asm-x86_64--xmm-reg'."
  (let ((cell (assq reg nelisp-asm-x86_64--xmm-reg)))
    (unless cell
      (signal 'nelisp-asm-x86_64-error
              (list :unknown-xmm-register reg)))
    (cdr cell)))

(defsubst nelisp-asm-x86_64--xmm-reg-low3 (reg)
  "Return xmm REG's low 3 bits (= ModR/M.reg / .rm field)."
  (logand (nelisp-asm-x86_64--xmm-reg-num reg) 7))

(defsubst nelisp-asm-x86_64--xmm-reg-ext (reg)
  "Return 1 when xmm REG is xmm8-xmm15 (= REX.R / REX.B set), else 0."
  (if (>= (nelisp-asm-x86_64--xmm-reg-num reg) 8) 1 0))

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
    (nelisp-asm-x86_64--byte-vec->string bytes)))

(defun nelisp-asm-x86_64--byte-vec->string (vec)
  "Convert byte VEC into a unibyte-string without a large-arity `apply'.
Standalone NeLisp currently mis-handles `(apply #'unibyte-string ...)'
once the arg list gets large, so emit the bytes in small chunks and
join them with `concat'."
  (let ((n (length vec))
        (i 0)
        (chunks nil))
    (while (< i n)
      (let ((limit (if (< (+ i 32) n) (+ i 32) n))
            (bytes nil))
        (while (< i limit)
          (setq bytes (cons (aref vec i) bytes))
          (setq i (1+ i)))
        (push (apply #'unibyte-string (nreverse bytes)) chunks)))
    (apply #'concat (nreverse chunks))))

;; ---- buffer abstraction (= §92.a (1)) ----
;;
;; Wave 19 — flat-vector buffer layout.  The buffer is a 7-slot
;; vector with one cell per field; `aref' / `aset' replace the legacy
;; plist-get / plist-put O(7) scan that dominated per-emit overhead in
;; the standalone NeLisp interpreter (= ~38% wall on trivial entry per
;; Wave 18 trace).  Slot order is fixed and documented below.
;;
;; Slot indices (= the canonical buffer layout):
;;
;;   0  CHUNKS        ; reverse-order list of unibyte-string chunks
;;   1  LENGTH        ; running cumulative byte count (= O(1) read)
;;   2  LABELS        ; alist ((NAME . POS) ...) reverse-defn order
;;   3  FIXUPS        ; alist ((SLOT-OFFSET . LABEL) ...) pending
;;   4  RELOCS        ; list of plists, emission order preserved
;;   5  ABI           ; 'sysv | 'win64 (immutable after make-buffer)
;;   6  EXTERN-NAMES  ; reserved for Wave 20+ extern-name dedup cache
;;
;; The `--unwrap' / `--rewrap' / `--slot-*' helpers below are
;; `defsubst' so the host-Emacs byte-compiler inlines them to bare
;; aref / aset.  The standalone NeLisp interpreter, where defsubst
;; inline does not fire, still benefits because each helper is a
;; single primitive call instead of a plist-get scan + plist-put copy.

(defconst nelisp-asm-x86_64--slot-chunks 0)
(defconst nelisp-asm-x86_64--slot-length 1)
(defconst nelisp-asm-x86_64--slot-labels 2)
(defconst nelisp-asm-x86_64--slot-fixups 3)
(defconst nelisp-asm-x86_64--slot-relocs 4)
(defconst nelisp-asm-x86_64--slot-abi    5)
(defconst nelisp-asm-x86_64--slot-extern-names 6)

(defsubst nelisp-asm-x86_64--get-chunks (buf) (aref buf 0))
(defsubst nelisp-asm-x86_64--set-chunks (buf v) (aset buf 0 v))
(defsubst nelisp-asm-x86_64--get-length (buf) (aref buf 1))
(defsubst nelisp-asm-x86_64--set-length (buf v) (aset buf 1 v))
(defsubst nelisp-asm-x86_64--get-labels (buf) (aref buf 2))
(defsubst nelisp-asm-x86_64--set-labels (buf v) (aset buf 2 v))
(defsubst nelisp-asm-x86_64--get-fixups (buf) (aref buf 3))
(defsubst nelisp-asm-x86_64--set-fixups (buf v) (aset buf 3 v))
(defsubst nelisp-asm-x86_64--get-relocs (buf) (aref buf 4))
(defsubst nelisp-asm-x86_64--set-relocs (buf v) (aset buf 4 v))
(defsubst nelisp-asm-x86_64--get-abi    (buf) (aref buf 5))

;; Legacy plist-shape compatibility shim.  `--unwrap' reconstructs the
;; old `(:chunks ... :length ... :labels ... :fixups ... :relocs ...
;; :abi ...)' plist on demand so any external caller that still pokes
;; at the inner shape keeps working.  `--rewrap' splats a plist back
;; into the slot vector.  Internal call sites all use the flat-vector
;; accessors above — these shims exist only to preserve back-compat
;; for code outside this file that might inspect the buffer.
(defun nelisp-asm-x86_64--unwrap (buf)
  "Return BUF's state as a legacy plist (back-compat helper).
Internal hot-path code uses `--get-*' / `--set-*' accessors directly
to avoid the plist materialization cost."
  (list :chunks (aref buf 0)
        :length (aref buf 1)
        :labels (aref buf 2)
        :fixups (aref buf 3)
        :relocs (aref buf 4)
        :abi    (aref buf 5)))

(defun nelisp-asm-x86_64--rewrap (buf plist)
  "Replace BUF's slots from PLIST (back-compat helper).  Mutates BUF."
  (aset buf 0 (plist-get plist :chunks))
  (aset buf 1 (plist-get plist :length))
  (aset buf 2 (plist-get plist :labels))
  (aset buf 3 (plist-get plist :fixups))
  (aset buf 4 (plist-get plist :relocs))
  (aset buf 5 (plist-get plist :abi))
  buf)

;; ---- ABI descriptor (= Doc 101 §101.B Wave 4 framework) ----
;;
;; Two ABI flavours are modelled:
;;
;;   'sysv   — System V AMD64 ABI (Linux / macOS).
;;             Integer args: RDI RSI RDX RCX R8 R9.
;;             Callee-saved: RBP RBX R12-R15.
;;             No shadow space.
;;
;;   'win64  — Microsoft x64 ABI (Windows x86_64).
;;             Integer args: RCX RDX R8 R9.
;;             Callee-saved: RBP RBX RDI RSI R12-R15 (XMM6-XMM15).
;;             Caller allocates 32-byte shadow space before CALL.
;;
;; Win64 integer argument remapping, caller shadow-space helpers, and
;; callee-saved XMM register metadata are operational.  The compiler
;; call sites choose which saves to materialise for each function body.

(defconst nelisp-asm-x86_64--abi-sysv-arg-regs
  '(rdi rsi rdx rcx r8 r9)
  "System V AMD64 ABI integer argument registers (positional 1..6).")

(defconst nelisp-asm-x86_64--abi-win64-arg-regs
  '(rcx rdx r8 r9)
  "Microsoft x64 ABI integer argument registers (positional 1..4).")

(defconst nelisp-asm-x86_64--abi-sysv-callee-saved
  '(rbp rbx r12 r13 r14 r15)
  "System V AMD64 ABI callee-saved integer registers.")

(defconst nelisp-asm-x86_64--abi-win64-callee-saved
  '(rbp rbx rdi rsi r12 r13 r14 r15)
  "Microsoft x64 ABI callee-saved integer registers; XMM list is separate.")

(defconst nelisp-asm-x86_64--abi-win64-xmm-callee-saved
  '(xmm6 xmm7 xmm8 xmm9 xmm10 xmm11 xmm12 xmm13 xmm14 xmm15)
  "Microsoft x64 ABI callee-saved XMM registers.")

(defun nelisp-asm-x86_64-abi-arg-regs (abi)
  "Return the integer argument register list for ABI ('sysv or 'win64).
Doc 101 §101.B Wave 5: both ABIs fully implemented.
  'sysv  — RDI RSI RDX RCX R8 R9 (System V AMD64)
  'win64 — RCX RDX R8 R9 (Microsoft x64)
Signals `nelisp-asm-x86_64-error' with :unknown-abi for any other value."
  (cond
   ((eq abi 'sysv)  nelisp-asm-x86_64--abi-sysv-arg-regs)
   ((eq abi 'win64) nelisp-asm-x86_64--abi-win64-arg-regs)
   (t
    (signal 'nelisp-asm-x86_64-error
            (list :unknown-abi abi)))))

(defun nelisp-asm-x86_64-abi-callee-saved (abi)
  "Return the callee-saved integer register list for ABI ('sysv or 'win64).
Doc 101 §101.B Wave 5: both ABIs fully implemented.
  'sysv  — RBP RBX R12-R15
  'win64 — RBP RBX RDI RSI R12-R15 (GP-only list)
Signals `nelisp-asm-x86_64-error' with :unknown-abi for any other value."
  (cond
   ((eq abi 'sysv)  nelisp-asm-x86_64--abi-sysv-callee-saved)
   ((eq abi 'win64) nelisp-asm-x86_64--abi-win64-callee-saved)
   (t
    (signal 'nelisp-asm-x86_64-error
            (list :unknown-abi abi)))))

(defun nelisp-asm-x86_64-abi-shadow-space (abi)
  "Return the shadow-space byte count that caller must reserve before CALL.
Doc 101 §101.B Wave 5:
  'sysv  → 0   (SysV has no shadow space requirement)
  'win64 → 32  (4 register home slots × 8 bytes each)
Signals `nelisp-asm-x86_64-error' with :unknown-abi for unknown ABI."
  (cond
   ((eq abi 'sysv)  0)
   ((eq abi 'win64) 32)
   (t (signal 'nelisp-asm-x86_64-error (list :unknown-abi abi)))))

(defun nelisp-asm-x86_64-make-buffer (&optional abi)
  "Return a fresh empty x86_64 assembler buffer.
Optional ABI argument selects the calling convention:
  'sysv  (default) — System V AMD64 (Linux / macOS)
  'win64            — Microsoft x64 (Windows x86_64)

The buffer is opaque; use the accessors below to inspect or
extend it.  §92.d chunk-build: slot 0 holds the reverse-order
list of unibyte-string chunks pushed by per-instruction emitters,
slot 1 tracks the running cumulative byte count (= O(1) read).

Wave 19 flat-vector layout: the buffer is a 7-slot vector — see
the `--slot-*' defconsts above for the canonical slot order.  The
prior plist-in-vector wrapper was replaced because plist-get /
plist-put dominated the per-emit cost on the standalone NeLisp
interpreter (= Wave 18 trace, ~38% wall on trivial entry).

Doc 101 §101.B Wave 5: both 'sysv and 'win64 are fully operational.
Slot 5 stores the ABI so emitters can gate Win64-specific codegen
(shadow space, arg register selection)."
  (let ((resolved-abi (or abi 'sysv)))
    (unless (memq resolved-abi '(sysv win64))
      (signal 'nelisp-asm-x86_64-error
              (list :unknown-abi resolved-abi)))
    ;; Wave 5: all ABI values accepted and fully operational.
    ;; Wave 19: flat 7-slot vector layout.
    ;;   [CHUNKS LENGTH LABELS FIXUPS RELOCS ABI EXTERN-NAMES]
    (vector nil 0 nil nil nil resolved-abi nil)))

(defun nelisp-asm-x86_64-buffer-bytes (buf)
  "Return BUF's accumulated bytes as a unibyte-string.
Finalizes the §92.d chunk-build accumulator via one
`(apply #\\='concat (nreverse ...))' call (= O(total-bytes) not
O(N²)).  The string is not patched — call
`nelisp-asm-x86_64-resolve-fixups' first if any `emit-fixup'
entries are pending."
  (apply #'concat (nreverse (copy-sequence (aref buf 0)))))

(defun nelisp-asm-x86_64-buffer-pos (buf)
  "Return BUF's current byte offset (= number of bytes written).
§92.d: read from slot 1 (= O(1))."
  (aref buf 1))

(defun nelisp-asm-x86_64-buffer-abi (buf)
  "Return BUF's ABI symbol ('sysv or 'win64).
Doc 101 §101.B Wave 4 accessor.  Returns 'sysv for buffers created
before the :abi field was introduced (= legacy nil → fallback)."
  (or (aref buf 5) 'sysv))

(defun nelisp-asm-x86_64-buffer-labels (buf)
  "Return BUF's labels alist `((NAME . POS) ...)' (reverse-defn order)."
  (aref buf 2))

(defun nelisp-asm-x86_64-buffer-fixups (buf)
  "Return BUF's pending fixups alist `((SLOT-OFFSET . LABEL) ...)'."
  (aref buf 3))

(defun nelisp-asm-x86_64-buffer-relocs (buf)
  "Return BUF's pending relocations as a list of plists.
Each entry is `(:type TYPE :sym SYM :offset OFFSET :addend N)' —
order matches emit order, suitable for Doc 93 linker handoff."
  (aref buf 4))

(defun nelisp-asm-x86_64--append-bytes (buf bs)
  "Append unibyte-string BS to BUF's byte stream and advance pos.
Internal mutator — call sites are the per-instruction emitters.
§92.d chunk-build: cons BS onto the chunks slot (= O(1) push) and
bump the length slot, instead of `(concat old bs)' which was O(N²)
for long buffers.  Wave 19: direct aref / aset on the flat-vector
layout replaces the plist-get / plist-put roundtrip."
  (aset buf 0 (cons bs (aref buf 0)))
  (aset buf 1 (+ (aref buf 1) (length bs)))
  buf)

;; ---- Wave 20 — hand-inline emit macros for hot prologue/epilogue path ----
;;
;; The standalone NeLisp interpreter does not inline `defsubst', so each
;; per-emit helper (`mov-reg-reg', `push', `pop', `mov-imm32', `ret',
;; `define-label', `sub-imm32', `mov-mem-reg-disp8') incurs the full
;; function-dispatch cost (~3s per call on Wave 19 baseline for spike-
;; noop, ~26s emit total for 8 calls).  Wave 18 trace + Wave 19-verify
;; pin this to the per-call dispatch + nested `--reg-ext' / `--reg-low3'
;; / `--rex' / `--modrm' chain that each invokes 4-8 sub-calls.
;;
;; These macros hand-expand to the helper body inline.  When the
;; register argument is a quoted-literal symbol the expansion looks up
;; the register number at macroexpand time and folds REX / ModR/M into
;; constant bytes.  Dynamic-register call-sites still work — they fall
;; back to the runtime helper.
;;
;; Byte-output invariant: each macro produces the same byte sequence
;; as its corresponding `defun' helper.  This is verified by md5sum on
;; spike-noop.o and fact-i64.o post-Wave-20.

(eval-and-compile
  (defun nelisp-asm-x86_64--quoted-reg-num (form)
    "If FORM is `(quote REG)' or a bare symbol REG known in `--reg', return
REG's encoding number (0-15).  Otherwise return nil (= caller must
emit a runtime fallback path)."
    (let ((sym (cond
                ((and (consp form) (eq (car form) 'quote) (symbolp (cadr form)))
                 (cadr form))
                ((symbolp form) form)
                (t nil))))
      (when sym
        (let ((cell (assq sym '((rax . 0) (rcx . 1) (rdx . 2) (rbx . 3)
                                (rsp . 4) (rbp . 5) (rsi . 6) (rdi . 7)
                                (r8  . 8) (r9  . 9) (r10 . 10) (r11 . 11)
                                (r12 . 12) (r13 . 13) (r14 . 14) (r15 . 15)))))
          (and cell (cdr cell)))))))

(defmacro nelisp-asm-x86_64--mov-reg-reg-inline (buf-form dst-form src-form)
  "Inline `MOV DST, SRC' (64-bit MR form, opcode 0x89, 3 bytes).
If DST-FORM / SRC-FORM are quoted literals, the REX + opcode + ModR/M
bytes are folded to constants at macroexpand time.  Dynamic args
fall back to `nelisp-asm-x86_64-mov-reg-reg' (= runtime helper).
Byte output identical to the runtime helper."
  (let ((dst-num (nelisp-asm-x86_64--quoted-reg-num dst-form))
        (src-num (nelisp-asm-x86_64--quoted-reg-num src-form)))
    (if (and dst-num src-num)
        (let* ((rex-b (if (>= dst-num 8) 1 0))
               (rex-r (if (>= src-num 8) 1 0))
               (rex (logior #x48 (ash rex-r 2) rex-b))
               (modrm (logior #xC0
                              (ash (logand src-num 7) 3)
                              (logand dst-num 7)))
               (buf-sym (make-symbol "buf"))
               (bs-sym  (make-symbol "bs")))
          `(let* ((,buf-sym ,buf-form)
                  (,bs-sym (unibyte-string ,rex #x89 ,modrm)))
             (aset ,buf-sym 0 (cons ,bs-sym (aref ,buf-sym 0)))
             (aset ,buf-sym 1 (+ (aref ,buf-sym 1) 3))
             ,buf-sym))
      `(nelisp-asm-x86_64-mov-reg-reg ,buf-form ,dst-form ,src-form))))

(defmacro nelisp-asm-x86_64--push-inline (buf-form reg-form)
  "Inline `PUSH REG' (= [REX.B] + 0x50+rd, 1 or 2 bytes).
Quoted-literal REG-FORM folds to a constant byte sequence at
macroexpand time."
  (let ((reg-num (nelisp-asm-x86_64--quoted-reg-num reg-form)))
    (if reg-num
        (let* ((ext (if (>= reg-num 8) 1 0))
               (low (logand reg-num 7))
               (buf-sym (make-symbol "buf"))
               (bs-sym  (make-symbol "bs")))
          (if (zerop ext)
              `(let* ((,buf-sym ,buf-form)
                      (,bs-sym (unibyte-string ,(+ #x50 low))))
                 (aset ,buf-sym 0 (cons ,bs-sym (aref ,buf-sym 0)))
                 (aset ,buf-sym 1 (+ (aref ,buf-sym 1) 1))
                 ,buf-sym)
            `(let* ((,buf-sym ,buf-form)
                    (,bs-sym (unibyte-string #x41 ,(+ #x50 low))))
               (aset ,buf-sym 0 (cons ,bs-sym (aref ,buf-sym 0)))
               (aset ,buf-sym 1 (+ (aref ,buf-sym 1) 2))
               ,buf-sym)))
      `(nelisp-asm-x86_64-push ,buf-form ,reg-form))))

(defmacro nelisp-asm-x86_64--pop-inline (buf-form reg-form)
  "Inline `POP REG' (= [REX.B] + 0x58+rd, 1 or 2 bytes)."
  (let ((reg-num (nelisp-asm-x86_64--quoted-reg-num reg-form)))
    (if reg-num
        (let* ((ext (if (>= reg-num 8) 1 0))
               (low (logand reg-num 7))
               (buf-sym (make-symbol "buf"))
               (bs-sym  (make-symbol "bs")))
          (if (zerop ext)
              `(let* ((,buf-sym ,buf-form)
                      (,bs-sym (unibyte-string ,(+ #x58 low))))
                 (aset ,buf-sym 0 (cons ,bs-sym (aref ,buf-sym 0)))
                 (aset ,buf-sym 1 (+ (aref ,buf-sym 1) 1))
                 ,buf-sym)
            `(let* ((,buf-sym ,buf-form)
                    (,bs-sym (unibyte-string #x41 ,(+ #x58 low))))
               (aset ,buf-sym 0 (cons ,bs-sym (aref ,buf-sym 0)))
               (aset ,buf-sym 1 (+ (aref ,buf-sym 1) 2))
               ,buf-sym)))
      `(nelisp-asm-x86_64-pop ,buf-form ,reg-form))))

(defmacro nelisp-asm-x86_64--ret-inline (buf-form)
  "Inline `RET' (= 0xC3, 1 byte)."
  (let ((buf-sym (make-symbol "buf"))
        (bs-sym  (make-symbol "bs")))
    `(let* ((,buf-sym ,buf-form)
            (,bs-sym (unibyte-string #xC3)))
       (aset ,buf-sym 0 (cons ,bs-sym (aref ,buf-sym 0)))
       (aset ,buf-sym 1 (+ (aref ,buf-sym 1) 1))
       ,buf-sym)))

(defmacro nelisp-asm-x86_64--define-label-inline (buf-form name-form)
  "Inline `define-label' = current-pos snapshot + push (NAME . POS) onto
labels alist.  Duplicate-label detection still runs at runtime."
  (let ((buf-sym (make-symbol "buf"))
        (name-sym (make-symbol "name")))
    `(let ((,buf-sym ,buf-form)
           (,name-sym ,name-form))
       (when (assq ,name-sym (aref ,buf-sym 2))
         (signal 'nelisp-asm-x86_64-error
                 (list :duplicate-label ,name-sym)))
       (aset ,buf-sym 2
             (cons (cons ,name-sym (aref ,buf-sym 1)) (aref ,buf-sym 2)))
       ,buf-sym)))

(defmacro nelisp-asm-x86_64--mov-imm32-inline (buf-form reg-form imm-form)
  "Inline `MOV REG, IMM32' = REX.W + 0xC7 /0 + imm32 (7 bytes).
Quoted-literal REG folds REX + ModR/M bytes to constants.  IMM is
encoded at runtime (caller-provided value)."
  (let ((reg-num (nelisp-asm-x86_64--quoted-reg-num reg-form)))
    (if reg-num
        (let* ((rex-b (if (>= reg-num 8) 1 0))
               (rex (logior #x48 rex-b))
               (modrm (logior #xC0 (logand reg-num 7)))
               (buf-sym (make-symbol "buf"))
               (imm-sym (make-symbol "imm"))
               (bs-sym  (make-symbol "bs")))
          `(let* ((,buf-sym ,buf-form)
                  (,imm-sym ,imm-form)
                  (,bs-sym
                   (concat (unibyte-string ,rex #xC7 ,modrm)
                           (nelisp-asm-x86_64--imm32-bytes ,imm-sym))))
             (aset ,buf-sym 0 (cons ,bs-sym (aref ,buf-sym 0)))
             (aset ,buf-sym 1 (+ (aref ,buf-sym 1) 7))
             ,buf-sym))
      `(nelisp-asm-x86_64-mov-imm32 ,buf-form ,reg-form ,imm-form))))

(defmacro nelisp-asm-x86_64--sub-imm32-inline (buf-form reg-form imm-form)
  "Inline `SUB REG, IMM32' = REX.W + 0x81 /5 + imm32 (7 bytes).
Quoted-literal REG folds REX + ModR/M bytes to constants."
  (let ((reg-num (nelisp-asm-x86_64--quoted-reg-num reg-form)))
    (if reg-num
        (let* ((rex-b (if (>= reg-num 8) 1 0))
               (rex (logior #x48 rex-b))
               (modrm (logior #xC0 (ash 5 3) (logand reg-num 7)))
               (buf-sym (make-symbol "buf"))
               (imm-sym (make-symbol "imm"))
               (bs-sym  (make-symbol "bs")))
          `(let* ((,buf-sym ,buf-form)
                  (,imm-sym ,imm-form)
                  (,bs-sym
                   (concat (unibyte-string ,rex #x81 ,modrm)
                           (nelisp-asm-x86_64--imm32-bytes ,imm-sym))))
             (aset ,buf-sym 0 (cons ,bs-sym (aref ,buf-sym 0)))
             (aset ,buf-sym 1 (+ (aref ,buf-sym 1) 7))
             ,buf-sym))
      `(nelisp-asm-x86_64-sub-imm32 ,buf-form ,reg-form ,imm-form))))

(defmacro nelisp-asm-x86_64--mov-mem-reg-disp8-inline (buf-form base-form disp-form src-form)
  "Inline `MOV QWORD PTR [BASE + DISP], SRC' = REX.W + 89 ModR/M + disp8.
Quoted-literal BASE / SRC fold REX + ModR/M to constants.  DISP is
encoded at runtime.  Refuses `rsp' / `r12' as base at expansion (SIB
not modelled)."
  (let ((base-num (nelisp-asm-x86_64--quoted-reg-num base-form))
        (src-num  (nelisp-asm-x86_64--quoted-reg-num src-form)))
    (if (and base-num src-num
             (not (memq base-num '(4 12))))  ; rsp=4 r12=12 need SIB
        (let* ((rex-b (if (>= base-num 8) 1 0))
               (rex-r (if (>= src-num  8) 1 0))
               (rex (logior #x48 (ash rex-r 2) rex-b))
               (modrm (logior #x40
                              (ash (logand src-num 7) 3)
                              (logand base-num 7)))
               (buf-sym  (make-symbol "buf"))
               (disp-sym (make-symbol "disp"))
               (bs-sym   (make-symbol "bs")))
          `(let* ((,buf-sym ,buf-form)
                  (,disp-sym ,disp-form))
             (unless (and (integerp ,disp-sym) (<= -128 ,disp-sym 127))
               (signal 'nelisp-asm-x86_64-error
                       (list :disp8-out-of-range ,disp-sym)))
             (let ((,bs-sym (unibyte-string ,rex #x89 ,modrm
                                            (logand ,disp-sym #xFF))))
               (aset ,buf-sym 0 (cons ,bs-sym (aref ,buf-sym 0)))
               (aset ,buf-sym 1 (+ (aref ,buf-sym 1) 4))
               ,buf-sym)))
      `(nelisp-asm-x86_64-mov-mem-reg-disp8
        ,buf-form ,base-form ,disp-form ,src-form))))

(defun nelisp-asm-x86_64-define-label (buf name)
  "Mark NAME as resolved at BUF's current byte position.
Signals `nelisp-asm-x86_64-error' on duplicate label — silent
shadow would mask codegen bugs."
  (when (assq name (aref buf 2))
    (signal 'nelisp-asm-x86_64-error
            (list :duplicate-label name)))
  (aset buf 2 (cons (cons name (aref buf 1)) (aref buf 2)))
  buf)

(defun nelisp-asm-x86_64-emit-fixup (buf slot-offset label)
  "Record a 4-byte rel32 fixup at SLOT-OFFSET against LABEL.
SLOT-OFFSET is the absolute byte position in BUF where the 4-
byte little-endian displacement begins.  Resolution computes
`(label-pos - (slot-offset + 4))' at finalize time.  This helper
records only — the placeholder bytes themselves must already
have been emitted by the caller (= `call-rel32' / `jmp-rel32'
write 4 zero bytes before recording the fixup)."
  (aset buf 3 (cons (cons slot-offset label) (aref buf 3)))
  buf)

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
         (offset (aref buf 1))
         (entry (list :offset offset
                      :type type
                      :symbol sym
                      :sym sym
                      :addend (or addend 0)
                      :section section)))
    (aset buf 4 (append (aref buf 4) (list entry)))
    buf))

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
          (aref buf 4)))

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
  (let ((offset (aref buf 1)))
    (nelisp-asm-x86_64--append-bytes buf (unibyte-string 0 0 0 0))
    (nelisp-asm-x86_64-emit-reloc buf 'pc32 sym-name addend
                                  :section (or section 'text))
    ;; The reloc was recorded at the *new* pos (= post-append) so
    ;; rewrite the last entry's `:offset' to the placeholder start.
    (let* ((relocs (aref buf 4))
           (last (car (last relocs))))
      (setq last (plist-put last :offset offset))
      (aset buf 4 (append (butlast relocs) (list last))))
    offset))

(defun nelisp-asm-x86_64-reloc-abs64-here (buf sym-name addend
                                               &optional section)
  "Emit 8 zero bytes + record an `abs64' reloc at the placeholder.
BUF is mutated; SYM-NAME and ADDEND are forwarded to
`nelisp-asm-x86_64-emit-reloc'.  SECTION defaults to `text'.
Returns the offset where the placeholder begins."
  (let ((offset (aref buf 1)))
    (nelisp-asm-x86_64--append-bytes
     buf (unibyte-string 0 0 0 0 0 0 0 0))
    (nelisp-asm-x86_64-emit-reloc buf 'abs64 sym-name addend
                                  :section (or section 'text))
    (let* ((relocs (aref buf 4))
           (last (car (last relocs))))
      (setq last (plist-put last :offset offset))
      (aset buf 4 (append (butlast relocs) (list last))))
    offset))

(defun nelisp-asm-x86_64-reloc-plt32-here (buf sym-name addend
                                               &optional section)
  "Emit 4 zero bytes + record a `plt32' reloc at the placeholder.
Semantically identical to `nelisp-asm-x86_64-reloc-pc32-here' for
ET_EXEC static linking (= no PLT trampoline needed) but emits
R_X86_64_PLT32 so the linker can distinguish PIC-aware call
sites.  SECTION defaults to `text'.  Returns the placeholder
offset."
  (let ((offset (aref buf 1)))
    (nelisp-asm-x86_64--append-bytes buf (unibyte-string 0 0 0 0))
    (nelisp-asm-x86_64-emit-reloc buf 'plt32 sym-name addend
                                  :section (or section 'text))
    (let* ((relocs (aref buf 4))
           (last (car (last relocs))))
      (setq last (plist-put last :offset offset))
      (aset buf 4 (append (butlast relocs) (list last))))
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
unibyte-string; BUF is mutated in place.

§92.d chunk-build: finalize chunks once into a single materialized
unibyte-string, patch via a mutable vector, then store back as a
single chunk (= the cached chunk list collapses to length 1 so
subsequent `buffer-bytes' calls remain O(total-bytes))."
  (let* ((bytes  (apply #'concat (nreverse (copy-sequence (aref buf 0)))))
         (labels (aref buf 2))
         (fixups (aref buf 3))
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
    (let ((patched (nelisp-asm-x86_64--byte-vec->string vec)))
      ;; Collapse chunk list to a single materialized chunk so
      ;; subsequent `buffer-bytes' calls return the patched form.
      (aset buf 0 (list patched))
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

;; ---- Doc 100 v2 §100.B Sexp ABI direct-access ops ----
;;
;; Four memory-access primitives the Phase 47 compiler uses when it
;; emits direct loads / stores against a Sexp value held in a caller-
;; provided register pointer.  Each maps to a single x86_64 instruction
;; against `[base]' or `[base + imm8]'.  No SIB byte: `rsp' / `r12' as
;; the base register would trigger SIB-required encoding (= ModR/M.rm
;; = 100), which Doc 100 §100.B does not need.  All callers pass `rdi'
;; or `rsi' as base.  See `docs/arch/sexp-abi.md' §5.

(defun nelisp-asm-x86_64-movzx-reg-byte-mem (buf dst base)
  "Emit `MOVZX DST, BYTE PTR [BASE]' = REX.W + 0F B6 ModR/M (4 bytes).
Zero-extends the byte at `[base]' into the 64-bit DST.  Base must
not be `rsp' / `r12' (= SIB-required, not modelled here)."
  (when (memq base '(rsp r12))
    (signal 'nelisp-asm-x86_64-error
            (list :movzx-rsp-r12-needs-sib base)))
  (let* ((rex (nelisp-asm-x86_64--rex
               1
               (nelisp-asm-x86_64--reg-ext dst)
               0
               (nelisp-asm-x86_64--reg-ext base)))
         (modrm (nelisp-asm-x86_64--modrm
                 0
                 (nelisp-asm-x86_64--reg-low3 dst)
                 (nelisp-asm-x86_64--reg-low3 base))))
    (nelisp-asm-x86_64--append-bytes
     buf (unibyte-string rex #x0F #xB6 modrm))))

(defun nelisp-asm-x86_64-mov-reg-mem-disp8 (buf dst base disp)
  "Emit `MOV DST, QWORD PTR [BASE + DISP]'.
Uses the compact disp8 encoding when DISP fits [-128, 127], otherwise
uses the disp32 form.  Base must not be `rsp' / `r12' (= SIB-required,
not modelled here)."
  (when (memq base '(rsp r12))
    (signal 'nelisp-asm-x86_64-error
            (list :mov-rsp-r12-base-needs-sib base)))
  (unless (and (integerp disp)
               (<= (- (ash 1 31)) disp (1- (ash 1 31))))
    (signal 'nelisp-asm-x86_64-error
            (list :disp32-out-of-range disp)))
  (let* ((rex (nelisp-asm-x86_64--rex
               1
               (nelisp-asm-x86_64--reg-ext dst)
               0
               (nelisp-asm-x86_64--reg-ext base)))
         (reg (nelisp-asm-x86_64--reg-low3 dst))
         (rm (nelisp-asm-x86_64--reg-low3 base)))
    (if (<= -128 disp 127)
        (nelisp-asm-x86_64--append-bytes
         buf (unibyte-string rex #x8B
                             (nelisp-asm-x86_64--modrm 1 reg rm)
                             (logand disp #xFF)))
      (nelisp-asm-x86_64--append-bytes
       buf (concat (unibyte-string rex #x8B
                                   (nelisp-asm-x86_64--modrm 2 reg rm))
                   (nelisp-asm-x86_64--imm32-bytes disp))))))

(defun nelisp-asm-x86_64-mov-reg-mem-rsp-disp (buf dst disp)
  "Emit `MOV DST, QWORD PTR [RSP + DISP]' using SIB addressing.
DISP=0 uses no displacement, signed disp8 uses the compact form, and
larger signed/unsigned i32 displacements use disp32.  This is the
minimal RSP-base load needed by Doc 129 call-spill assembly."
  (unless (and (integerp disp) (<= 0 disp (1- (ash 1 32))))
    (signal 'nelisp-asm-x86_64-error
            (list :rsp-disp-out-of-range disp)))
  (let* ((rex (nelisp-asm-x86_64--rex
               1
               (nelisp-asm-x86_64--reg-ext dst)
               0
               0))
         (reg (nelisp-asm-x86_64--reg-low3 dst))
         (sib #x24))
    (cond
     ((zerop disp)
      (nelisp-asm-x86_64--append-bytes
       buf (unibyte-string rex #x8B
                           (nelisp-asm-x86_64--modrm 0 reg 4)
                           sib)))
     ((<= -128 disp 127)
      (nelisp-asm-x86_64--append-bytes
       buf (unibyte-string rex #x8B
                           (nelisp-asm-x86_64--modrm 1 reg 4)
                           sib
                           (logand disp #xFF))))
     (t
      (nelisp-asm-x86_64--append-bytes
       buf (concat (unibyte-string rex #x8B
                                   (nelisp-asm-x86_64--modrm 2 reg 4)
                                   sib)
                   (nelisp-asm-x86_64--imm32-bytes disp)))))))

(defun nelisp-asm-x86_64-mov-mem-rsp-disp-reg (buf disp src)
  "Emit `MOV QWORD PTR [RSP + DISP], SRC' using SIB addressing.
DISP=0 uses no displacement, signed disp8 uses the compact form, and
larger signed/unsigned i32 displacements use disp32.  This is the
RSP-base store companion to `nelisp-asm-x86_64-mov-reg-mem-rsp-disp'."
  (unless (and (integerp disp) (<= 0 disp (1- (ash 1 32))))
    (signal 'nelisp-asm-x86_64-error
            (list :rsp-disp-out-of-range disp)))
  (let* ((rex (nelisp-asm-x86_64--rex
               1
               (nelisp-asm-x86_64--reg-ext src)
               0
               0))
         (reg (nelisp-asm-x86_64--reg-low3 src))
         (sib #x24))
    (cond
     ((zerop disp)
      (nelisp-asm-x86_64--append-bytes
       buf (unibyte-string rex #x89
                           (nelisp-asm-x86_64--modrm 0 reg 4)
                           sib)))
     ((<= -128 disp 127)
      (nelisp-asm-x86_64--append-bytes
       buf (unibyte-string rex #x89
                           (nelisp-asm-x86_64--modrm 1 reg 4)
                           sib
                           (logand disp #xFF))))
     (t
      (nelisp-asm-x86_64--append-bytes
       buf (concat (unibyte-string rex #x89
                                   (nelisp-asm-x86_64--modrm 2 reg 4)
                                   sib)
                   (nelisp-asm-x86_64--imm32-bytes disp)))))))

(defun nelisp-asm-x86_64-mov-mem-imm8 (buf base imm)
  "Emit `MOV BYTE PTR [BASE], IMM8' = [REX.B] + C6 /0 ModR/M + imm8.
2 bytes (no REX) for base in rax-rdi, 3 bytes with REX.B for r8-r15.
IMM is the unsigned byte value [0, 255] (= one of the
`SEXP_TAG_*' constants in practice)."
  (when (memq base '(rsp r12))
    (signal 'nelisp-asm-x86_64-error
            (list :mov-rsp-r12-base-needs-sib base)))
  (unless (and (integerp imm) (<= 0 imm 255))
    (signal 'nelisp-asm-x86_64-error
            (list :imm8-out-of-range imm)))
  (let* ((ext (nelisp-asm-x86_64--reg-ext base))
         (modrm (nelisp-asm-x86_64--modrm
                 0 0 (nelisp-asm-x86_64--reg-low3 base)))
         (prefix (if (zerop ext)
                     (unibyte-string)
                   (unibyte-string (nelisp-asm-x86_64--rex 0 0 0 1)))))
    (nelisp-asm-x86_64--append-bytes
     buf (concat prefix
                 (unibyte-string #xC6 modrm (logand imm #xFF))))))

(defun nelisp-asm-x86_64-mov-mem-reg-disp8 (buf base disp src)
  "Emit `MOV QWORD PTR [BASE + DISP], SRC'.
Uses the compact disp8 encoding when DISP fits [-128, 127], otherwise
uses the disp32 form.  Base must not be `rsp' / `r12' (= SIB-required,
not modelled here)."
  (when (memq base '(rsp r12))
    (signal 'nelisp-asm-x86_64-error
            (list :mov-rsp-r12-base-needs-sib base)))
  (unless (and (integerp disp)
               (<= (- (ash 1 31)) disp (1- (ash 1 31))))
    (signal 'nelisp-asm-x86_64-error
            (list :disp32-out-of-range disp)))
  (let* ((rex (nelisp-asm-x86_64--rex
               1
               (nelisp-asm-x86_64--reg-ext src)
               0
               (nelisp-asm-x86_64--reg-ext base)))
         (reg (nelisp-asm-x86_64--reg-low3 src))
         (rm (nelisp-asm-x86_64--reg-low3 base)))
    (if (<= -128 disp 127)
        (nelisp-asm-x86_64--append-bytes
         buf (unibyte-string rex #x89
                             (nelisp-asm-x86_64--modrm 1 reg rm)
                             (logand disp #xFF)))
      (nelisp-asm-x86_64--append-bytes
       buf (concat (unibyte-string rex #x89
                                   (nelisp-asm-x86_64--modrm 2 reg rm))
                   (nelisp-asm-x86_64--imm32-bytes disp))))))

;; ---- Doc 110 §110.A f64 register-class MOVSD helpers ----
;;
;; SSE2 scalar-double `MOVSD' instruction in three forms used by the
;; §110.A-F f64 ABI roll-out: register-to-register move, load from
;; `[base + disp8]', store to `[base + disp8]', and load from
;; `[rip + disp32]' (for f64 literals embedded in `.rodata').
;;
;; All forms share the `F2 0F 10 /r' (load) or `F2 0F 11 /r' (store)
;; opcode skeleton.  The mandatory `F2' prefix discriminates SSE2
;; scalar-double from the other variants of `MOV[A-Z]+'.  REX.W is
;; *not* set (xmm width is fixed at 128 bits; REX.W has no effect on
;; xmm operations).  REX.R / REX.B turn on only when an xmm dst /
;; src is in `xmm8-xmm15' or a GP base is in `r8-r15'.  When all
;; extension bits would be zero the REX byte is elided entirely (= 4
;; bytes for reg-reg vs. 5 with REX, etc.).
;;
;; The §110.A.1 set covers the asm-layer encodings only; the Phase
;; 47 compiler's `(f64 ...)' parse and emit dispatch land in §110.B
;; once these primitives have ert byte coverage.  No callsite from
;; `nelisp-phase47-compiler.el' uses these helpers yet.

(defun nelisp-asm-x86_64--emit-sse2-rr (buf prefix opcode dst src)
  "Internal: emit `PREFIX [REX?] 0F OPCODE ModR/M' SSE2 xmm-xmm op.
PREFIX is the mandatory single-byte SSE2 prefix (= F2 scalar
double, F3 scalar single, 66 packed double, omitted for packed
single).  OPCODE is the second escape byte (= the opcode after
0F).  DST is ModR/M.reg, SRC is ModR/M.rm.  REX elided when both
xmm regs are in the xmm0-xmm7 bank.

Used by MOVSD / ADDSD / SUBSD / MULSD / DIVSD (prefix=F2) and
UCOMISD (prefix=66) to share the register-form encoding skeleton."
  (let* ((ext-r (nelisp-asm-x86_64--xmm-reg-ext dst))
         (ext-b (nelisp-asm-x86_64--xmm-reg-ext src))
         (need-rex (or (= ext-r 1) (= ext-b 1)))
         (rex (if need-rex
                  (unibyte-string
                   (nelisp-asm-x86_64--rex 0 ext-r 0 ext-b))
                ""))
         (modrm (nelisp-asm-x86_64--modrm
                 3
                 (nelisp-asm-x86_64--xmm-reg-low3 dst)
                 (nelisp-asm-x86_64--xmm-reg-low3 src))))
    (nelisp-asm-x86_64--append-bytes
     buf (concat (unibyte-string prefix)
                 rex
                 (unibyte-string #x0F opcode modrm)))))

(defun nelisp-asm-x86_64--emit-sse2-scalar-double-rr (buf opcode dst src)
  "Internal: SCALAR-DOUBLE shorthand for `--emit-sse2-rr' with F2 prefix.
Retained as a thin alias so existing MOVSD / ADDSD callers stay
expressive.  Equivalent to `(--emit-sse2-rr buf #xF2 opcode dst src)'."
  (nelisp-asm-x86_64--emit-sse2-rr buf #xF2 opcode dst src))

(defun nelisp-asm-x86_64-movsd-reg-reg (buf dst src)
  "Emit `MOVSD DST, SRC' (xmm-to-xmm) = F2 [REX?] 0F 10 ModR/M.
4 bytes when both regs are xmm0-xmm7, 5 with REX.  DST and SRC
must both be xmm symbols (= members of
`nelisp-asm-x86_64--xmm-reg')."
  (nelisp-asm-x86_64--emit-sse2-scalar-double-rr buf #x10 dst src))

(defun nelisp-asm-x86_64-movsd-xmm-mem-disp8 (buf dst base disp)
  "Emit `MOVSD DST, QWORD PTR [BASE + DISP]' = F2 [REX?] 0F 10 ModR/M disp8.
DST is an xmm register; BASE is a GP register that must not be
`rsp' / `r12' (= SIB-required encoding, not modelled here).  DISP
must fit in a signed 8-bit value [-128, 127].  5 bytes without REX,
6 with."
  (when (memq base '(rsp r12))
    (signal 'nelisp-asm-x86_64-error
            (list :movsd-rsp-r12-base-needs-sib base)))
  (unless (and (integerp disp) (<= -128 disp 127))
    (signal 'nelisp-asm-x86_64-error
            (list :disp8-out-of-range disp)))
  (let* ((ext-r (nelisp-asm-x86_64--xmm-reg-ext dst))
         (ext-b (nelisp-asm-x86_64--reg-ext base))
         (need-rex (or (= ext-r 1) (= ext-b 1)))
         (rex (if need-rex
                  (unibyte-string
                   (nelisp-asm-x86_64--rex 0 ext-r 0 ext-b))
                ""))
         (modrm (nelisp-asm-x86_64--modrm
                 1
                 (nelisp-asm-x86_64--xmm-reg-low3 dst)
                 (nelisp-asm-x86_64--reg-low3 base))))
    (nelisp-asm-x86_64--append-bytes
     buf (concat (unibyte-string #xF2)
                 rex
                 (unibyte-string #x0F #x10 modrm (logand disp #xFF))))))

(defun nelisp-asm-x86_64-movsd-mem-disp8-xmm (buf base disp src)
  "Emit `MOVSD QWORD PTR [BASE + DISP], SRC' = F2 [REX?] 0F 11 ModR/M disp8.
Reverse of `movsd-xmm-mem-disp8' — stores SRC's lower 64 bits at
the memory location.  Same SIB / disp8 / REX rules apply.  Used to
spill an xmm value to a `[rbp + disp]' stack slot."
  (when (memq base '(rsp r12))
    (signal 'nelisp-asm-x86_64-error
            (list :movsd-rsp-r12-base-needs-sib base)))
  (unless (and (integerp disp) (<= -128 disp 127))
    (signal 'nelisp-asm-x86_64-error
            (list :disp8-out-of-range disp)))
  (let* ((ext-r (nelisp-asm-x86_64--xmm-reg-ext src))
         (ext-b (nelisp-asm-x86_64--reg-ext base))
         (need-rex (or (= ext-r 1) (= ext-b 1)))
         (rex (if need-rex
                  (unibyte-string
                   (nelisp-asm-x86_64--rex 0 ext-r 0 ext-b))
                ""))
         (modrm (nelisp-asm-x86_64--modrm
                 1
                 (nelisp-asm-x86_64--xmm-reg-low3 src)
                 (nelisp-asm-x86_64--reg-low3 base))))
    (nelisp-asm-x86_64--append-bytes
     buf (concat (unibyte-string #xF2)
                 rex
                 (unibyte-string #x0F #x11 modrm (logand disp #xFF))))))

(defun nelisp-asm-x86_64-movdqu-xmm-mem-disp8 (buf dst base disp)
  "Emit `MOVDQU DST, XMMWORD PTR [BASE + DISP]'.
Encoding: F3 [REX?] 0F 6F ModR/M disp8/disp32.  Used by Doc 101
§101.B's Cons slot copies (= 16-byte unaligned loads from
`NlConsBox.car' / `cdr' into an xmm scratch register), and by Win64
XMM callee-save restore slots."
  (when (memq base '(rsp r12))
    (signal 'nelisp-asm-x86_64-error
            (list :movdqu-rsp-r12-base-needs-sib base)))
  (unless (and (integerp disp)
               (<= (- (ash 1 31)) disp (1- (ash 1 31))))
    (signal 'nelisp-asm-x86_64-error
            (list :disp32-out-of-range disp)))
  (let* ((ext-r (nelisp-asm-x86_64--xmm-reg-ext dst))
         (ext-b (nelisp-asm-x86_64--reg-ext base))
         (need-rex (or (= ext-r 1) (= ext-b 1)))
         (rex (if need-rex
                  (unibyte-string
                   (nelisp-asm-x86_64--rex 0 ext-r 0 ext-b))
                ""))
         (reg (nelisp-asm-x86_64--xmm-reg-low3 dst))
         (rm (nelisp-asm-x86_64--reg-low3 base)))
    (if (<= -128 disp 127)
        (nelisp-asm-x86_64--append-bytes
         buf (concat (unibyte-string #xF3)
                     rex
                     (unibyte-string #x0F #x6F
                                     (nelisp-asm-x86_64--modrm 1 reg rm)
                                     (logand disp #xFF))))
      (nelisp-asm-x86_64--append-bytes
       buf (concat (unibyte-string #xF3)
                   rex
                   (unibyte-string #x0F #x6F
                                   (nelisp-asm-x86_64--modrm 2 reg rm))
                   (nelisp-asm-x86_64--imm32-bytes disp))))))

(defun nelisp-asm-x86_64-movdqu-mem-disp8-xmm (buf base disp src)
  "Emit `MOVDQU XMMWORD PTR [BASE + DISP], SRC'.
Encoding: F3 [REX?] 0F 7F ModR/M disp8/disp32.  Used by Doc 101
§101.B's Cons slot copies (= 16-byte unaligned stores into the
caller-owned 32-byte Sexp slot), and by Win64 XMM callee-save slots."
  (when (memq base '(rsp r12))
    (signal 'nelisp-asm-x86_64-error
            (list :movdqu-rsp-r12-base-needs-sib base)))
  (unless (and (integerp disp)
               (<= (- (ash 1 31)) disp (1- (ash 1 31))))
    (signal 'nelisp-asm-x86_64-error
            (list :disp32-out-of-range disp)))
  (let* ((ext-r (nelisp-asm-x86_64--xmm-reg-ext src))
         (ext-b (nelisp-asm-x86_64--reg-ext base))
         (need-rex (or (= ext-r 1) (= ext-b 1)))
         (rex (if need-rex
                  (unibyte-string
                   (nelisp-asm-x86_64--rex 0 ext-r 0 ext-b))
                ""))
         (reg (nelisp-asm-x86_64--xmm-reg-low3 src))
         (rm (nelisp-asm-x86_64--reg-low3 base)))
    (if (<= -128 disp 127)
        (nelisp-asm-x86_64--append-bytes
         buf (concat (unibyte-string #xF3)
                     rex
                     (unibyte-string #x0F #x7F
                                     (nelisp-asm-x86_64--modrm 1 reg rm)
                                     (logand disp #xFF))))
      (nelisp-asm-x86_64--append-bytes
       buf (concat (unibyte-string #xF3)
                   rex
                   (unibyte-string #x0F #x7F
                                   (nelisp-asm-x86_64--modrm 2 reg rm))
                   (nelisp-asm-x86_64--imm32-bytes disp))))))

(defun nelisp-asm-x86_64-movsd-xmm-rip-disp32 (buf dst disp32)
  "Emit `MOVSD DST, QWORD PTR [RIP + DISP32]' = F2 [REX?] 0F 10 ModR/M disp32.
Loads 8 bytes at the address `RIP + DISP32', where RIP is the
address of the NEXT instruction (= 4 bytes past the disp32 itself).
DST is an xmm register.  DISP32 must fit signed 32-bit.  Callers
that need a forward fixup to an as-yet-unresolved `.rodata' offset
should pass DISP32 = 0 and use the buffer's reloc / fixup
machinery to patch the disp32 slot post-finalize."
  (unless (and (integerp disp32) (<= (- (ash 1 31)) disp32 (1- (ash 1 31))))
    (signal 'nelisp-asm-x86_64-error
            (list :disp32-out-of-range disp32)))
  (let* ((ext-r (nelisp-asm-x86_64--xmm-reg-ext dst))
         (need-rex (= ext-r 1))
         (rex (if need-rex
                  (unibyte-string
                   (nelisp-asm-x86_64--rex 0 ext-r 0 0))
                ""))
         ;; mod=00, reg=dst.low3, rm=101 (= RIP-relative in 64-bit
         ;; mode; the rm=101+mod=00 combination is documented in
         ;; Intel SDM Vol 2A §2.2.1.6 "RIP-Relative Addressing").
         (modrm (nelisp-asm-x86_64--modrm
                 0
                 (nelisp-asm-x86_64--xmm-reg-low3 dst)
                 5)))
    (nelisp-asm-x86_64--append-bytes
     buf (concat (unibyte-string #xF2)
                 rex
                 (unibyte-string #x0F #x10 modrm)
                 (nelisp-asm-x86_64--imm32-bytes disp32)))))

;; ---- Doc 110 §110.B f64 arithmetic (= ADDSD / SUBSD / MULSD / DIVSD) ----
;;
;; Four SSE2 scalar-double arithmetic ops sharing the
;; `--emit-sse2-scalar-double-rr' skeleton.  Each emits
;; `F2 [REX?] 0F OPCODE ModR/M' against two xmm registers; the
;; result lands in DST.  Used by §110.E float.rs swap (= nl_jit_
;; float_add / _sub / _mul / _div trampolines).
;;
;; Intel SDM opcodes (Vol 2A):
;;   ADDSD xmm1, xmm2/m64 = F2 0F 58 /r
;;   SUBSD xmm1, xmm2/m64 = F2 0F 5C /r
;;   MULSD xmm1, xmm2/m64 = F2 0F 59 /r
;;   DIVSD xmm1, xmm2/m64 = F2 0F 5E /r
;;
;; Reg-reg form only — memory-source variants land in a later stage
;; when xmm spill / fill is wired (= Doc 112, currently out of
;; scope per Doc 110 §0.4).

(defun nelisp-asm-x86_64-addsd-reg-reg (buf dst src)
  "Emit `ADDSD DST, SRC' (xmm-to-xmm) = F2 [REX?] 0F 58 ModR/M.
Adds SRC's f64 to DST's f64 in IEEE 754 double precision; result
lands in DST.  Both regs must be xmm symbols."
  (nelisp-asm-x86_64--emit-sse2-scalar-double-rr buf #x58 dst src))

(defun nelisp-asm-x86_64-subsd-reg-reg (buf dst src)
  "Emit `SUBSD DST, SRC' (xmm-to-xmm) = F2 [REX?] 0F 5C ModR/M.
Subtracts SRC's f64 from DST's f64; result in DST."
  (nelisp-asm-x86_64--emit-sse2-scalar-double-rr buf #x5C dst src))

(defun nelisp-asm-x86_64-mulsd-reg-reg (buf dst src)
  "Emit `MULSD DST, SRC' (xmm-to-xmm) = F2 [REX?] 0F 59 ModR/M.
Multiplies DST's f64 by SRC's f64; result in DST."
  (nelisp-asm-x86_64--emit-sse2-scalar-double-rr buf #x59 dst src))

(defun nelisp-asm-x86_64-divsd-reg-reg (buf dst src)
  "Emit `DIVSD DST, SRC' (xmm-to-xmm) = F2 [REX?] 0F 5E ModR/M.
Divides DST's f64 by SRC's f64; result in DST.  Division-by-zero
behaviour follows IEEE 754 (= ±inf or NaN, not a trap)."
  (nelisp-asm-x86_64--emit-sse2-scalar-double-rr buf #x5E dst src))

(defun nelisp-asm-x86_64-andpd-reg-reg (buf dst src)
  "Emit `ANDPD DST, SRC' (xmm-to-xmm) = 66 [REX?] 0F 54 ModR/M.
Packed-double bitwise AND.  Doc 110 §110.C.2.b reuses this for
the abs-mask step of EQ-EPS (= `|a-b|' via `xmm0 AND
0x7FFFFFFFFFFFFFFF' to clear the sign bit of the lower 64-bit
half).  Operates on the full 128-bit xmm width but only the
lower 64 bits matter for our f64 use case — the upper 64 of
the mask register are 0 after `MOVQ xmm, r64', so ANDing
zero against anything gives zero in the upper half."
  (nelisp-asm-x86_64--emit-sse2-rr buf #x66 #x54 dst src))

(defun nelisp-asm-x86_64-movq-xmm-r64 (buf xmm-dst gp-src)
  "Emit `MOVQ XMM-DST, GP-SRC' = 66 REX.W [REX.R/B?] 0F 6E ModR/M.
Doc 110 §110.C.2.b — GP-to-xmm 64-bit value transfer used to
materialise f64 immediates (= bit pattern in r10, transferred
to xmm1 for ANDPD / UCOMISD).  REX.W = 1 selects the 64-bit
operand width; the upper 64 bits of XMM-DST are zero-extended."
  (let* ((ext-r (nelisp-asm-x86_64--xmm-reg-ext xmm-dst))
         (ext-b (nelisp-asm-x86_64--reg-ext gp-src))
         (rex (nelisp-asm-x86_64--rex 1 ext-r 0 ext-b))
         (modrm (nelisp-asm-x86_64--modrm
                 3
                 (nelisp-asm-x86_64--xmm-reg-low3 xmm-dst)
                 (nelisp-asm-x86_64--reg-low3 gp-src))))
    (nelisp-asm-x86_64--append-bytes
     buf (unibyte-string #x66 rex #x0F #x6E modrm))))

(defun nelisp-asm-x86_64-cvtsi2sd-xmm-r64 (buf xmm-dst gp-src)
  "Emit `CVTSI2SD XMM-DST, GP-SRC' = F2 + REX.W + 0F 2A + ModR/M (5 bytes).
Convert signed i64 in GP-SRC's full register to f64 in XMM-DST's low
64 bits.  Inverse of `cvttsd2si-r64-xmm'.  Used to lift Sexp::Int
payloads into f64-class for arithmetic chains.

ModR/M encoding: reg field = XMM-DST.low3, rm field = GP-SRC.low3.
REX.R extends the xmm reg; REX.B extends the GP src."
  (let* ((ext-r (nelisp-asm-x86_64--xmm-reg-ext xmm-dst))
         (ext-b (nelisp-asm-x86_64--reg-ext gp-src))
         (rex (nelisp-asm-x86_64--rex 1 ext-r 0 ext-b))
         (modrm (nelisp-asm-x86_64--modrm
                 3
                 (nelisp-asm-x86_64--xmm-reg-low3 xmm-dst)
                 (nelisp-asm-x86_64--reg-low3 gp-src))))
    (nelisp-asm-x86_64--append-bytes
     buf (unibyte-string #xF2 rex #x0F #x2A modrm))))

(defun nelisp-asm-x86_64-cvttsd2si-r64-xmm (buf gp-dst xmm-src)
  "Emit `CVTTSD2SI GP-DST, XMM-SRC' = F2 + REX.W + 0F 2C + ModR/M (5 bytes).
Convert (with truncation) the f64 in XMM-SRC's low 64 bits to a signed
i64 in GP-DST.  Per SDM Vol. 2A §3.3 (= `Out-of-range f64 → 0x8000_0000_0000_0000'
sentinel, no trap), this is the standard f64 → i64 cast used by
C `(int64_t) f' / Rust `f as i64'.  GP-DST and XMM-SRC must be in the
low 16-register class (= rax..r15 / xmm0..xmm15); the REX.W prefix
carries the 64-bit mode and REX.R / REX.B encode the high half of the
GP / xmm regs.

ModR/M encoding: reg field = GP-DST.low3 (= destination on reg side
since CVTTSD2SI's syntax is `gpr, xmm/m64'), rm field = XMM-SRC.low3.
REX.R extends the GP reg; REX.B extends the xmm src."
  (let* ((ext-r (nelisp-asm-x86_64--reg-ext gp-dst))
         (ext-b (nelisp-asm-x86_64--xmm-reg-ext xmm-src))
         (rex (nelisp-asm-x86_64--rex 1 ext-r 0 ext-b))
         (modrm (nelisp-asm-x86_64--modrm
                 3
                 (nelisp-asm-x86_64--reg-low3 gp-dst)
                 (nelisp-asm-x86_64--xmm-reg-low3 xmm-src))))
    (nelisp-asm-x86_64--append-bytes
     buf (unibyte-string #xF2 rex #x0F #x2C modrm))))

(defun nelisp-asm-x86_64-movq-r64-xmm (buf gp-dst xmm-src)
  "Emit `MOVQ GP-DST, XMM-SRC' = 66 REX.W [REX.R/B?] 0F 7E ModR/M.
Doc 122 §122.C — xmm-to-GP 64-bit transfer used by `extern-call-
f64' / `extern-call-varargs' to spill an f64 argument across a
push/pop pair (= the SysV ABI for variadic args still passes f64
in xmm0-7 + GP in rdi-r9, but our emit pipeline serializes arg
saves through `push rax').  REX.W = 1 selects the 64-bit operand
width; the high bits of GP-DST are filled with the full 64-bit
f64 bit pattern.  Inverse of `movq-xmm-r64'."
  (let* ((ext-r (nelisp-asm-x86_64--xmm-reg-ext xmm-src))
         (ext-b (nelisp-asm-x86_64--reg-ext gp-dst))
         (rex (nelisp-asm-x86_64--rex 1 ext-r 0 ext-b))
         (modrm (nelisp-asm-x86_64--modrm
                 3
                 (nelisp-asm-x86_64--xmm-reg-low3 xmm-src)
                 (nelisp-asm-x86_64--reg-low3 gp-dst))))
    (nelisp-asm-x86_64--append-bytes
     buf (unibyte-string #x66 rex #x0F #x7E modrm))))

(defun nelisp-asm-x86_64-and-r8-r8 (buf dst src)
  "Emit 8-bit `AND DST, SRC' = 20 ModR/M.
Doc 110 §110.C.2.b — used to AND the SETB result with SETNP
result for NaN-aware EQ-EPS (= mask out the unordered case so
NaN compares yield 0, matching Rust semantics).  DST and SRC
must be one of the low-byte registers `al', `cl', `dl', `bl';
the encoding REX-less so the legacy 8-bit register set is in
play (= cannot mix with `sil' / `dil' / R8B..R15B which need
REX prefix to disambiguate from AH..BH).  ModR/M reg field
holds SRC, rm holds DST per Intel SDM `AND r/m8, r8'."
  (let ((legal '(al cl dl bl)))
    (unless (memq dst legal)
      (signal 'nelisp-asm-x86_64-error
              (list :and-r8-dst-not-legacy dst)))
    (unless (memq src legal)
      (signal 'nelisp-asm-x86_64-error
              (list :and-r8-src-not-legacy src))))
  (let* ((low-byte-num
          (lambda (r)
            (cond ((eq r 'al) 0) ((eq r 'cl) 1)
                  ((eq r 'dl) 2) ((eq r 'bl) 3))))
         (modrm (nelisp-asm-x86_64--modrm
                 3
                 (funcall low-byte-num src)
                 (funcall low-byte-num dst))))
    (nelisp-asm-x86_64--append-bytes
     buf (unibyte-string #x20 modrm))))

;; ---- Doc 110 §110.C f64 compare + boolean materialise ----
;;
;; UCOMISD sets EFLAGS by comparing the lower 64-bit f64 of two xmm
;; regs (= QNaN-quiet variant; COMISD raises #IA on SNaN).  Note
;; the prefix is 66 (packed-double convention) not F2 — Intel
;; historical naming; UCOMISD still operates on scalar lower 64
;; bits regardless of the packed-double mnemonic family.
;;
;; SETcc r8 stores 0 or 1 in the low byte of a GP register based
;; on the matched condition code; we wire it only to AL because
;; the downstream MOVZX RAX, AL widens to the 64-bit return slot
;; expected by the §3.E `extern "C" fn(f64, f64) -> i64' shape.
;;
;; UCOMISD flag map (Intel SDM Vol 2A §3.2 UCOMISD):
;;   ordered greater (a > b)  : ZF=0 PF=0 CF=0  → SETA  yields 1
;;   ordered less    (a < b)  : ZF=0 PF=0 CF=1  → SETB  yields 1
;;   ordered equal   (a == b) : ZF=1 PF=0 CF=0  → SETE  yields 1
;;   unordered (NaN involved) : ZF=1 PF=1 CF=1  → all of SETB/SETBE/SETE yield 1
;;
;; NaN-correct compares (= match Rust `<' / `=' which treat any
;; NaN as not-less-and-not-equal) need a 2-instruction sequence
;; that gates on PF.  §110.A asm layer surfaces the primitive
;; SETcc opcodes; the NaN-aware sequencing happens at the §110.C
;; compiler emit stage (= one ert in the future
;; `phase47-compiler-f64-cmp' family per cmp variant covers the
;; pairwise emit decision).

(defun nelisp-asm-x86_64-ucomisd-reg-reg (buf dst src)
  "Emit `UCOMISD DST, SRC' (xmm-to-xmm) = 66 [REX?] 0F 2E ModR/M.
Compares lower-64-bit f64 of DST vs. SRC and sets EFLAGS (= ZF,
PF, CF; OF and SF are cleared).  Result is consumed by a following
`nelisp-asm-x86_64-setcc-al' (= 0F 9X C0); the xmm registers
themselves are not modified.

UCOMISD flag map (Intel SDM Vol 2A §3.2 UCOMISD):
  ordered greater (a > b)  : ZF=0 PF=0 CF=0  → SETA  yields 1
  ordered less    (a < b)  : ZF=0 PF=0 CF=1  → SETB  yields 1
  ordered equal   (a == b) : ZF=1 PF=0 CF=0  → SETE  yields 1
  unordered (NaN involved) : ZF=1 PF=1 CF=1  → SETB / SETBE / SETE
                                                each yield 1
NaN-correct compares need the §110.C compiler emit to also test
PF (= 1 indicates unordered) via SETNP and AND with the primary
cc result so Rust's `(NaN OP x) == false' semantics are matched."
  (nelisp-asm-x86_64--emit-sse2-rr buf #x66 #x2E dst src))

(defun nelisp-asm-x86_64-add-reg-reg (buf dst src)
  "Emit `ADD DST, SRC' (MR form, 64-bit, opcode 0x01)."
  (nelisp-asm-x86_64--emit-mr buf #x01 dst src))

(defun nelisp-asm-x86_64-sub-reg-reg (buf dst src)
  "Emit `SUB DST, SRC' (MR form, 64-bit, opcode 0x29)."
  (nelisp-asm-x86_64--emit-mr buf #x29 dst src))

;; ---- Doc 100 §100.D bitwise + shift helpers (= nl_jit_arith_*
;; swap) ----
;;
;; Three reg-reg bitwise binops mirror ADD/SUB/MUL: each is one
;; opcode byte in the MR form, all 3 bytes total with REX.W.
;; Shifts are special — x86_64 SHL/SAR by register require the count
;; in CL, so the helpers fix RAX as the destination and CL as the
;; (implicit) count source.  Phase 47 wires the count into RCX via
;; an existing `mov-reg-reg' before calling these.

(defun nelisp-asm-x86_64-or-reg-reg (buf dst src)
  "Emit `OR DST, SRC' (MR form, 64-bit, opcode 0x09)."
  (nelisp-asm-x86_64--emit-mr buf #x09 dst src))

(defun nelisp-asm-x86_64-and-reg-reg (buf dst src)
  "Emit `AND DST, SRC' (MR form, 64-bit, opcode 0x21)."
  (nelisp-asm-x86_64--emit-mr buf #x21 dst src))

(defun nelisp-asm-x86_64-xor-reg-reg (buf dst src)
  "Emit `XOR DST, SRC' (MR form, 64-bit, opcode 0x31)."
  (nelisp-asm-x86_64--emit-mr buf #x31 dst src))

(defun nelisp-asm-x86_64-shl-rax-cl (buf)
  "Emit `SHL RAX, CL' = REX.W + D3 /4 + ModR/M=0xE0 (3 bytes).
Logical-left shift; the count is implicit in CL (= low 8 bits of
RCX) per x86_64 ISA."
  (nelisp-asm-x86_64--append-bytes
   buf (unibyte-string #x48 #xD3 #xE0)))

(defun nelisp-asm-x86_64-shl-reg-imm8 (buf reg count)
  "Emit `SHL REG, COUNT' = REX.W + C1 /4 + ModR/M + ib (4 bytes).
REG is a 64-bit GP register symbol (`rax', `rsi', `r10', ...).
COUNT is the unsigned shift amount [0, 63] (logical-left shift).
A 4-byte fixed-width instruction — pass-1/pass-2 byte invariance
holds.  Doc 49 Wave 11.1 dependency for `static-imm32-table-
lookup' INDEX*4 byte-offset computation (= shl reg, 2)."
  (unless (and (integerp count) (<= 0 count 63))
    (signal 'nelisp-asm-x86_64-error
            (list :shl-count-out-of-range count)))
  (let* ((rex (nelisp-asm-x86_64--rex
               1 0 0 (nelisp-asm-x86_64--reg-ext reg)))
         (modrm (nelisp-asm-x86_64--modrm
                 3 4 (nelisp-asm-x86_64--reg-low3 reg))))
    (nelisp-asm-x86_64--append-bytes
     buf (unibyte-string rex #xC1 modrm (logand count #xFF)))))

(defun nelisp-asm-x86_64-sar-rax-cl (buf)
  "Emit `SAR RAX, CL' = REX.W + D3 /7 + ModR/M=0xF8 (3 bytes).
Arithmetic-right shift; the count is implicit in CL and the sign
bit replicates into the high bits."
  (nelisp-asm-x86_64--append-bytes
   buf (unibyte-string #x48 #xD3 #xF8)))

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

(defun nelisp-asm-x86_64-lea-reg-rip-label (buf reg label)
  "Emit `LEA REG, [rip + disp32]' (= REX.W 0x8D /r, 7 bytes) with a
fixup against LABEL.  Loads the runtime address of LABEL into REG —
the address-taking primitive behind Doc 133 Phase 0 `addr-of', used
to materialise a function pointer for indirect dispatch.  ModRM =
mod=00, reg=REG, rm=101 (RIP-relative); REX.W (+ REX.R for r8-r15).
The disp32 = `(label-pos - (slot + 4))' is patched by
`resolve-fixups', i.e. relative to the next instruction — exactly
the RIP base the CPU uses."
  (let* ((ext (nelisp-asm-x86_64--reg-ext reg))
         (low (nelisp-asm-x86_64--reg-low3 reg))
         (rex (nelisp-asm-x86_64--rex 1 ext 0 0))
         (modrm (nelisp-asm-x86_64--modrm 0 low 5))
         (slot (+ 3 (nelisp-asm-x86_64-buffer-pos buf))))
    (nelisp-asm-x86_64--append-bytes
     buf (unibyte-string rex #x8D modrm 0 0 0 0))
    (nelisp-asm-x86_64-emit-fixup buf slot label)))

(defun nelisp-asm-x86_64-call-reg (buf reg)
  "Emit `CALL r/m64' indirect through REG (= 0xFF /2, 2 or 3 bytes).
Calls the absolute address held in REG.  ModRM = mod=11, reg=2 (the
/2 opcode extension), rm = the low 3 bits of REG; REX.B lights up for
r8-r15.  This is the function-pointer / indirect-dispatch primitive
required by Doc 133 Phase 0 (`sys:call-ptr')."
  (let ((ext (nelisp-asm-x86_64--reg-ext reg))
        (low (nelisp-asm-x86_64--reg-low3 reg)))
    (if (zerop ext)
        (nelisp-asm-x86_64--append-bytes
         buf (unibyte-string #xFF (nelisp-asm-x86_64--modrm 3 2 low)))
      (nelisp-asm-x86_64--append-bytes
       buf (unibyte-string (nelisp-asm-x86_64--rex 0 0 0 1)
                           #xFF (nelisp-asm-x86_64--modrm 3 2 low))))))

(defun nelisp-asm-x86_64-jmp-rel32 (buf label)
  "Emit `JMP rel32' (opcode 0xE9) with a fixup against LABEL.
Writes 0xE9 + 4 zero placeholder bytes (5 bytes total), then
records a fixup at the placeholder offset."
  (let ((slot (1+ (nelisp-asm-x86_64-buffer-pos buf))))
    (nelisp-asm-x86_64--append-bytes
     buf (unibyte-string #xE9 0 0 0 0))
    (nelisp-asm-x86_64-emit-fixup buf slot label)))

(defun nelisp-asm-x86_64-jz-rel32 (buf label)
  "Emit `JZ rel32' (opcode 0x0F 0x84) with a fixup against LABEL.
Writes 0x0F 0x84 + 4 zero placeholder bytes (6 bytes total),
then records a fixup at the placeholder offset.  Jump is taken
when the previous flag-setting instruction (= typically `cmp')
left ZF=1."
  (let ((slot (+ 2 (nelisp-asm-x86_64-buffer-pos buf))))
    (nelisp-asm-x86_64--append-bytes
     buf (unibyte-string #x0F #x84 0 0 0 0))
    (nelisp-asm-x86_64-emit-fixup buf slot label)))

(defun nelisp-asm-x86_64-jnz-rel32 (buf label)
  "Emit `JNZ rel32' (opcode 0x0F 0x85) with a fixup against LABEL.
Writes 0x0F 0x85 + 4 zero placeholder bytes (6 bytes total),
then records a fixup at the placeholder offset.  Jump is taken
when ZF=0."
  (let ((slot (+ 2 (nelisp-asm-x86_64-buffer-pos buf))))
    (nelisp-asm-x86_64--append-bytes
     buf (unibyte-string #x0F #x85 0 0 0 0))
    (nelisp-asm-x86_64-emit-fixup buf slot label)))

(defun nelisp-asm-x86_64-cmp-reg-reg (buf dst src)
  "Emit `CMP DST, SRC' (MR form, 64-bit, opcode 0x39, 3 bytes).
Sets flags according to (DST - SRC) without modifying DST.
Used by Doc 97.c comparison + control-flow emitters."
  (nelisp-asm-x86_64--emit-mr buf #x39 dst src))

(defconst nelisp-asm-x86_64--setcc-opcodes
  '(;; Signed comparisons — Doc 97.c GP-int cmp result materialise.
    (setl  . #x9C)   ; SF != OF (a < b signed)
    (setg  . #x9F)   ; ZF=0 and SF == OF (a > b signed)
    (setle . #x9E)   ; ZF=1 or SF != OF
    (setge . #x9D)   ; SF == OF
    (sete  . #x94)   ; ZF=1 (a == b)
    (setne . #x95)   ; ZF=0 (a != b)
    ;; Unsigned / f64 comparisons — Doc 110 §110.C UCOMISD result
    ;; materialise.  UCOMISD sets CF/PF/ZF using unsigned-arithmetic
    ;; conventions even for f64 inputs, so the SETB / SETA family
    ;; reads cleanly off the EFLAGS bits per Intel SDM Vol 2A §3.2.
    (setb  . #x92)   ; CF=1 (a < b unsigned / f64 below)
    (setae . #x93)   ; CF=0 (a >= b unsigned / f64 above-or-equal)
    (setbe . #x96)   ; CF=1 or ZF=1 (a <= b unsigned / f64 below-or-equal)
    (seta  . #x97)   ; CF=0 and ZF=0 (a > b unsigned / f64 above)
    ;; NaN-aware helpers — PF=1 indicates unordered (= NaN involved).
    ;; Doc 110 §110.C compiler emit pairs SETNP with the primary
    ;; cc result to mask out unordered cases per Rust comparison
    ;; semantics.
    (setp  . #x9A)   ; PF=1 (unordered)
    (setnp . #x9B))  ; PF=0 (ordered)
  "Map setCC mnemonic -> second opcode byte for `0F XX' form.
Used by `nelisp-asm-x86_64-setcc-al' to materialise comparison
results from the flag register into AL.  Coverage spans Doc 97.c
signed-GP cc set + Doc 110 §110.C unsigned / f64 cc set + the
NaN-aware PF helpers.")

(defun nelisp-asm-x86_64-setcc-al (buf cc)
  "Emit `SETcc AL' = 0x0F + opcode + ModR/M C0 (3 bytes).
CC is any key in `nelisp-asm-x86_64--setcc-opcodes' — Doc 97.c
signed cc (`setl' / `setg' / `setle' / `setge' / `sete' /
`setne'), Doc 110 §110.C unsigned-/f64 cc (`setb' / `setae' /
`setbe' / `seta'), or NaN helper (`setp' / `setnp').  Writes 0
or 1 into AL based on the flag register; caller is responsible
for a prior flag-setting instruction (= `cmp' / `ucomisd' /
`test') and for zero-extending AL afterwards via
`nelisp-asm-x86_64-movzx-eax-al' if a wider result is needed.

Thin wrapper around `nelisp-asm-x86_64-setcc-byte-r8' targeting
AL (= legacy reg-low byte for rax)."
  (nelisp-asm-x86_64-setcc-byte-r8 buf cc 'al))

(defun nelisp-asm-x86_64-setcc-byte-r8 (buf cc r8)
  "Emit `SETcc R8' = 0x0F + opcode + ModR/M (3 bytes, no REX).
R8 must be one of the legacy low-byte registers `al' / `cl' /
`dl' / `bl' (= ModR/M.rm 0..3 without REX).  Modern low-byte
registers `sil' / `dil' / `bpl' / `spl' / R8B-R15B would need
REX prefix to disambiguate from AH..BH and are deferred until
needed.  Doc 110 §110.C.2.b uses this for `SETNP cl' alongside
`SETB al' to build the NaN mask for EQ-EPS."
  (let* ((legal-r8 '((al . 0) (cl . 1) (dl . 2) (bl . 3)))
         (r8-cell (assq r8 legal-r8))
         (cc-cell (assq cc nelisp-asm-x86_64--setcc-opcodes)))
    (unless r8-cell
      (signal 'nelisp-asm-x86_64-error
              (list :setcc-r8-not-legacy r8)))
    (unless cc-cell
      (signal 'nelisp-asm-x86_64-error (list :unknown-setcc cc)))
    (let ((modrm (nelisp-asm-x86_64--modrm 3 0 (cdr r8-cell))))
      (nelisp-asm-x86_64--append-bytes
       buf (unibyte-string #x0F (cdr cc-cell) modrm)))))

(defun nelisp-asm-x86_64-movzx-eax-al (buf)
  "Emit `MOVZX EAX, AL' = 0x0F 0xB6 0xC0 (3 bytes).
Zero-extends AL into EAX; because the CPU implicitly zero-
extends 32-bit writes into the full 64-bit RAX, this also
clears the high 32 bits of RAX.  Used after `setcc-al' to
turn the 0/1 byte into a proper 64-bit integer for the
Doc 97.c comparison emitters."
  (nelisp-asm-x86_64--append-bytes
   buf (unibyte-string #x0F #xB6 #xC0)))

;; ---- Doc 131 §131.A — atomic + raw-memory inline asm helpers ----
;;
;; These helpers replace the PLT-call pattern used by the Phase 47
;; grammar ops `atomic-fetch-add', `atomic-compare-exchange',
;; `ptr-read-u{8,16,32,64}', and `ptr-write-u{8,16,32,64}'.  Each
;; emits a single inline x86_64 instruction (or minimal prefix+opcode
;; sequence) that is semantically equivalent to the Rust `#[no_mangle]'
;; extern body it replaces.  All helpers follow the existing register
;; contract: RDI = ptr / base, RSI = delta / offset / index, RDX =
;; new-val / store value (ptr-write only).  Result lands in RAX.
;;
;; Encodings:
;;   LOCK XADD  r/m64, r64  — F0 REX.W 0F C1 ModR/M
;;   LOCK CMPXCHG r/m64, r64 — F0 REX.W 0F B1 ModR/M
;;   SETE AL                 — 0F 94 C0
;;   MOV  RAX, [RDI+RSI]     — REX.W 8B 04 37  (SIB: base=rdi, index=rsi)
;;   MOV  [RDI+RSI], RDX     — REX.W 89 14 37
;;   MOVZX RAX, BYTE  [RDI+RSI] — REX.W 0F B6 04 37
;;   MOV   BYTE  [RDI+RSI], DL  — 88 14 37
;;   MOVZX RAX, WORD  [RDI+RSI] — REX.W 0F B7 04 37
;;   MOV   WORD  [RDI+RSI], DX  — 66 89 14 37
;;   MOV  EAX, [RDI+RSI]        — 8B 04 37  (32→64 zero-extend by CPU)
;;   MOV  DWORD [RDI+RSI], EDX  — 89 14 37  (no REX.W)
;;
;; SIB byte 0x37 = {SS=00, INDEX=110(rsi), BASE=111(rdi)}.
;; SIB byte 0x14 trick for write: ModR/M encodes /2 (rdx) with RM=100
;; (SIB follows), SIB 0x37 = base=rdi index=rsi.

(defun nelisp-asm-x86_64-lock-xadd-mem-rax-rdi (buf)
  "Emit `LOCK XADD QWORD PTR [RDI], RAX' (5 bytes).
Exchange RAX with [RDI] and add; on exit RAX = old [RDI],
[RDI] = old + RAX.  Used for SeqCst `atomic-fetch-add':
caller puts delta in RAX before calling this helper."
  (nelisp-asm-x86_64--append-bytes
   buf (unibyte-string #xF0 #x48 #x0F #xC1 #x07)))

(defun nelisp-asm-x86_64-lock-cmpxchg-mem-rdx-rdi (buf)
  "Emit `LOCK CMPXCHG QWORD PTR [RDI], RDX' (5 bytes).
Compare RAX with [RDI]; if equal write RDX to [RDI] and set ZF.
Used for SeqCst `atomic-compare-exchange': caller puts expected
in RAX and new-val in RDX before calling this helper."
  (nelisp-asm-x86_64--append-bytes
   buf (unibyte-string #xF0 #x48 #x0F #xB1 #x17)))

(defun nelisp-asm-x86_64-sete-al (buf)
  "Emit `SETE AL' (3 bytes).
Sets AL to 1 if ZF is set (= previous CMPXCHG succeeded), 0 otherwise."
  (nelisp-asm-x86_64--append-bytes
   buf (unibyte-string #x0F #x94 #xC0)))

(defun nelisp-asm-x86_64-mov-rax-qword-rdi-rsi (buf)
  "Emit `MOV RAX, QWORD PTR [RDI+RSI]' (4 bytes: REX.W 8B 04 37).
Loads 8 bytes from address RDI+RSI into RAX (zero-extends)."
  (nelisp-asm-x86_64--append-bytes
   buf (unibyte-string #x48 #x8B #x04 #x37)))

(defun nelisp-asm-x86_64-mov-qword-rdi-rsi-rdx (buf)
  "Emit `MOV QWORD PTR [RDI+RSI], RDX' (4 bytes: REX.W 89 14 37).
Stores 8 bytes from RDX to address RDI+RSI."
  (nelisp-asm-x86_64--append-bytes
   buf (unibyte-string #x48 #x89 #x14 #x37)))

(defun nelisp-asm-x86_64-movzx-rax-byte-rdi-rsi (buf)
  "Emit `MOVZX RAX, BYTE PTR [RDI+RSI]' (5 bytes: REX.W 0F B6 04 37).
Loads 1 byte from address RDI+RSI, zero-extends to 64 bits in RAX."
  (nelisp-asm-x86_64--append-bytes
   buf (unibyte-string #x48 #x0F #xB6 #x04 #x37)))

(defun nelisp-asm-x86_64-mov-byte-rdi-rsi-dl (buf)
  "Emit `MOV BYTE PTR [RDI+RSI], DL' (3 bytes: 88 14 37).
Stores the low byte of RDX to address RDI+RSI."
  (nelisp-asm-x86_64--append-bytes
   buf (unibyte-string #x88 #x14 #x37)))

(defun nelisp-asm-x86_64-movzx-rax-word-rdi-rsi (buf)
  "Emit `MOVZX RAX, WORD PTR [RDI+RSI]' (5 bytes: REX.W 0F B7 04 37).
Loads 2 bytes from address RDI+RSI, zero-extends to 64 bits in RAX."
  (nelisp-asm-x86_64--append-bytes
   buf (unibyte-string #x48 #x0F #xB7 #x04 #x37)))

(defun nelisp-asm-x86_64-mov-word-rdi-rsi-dx (buf)
  "Emit `MOV WORD PTR [RDI+RSI], DX' (4 bytes: 66 89 14 37).
Stores the low 2 bytes of RDX to address RDI+RSI."
  (nelisp-asm-x86_64--append-bytes
   buf (unibyte-string #x66 #x89 #x14 #x37)))

(defun nelisp-asm-x86_64-mov-eax-dword-rdi-rsi (buf)
  "Emit `MOV EAX, DWORD PTR [RDI+RSI]' (3 bytes: 8B 04 37).
Loads 4 bytes from address RDI+RSI into EAX.  The CPU implicitly
zero-extends EAX to RAX (clears high 32 bits)."
  (nelisp-asm-x86_64--append-bytes
   buf (unibyte-string #x8B #x04 #x37)))

(defun nelisp-asm-x86_64-mov-dword-rdi-rsi-edx (buf)
  "Emit `MOV DWORD PTR [RDI+RSI], EDX' (3 bytes: 89 14 37).
Stores the low 4 bytes of RDX to address RDI+RSI."
  (nelisp-asm-x86_64--append-bytes
   buf (unibyte-string #x89 #x14 #x37)))

;; ---- §92.d benchmark helper (= chunk-build perf gate) ----

(defun nelisp-asm-x86_64-benchmark-emit (buf nbytes)
  "Emit NBYTES of synthetic NOP instructions into BUF.
Used by §92.d perf gate (= 1 MB synthetic emit must finish in
< 5 sec on commodity hardware).  Each iteration calls
`nelisp-asm-x86_64-nop' (= push 1-byte chunk onto :chunks) so
the total work exercises the chunk-build hot path at realistic
payload sizes.  Returns BUF (= chainable).  Mirrors Doc 91
§91.d's `nelisp-elf-benchmark-write-binary' pattern."
  (let ((i 0))
    (while (< i nbytes)
      (nelisp-asm-x86_64-nop buf)
      (setq i (1+ i))))
  buf)

(provide 'nelisp-asm-x86_64)

;;; nelisp-asm-x86_64.el ends here
