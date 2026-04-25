;;; nelisp-cc-x86_64.el --- NeLisp x86_64 backend skeleton (Phase 7.1.2)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 7.1.2 *skeleton subset* — see docs/design/28-phase7.1-native-compiler.org
;; §3.2.  Doc 28 LOCKED-2026-04-25-v2 commits Phase 7.1.2 to ~1500-2500
;; LOC (full) of x86_64 / System V AMD64 codegen.  This skeleton lands
;; the *encoder + buffer + dispatch substrate only* (~600-800 LOC) so a
;; subsequent agent can layer the missing instruction encodings, spill
;; / reload code, and call-resolution glue on a frozen substrate.
;;
;; In scope (this file):
;;   - System V AMD64 ABI register table
;;       * `nelisp-cc-x86_64--int-arg-regs'         — rdi rsi rdx rcx r8 r9
;;       * `nelisp-cc-x86_64--callee-saved'         — rbx rbp r12 r13 r14 r15
;;       * `nelisp-cc-x86_64--caller-saved'         — rax rcx rdx rsi rdi r8-r11
;;       * `nelisp-cc-x86_64--return-reg'           — rax
;;       * `nelisp-cc-x86_64--virtual-to-physical'  — T4 virtual → physical
;;   - Instruction byte emit helpers (REX prefix + ModR/M + immediates)
;;       MOV r64,r64 / MOV r64,imm32 / ADD / SUB / IMUL / CMP / RET /
;;       CALL rel32 / JMP rel32 / JCC rel32
;;   - Byte buffer + label / forward-reference resolution
;;       `nelisp-cc-x86_64--buffer'  cl-defstruct
;;       emit-byte / emit-bytes / define-label / resolve-fixups / finalize
;;   - SSA → x86_64 codegen *minimum subset*
;;       :const / :load-var / :store-var / :call / :branch / :return
;;     phi nodes are signalled out (caller must lower phis to copies
;;     before invoking the backend); `:call' is left :unresolved (Phase
;;     7.5 nelisp-defs-index integration).
;;
;; Out of scope (deferred to subsequent agent layer):
;;   - spill / reload (linear-scan `:spill' marker handling — currently
;;     `signal'ed)
;;   - macro / inline primitive fold (e.g. `+' inlined as ADD rather
;;     than via primitive call) — deferred to Phase 7.1.5
;;   - mmap PROT_EXEC page write + execute (Phase 7.1.4)
;;   - macOS Mach-O linker / relocation (Phase 7.5)
;;   - Win64 ABI variant (§2.3 opt-in, Phase 7.1.5+)
;;   - SIMD / xmm0-xmm7 / float ABI lowering (Phase 7.1.5+)
;;   - prologue / epilogue / red zone use (Phase 7.1.4)
;;   - exception unwind tables (.eh_frame) (Phase 7.5)
;;
;; Module convention (matches nelisp-cc.el):
;;   - `nelisp-cc-x86_64-' = public API
;;   - `nelisp-cc-x86_64--' = private helper
;;   - errors derive from `nelisp-cc-error' (defined in nelisp-cc)

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'nelisp-cc)

;;; Errors -----------------------------------------------------------

(define-error 'nelisp-cc-x86_64-error
  "NeLisp x86_64 backend error" 'nelisp-cc-error)
(define-error 'nelisp-cc-x86_64-encoding-error
  "x86_64 instruction encoding violation" 'nelisp-cc-x86_64-error)
(define-error 'nelisp-cc-x86_64-unresolved-label
  "x86_64 forward fixup references a label that was never defined"
  'nelisp-cc-x86_64-error)
(define-error 'nelisp-cc-x86_64-unsupported-opcode
  "x86_64 backend skeleton does not yet lower this SSA opcode"
  'nelisp-cc-x86_64-error)

;;; ABI register table ----------------------------------------------
;;
;; System V AMD64 (Linux + macOS, non-Windows).  Phase 7.1.5+ adds Win64
;; (rcx/rdx/r8/r9 + 32-byte shadow space) as a sibling table.  See Doc
;; 27 §2.12 LOCKED A and Doc 28 §2.3 (calling convention).

(defconst nelisp-cc-x86_64--int-arg-regs
  '(rdi rsi rdx rcx r8 r9)
  "Integer / pointer argument-passing registers, in positional order.
Matches System V AMD64 ABI §3.2.3.  Arguments past the 6th spill onto
the stack at +16(%rbp) and up.  The skeleton signals
`nelisp-cc-x86_64-unsupported-opcode' for any function with >6 params
until the spill machinery lands in a follow-up.")

(defconst nelisp-cc-x86_64--callee-saved
  '(rbx rbp r12 r13 r14 r15)
  "Callee-saved registers (preserved across CALL).
The prologue must save any of these the function clobbers; the
epilogue restores them.  rbp is conventionally also the frame pointer
when frame-pointer omission is disabled.")

(defconst nelisp-cc-x86_64--caller-saved
  '(rax rcx rdx rsi rdi r8 r9 r10 r11)
  "Caller-saved registers (clobbered across CALL).
The caller must spill anything live across a call site that lives in
one of these.  rax is also the integer return-value register; r10 /
r11 are scratch with no ABI role.")

(defconst nelisp-cc-x86_64--return-reg 'rax
  "Integer / pointer return-value register (System V AMD64).
Wider returns (struct in two regs, _Float128 in xmm0:xmm1) are out of
scope for the skeleton.")

(defconst nelisp-cc-x86_64--virtual-to-physical
  '((r0 . rdi)
    (r1 . rsi)
    (r2 . rdx)
    (r3 . rcx)
    (r4 . r8)
    (r5 . r9)
    (r6 . r10)
    (r7 . r11))
  "Default mapping from `nelisp-cc--default-int-registers' (T4 linear-scan
output names) to physical x86_64 registers.

Rationale: r0..r5 alias the six System V argument registers in
positional order, so a function that uses ≤6 arguments and ≤8 SSA
values total never needs a register move at the call boundary.  r6 /
r7 map to the two ABI-scratch registers (r10 / r11) so the linear-scan
allocator sees an 8-register pool with no callee-saved fallout.

This is *one* possible mapping — Phase 7.1.5 graph-coloring may
override it once it knows about callee-saved exploitation and call-
across spill cost.  The skeleton commits the simplest defensible
choice.")

;;; Register → encoding -----------------------------------------------
;;
;; x86_64 register encoding splits into:
;;   - lower 3 bits (the ModR/M.reg / ModR/M.rm field, or SIB equivalent)
;;   - REX.R / REX.B / REX.X bit (the high-extension bits for r8-r15)
;;
;; The cleanest encoding is therefore a (LOW3 . NEEDS-REX) pair.

(defconst nelisp-cc-x86_64--reg-encoding
  ;; (REG . (LOW3 . NEEDS-REX-EXT))
  '((rax . (0 . nil)) (rcx . (1 . nil)) (rdx . (2 . nil)) (rbx . (3 . nil))
    (rsp . (4 . nil)) (rbp . (5 . nil)) (rsi . (6 . nil)) (rdi . (7 . nil))
    (r8  . (0 . t))   (r9  . (1 . t))   (r10 . (2 . t))   (r11 . (3 . t))
    (r12 . (4 . t))   (r13 . (5 . t))   (r14 . (6 . t))   (r15 . (7 . t)))
  "Per-register x86_64 encoding map.

Each entry is `(REG . (LOW3 . NEEDS-REX-EXT))'.  LOW3 is the bottom 3
bits of the register's encoding, used in ModR/M.reg or ModR/M.rm.
NEEDS-REX-EXT is t when the register is r8-r15 and therefore requires
the matching REX.R / REX.B / REX.X bit.

The skeleton encodes only the integer general-purpose registers — xmm
/ ymm / zmm and the segment registers are out of scope.")

(defun nelisp-cc-x86_64--reg-low3 (reg)
  "Return the 3-bit ModR/M field for REG.

Signals `nelisp-cc-x86_64-encoding-error' if REG is not a known
x86_64 integer register name."
  (let ((cell (cdr (assq reg nelisp-cc-x86_64--reg-encoding))))
    (unless cell
      (signal 'nelisp-cc-x86_64-encoding-error
              (list :unknown-register reg)))
    (car cell)))

(defun nelisp-cc-x86_64--reg-rex-ext-p (reg)
  "Return non-nil when REG (r8-r15) requires the REX extension bit."
  (let ((cell (cdr (assq reg nelisp-cc-x86_64--reg-encoding))))
    (unless cell
      (signal 'nelisp-cc-x86_64-encoding-error
              (list :unknown-register reg)))
    (cdr cell)))

(defun nelisp-cc-x86_64-resolve-virtual (vreg)
  "Map VREG (a virtual register name from T4 linear-scan) to a physical
x86_64 register.  Returns the physical register symbol.

Signals `nelisp-cc-x86_64-encoding-error' if VREG is not in
`nelisp-cc-x86_64--virtual-to-physical' — this catches the linear-scan
sending an unmapped pool name into the backend."
  (let ((cell (assq vreg nelisp-cc-x86_64--virtual-to-physical)))
    (unless cell
      (signal 'nelisp-cc-x86_64-encoding-error
              (list :unmapped-virtual-register vreg)))
    (cdr cell)))

;;; REX prefix + ModR/M --------------------------------------------------

(defun nelisp-cc-x86_64--rex-byte (w r x b)
  "Compose a REX prefix byte from its bits.
W (1 bit, 64-bit operand width), R (extend ModR/M.reg), X (extend
SIB.index), B (extend ModR/M.rm).  Each argument is a generalized
boolean — t / 1 are set, nil / 0 are clear.

Returns the integer 0x40 + (W<<3 | R<<2 | X<<1 | B).  Callers may
elide the prefix entirely when all four bits are 0 (no REX prefix
needed for those instructions); the helper does *not* perform that
elision — it always returns a valid REX byte."
  (logior #x40
          (if (or (eq w t) (and (numberp w) (/= w 0))) 8 0)
          (if (or (eq r t) (and (numberp r) (/= r 0))) 4 0)
          (if (or (eq x t) (and (numberp x) (/= x 0))) 2 0)
          (if (or (eq b t) (and (numberp b) (/= b 0))) 1 0)))

(defun nelisp-cc-x86_64--modrm-byte (mod reg rm)
  "Compose a ModR/M byte from MOD (2 bits), REG (3 bits), RM (3 bits).
The result is `(mod << 6) | (reg << 3) | rm'."
  (unless (and (<= 0 mod) (<= mod 3))
    (signal 'nelisp-cc-x86_64-encoding-error (list :bad-mod mod)))
  (unless (and (<= 0 reg) (<= reg 7))
    (signal 'nelisp-cc-x86_64-encoding-error (list :bad-reg-field reg)))
  (unless (and (<= 0 rm) (<= rm 7))
    (signal 'nelisp-cc-x86_64-encoding-error (list :bad-rm-field rm)))
  (logior (ash mod 6) (ash reg 3) rm))

(defun nelisp-cc-x86_64--imm32-bytes (imm)
  "Encode IMM as a 4-byte little-endian list (low byte first).
IMM may be a 32-bit signed integer in the range [-2^31, 2^31-1] or an
unsigned 32-bit integer in [0, 2^32-1]; both representations land on
the same 4 bytes after `logand'.  Out-of-range values raise
`nelisp-cc-x86_64-encoding-error'."
  (cond
   ((not (integerp imm))
    (signal 'nelisp-cc-x86_64-encoding-error
            (list :imm-not-integer imm)))
   ((or (< imm (- (ash 1 31)))
        (>= imm (ash 1 32)))
    (signal 'nelisp-cc-x86_64-encoding-error
            (list :imm32-out-of-range imm)))
   (t
    (let ((u (logand imm #xFFFFFFFF)))
      (list (logand u #xFF)
            (logand (ash u -8) #xFF)
            (logand (ash u -16) #xFF)
            (logand (ash u -24) #xFF))))))

;;; Instruction emit helpers --------------------------------------------
;;
;; Each `--emit-*' helper returns a *list of bytes* in encoding order.
;; Callers (the buffer / codegen layer) splice them into the running
;; byte stream.  No emitter writes to a buffer directly — that
;; separation makes the emitters trivially unit-testable as pure
;; functions, which is exactly what the bytes-level golden tests
;; (Doc 28 §5 4-test class "backend encoding golden") require.

(defun nelisp-cc-x86_64--emit-mov-reg-reg (dst src)
  "Encode MOV DST, SRC (64-bit register-to-register).

Selected encoding: `MOV r/m64, r64' (MR form), opcode 0x89.  This is
the standard System V choice — using the alternate `MOV r64, r/m64'
(RM form, opcode 0x8B) would yield identical bytes after swapping
the ModR/M.reg / ModR/M.rm fields.  We commit to MR for consistency
with ADD / SUB / CMP below.

Bytes: REX.W=1 [+R if SRC=r8..r15] [+B if DST=r8..r15] | 0x89 |
ModR/M(mod=11, reg=SRC.low3, rm=DST.low3)."
  (let* ((src-low3 (nelisp-cc-x86_64--reg-low3 src))
         (dst-low3 (nelisp-cc-x86_64--reg-low3 dst))
         (rex.r    (nelisp-cc-x86_64--reg-rex-ext-p src))
         (rex.b    (nelisp-cc-x86_64--reg-rex-ext-p dst))
         (rex      (nelisp-cc-x86_64--rex-byte t rex.r nil rex.b))
         (modrm    (nelisp-cc-x86_64--modrm-byte 3 src-low3 dst-low3)))
    (list rex #x89 modrm)))

(defun nelisp-cc-x86_64--emit-mov-reg-imm32 (dst imm)
  "Encode MOV DST, IMM (sign-extended 32-bit immediate to 64-bit).

Selected encoding: `MOV r/m64, imm32' opcode 0xC7 /0.  This 7-byte
form is preferable to the 10-byte `MOV r64, imm64' (REX.W + 0xB8+rd
+ 8 bytes) when IMM fits in 32 bits sign-extended, which the
skeleton enforces.  Larger immediates are out of scope (the backend
loads them through the constant pool when Phase 7.1.4 lands).

Bytes: REX.W=1 [+B if DST=r8..r15] | 0xC7 | ModR/M(mod=11, reg=0,
rm=DST.low3) | imm32 little-endian."
  (let* ((dst-low3 (nelisp-cc-x86_64--reg-low3 dst))
         (rex.b    (nelisp-cc-x86_64--reg-rex-ext-p dst))
         (rex      (nelisp-cc-x86_64--rex-byte t nil nil rex.b))
         (modrm    (nelisp-cc-x86_64--modrm-byte 3 0 dst-low3)))
    (append (list rex #xC7 modrm)
            (nelisp-cc-x86_64--imm32-bytes imm))))

(defun nelisp-cc-x86_64--emit-binop-reg-reg (opcode dst src)
  "Helper for the 64-bit MR-form binary ops (ADD / SUB / CMP).
OPCODE is the single-byte opcode (0x01 / 0x29 / 0x39).

Bytes: REX.W=1 [+R if SRC=r8..r15] [+B if DST=r8..r15] | OPCODE |
ModR/M(mod=11, reg=SRC.low3, rm=DST.low3)."
  (let* ((src-low3 (nelisp-cc-x86_64--reg-low3 src))
         (dst-low3 (nelisp-cc-x86_64--reg-low3 dst))
         (rex.r    (nelisp-cc-x86_64--reg-rex-ext-p src))
         (rex.b    (nelisp-cc-x86_64--reg-rex-ext-p dst))
         (rex      (nelisp-cc-x86_64--rex-byte t rex.r nil rex.b))
         (modrm    (nelisp-cc-x86_64--modrm-byte 3 src-low3 dst-low3)))
    (list rex opcode modrm)))

(defun nelisp-cc-x86_64--emit-add-reg-reg (dst src)
  "Encode ADD DST, SRC (64-bit, MR form, opcode 0x01)."
  (nelisp-cc-x86_64--emit-binop-reg-reg #x01 dst src))

(defun nelisp-cc-x86_64--emit-sub-reg-reg (dst src)
  "Encode SUB DST, SRC (64-bit, MR form, opcode 0x29)."
  (nelisp-cc-x86_64--emit-binop-reg-reg #x29 dst src))

(defun nelisp-cc-x86_64--emit-cmp-reg-reg (a b)
  "Encode CMP A, B (64-bit, MR form, opcode 0x39).
Sets flags as A - B; does not write any register."
  (nelisp-cc-x86_64--emit-binop-reg-reg #x39 a b))

(defun nelisp-cc-x86_64--emit-imul-reg-reg (dst src)
  "Encode IMUL DST, SRC (signed 64-bit multiply, two-operand form).

Selected encoding: `IMUL r64, r/m64' opcode 0x0F 0xAF (RM form).
This is the only two-operand IMUL — it computes DST = DST * SRC and
writes the low 64 bits, with overflow flagging in CF / OF (which the
skeleton does not consume yet).

Bytes: REX.W=1 [+R if DST=r8..r15] [+B if SRC=r8..r15] | 0x0F | 0xAF
| ModR/M(mod=11, reg=DST.low3, rm=SRC.low3)."
  (let* ((dst-low3 (nelisp-cc-x86_64--reg-low3 dst))
         (src-low3 (nelisp-cc-x86_64--reg-low3 src))
         (rex.r    (nelisp-cc-x86_64--reg-rex-ext-p dst))
         (rex.b    (nelisp-cc-x86_64--reg-rex-ext-p src))
         (rex      (nelisp-cc-x86_64--rex-byte t rex.r nil rex.b))
         (modrm    (nelisp-cc-x86_64--modrm-byte 3 dst-low3 src-low3)))
    (list rex #x0F #xAF modrm)))

(defun nelisp-cc-x86_64--emit-ret ()
  "Encode RET (near return).  Single byte 0xC3."
  (list #xC3))

(defun nelisp-cc-x86_64--emit-sub-rsp-imm32 (imm)
  "Encode SUB rsp, imm32 (8-byte form).
Used by the prologue to allocate stack frame for spill slots.

Bytes: 0x48 0x81 0xEC | imm32 (little-endian)."
  (append (list #x48 #x81 #xEC)
          (nelisp-cc-x86_64--imm32-bytes imm)))

(defun nelisp-cc-x86_64--emit-add-rsp-imm32 (imm)
  "Encode ADD rsp, imm32 (8-byte form).
Used by the epilogue to release the spill frame allocated by the
prologue's matching SUB rsp.

Bytes: 0x48 0x81 0xC4 | imm32 (little-endian)."
  (append (list #x48 #x81 #xC4)
          (nelisp-cc-x86_64--imm32-bytes imm)))

(defun nelisp-cc-x86_64--emit-call-rel32 (rel32)
  "Encode CALL rel32 with REL32 the displacement from the *next*
instruction (i.e. from the byte after the 5-byte CALL itself).

Bytes: 0xE8 | rel32 little-endian."
  (cons #xE8 (nelisp-cc-x86_64--imm32-bytes rel32)))

(defun nelisp-cc-x86_64--emit-jmp-rel32 (rel32)
  "Encode JMP rel32 (unconditional near jump, 5 bytes).

Bytes: 0xE9 | rel32 little-endian."
  (cons #xE9 (nelisp-cc-x86_64--imm32-bytes rel32)))

(defconst nelisp-cc-x86_64--jcc-opcodes
  '((jo  . #x80) (jno . #x81) (jb  . #x82) (jnb . #x83)
    (je  . #x84) (jne . #x85) (jbe . #x86) (ja  . #x87)
    (js  . #x88) (jns . #x89) (jp  . #x8A) (jnp . #x8B)
    (jl  . #x8C) (jge . #x8D) (jle . #x8E) (jg  . #x8F))
  "Second-byte opcode for the `0x0F xx' near JCC family.
The condition code is encoded in the low 4 bits of the second byte;
the table names match Intel's mnemonic exactly so backend lowering
can pcase on the SSA cmp-then-branch tag without translation.")

(defun nelisp-cc-x86_64--emit-jcc-rel32 (cc rel32)
  "Encode the conditional jump JCC for condition CC with REL32 displacement.
CC is a symbol from `nelisp-cc-x86_64--jcc-opcodes'.

Bytes: 0x0F | (0x80..0x8F) | rel32 little-endian (6 bytes total)."
  (let ((opcell (assq cc nelisp-cc-x86_64--jcc-opcodes)))
    (unless opcell
      (signal 'nelisp-cc-x86_64-encoding-error
              (list :unknown-condition-code cc)))
    (cons #x0F
          (cons (cdr opcell)
                (nelisp-cc-x86_64--imm32-bytes rel32)))))

;;; Byte buffer + label resolution --------------------------------------
;;
;; `nelisp-cc-x86_64--buffer' is a small mutable accumulator: a reverse-
;; ordered byte list (so prepend is O(1)), a forward offset counter, a
;; label table for backward references, and a fixup table for forward
;; references that we patch at finalize time.
;;
;; Patching policy: every fixup is a 4-byte rel32 displacement at a
;; recorded byte offset, computed as (label-target-offset - (fixup-
;; offset + 4)).  This matches CALL / JMP / JCC rel32 encoding.  Other
;; widths (rel8 / abs64) are out of scope for the skeleton.

(cl-defstruct (nelisp-cc-x86_64--buffer
               (:constructor nelisp-cc-x86_64--buffer-make)
               (:copier nil))
  "Mutable byte accumulator for the x86_64 backend.

BYTES is a *reversed* list of integers in [0, 255] — the head is the
most recently emitted byte.  `finalize' reverses + flattens it into a
unibyte vector.

OFFSET is the running byte position at the head of the buffer; it is
incremented on every emit and is the natural target / source for
labels and fixups.

LABELS is an alist `((NAME . OFFSET) ...)' — defining a label twice
is a programming error and signals
`nelisp-cc-x86_64-encoding-error'.

FIXUPS is an alist `((BYTE-OFFSET . LABEL) ...)' — each entry says
\"there is a 4-byte rel32 displacement starting at BYTE-OFFSET that
must be patched to point at LABEL\".  Resolution happens in
`finalize' (or explicitly via `resolve-fixups')."
  (bytes nil)
  (offset 0)
  (labels nil)
  (fixups nil))

(defun nelisp-cc-x86_64--buffer-emit-byte (buf b)
  "Append the integer B (0-255) to BUF.  Mutates and returns BUF."
  (unless (and (integerp b) (<= 0 b) (< b 256))
    (signal 'nelisp-cc-x86_64-encoding-error
            (list :byte-out-of-range b)))
  (push b (nelisp-cc-x86_64--buffer-bytes buf))
  (cl-incf (nelisp-cc-x86_64--buffer-offset buf))
  buf)

(defun nelisp-cc-x86_64--buffer-emit-bytes (buf bs)
  "Append every byte in BS (a list of integers) to BUF in order.
Mutates and returns BUF."
  (dolist (b bs)
    (nelisp-cc-x86_64--buffer-emit-byte buf b))
  buf)

(defun nelisp-cc-x86_64--buffer-define-label (buf name)
  "Mark NAME as resolved at BUF's current OFFSET.

Signals `nelisp-cc-x86_64-encoding-error' if NAME has already been
defined — duplicate labels in the same buffer are unambiguously a
codegen bug."
  (when (assq name (nelisp-cc-x86_64--buffer-labels buf))
    (signal 'nelisp-cc-x86_64-encoding-error
            (list :duplicate-label name)))
  (push (cons name (nelisp-cc-x86_64--buffer-offset buf))
        (nelisp-cc-x86_64--buffer-labels buf))
  buf)

(defun nelisp-cc-x86_64--buffer-emit-fixup (buf instr-bytes label
                                                fixup-rel-offset)
  "Emit INSTR-BYTES into BUF and record a forward fixup against LABEL.

FIXUP-REL-OFFSET is the byte offset *within INSTR-BYTES* at which the
4-byte rel32 displacement begins (e.g. 1 for a JMP rel32 — the 0xE9
opcode is byte 0, rel32 starts at byte 1; 2 for a JCC rel32 — bytes
0x0F + opcode then rel32 at byte 2).  The placeholder displacement
emitted is 0; `resolve-fixups' patches the actual rel32 in place at
finalize time.

The fixup is recorded as `(ABS-OFFSET . LABEL)' where ABS-OFFSET is
the absolute byte position in the buffer where the 4-byte rel32
begins.  This matches `--patch-rel32' below."
  (let ((before (nelisp-cc-x86_64--buffer-offset buf)))
    (nelisp-cc-x86_64--buffer-emit-bytes buf instr-bytes)
    (push (cons (+ before fixup-rel-offset) label)
          (nelisp-cc-x86_64--buffer-fixups buf))
    buf))

(defun nelisp-cc-x86_64--patch-rel32 (vec abs-offset rel32)
  "Patch VEC's 4 bytes at ABS-OFFSET with REL32 little-endian.
VEC is a unibyte vector and is mutated in place."
  (let ((u (logand rel32 #xFFFFFFFF)))
    (aset vec    abs-offset       (logand u #xFF))
    (aset vec (+ abs-offset 1)    (logand (ash u -8) #xFF))
    (aset vec (+ abs-offset 2)    (logand (ash u -16) #xFF))
    (aset vec (+ abs-offset 3)    (logand (ash u -24) #xFF))))

(defun nelisp-cc-x86_64--buffer-resolve-fixups (buf vec)
  "Apply every fixup recorded in BUF to the unibyte vector VEC.

Each fixup `(ABS-OFFSET . LABEL)' resolves to the displacement
`LABEL.offset - (ABS-OFFSET + 4)' (the +4 because rel32 is measured
from the byte *after* the displacement field).

Signals `nelisp-cc-x86_64-unresolved-label' when a fixup references
a LABEL that was never defined — the skeleton does not silently emit
a zero displacement, because the resulting code would jump to itself
and the bug would not surface until execution.  The error names the
offending label so the offending codegen path is obvious."
  (let ((labels (nelisp-cc-x86_64--buffer-labels buf)))
    (dolist (fix (nelisp-cc-x86_64--buffer-fixups buf))
      (let* ((abs-offset (car fix))
             (label (cdr fix))
             (cell (assq label labels)))
        (unless cell
          (signal 'nelisp-cc-x86_64-unresolved-label
                  (list :label label :at-offset abs-offset)))
        (let ((rel32 (- (cdr cell) (+ abs-offset 4))))
          (nelisp-cc-x86_64--patch-rel32 vec abs-offset rel32))))
    vec))

(defun nelisp-cc-x86_64--buffer-finalize (buf)
  "Resolve all forward fixups in BUF and return the final unibyte vector.

The output is suitable to be written into an mmap'ed PROT_EXEC page
(Phase 7.1.4) and passed to `nelisp-runtime' for execution.  Phase
7.1.2 does not exercise that path — the skeleton stops here, and the
caller is expected to inspect the bytes only."
  (let* ((rev (nelisp-cc-x86_64--buffer-bytes buf))
         (n (length rev))
         (vec (make-vector n 0)))
    ;; Forward-write — rev is in reverse emit order, so vec[n-1-i] = rev[i].
    (let ((i 0))
      (dolist (b rev)
        (aset vec (- n 1 i) b)
        (cl-incf i)))
    (nelisp-cc-x86_64--buffer-resolve-fixups buf vec)
    vec))

;;; SSA → x86_64 codegen (skeleton subset) ------------------------------
;;
;; The codegen converts a *register-allocated* SSA function into raw
;; bytes.  It walks blocks in reverse-postorder (matching the
;; allocator's linearisation) and dispatches each instruction to a
;; per-opcode lower helper.
;;
;; *Register-allocated* means: every SSA value referenced by an
;; instruction must have an entry in ALLOC-STATE, mapping it to either
;; a virtual register name (which `resolve-virtual' turns into a
;; physical reg) or the keyword `:spill' (which the skeleton does
;; *not* yet support — it signals).
;;
;; The skeleton implements 6 opcodes:
;;   :const     — load literal into the def's register
;;   :load-var  — placeholder, emitted as a 0-immediate const so phase
;;                7.5 can patch in the actual symbol-value load later
;;   :store-var — placeholder, emitted as no-op (the def value is the
;;                source operand's register, since SSA store has no
;;                rename)
;;   :call      — placeholder, emit CALL rel32 with a fixup against the
;;                callee's :unresolved meta tag (Phase 7.5 patches this)
;;   :branch    — emit CMP + JE/JNE based on phi-arm meta
;;   :return    — move return value into rax then RET
;;
;; phi nodes must be lowered out of the SSA before codegen — the
;; skeleton signals `nelisp-cc-x86_64-unsupported-opcode' if it sees
;; one.  A trivial copy-out pass (per-edge phi → MOV) belongs in a
;; companion module `nelisp-cc-phi-out' which Phase 7.1.4 will land.

(cl-defstruct (nelisp-cc-x86_64--codegen
               (:constructor nelisp-cc-x86_64--codegen-make)
               (:copier nil))
  "Per-call codegen state, distinct from the byte buffer.

FUNCTION is the input `nelisp-cc--ssa-function'.  ALLOC-STATE is the
linear-scan output (an alist `(VID . REGISTER-OR-:spill)').  BUFFER
is a fresh `nelisp-cc-x86_64--buffer'.  The reverse-postorder block
list RPO is cached so we don't re-derive it on every BLOCK lookup.

SLOT-ALIST is the spill stack-slot map ((VID . OFFSET) ...) — see
`nelisp-cc--allocate-stack-slots'.  FRAME-SIZE is the total bytes
allocated for spill slots (already 16-aligned).

CALL-FIXUPS is an alist `((BYTE-OFFSET . CALLEE-SYMBOL) ...)' that
the skeleton accumulates for unresolved :call sites — Phase 7.5 will
walk this list and patch each rel32 against the callee's actual
address (via `nelisp-defs-index')."
  (function nil :read-only t)
  (alloc-state nil :read-only t)
  (buffer nil :read-only t)
  (rpo nil)
  (slot-alist nil)
  (frame-size 0)
  (call-fixups nil))

;;; Phase 7.1 T15 — spill/reload helpers ----------------------------
;;
;; Strategy: a single dedicated *scratch* register (`rax') buffers
;; spilled operands and spilled defs.  rax is outside the linear-
;; scan pool (the pool is r0..r7 mapped to rdi/rsi/rdx/rcx/r8-r11)
;; and is also the System V return register, so a brief use of it
;; for spill marshalling never collides with the allocator's
;; assignments.
;;
;; Per-instruction protocol:
;;   - operand spill : MOV rax, [rbp - off]; substitute rax for the
;;                     operand's "physical" slot in the per-opcode
;;                     emit.
;;   - def spill     : the helper computes the result into rax (or
;;                     into the def's "physical slot") and emits
;;                     MOV [rbp - off], rax after the compute.
;;
;; The simple cases (`const' / `load-var' / `store-var' / `branch' /
;; `return' / `call' / `copy') need at most ONE scratch register
;; per instruction at any given moment, so the single-rax design
;; suffices.  Multi-spilled binary ops are out of scope for the MVP
;; — the lowering pass currently funnels arithmetic through `:call'
;; primitives, which load each operand into its own argument
;; register independently (see `--lower-call' below) so spill of
;; multiple operands is handled via the per-arg load loop.

(defconst nelisp-cc-x86_64--spill-scratch 'rax
  "Register reserved as the spill-slot scratch.
Outside the linear-scan pool (which uses r0..r7) so a temporary
load/store never collides with an allocator assignment.")

(defun nelisp-cc-x86_64--emit-load-spill (buf dst-reg offset)
  "Emit MOV DST-REG, [rbp - OFFSET] (8-byte load from a spill slot).

OFFSET is a positive byte distance from rbp (matches the `spill
slot' offset returned by `nelisp-cc--allocate-stack-slots').  The
encoding is `MOV r64, r/m64' opcode 0x8B with mod=10 (disp32),
rm=5 (rbp-relative).

Bytes: REX.W=1 [+R if DST=r8..r15] | 0x8B | ModR/M(mod=10, reg=DST.low3,
rm=5 = rbp) | disp32 = -OFFSET (sign-extended)."
  (let* ((dst-low3 (nelisp-cc-x86_64--reg-low3 dst-reg))
         (rex.r    (nelisp-cc-x86_64--reg-rex-ext-p dst-reg))
         (rex      (nelisp-cc-x86_64--rex-byte t rex.r nil nil))
         (modrm    (nelisp-cc-x86_64--modrm-byte 2 dst-low3 5))
         (disp32   (nelisp-cc-x86_64--imm32-bytes (- offset))))
    (nelisp-cc-x86_64--buffer-emit-bytes
     buf (append (list rex #x8B modrm) disp32))))

(defun nelisp-cc-x86_64--emit-store-spill (buf src-reg offset)
  "Emit MOV [rbp - OFFSET], SRC-REG (8-byte store to a spill slot).

OFFSET is a positive byte distance from rbp.  Encoding: `MOV r/m64,
r64' opcode 0x89 with mod=10 (disp32), rm=5 (rbp-relative).

Bytes: REX.W=1 [+R if SRC=r8..r15] | 0x89 | ModR/M(mod=10,
reg=SRC.low3, rm=5) | disp32 = -OFFSET."
  (let* ((src-low3 (nelisp-cc-x86_64--reg-low3 src-reg))
         (rex.r    (nelisp-cc-x86_64--reg-rex-ext-p src-reg))
         (rex      (nelisp-cc-x86_64--rex-byte t rex.r nil nil))
         (modrm    (nelisp-cc-x86_64--modrm-byte 2 src-low3 5))
         (disp32   (nelisp-cc-x86_64--imm32-bytes (- offset))))
    (nelisp-cc-x86_64--buffer-emit-bytes
     buf (append (list rex #x89 modrm) disp32))))

(defun nelisp-cc-x86_64--reg-or-spill (cg value)
  "Resolve VALUE to either a physical register or a spill descriptor.

Returns either:
  - (:reg PHYS-REG)        — value lives in PHYS-REG
  - (:spill OFFSET)        — value lives in [rbp - OFFSET]

Allocator assignments funnel through here.  This is the spill-aware
sibling of `--reg-of'; existing call sites that cannot tolerate a
spilled operand keep using `--reg-of' (which still signals)."
  (let* ((alloc (nelisp-cc-x86_64--codegen-alloc-state cg))
         (vid (nelisp-cc--ssa-value-id value))
         (cell (assq vid alloc)))
    (unless cell
      (signal 'nelisp-cc-x86_64-encoding-error
              (list :unallocated-value vid)))
    (let ((reg (cdr cell)))
      (cond
       ((eq reg :spill)
        (let ((offset (nelisp-cc--stack-slot-of
                       (nelisp-cc-x86_64--codegen-slot-alist cg) vid)))
          (unless offset
            (signal 'nelisp-cc-x86_64-encoding-error
                    (list :spilled-without-slot vid)))
          (list :spill offset)))
       (t (list :reg (nelisp-cc-x86_64-resolve-virtual reg)))))))

(defun nelisp-cc-x86_64--materialise-operand (cg value)
  "Ensure VALUE's bits live in a register and return that register.

If VALUE is in a register: return it (no instruction emitted).
If VALUE is spilled:        emit MOV rax, [rbp - off] and return rax.

This is the canonical pre-amble for any per-opcode lower helper that
needs an operand in a register."
  (let ((slot (nelisp-cc-x86_64--reg-or-spill cg value))
        (buf  (nelisp-cc-x86_64--codegen-buffer cg)))
    (pcase slot
      (`(:reg ,r) r)
      (`(:spill ,off)
       (nelisp-cc-x86_64--emit-load-spill
        buf nelisp-cc-x86_64--spill-scratch off)
       nelisp-cc-x86_64--spill-scratch))))

(defun nelisp-cc-x86_64--writeback-def (cg def src-reg)
  "After computing DEF's value into SRC-REG, route it to its home.

If DEF is in a register: emit MOV phys-reg, SRC-REG (elided when
phys-reg already equals SRC-REG).
If DEF is spilled:        emit MOV [rbp - off], SRC-REG."
  (when def
    (let ((slot (nelisp-cc-x86_64--reg-or-spill cg def))
          (buf  (nelisp-cc-x86_64--codegen-buffer cg)))
      (pcase slot
        (`(:reg ,r)
         (unless (eq r src-reg)
           (nelisp-cc-x86_64--buffer-emit-bytes
            buf (nelisp-cc-x86_64--emit-mov-reg-reg r src-reg))))
        (`(:spill ,off)
         (nelisp-cc-x86_64--emit-store-spill buf src-reg off))))))

(defun nelisp-cc-x86_64--def-target (cg def)
  "Pick a register to compute DEF's value into.
For register-allocated defs return their physical register.
For spilled defs return the scratch (rax) so callers compute into
it and then `--writeback-def' to the slot."
  (let ((slot (nelisp-cc-x86_64--reg-or-spill cg def)))
    (pcase slot
      (`(:reg ,r) r)
      (`(:spill ,_) nelisp-cc-x86_64--spill-scratch))))

(defun nelisp-cc-x86_64--reg-of (alloc-state value)
  "Return the *physical* x86_64 register assigned to VALUE.

VALUE is a `nelisp-cc--ssa-value'.  ALLOC-STATE is a linear-scan
assignments alist.  Resolves the virtual-register layer
transparently: VALUE's allocator entry holds a virtual register
symbol like `r0', and this helper returns the physical mapping
(`rdi' for `r0' in the default table).

This *non-spill-aware* variant is preserved for legacy callers that
encode an instruction whose operand happens to never be spilled
(e.g. branch-condition values that the allocator pinned).  The
spill-aware sibling is `--reg-or-spill', which returns a tagged
descriptor so callers can emit a load before use; T15 added that
helper plus `--materialise-operand' / `--writeback-def' which most
new lowerers use.  A `:spill' assignment from this entry-point
signals `nelisp-cc-x86_64-unsupported-opcode'.

A missing assignment (VALUE absent from ALLOC-STATE) signals
`nelisp-cc-x86_64-encoding-error' since that means the SSA was not
fully register-allocated."
  (let* ((vid (nelisp-cc--ssa-value-id value))
         (cell (assq vid alloc-state)))
    (unless cell
      (signal 'nelisp-cc-x86_64-encoding-error
              (list :unallocated-value vid)))
    (let ((reg (cdr cell)))
      (cond
       ((eq reg :spill)
        (signal 'nelisp-cc-x86_64-unsupported-opcode
                (list :spill-not-implemented vid)))
       (t
        (nelisp-cc-x86_64-resolve-virtual reg))))))

(defun nelisp-cc-x86_64--lower-const (cg instr)
  "Lower an SSA :const INSTR — load its META :literal into the def's reg.

The skeleton handles three literal shapes:
  - integer in [-2^31, 2^31-1]   → MOV r64, imm32 (sign-extended)
  - nil                          → MOV r64, 0
  - t                            → MOV r64, 1 (NeLisp tagging happens
                                    in Phase 7.5 — the skeleton just
                                    parks a non-zero word in the slot)

Anything else (string / vector / symbol literals) signals
`nelisp-cc-x86_64-unsupported-opcode' — the constant-pool path lands
in Phase 7.1.4.

Spill-aware: if DEF is a spilled value the literal is materialised
into the scratch register and then stored to its slot via
`--writeback-def'."
  (let* ((buf   (nelisp-cc-x86_64--codegen-buffer cg))
         (def   (nelisp-cc--ssa-instr-def instr))
         (meta  (nelisp-cc--ssa-instr-meta instr))
         (lit   (plist-get meta :literal))
         (dst   (nelisp-cc-x86_64--def-target cg def))
         (imm   (cond
                 ((null lit) 0)
                 ((eq lit t) 1)
                 ((integerp lit) lit)
                 (t (signal 'nelisp-cc-x86_64-unsupported-opcode
                            (list :unsupported-literal lit))))))
    (nelisp-cc-x86_64--buffer-emit-bytes
     buf (nelisp-cc-x86_64--emit-mov-reg-imm32 dst imm))
    (nelisp-cc-x86_64--writeback-def cg def dst)))

(defun nelisp-cc-x86_64--lower-load-var (cg instr)
  "Lower an SSA :load-var INSTR — placeholder MOV r64, 0.

The skeleton does not yet have a symbol-value-cell load; Phase 7.5
will patch this site against `nelisp-defs-index' so the eventual
encoding becomes `MOV reg, [rip+disp32]' (RIP-relative load of the
symbol-value cell).  Until then the skeleton emits MOV r64, 0 as a
safe placeholder — running this code would simply read 0, but the
skeleton never executes the bytes.

Spill-aware: when DEF is spilled the placeholder zero lands in the
scratch register first, then is stored to the spill slot."
  (let* ((buf   (nelisp-cc-x86_64--codegen-buffer cg))
         (def   (nelisp-cc--ssa-instr-def instr))
         (dst   (nelisp-cc-x86_64--def-target cg def)))
    (nelisp-cc-x86_64--buffer-emit-bytes
     buf (nelisp-cc-x86_64--emit-mov-reg-imm32 dst 0))
    (nelisp-cc-x86_64--writeback-def cg def dst)))

(defun nelisp-cc-x86_64--lower-store-var (cg instr)
  "Lower an SSA :store-var INSTR — MOV def-reg, src-reg with spill marshalling.

The store has a single operand (the new value) and an SSA def value
(the same value, in SSA form `(setq x EXPR)' returns EXPR).  Phase
7.5 patches this site to additionally store into the symbol-value
cell.

Spill-aware: source and / or destination may be spilled.  We
materialise the operand into a register (loading from the slot via
the scratch when spilled) and then route it to the def via
`--writeback-def'."
  (let* ((def      (nelisp-cc--ssa-instr-def instr))
         (operands (nelisp-cc--ssa-instr-operands instr))
         (src-val  (car operands))
         (src-reg  (nelisp-cc-x86_64--materialise-operand cg src-val)))
    (nelisp-cc-x86_64--writeback-def cg def src-reg)))

(defun nelisp-cc-x86_64--lower-call (cg instr)
  "Lower an SSA :call INSTR — placeholder CALL rel32 with :unresolved fixup.

ABI lowering: the skeleton assumes operands ≤6 and emits MOV
instructions to land each operand in the matching System V
argument register (rdi, rsi, rdx, rcx, r8, r9) before CALL.
Operands already in their target register are skipped.  The
return value is fetched from rax into the def's register after CALL
(again, MOV is elided if rax already is the def's register).

Spill-aware: a spilled operand is loaded directly into its argument
register from the slot; a spilled def is captured from rax into the
slot via `--writeback-def'.

The CALL itself is emitted with a 0 displacement and a CALL-FIXUPS
entry — the actual address is patched in Phase 7.5 once the callee
has been resolved against `nelisp-defs-index'.  The fixup keys on
the callee symbol (META :fn) so the patcher can look it up."
  (let* ((buf      (nelisp-cc-x86_64--codegen-buffer cg))
         (def      (nelisp-cc--ssa-instr-def instr))
         (operands (nelisp-cc--ssa-instr-operands instr))
         (meta     (nelisp-cc--ssa-instr-meta instr))
         (callee   (plist-get meta :fn))
         (n        (length operands)))
    (when (> n (length nelisp-cc-x86_64--int-arg-regs))
      (signal 'nelisp-cc-x86_64-unsupported-opcode
              (list :stack-arg-spill-not-implemented n)))
    ;; Move each operand into its argument register (load from slot
    ;; when spilled, MOV reg→arg-reg when register-allocated, skip
    ;; when already in the right register).
    (cl-loop for op in operands
             for arg-reg in nelisp-cc-x86_64--int-arg-regs
             for slot = (nelisp-cc-x86_64--reg-or-spill cg op)
             do (pcase slot
                  (`(:reg ,r)
                   (unless (eq r arg-reg)
                     (nelisp-cc-x86_64--buffer-emit-bytes
                      buf (nelisp-cc-x86_64--emit-mov-reg-reg
                           arg-reg r))))
                  (`(:spill ,off)
                   (nelisp-cc-x86_64--emit-load-spill
                    buf arg-reg off))))
    ;; Emit CALL rel32 with placeholder 0 displacement — the fixup
    ;; carries the unresolved callee symbol so Phase 7.5 can patch
    ;; against `nelisp-defs-index'.
    (let ((before-call (nelisp-cc-x86_64--buffer-offset buf)))
      (nelisp-cc-x86_64--buffer-emit-bytes
       buf (nelisp-cc-x86_64--emit-call-rel32 0))
      (push (cons (+ before-call 1) callee)
            (nelisp-cc-x86_64--codegen-call-fixups cg)))
    ;; Route the return value (currently in rax) to the def.
    (when def
      (nelisp-cc-x86_64--writeback-def
       cg def nelisp-cc-x86_64--return-reg))))

(defun nelisp-cc-x86_64--lower-branch (cg instr)
  "Lower an SSA :branch INSTR — CMP cond,0 + JE else / fallthrough then.

The branch carries (META :then THEN-BLOCK-ID :else ELSE-BLOCK-ID) and
a single operand (the condition value).  The skeleton emits

    CMP cond, 0    ; flags = cond - 0
    JE  L_else     ; jump if zero (i.e. nil)
    ;; fallthrough = then-block

…assuming the codegen will lay the THEN block immediately after the
branch.  When that ordering does not hold (e.g. THEN is later in
RPO), Phase 7.1.4 will insert an unconditional JMP to THEN before
the JE.  The skeleton emits the simpler form so the encoding test
matrix is small.

Note: x86_64 has no `CMP r64, imm0' shortcut shorter than `TEST
r64,r64' (two bytes 0x48 0x85 + ModR/M).  The skeleton uses TEST as
the canonical zero-test because it is one byte shorter than `CMP
r64, imm32 0' and matches what gcc -O2 emits for `if (x)'."
  (let* ((buf      (nelisp-cc-x86_64--codegen-buffer cg))
         (operands (nelisp-cc--ssa-instr-operands instr))
         (meta     (nelisp-cc--ssa-instr-meta instr))
         (else-id  (plist-get meta :else))
         (cond-val (car operands))
         (cond-reg (nelisp-cc-x86_64--materialise-operand cg cond-val)))
    ;; TEST cond, cond — sets ZF if cond is 0 (NeLisp nil).
    ;; Encoded as 0x85 /r in MR form (just like the binop family).
    (nelisp-cc-x86_64--buffer-emit-bytes
     buf (nelisp-cc-x86_64--emit-binop-reg-reg #x85 cond-reg cond-reg))
    ;; JE label_else — fixup against a synthetic label name keyed on
    ;; the else block id so the per-block label-define step can match.
    (let ((else-label (intern (format "L_block_%d" else-id))))
      (nelisp-cc-x86_64--buffer-emit-fixup
       buf (nelisp-cc-x86_64--emit-jcc-rel32 'je 0)
       else-label
       2))))

(defun nelisp-cc-x86_64--lower-copy (cg instr)
  "Lower a `:copy' SSA instruction inserted by phi resolution.

The instruction has one operand (the source value) and one def (the
phi's destination value).  Emits a register-to-register move,
honouring spill slots on either end.  After T15 phi resolution, the
backend sees one or more `:copy' nodes per predecessor block (one
per phi destination) just before the block's terminator."
  (let* ((def      (nelisp-cc--ssa-instr-def instr))
         (operands (nelisp-cc--ssa-instr-operands instr))
         (src-val  (car operands))
         (src-reg  (nelisp-cc-x86_64--materialise-operand cg src-val)))
    (nelisp-cc-x86_64--writeback-def cg def src-reg)))

(defun nelisp-cc-x86_64--lower-jump (cg instr)
  "Lower an SSA :jump INSTR — unconditional JMP rel32 to the successor block.

The current block has exactly one successor (the IR's `:jump'
terminator semantic).  The skeleton emits JMP rel32 with a fixup
against the successor's synthetic label.  When the successor is the
*next* block in RPO the fixup resolves to a 0 displacement at
finalize time, which is technically a valid (but redundant) JMP — a
peephole pass will elide it later."
  (let* ((buf     (nelisp-cc-x86_64--codegen-buffer cg))
         (block   (nelisp-cc--ssa-instr-block instr))
         (succs   (nelisp-cc--ssa-block-successors block))
         (succ    (car succs)))
    (unless succ
      (signal 'nelisp-cc-x86_64-encoding-error
              (list :jump-with-no-successor
                    (nelisp-cc--ssa-block-id block))))
    (let ((target-label
           (intern (format "L_block_%d" (nelisp-cc--ssa-block-id succ)))))
      (nelisp-cc-x86_64--buffer-emit-fixup
       buf (nelisp-cc-x86_64--emit-jmp-rel32 0)
       target-label
       1))))

(defun nelisp-cc-x86_64--lower-return (cg instr)
  "Lower an SSA :return INSTR — MOV rax, value-reg + epilogue + RET.

If the value already lives in rax (the allocator may coalesce when
the last use is the return), the MOV is elided.  The epilogue
(`ADD rsp, frame_size; POP rbp') matches the prologue T15 emits at
function entry; both are gated on FRAME-SIZE, so a spill-free
function still produces a `MOV rax, X` + `RET` pair without extra
bytes — preserving the existing T9 byte-level golden tests.

Spill-aware: a spilled return-value operand is loaded directly into
rax via the spill-load helper (no MOV needed afterwards because the
load lands the value directly in the return register)."
  (let* ((buf      (nelisp-cc-x86_64--codegen-buffer cg))
         (frame    (nelisp-cc-x86_64--codegen-frame-size cg))
         (operands (nelisp-cc--ssa-instr-operands instr))
         (rval     (car operands)))
    ;; Bring the return value into rax.
    (let ((slot (nelisp-cc-x86_64--reg-or-spill cg rval)))
      (pcase slot
        (`(:reg ,src-reg)
         (unless (eq src-reg nelisp-cc-x86_64--return-reg)
           (nelisp-cc-x86_64--buffer-emit-bytes
            buf (nelisp-cc-x86_64--emit-mov-reg-reg
                 nelisp-cc-x86_64--return-reg src-reg))))
        (`(:spill ,off)
         (nelisp-cc-x86_64--emit-load-spill
          buf nelisp-cc-x86_64--return-reg off))))
    ;; Epilogue: tear down the spill frame (when non-empty) and
    ;; restore rbp.  Order is the reverse of the prologue.  When
    ;; FRAME-SIZE is 0 we still need to POP rbp to balance the
    ;; entry PUSH rbp.
    (when (and frame (> frame 0))
      (nelisp-cc-x86_64--buffer-emit-bytes
       buf (nelisp-cc-x86_64--emit-add-rsp-imm32 frame)))
    (when (and frame (>= frame 0))
      (nelisp-cc-x86_64--buffer-emit-byte buf #x5D)) ; POP rbp
    (nelisp-cc-x86_64--buffer-emit-bytes buf (nelisp-cc-x86_64--emit-ret))))

(defun nelisp-cc-x86_64--lower-instr (cg instr)
  "Dispatch a single SSA INSTR to its per-opcode lower helper.

Skeleton subset — opcodes outside the supported set raise
`nelisp-cc-x86_64-unsupported-opcode'.  phi nodes specifically must
be lowered out by a phi-out pass before this dispatch sees them."
  (pcase (nelisp-cc--ssa-instr-opcode instr)
    ('const     (nelisp-cc-x86_64--lower-const cg instr))
    ('load-var  (nelisp-cc-x86_64--lower-load-var cg instr))
    ('store-var (nelisp-cc-x86_64--lower-store-var cg instr))
    ('call      (nelisp-cc-x86_64--lower-call cg instr))
    ('copy      (nelisp-cc-x86_64--lower-copy cg instr))
    ('branch    (nelisp-cc-x86_64--lower-branch cg instr))
    ('jump      (nelisp-cc-x86_64--lower-jump cg instr))
    ('return    (nelisp-cc-x86_64--lower-return cg instr))
    ('phi
     (signal 'nelisp-cc-x86_64-unsupported-opcode
             (list :phi-must-be-lowered-out-before-codegen
                   (nelisp-cc--ssa-instr-id instr))))
    (_
     (signal 'nelisp-cc-x86_64-unsupported-opcode
             (list :unknown-opcode
                   (nelisp-cc--ssa-instr-opcode instr)
                   (nelisp-cc--ssa-instr-id instr))))))

(defun nelisp-cc-x86_64--emit-prologue (buf frame-size)
  "Emit the System V AMD64 prologue into BUF.

Sequence:
  PUSH rbp                ; 0x55                       (1 byte)
  MOV  rbp, rsp           ; 0x48 0x89 0xE5             (3 bytes)
  [SUB rsp, FRAME-SIZE]   ; 0x48 0x81 0xEC <imm32>     (7 bytes, omitted
                                                         when FRAME-SIZE = 0)

Total prologue size: 4 bytes (no spill) or 11 bytes (with spill).
The matching epilogue is emitted by `--lower-return'."
  (nelisp-cc-x86_64--buffer-emit-byte buf #x55) ; PUSH rbp
  (nelisp-cc-x86_64--buffer-emit-bytes
   buf (nelisp-cc-x86_64--emit-mov-reg-reg 'rbp 'rsp))
  (when (and frame-size (> frame-size 0))
    (nelisp-cc-x86_64--buffer-emit-bytes
     buf (nelisp-cc-x86_64--emit-sub-rsp-imm32 frame-size))))

(defun nelisp-cc-x86_64--compile-internal (function alloc-state)
  "Shared body of `--compile' and `--compile-with-meta'.

Runs T15 phi resolution + stack-slot allocation, emits the
prologue, walks RPO emitting per-instruction byte sequences, and
returns the live `nelisp-cc-x86_64--codegen' struct so the public
entries can finalise the buffer / extract fixups."
  ;; T15 step (a): lower phi nodes out into per-predecessor :copy
  ;; instructions before any byte emission.  This mutates FUNCTION
  ;; in place; callers that re-use the SSA value should pass a
  ;; freshly built function (the production pipeline does — the
  ;; AST→SSA frontend builds a new IR per compile).
  (nelisp-cc--resolve-phis function)
  (let* ((slots-pair (nelisp-cc--allocate-stack-slots alloc-state))
         (slot-alist (car slots-pair))
         (frame-size (cdr slots-pair))
         (buf (nelisp-cc-x86_64--buffer-make))
         (cg  (nelisp-cc-x86_64--codegen-make
               :function function
               :alloc-state alloc-state
               :buffer buf
               :slot-alist slot-alist
               :frame-size frame-size)))
    ;; T15 step (b): function prologue.  PUSH rbp + MOV rbp, rsp
    ;; always; SUB rsp, FRAME-SIZE only when there is a spill frame.
    (nelisp-cc-x86_64--emit-prologue buf frame-size)
    ;; RPO matches the linear-scan linearisation, so block-relative
    ;; offsets are stable between allocator and backend.
    (let ((rpo (nelisp-cc--ssa--reverse-postorder function)))
      (setf (nelisp-cc-x86_64--codegen-rpo cg) rpo)
      (dolist (blk rpo)
        ;; Define the block's label at the buffer's current offset so
        ;; branches can target it.
        (let ((lbl (intern (format "L_block_%d"
                                   (nelisp-cc--ssa-block-id blk)))))
          (nelisp-cc-x86_64--buffer-define-label buf lbl))
        (dolist (instr (nelisp-cc--ssa-block-instrs blk))
          (nelisp-cc-x86_64--lower-instr cg instr))))
    cg))

(defun nelisp-cc-x86_64-compile (function alloc-state)
  "Compile FUNCTION (an `nelisp-cc--ssa-function') with ALLOC-STATE
(the linear-scan output, an alist of (VALUE-ID . REGISTER-OR-:spill))
to a unibyte vector of x86_64 machine code bytes.

T15 SHIPPED — emits a System V AMD64 prologue (PUSH rbp + MOV rbp,
rsp, plus SUB rsp when spill slots are present), lowers `:phi'
nodes out into per-predecessor `:copy' instructions, and threads
spilled values through stack slots on rbp-relative addresses.  The
matching epilogue is woven into each `:return' instruction so
multi-RET lambdas tear down the frame correctly even when the
allocator assigns different slot counts to alternate paths.

The result is suitable to be written into an mmap'ed PROT_EXEC page
and executed via Phase 7.5.1 FFI bridge.  Anything the backend
cannot handle (e.g. unsupported literals, >6 call args) signals
`nelisp-cc-x86_64-unsupported-opcode' immediately rather than
miscompiling silently."
  (let ((cg (nelisp-cc-x86_64--compile-internal function alloc-state)))
    (nelisp-cc-x86_64--buffer-finalize
     (nelisp-cc-x86_64--codegen-buffer cg))))

(defun nelisp-cc-x86_64-compile-with-meta (function alloc-state)
  "Like `nelisp-cc-x86_64-compile' but return (BYTES . CALL-FIXUPS).

CALL-FIXUPS is the alist of (BYTE-OFFSET . CALLEE-SYMBOL) entries that
Phase 7.5 will consume to patch each :call site against
`nelisp-defs-index'.  The byte offsets are *post-prologue*, i.e.
shifted by the prologue size.  Phase 7.5 patcher does not need to
account for the prologue specifically — the offsets index into the
final byte vector directly."
  (let ((cg (nelisp-cc-x86_64--compile-internal function alloc-state)))
    (cons (nelisp-cc-x86_64--buffer-finalize
           (nelisp-cc-x86_64--codegen-buffer cg))
          (nreverse (nelisp-cc-x86_64--codegen-call-fixups cg)))))

(provide 'nelisp-cc-x86_64)
;;; nelisp-cc-x86_64.el ends here
