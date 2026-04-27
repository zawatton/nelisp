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

;; T43 Phase 7.5.6 — declared here (loaded lazily by `compile-with-link')
;; to break a circular dependency: nelisp-cc-callees requires nelisp-cc
;; for the SSA accessors, and we require it back for the link step.
(declare-function nelisp-cc--link-unresolved-calls "nelisp-cc-callees"
                  (bytes call-fixups &optional backend))
;; T84 Phase 7.5 wire — declared lazily because `nelisp-cc-callees'
;; requires `nelisp-cc' which is the parent of this module.  The
;; new `compile-and-link' pipeline uses these helpers via runtime
;; (require 'nelisp-cc-callees).
(declare-function nelisp-cc-callees-known-p "nelisp-cc-callees"
                  (sym &optional backend))
(declare-function nelisp-cc-callees-trampoline-bytes "nelisp-cc-callees"
                  (sym &optional backend))
(declare-function nelisp-cc-callees--collect-cell-symbols
                  "nelisp-cc-callees" (function))
(declare-function nelisp-cc-callees--collect-inner-functions
                  "nelisp-cc-callees" (function))
(declare-function nelisp-cc-callees--walk-calls
                  "nelisp-cc-callees" (function visit))
(declare-function nelisp-cc-callees-rewrite-setq-vars
                  "nelisp-cc-callees" (function))
(declare-function nelisp-cc-callees--rewrite-setq-vars
                  "nelisp-cc-callees" (function))
(declare-function nelisp-cc-callees--mark-tail-self-calls
                  "nelisp-cc-callees" (function letrec-name))

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

;;; T96 Phase 7.1.5 — inline primitive emit helpers ----------------------
;;
;; The following helpers exist to support inline expansion of
;; primitives in the hot path (Doc 28 v2 §5.2 timing gate).  They cover
;; the operand shapes the inliner emits:
;;
;;   - in-place ops with a memory operand (ADD/SUB/CMP/IMUL r, [rbp-off])
;;     so we can fold a spilled operand into the binop without a
;;     separate spill load,
;;   - INC / DEC reg (for 1+ / 1-),
;;   - XOR reg, reg (zeroing — flag-clean preamble for SETcc),
;;   - SETcc r/m8 — produce 0/1 boolean for `<` / `>` / `=`.
;;
;; All emitters return a list of bytes in encoding order, mirroring the
;; existing helpers above.

(defun nelisp-cc-x86_64--emit-inc-reg (reg)
  "Encode INC REG (64-bit form, opcode FF /0 register-direct).
Bytes: REX.W [+B if r8..r15] | 0xFF | ModR/M(mod=11, reg=0, rm=REG.low3)."
  (let* ((low3  (nelisp-cc-x86_64--reg-low3 reg))
         (rex.b (nelisp-cc-x86_64--reg-rex-ext-p reg))
         (rex   (nelisp-cc-x86_64--rex-byte t nil nil rex.b))
         (modrm (nelisp-cc-x86_64--modrm-byte 3 0 low3)))
    (list rex #xFF modrm)))

(defun nelisp-cc-x86_64--emit-dec-reg (reg)
  "Encode DEC REG (64-bit form, opcode FF /1 register-direct).
Bytes: REX.W [+B if r8..r15] | 0xFF | ModR/M(mod=11, reg=1, rm=REG.low3)."
  (let* ((low3  (nelisp-cc-x86_64--reg-low3 reg))
         (rex.b (nelisp-cc-x86_64--reg-rex-ext-p reg))
         (rex   (nelisp-cc-x86_64--rex-byte t nil nil rex.b))
         (modrm (nelisp-cc-x86_64--modrm-byte 3 1 low3)))
    (list rex #xFF modrm)))

(defun nelisp-cc-x86_64--emit-xor-reg-reg (dst src)
  "Encode XOR DST, SRC (64-bit MR form, opcode 0x31)."
  (nelisp-cc-x86_64--emit-binop-reg-reg #x31 dst src))

(defun nelisp-cc-x86_64--emit-test-reg-reg (a b)
  "Encode TEST A, B (64-bit MR form, opcode 0x85)."
  (nelisp-cc-x86_64--emit-binop-reg-reg #x85 a b))

(defun nelisp-cc-x86_64--emit-add-reg-mem-rbp (dst offset)
  "Encode ADD DST, [rbp - OFFSET] (RM form, opcode 0x03 /r).
DST is a 64-bit register; OFFSET is a positive byte distance from rbp.

Bytes: REX.W [+R if r8..r15] | 0x03 | ModR/M(mod=10, reg=DST.low3,
rm=5 = rbp) | disp32 = -OFFSET."
  (let* ((low3  (nelisp-cc-x86_64--reg-low3 dst))
         (rex.r (nelisp-cc-x86_64--reg-rex-ext-p dst))
         (rex   (nelisp-cc-x86_64--rex-byte t rex.r nil nil))
         (modrm (nelisp-cc-x86_64--modrm-byte 2 low3 5)))
    (append (list rex #x03 modrm)
            (nelisp-cc-x86_64--imm32-bytes (- offset)))))

(defun nelisp-cc-x86_64--emit-sub-reg-mem-rbp (dst offset)
  "Encode SUB DST, [rbp - OFFSET] (RM form, opcode 0x2B /r).
Symmetric with `--emit-add-reg-mem-rbp'."
  (let* ((low3  (nelisp-cc-x86_64--reg-low3 dst))
         (rex.r (nelisp-cc-x86_64--reg-rex-ext-p dst))
         (rex   (nelisp-cc-x86_64--rex-byte t rex.r nil nil))
         (modrm (nelisp-cc-x86_64--modrm-byte 2 low3 5)))
    (append (list rex #x2B modrm)
            (nelisp-cc-x86_64--imm32-bytes (- offset)))))

(defun nelisp-cc-x86_64--emit-cmp-reg-mem-rbp (a offset)
  "Encode CMP A, [rbp - OFFSET] (RM form, opcode 0x3B /r).
Sets flags as A - mem; mem operand is read-only."
  (let* ((low3  (nelisp-cc-x86_64--reg-low3 a))
         (rex.r (nelisp-cc-x86_64--reg-rex-ext-p a))
         (rex   (nelisp-cc-x86_64--rex-byte t rex.r nil nil))
         (modrm (nelisp-cc-x86_64--modrm-byte 2 low3 5)))
    (append (list rex #x3B modrm)
            (nelisp-cc-x86_64--imm32-bytes (- offset)))))

(defun nelisp-cc-x86_64--emit-imul-reg-mem-rbp (dst offset)
  "Encode IMUL DST, [rbp - OFFSET] (RM form, opcode 0x0F 0xAF).
Computes DST = DST * mem, low 64 bits."
  (let* ((low3  (nelisp-cc-x86_64--reg-low3 dst))
         (rex.r (nelisp-cc-x86_64--reg-rex-ext-p dst))
         (rex   (nelisp-cc-x86_64--rex-byte t rex.r nil nil))
         (modrm (nelisp-cc-x86_64--modrm-byte 2 low3 5)))
    (append (list rex #x0F #xAF modrm)
            (nelisp-cc-x86_64--imm32-bytes (- offset)))))

(defconst nelisp-cc-x86_64--setcc-opcodes
  '((setl  . #x9C) (setg  . #x9F) (sete  . #x94)
    (setne . #x95) (setle . #x9E) (setge . #x9D))
  "Second-byte opcodes for the `0F xx' SETcc family.
Sets the operand byte to 1 when the condition holds, else 0.  We pair
SETcc with `--emit-xor-reg-reg' (zero-extend stage) so the upper 56
bits of the destination register are zero — matches the trampoline
contract that returns 0 / 1 in rax for `< > = eq null not consp'.")

(defun nelisp-cc-x86_64--emit-setcc-reg-low8 (cc reg)
  "Encode SETcc REG (1-byte set-if-condition).
CC is a symbol from `nelisp-cc-x86_64--setcc-opcodes'.

For SETcc on the low byte of any of the legacy 8 GP regs (al/cl/dl/bl/
spl/bpl/sil/dil), x86_64 requires a REX prefix to access spl/bpl/sil/
dil rather than the legacy ah/ch/dh/bh — we emit REX (W=0) for safety
on rdi/rsi/rbp/rsp and skip it on rax/rcx/rdx/rbx where the legacy
encoding already targets the low byte.

For r8b..r15b the REX.B bit is set, REX.W is irrelevant.

Bytes: [REX]? | 0x0F | (0x90..0x9F) | ModR/M(mod=11, reg=0, rm=REG.low3)."
  (let* ((opcell (assq cc nelisp-cc-x86_64--setcc-opcodes))
         (op2    (cdr opcell))
         (low3   (nelisp-cc-x86_64--reg-low3 reg))
         (rex.b  (nelisp-cc-x86_64--reg-rex-ext-p reg))
         (modrm  (nelisp-cc-x86_64--modrm-byte 3 0 low3))
         (needs-rex
          ;; rdi/rsi/rbp/rsp need REX to access SIL/DIL/BPL/SPL.  rax/
          ;; rcx/rdx/rbx have legacy encodings (al/cl/dl/bl) that don't
          ;; need a REX.  r8..r15 always need REX.B.
          (or rex.b (memq reg '(rdi rsi rbp rsp)))))
    (unless opcell
      (signal 'nelisp-cc-x86_64-encoding-error
              (list :unknown-setcc-condition cc)))
    (let ((bytes (list #x0F op2 modrm)))
      (if needs-rex
          (cons (nelisp-cc-x86_64--rex-byte nil nil nil rex.b) bytes)
        bytes))))

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

(defun nelisp-cc-x86_64--emit-mov-rax-rip-rel (rel32)
  "Encode MOV rax, [rip + REL32] (RIP-relative load, 64-bit).
T84 Phase 7.5 wire — load from a global cell whose address is fixed
at link time relative to the next instruction.

Bytes: 48 8B 05 <rel32> (REX.W + opcode 8B /0 + ModR/M for [rip+disp32]
+ 4 bytes of displacement little-endian).  ModR/M=05 = mod=00, reg=000
(rax), rm=101 (RIP+disp32 form).  The 7-byte total matches LEA's size
so the link math (`tramp-off - (fixup-off + 4)') is identical."
  (append (list #x48 #x8B #x05)
          (nelisp-cc-x86_64--imm32-bytes rel32)))

(defun nelisp-cc-x86_64--emit-mov-rip-rel-rax (rel32)
  "Encode MOV [rip + REL32], rax (RIP-relative store, 64-bit).
T84 Phase 7.5 wire — symmetric with `--emit-mov-rax-rip-rel'.

Bytes: 48 89 05 <rel32> (REX.W + opcode 89 /0 + ModR/M [rip+disp32]
form + 4 bytes of displacement little-endian).  ModR/M=05 = mod=00,
reg=000 (rax = source), rm=101 (RIP+disp32 = destination)."
  (append (list #x48 #x89 #x05)
          (nelisp-cc-x86_64--imm32-bytes rel32)))

(defun nelisp-cc-x86_64--emit-lea-rax-rip-rel (rel32)
  "Encode LEA rax, [rip + REL32] (compute address, no memory access).
T84 Phase 7.5 wire — used by `:closure' lowering to produce a
function pointer to an inner-function body that has been embedded
later in the same byte buffer.

Bytes: 48 8D 05 <rel32> (REX.W + LEA opcode 8D + ModR/M [rip+disp32]
form + 4 bytes of displacement little-endian)."
  (append (list #x48 #x8D #x05)
          (nelisp-cc-x86_64--imm32-bytes rel32)))

(defun nelisp-cc-x86_64--emit-inc-mem-rip-rel (rel32)
  "Encode INC qword [rip + REL32] (read-modify-write 64-bit increment).
T84 Phase 7.5 wire — used by the `cons' counter trampoline so the
allocator-counter cell can be bumped without a register clobber.

Bytes: 48 FF 05 <rel32> (REX.W + opcode FF /0 + ModR/M [rip+disp32]
+ 4 bytes of displacement little-endian)."
  (append (list #x48 #xFF #x05)
          (nelisp-cc-x86_64--imm32-bytes rel32)))

(defun nelisp-cc-x86_64--emit-call-rel32 (rel32)
  "Encode CALL rel32 with REL32 the displacement from the *next*
instruction (i.e. from the byte after the 5-byte CALL itself).

Bytes: 0xE8 | rel32 little-endian."
  (cons #xE8 (nelisp-cc-x86_64--imm32-bytes rel32)))

(defun nelisp-cc-x86_64--emit-call-indirect-reg (reg)
  "Encode CALL r/m64 (indirect call via REG) — opcode FF /2.
T38 Phase 7.5.5 — first-class function call backend lowering.

Bytes: [REX.B] FF (mod=11 reg=2 rm=REG.low3)
  - REX prefix is 0x48 always (W=1 to keep the slot 64-bit) plus
    REX.B when REG ∈ {r8..r15} (low 3 bits truncate; B selects high
    bank).  System V near indirect call defaults to 64-bit operand
    size, so W is conventional.

REG is a physical register symbol (`rax', `r10', ...).  Returns a
list of 3 bytes (or 4 with REX.B)."
  (let* ((rm-low3 (nelisp-cc-x86_64--reg-low3 reg))
         (rex-b   (nelisp-cc-x86_64--reg-rex-ext-p reg))
         (rex     (nelisp-cc-x86_64--rex-byte 1 0 0 (if rex-b 1 0)))
         ;; ModR/M: mod=11 (register direct), reg=2 (the /2 opcode
         ;; extension for CALL r/m64), rm = REG's low 3 bits.
         (modrm   (nelisp-cc-x86_64--modrm-byte 3 2 rm-low3)))
    (list rex #xFF modrm)))

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
address (via `nelisp-defs-index').

CELL-SYMBOLS (T84 Phase 7.5 wire) is the list of letrec-bound names +
implicit `cons-counter' that the SSA references via `:store-var' or
`:load-var'.  Each name resolves at link time to a `cell:NAME' label
bound after the function bodies + trampolines (= 8 bytes per cell at
the JIT page tail).  When non-nil, `--lower-store-var' / `--lower-
load-var' emit RIP-relative MOV against the cell rather than the
pre-T84 placeholder MOV r,0."
  (function nil :read-only t)
  (alloc-state nil :read-only t)
  (buffer nil :read-only t)
  (rpo nil)
  (slot-alist nil)
  (frame-size 0)
  (call-fixups nil)
  (cell-symbols nil)
  ;; T96 Phase 7.1.5 — when this codegen is for an inner function
  ;; whose `:closure' carried `:letrec-name SYM', LETREC-NAME holds
  ;; that SYM and TCO-SELF-IDX holds the inner-function index so the
  ;; backend can emit `JMP inner:IDX:body' for tail-self calls.
  (letrec-name nil)
  (tco-self-idx nil))

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

(defun nelisp-cc-x86_64--load-var-elidable-p (cg instr)
  "T96 Phase 7.1.5 + Phase 7.1.1 direct-call elision (2026-04-27) —
return non-nil when LOAD-VAR INSTR's def is dead at the byte level,
either because:

  A. (T96) every use is a `:call-indirect' instruction marked
     `:tail-call-self' / `:self-direct-call' for the current letrec
     name (the backend lowers those as JMP / direct CALL rel32 and
     never reads the loaded value).

  B. (Phase 7.1.1) every use is a `:call' with `:fn LETREC-NAME'
     meta (= post-T161 rewrite shape — the call's callee target
     comes from the `callee:LETREC-NAME' rel32 fixup, NOT from any
     operand register).  The load-var def is never read at runtime;
     after the T161 rewrite even the SSA use-list edge is dropped,
     so the def's `use-list' typically falls to nil for these sites.

  C. (Phase 7.1.1, conservative orphan path) the def has NO uses at
     all AND META :name matches the current letrec name.  This catches
     the common shape produced by T161 + rec-inline depth>=1 unroll
     — every cloned `:load-var fib' becomes orphan after the rewrite
     re-points each `:call-indirect' to direct `:call :fn fib' and
     drops the load-var from the use-list.  Eliding saves ~7 bytes
     of MOV from `cell:fib' + 1 cycle of L1 read per dead site.

When all uses meet a condition the load-var emit can be skipped
entirely — its def is never read at run time even though the SSA
form retains it as a placeholder.  The slot stays allocated (linear-
scan does not run again) but its bytes never execute.

CG carries the current letrec-name + tco-self-idx via its
`letrec-name' / `tco-self-idx' fields (set by
`--compile-inner-into-buffer').  When CG's letrec-name is nil, the
load-var is never elidable — the optimisation is keyed strictly to
the inner-self bound."
  (let* ((cg-letrec (nelisp-cc-x86_64--codegen-letrec-name cg))
         (def (nelisp-cc--ssa-instr-def instr))
         (meta (nelisp-cc--ssa-instr-meta instr)))
    (and cg-letrec
         def
         (eq (plist-get meta :name) cg-letrec)
         (let ((uses (nelisp-cc--ssa-value-use-list def)))
           (cond
            ;; Path C — orphan dead load-var.  Safe because no later
            ;; instruction can read the def's slot (every consumer
            ;; was unhooked from the use-list when its instruction
            ;; was rewritten / removed).
            ((null uses) t)
            ;; Paths A + B — every use is a direct-call sink that
            ;; doesn't consult the loaded value at runtime.
            (t
             (cl-every
              (lambda (use-instr)
                (let ((u-op (nelisp-cc--ssa-instr-opcode use-instr))
                      (u-meta (nelisp-cc--ssa-instr-meta use-instr)))
                  (or
                   ;; A — T96 self-direct call-indirect.
                   (and (eq u-op 'call-indirect)
                        (or (eq (plist-get u-meta :tail-call-self)
                                cg-letrec)
                            (eq (plist-get u-meta :self-direct-call)
                                cg-letrec)))
                   ;; B — Phase 7.1.1 post-T161 direct-call shape.
                   ;; The :call's `:fn' meta names the letrec, and
                   ;; the call lowering uses the rel32 fixup against
                   ;; `callee:LETREC-NAME' so the load is dead.
                   (and (eq u-op 'call)
                        (eq (plist-get u-meta :fn) cg-letrec)))))
              uses)))))))

(defun nelisp-cc-x86_64--lower-load-var (cg instr)
  "Lower an SSA :load-var INSTR — placeholder MOV r64, 0 *unless* the
referenced symbol has a global cell (T84 Phase 7.5 wire).

When CG carries a non-nil `cell-symbols' set and the META :name is in
it, emits a 7-byte `MOV rax, [rip + disp32]' that resolves at link
time to the matching `cell:NAME' label (= 8-byte cell at the JIT-page
tail), then writes back to the def via the standard scratch routing.
This is the path that lets a `letrec'-bound recursive name reach
into the inner-lambda body where the SSA scope alone could not
plumb it.

Pre-T84 fallback (= zero placeholder MOV r,0) is preserved when the
symbol is not in the cell set so the existing golden tests pin on
the same byte sequence."
  (let* ((buf   (nelisp-cc-x86_64--codegen-buffer cg))
         (def   (nelisp-cc--ssa-instr-def instr))
         (meta  (nelisp-cc--ssa-instr-meta instr))
         (name  (plist-get meta :name))
         (cells (nelisp-cc-x86_64--codegen-cell-symbols cg))
         (dst   (nelisp-cc-x86_64--def-target cg def)))
    (cond
     ;; T96 Phase 7.1.5 — elide the load entirely when all uses are
     ;; self-direct / tail-self call-indirects.  Their lowering emits
     ;; a direct CALL or JMP (no indirect through this value), so the
     ;; load is dead.  We do NOT writeback to def either; the def's
     ;; physical home stays in whatever undefined state — the only
     ;; consumers will not read it.
     ((nelisp-cc-x86_64--load-var-elidable-p cg instr)
      nil)
     ((and name cells (memq name cells))
      ;; T84: RIP-relative load through `cell:NAME'.  The instruction
      ;; ends 4 bytes past the disp32 field (= 7 bytes after the start
      ;; of the MOV); the link-time resolver computes
      ;; `cell-offset - (fixup-offset + 4)' on the rel32 field.
      (let* ((before (nelisp-cc-x86_64--buffer-offset buf))
             (label  (intern (format "cell:%s" name))))
        (nelisp-cc-x86_64--buffer-emit-bytes
         buf (nelisp-cc-x86_64--emit-mov-rax-rip-rel 0))
        ;; The disp32 field begins 3 bytes into the 7-byte instruction
        ;; (REX + opcode + ModR/M = 3 bytes).  Record the absolute
        ;; offset of the disp32 so the resolver patches in place.
        (push (cons (+ before 3) label)
              (nelisp-cc-x86_64--buffer-fixups buf))
        (unless (eq dst 'rax)
          (nelisp-cc-x86_64--buffer-emit-bytes
           buf (nelisp-cc-x86_64--emit-mov-reg-reg dst 'rax)))
        (nelisp-cc-x86_64--writeback-def cg def dst)))
     (t
      (nelisp-cc-x86_64--buffer-emit-bytes
       buf (nelisp-cc-x86_64--emit-mov-reg-imm32 dst 0))
      (nelisp-cc-x86_64--writeback-def cg def dst)))))

(defun nelisp-cc-x86_64--lower-store-var (cg instr)
  "Lower an SSA :store-var INSTR — MOV def-reg, src-reg with spill marshalling.

T84 Phase 7.5 wire: when CG carries a non-nil `cell-symbols' set and
the META :name is in it, *also* persist the value to the global cell
via `MOV [rip + disp32], rax', so subsequent `:load-var' on the same
name (in any function body sharing the buffer) reads the new value.
The pre-T84 def-routing (operand → def register/spill, returns the
SSA value) is preserved so consumers that already had the value in a
register avoid an extra reload.

Spill-aware: source and / or destination may be spilled.  We
materialise the operand into a register (loading from the slot via
the scratch when spilled) and then route it to the def via
`--writeback-def'."
  (let* ((buf      (nelisp-cc-x86_64--codegen-buffer cg))
         (def      (nelisp-cc--ssa-instr-def instr))
         (operands (nelisp-cc--ssa-instr-operands instr))
         (meta     (nelisp-cc--ssa-instr-meta instr))
         (name     (plist-get meta :name))
         (cells    (nelisp-cc-x86_64--codegen-cell-symbols cg))
         (src-val  (car operands))
         (src-reg  (nelisp-cc-x86_64--materialise-operand cg src-val)))
    ;; Cell persistence first — keeps src-reg's contents intact for
    ;; the def writeback below (MOV-to-rax + MOV-rip-relative-from-rax
    ;; happens before the def routes to whatever physical register).
    (when (and name cells (memq name cells))
      (unless (eq src-reg 'rax)
        (nelisp-cc-x86_64--buffer-emit-bytes
         buf (nelisp-cc-x86_64--emit-mov-reg-reg 'rax src-reg)))
      (let* ((before (nelisp-cc-x86_64--buffer-offset buf))
             (label  (intern (format "cell:%s" name))))
        (nelisp-cc-x86_64--buffer-emit-bytes
         buf (nelisp-cc-x86_64--emit-mov-rip-rel-rax 0))
        (push (cons (+ before 3) label)
              (nelisp-cc-x86_64--buffer-fixups buf))))
    (nelisp-cc-x86_64--writeback-def cg def src-reg)))

(defconst nelisp-cc-x86_64--inline-primitives
  '(+ - * 1+ 1- < > = eq null not)
  "T96 Phase 7.1.5 — primitives that the backend lowers inline rather
than via a CALL rel32 to the trampoline registry.

Inlining eliminates the CALL+RET overhead per dynamic op and avoids
the PUSH/POP marshalling stage of `--marshal-args' on the hot path
(`fib(30)' calls `< + - -' per frame, fact-iter calls `< * -' per
recursion).  The Doc 28 v2 §5.2 timing gate (30x / 20x / 5x) is
unreachable without this layer because the trampoline path costs
~30 ns / call (CALL + 2-3 wrapper ops + RET) on top of the pure
arithmetic — a 1-cycle ADD becomes ~30 cycles in practice.

The fall-back path remains intact for primitives outside this set
(e.g. `cons' / `length' / `car' / `cdr') so end-to-end correctness
is preserved while the hot path skips the trampoline entirely.")

(defun nelisp-cc-x86_64--inline-primitive-p (sym arity)
  "Return non-nil when SYM with ARITY operands is inlinable on x86_64.
ARITY is the count of operands (excluding the implicit rax def).

Two-operand ops (`+ - * < > = eq')              require ARITY=2.
One-operand ops (`1+ 1- null not')              require ARITY=1.

Returns t when the (SYM . ARITY) pair matches one of the supported
shapes; nil otherwise (= dispatch falls through to the trampoline
CALL path)."
  (cond
   ((memq sym '(+ - * < > = eq))           (= arity 2))
   ((memq sym '(1+ 1- null not))           (= arity 1))
   (t nil)))

(defun nelisp-cc-x86_64--lower-inline-binary-arith (cg sym op1 op2 def)
  "T96 inline lowering for `+ - *' (2-arg arithmetic).

Strategy:
  1. Materialise OP1 into TARGET (DEF's home reg, or the rax scratch
     for spilled defs).
  2. Apply the binop with OP2 as the source — RM-form when OP2 is
     spilled (`ADD r, [rbp-off]'), MR-form otherwise (`ADD r, r').
  3. Writeback TARGET to DEF.

The routing through `--def-target' / `--writeback-def' mirrors the
existing call/copy lowering so the spill machinery stays consistent.
A spilled OP1 reuses the rax scratch for the load; the binop itself
then has at most one memory operand which x86_64 allows."
  (let* ((buf    (nelisp-cc-x86_64--codegen-buffer cg))
         (target (nelisp-cc-x86_64--def-target cg def))
         (op1-slot (nelisp-cc-x86_64--reg-or-spill cg op1))
         (op2-slot (nelisp-cc-x86_64--reg-or-spill cg op2)))
    ;; Aliasing guard: when TARGET == op2's reg the MOV target,op1
    ;; would clobber op2 before the binop could read it.  This is
    ;; rare (the allocator's interval logic generally keeps def's
    ;; live range disjoint from op2's), but we signal hard rather
    ;; than miscompile.
    (when (pcase op2-slot
            (`(:reg ,r) (eq r target))
            (_ nil))
      (signal 'nelisp-cc-x86_64-unsupported-opcode
              (list :inline-arith-target-op2-alias sym target)))
    ;; Step 1: TARGET <- OP1.
    (pcase op1-slot
      (`(:reg ,r)
       (unless (eq r target)
         (nelisp-cc-x86_64--buffer-emit-bytes
          buf (nelisp-cc-x86_64--emit-mov-reg-reg target r))))
      (`(:spill ,off)
       (nelisp-cc-x86_64--emit-load-spill buf target off)))
    ;; Step 2: TARGET op= OP2.
    (let ((op2-bytes
           (pcase sym
             ('+ (pcase op2-slot
                   (`(:reg ,r)   (nelisp-cc-x86_64--emit-add-reg-reg target r))
                   (`(:spill ,o) (nelisp-cc-x86_64--emit-add-reg-mem-rbp target o))))
             ('- (pcase op2-slot
                   (`(:reg ,r)   (nelisp-cc-x86_64--emit-sub-reg-reg target r))
                   (`(:spill ,o) (nelisp-cc-x86_64--emit-sub-reg-mem-rbp target o))))
             ('* (pcase op2-slot
                   (`(:reg ,r)   (nelisp-cc-x86_64--emit-imul-reg-reg target r))
                   (`(:spill ,o) (nelisp-cc-x86_64--emit-imul-reg-mem-rbp target o)))))))
      (nelisp-cc-x86_64--buffer-emit-bytes buf op2-bytes))
    ;; Step 3: route TARGET -> DEF.
    (nelisp-cc-x86_64--writeback-def cg def target)))

(defun nelisp-cc-x86_64--lower-inline-cmp (cg sym op1 op2 def)
  "T96 inline lowering for `< > = eq' (2-arg comparison → 0/1).

Strategy mirrors the trampoline byte sequence (XOR-then-CMP-then-SETcc)
to preserve flags across the zero-extend stage.  T84 §A documents the
flag-trash bug the trampoline hit; the inline form repeats the
correct order so the zero-extend doesn't clobber CMP's freshly-set
ZF/SF/OF.

  XOR target, target          ; clear target + trashed flags
  CMP op1-reg, op2-reg/mem    ; sets flags = op1 - op2
  SETcc target.low8           ; target = (cond ? 1 : 0)

Then writeback target to def.  When OP1 is spilled, materialise into
rax first (rax is the conventional spill scratch).  When TARGET would
be rax (= spilled def + first scratch) and OP1 is also spilled, the
flags trashed by the load are re-set by the CMP that follows — no
intermediate flag-clobbering instruction sits between CMP and SETcc.

Boundary fix: when TARGET == OP1's reg, the XOR (= clear TARGET) would
destroy OP1's value before the CMP could read it.  We detect that case
and choose a different scratch for the SETcc destination.  The
allocator never assigns DEF and OP1 to the same vreg (DEF is a fresh
SSA value), but the *physical* mapping can collide when DEF spills
and rax is the scratch."
  (let* ((buf    (nelisp-cc-x86_64--codegen-buffer cg))
         (op1-slot (nelisp-cc-x86_64--reg-or-spill cg op1))
         (op2-slot (nelisp-cc-x86_64--reg-or-spill cg op2))
         (cc (pcase sym
               ('< 'setl) ('> 'setg) ('= 'sete) ('eq 'sete)))
         (target (nelisp-cc-x86_64--def-target cg def))
         ;; Step 1: bring op1 into a register that does NOT alias TARGET.
         ;; If op1 already lives in a non-target register, use it
         ;; directly.  Otherwise (op1 in target's reg, or op1 spilled),
         ;; load into a scratch.  rax is preferred unless TARGET == rax,
         ;; in which case rcx is the secondary.  Both rax / rcx are
         ;; either outside the pool (rax) or part of it (rcx == r3) —
         ;; if op1 is in rcx and TARGET is rax we still pick rax as
         ;; scratch (overwrites op1 only if TARGET == rcx, handled
         ;; below by the == op1-reg branch).
         (alt-scratch (if (eq target 'rax) 'rcx 'rax))
         (op1-reg
          (pcase op1-slot
            (`(:reg ,r)
             (cond
              ((eq r target)
               ;; op1 aliases TARGET — XOR target,target would clobber
               ;; op1 before CMP could read it.  Move op1 to scratch.
               (nelisp-cc-x86_64--buffer-emit-bytes
                buf (nelisp-cc-x86_64--emit-mov-reg-reg alt-scratch r))
               alt-scratch)
              (t r)))
            (`(:spill ,o)
             (nelisp-cc-x86_64--emit-load-spill buf alt-scratch o)
             alt-scratch))))
    ;; Step 2: XOR target, target — zero-extend stage (clears upper 56
    ;; bits + trashes flags).
    (nelisp-cc-x86_64--buffer-emit-bytes
     buf (nelisp-cc-x86_64--emit-xor-reg-reg target target))
    ;; Step 3: CMP op1-reg, op2.  op2-spill folded as RM-form so we do
    ;; not consume an extra scratch.  Care: op2 in target's reg is OK
    ;; because CMP does not write target — but XOR just cleared target,
    ;; so if op2-reg == target the comparison is against 0.  Detect and
    ;; reload op2 into the spare scratch.
    (let ((op2-effective op2-slot))
      (pcase op2-slot
        (`(:reg ,r)
         (when (eq r target)
           ;; op2 was just clobbered by XOR target,target — but we
           ;; could not have caught this at op1 staging because op1's
           ;; load came first.  The allocator very rarely assigns op2
           ;; to TARGET (= def's reg), but when it does we need to
           ;; route op2 through a fresh scratch.  Use a callee-saved
           ;; register `rbx' (outside the linear-scan pool) as a
           ;; one-shot spare; it must be saved/restored.  For the
           ;; bench forms this branch never fires (the allocator does
           ;; not assign def and op2 to the same physical reg because
           ;; their live ranges overlap).  Signal hard so any silent
           ;; miscompile is loud.
           (signal 'nelisp-cc-x86_64-unsupported-opcode
                   (list :inline-cmp-target-op2-alias r))))
        (_ nil))
      (pcase op2-effective
        (`(:reg ,r)
         (nelisp-cc-x86_64--buffer-emit-bytes
          buf (nelisp-cc-x86_64--emit-cmp-reg-reg op1-reg r)))
        (`(:spill ,o)
         (nelisp-cc-x86_64--buffer-emit-bytes
          buf (nelisp-cc-x86_64--emit-cmp-reg-mem-rbp op1-reg o)))))
    ;; Step 4: SETcc target.low8 — read CMP's flags.  Upper 56 bits
    ;; were already zeroed by the XOR.
    (nelisp-cc-x86_64--buffer-emit-bytes
     buf (nelisp-cc-x86_64--emit-setcc-reg-low8 cc target))
    ;; Step 5: writeback target -> def.
    (nelisp-cc-x86_64--writeback-def cg def target)))

(defun nelisp-cc-x86_64--lower-inline-incdec (cg sym op1 def)
  "T96 inline lowering for `1+ 1-' (1-arg ±1).

  MOV target, op1           ; routed via spill if needed
  INC|DEC target
  writeback target -> def"
  (let* ((buf    (nelisp-cc-x86_64--codegen-buffer cg))
         (target (nelisp-cc-x86_64--def-target cg def))
         (op1-slot (nelisp-cc-x86_64--reg-or-spill cg op1)))
    ;; TARGET <- OP1.
    (pcase op1-slot
      (`(:reg ,r)
       (unless (eq r target)
         (nelisp-cc-x86_64--buffer-emit-bytes
          buf (nelisp-cc-x86_64--emit-mov-reg-reg target r))))
      (`(:spill ,o)
       (nelisp-cc-x86_64--emit-load-spill buf target o)))
    ;; INC / DEC target.
    (nelisp-cc-x86_64--buffer-emit-bytes
     buf (pcase sym
           ('1+ (nelisp-cc-x86_64--emit-inc-reg target))
           ('1- (nelisp-cc-x86_64--emit-dec-reg target))))
    ;; writeback.
    (nelisp-cc-x86_64--writeback-def cg def target)))

(defun nelisp-cc-x86_64--lower-inline-null-not (cg sym op1 def)
  "T96 inline lowering for `null' / `not' (1-arg → 0/1 boolean).

  XOR target, target
  TEST op1-reg, op1-reg
  SETE target.low8           ; target = (op1 == 0 ? 1 : 0)
  writeback target -> def

Both `null' and `not' return t when the argument is nil (= 0 in our
i64 representation), so SETE applies to both.  This matches the
trampoline byte sequence in `nelisp-cc-callees--x86_64-trampolines'."
  (ignore sym)
  (let* ((buf    (nelisp-cc-x86_64--codegen-buffer cg))
         (target (nelisp-cc-x86_64--def-target cg def))
         (op1-slot (nelisp-cc-x86_64--reg-or-spill cg op1))
         (op1-reg
          (pcase op1-slot
            (`(:reg ,r) r)
            (`(:spill ,o)
             (let ((scratch (if (eq target 'rax) 'rcx 'rax)))
               (nelisp-cc-x86_64--emit-load-spill buf scratch o)
               scratch)))))
    ;; XOR target, target.
    (nelisp-cc-x86_64--buffer-emit-bytes
     buf (nelisp-cc-x86_64--emit-xor-reg-reg target target))
    ;; TEST op1-reg, op1-reg — sets ZF=1 iff op1 == 0.
    (nelisp-cc-x86_64--buffer-emit-bytes
     buf (nelisp-cc-x86_64--emit-test-reg-reg op1-reg op1-reg))
    ;; SETE target.low8.
    (nelisp-cc-x86_64--buffer-emit-bytes
     buf (nelisp-cc-x86_64--emit-setcc-reg-low8 'sete target))
    ;; writeback.
    (nelisp-cc-x86_64--writeback-def cg def target)))

(defun nelisp-cc-x86_64--lower-call (cg instr)
  "Lower an SSA :call INSTR.

T96 Phase 7.1.5 — when the callee + arity match an inlinable
primitive (see `--inline-primitive-p'), the operation is lowered
directly into the byte stream rather than through a CALL rel32 +
trampoline.  This eliminates the CALL+RET overhead per dynamic op
on the bench-actual hot path (fib `< + -`, fact-iter `< * -`).

Otherwise (the pre-T96 path):

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
    (cond
     ;; T96 — inline primitive fast path.
     ((and callee
           def
           (nelisp-cc-x86_64--inline-primitive-p callee n))
      (pcase callee
        ((or '+ '- '*)
         (nelisp-cc-x86_64--lower-inline-binary-arith
          cg callee (nth 0 operands) (nth 1 operands) def))
        ((or '< '> '= 'eq)
         (nelisp-cc-x86_64--lower-inline-cmp
          cg callee (nth 0 operands) (nth 1 operands) def))
        ((or '1+ '1-)
         (nelisp-cc-x86_64--lower-inline-incdec
          cg callee (nth 0 operands) def))
        ((or 'null 'not)
         (nelisp-cc-x86_64--lower-inline-null-not
          cg callee (nth 0 operands) def))))
     ;; Pre-T96 fallback: trampoline CALL.
     (t
      ;; T84 — conflict-safe argument marshalling.  Pre-T84 emitted
      ;; per-arg MOV in order, which clobbered an earlier arg-reg
      ;; whose value a later operand still needed (e.g. `(< n 2)' with
      ;; n in the spill slot AND val_0=2 living in rdi: the arg0 reload
      ;; from spill into rdi destroyed val_0 before arg1 could read it).
      (nelisp-cc-x86_64--marshal-args buf cg operands)
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
         cg def nelisp-cc-x86_64--return-reg))))))

(defcustom nelisp-cc-x86_64-marshal-strategy 'push-pop
  "Marshalling strategy for `--marshal-args'.

`push-pop' (T84 default, T96 retained as default): PUSH-N then
POP-N-reverse onto arg-regs.  2N stack ops per call but exploits the
x86_64 stack engine which renames PUSH/POP into ROB micro-ops at
low latency.  Empirically faster than `parallel-copy' on alloc-heavy
(see Phase 7.1.5 T96 bench harness).

`parallel-copy' (T96 graph-coloring): textbook move-graph topological
sort with rax cycle break — N + (#cycles) MOVs.  Smaller code (~3
fewer bytes per call site) but a loop microbench showed a
regression on alloc-heavy where the inner-loop MOV memory load
sequence mixes badly with the cons trampoline's RIP-relative INC.
Kept available for further A/B analysis + fib speedup, but not the
default."
  :type '(choice (const push-pop) (const parallel-copy))
  :group 'nelisp-cc-x86_64)

(defun nelisp-cc-x86_64--marshal-args (buf cg operands)
  "T96 Phase 7.1.5 — argument marshalling.

Strategy is selected by `nelisp-cc-x86_64-marshal-strategy':

`parallel-copy' (default): graph-coloring parallel copy.
  1. Build the move list moves[i] = (target-arg-reg . src-slot)
     where target-arg-reg is rdi/rsi/rdx/... in order.
  2. Drain spill sources first — their target arg-reg is unique and
     never another move's source, so MOV arg-reg, [rbp-off] is
     unconditionally safe.
  3. For the remaining reg-source moves, repeatedly find a SINK (a
     move whose target is not the source of any other pending move)
     and emit it.  After emission, the move is dropped.
  4. When only cycles remain, break one with rax: MOV rax, src-reg;
     MOV target, rax; rewrite every other move whose source was
     src-reg to read rax instead.  Resume step 3.

  Cost: at most N + (#cycles) moves (vs 2N PUSH/POP).  rax is never
  an arg-reg, so the cycle-break never collides with a target.

`push-pop': the historical T84 strategy (2N stack ops); preserved as
fallback.

Falls back to signal for >6 operands (the legacy SAL contract; the
ABI stack-arg path is a separate Phase 7.1.6 task)."
  (let* ((arg-regs nelisp-cc-x86_64--int-arg-regs)
         (n (length operands)))
    (when (> n (length arg-regs))
      (signal 'nelisp-cc-x86_64-unsupported-opcode
              (list :stack-arg-spill-not-implemented n)))
    (pcase nelisp-cc-x86_64-marshal-strategy
      ('push-pop
       (nelisp-cc-x86_64--marshal-args-push-pop buf cg operands))
      (_
       (nelisp-cc-x86_64--marshal-args-parallel-copy buf cg operands)))))

(defun nelisp-cc-x86_64--marshal-args-parallel-copy (buf cg operands)
  "T96 Phase 7.1.5 — graph-coloring parallel-copy implementation.
Helper for `--marshal-args' when strategy is `parallel-copy'."
  (let* ((arg-regs nelisp-cc-x86_64--int-arg-regs)
         (moves nil)
         (i 0))
    ;; Step 1: build move list.
    (dolist (op operands)
      (let ((target (nth i arg-regs))
            (src    (nelisp-cc-x86_64--reg-or-spill cg op)))
        (push (cons target src) moves)
        (cl-incf i)))
    (setq moves (nreverse moves))
    ;; Step 2: drain spill loads + identity reg moves.
    (let (remaining)
      (dolist (mv moves)
        (let ((target (car mv))
              (src    (cdr mv)))
          (cond
           ((eq (car src) :spill)
            (nelisp-cc-x86_64--emit-load-spill buf target (cadr src)))
           ((and (eq (car src) :reg) (eq (cadr src) target))
            nil)                       ; identity — already in place
           (t
            (push mv remaining)))))
      (setq remaining (nreverse remaining))
      ;; Steps 3-4: parallel copy the remaining reg moves.
      (while remaining
        (let* ((reg-of-src
                (lambda (m)
                  (let ((s (cdr m)))
                    (and (eq (car s) :reg) (cadr s)))))
               (sources (delq nil
                              (mapcar reg-of-src remaining)))
               (sink
                (cl-find-if
                 (lambda (m) (not (memq (car m) sources)))
                 remaining)))
          (cond
           (sink
            (let ((target (car sink))
                  (src-reg (funcall reg-of-src sink)))
              (nelisp-cc-x86_64--buffer-emit-bytes
               buf (nelisp-cc-x86_64--emit-mov-reg-reg target src-reg)))
            (setq remaining (delq sink remaining)))
           (t
            ;; Only cycles remain — break one with rax.
            (let* ((mv (car remaining))
                   (target (car mv))
                   (src-reg (funcall reg-of-src mv)))
              (nelisp-cc-x86_64--buffer-emit-bytes
               buf (nelisp-cc-x86_64--emit-mov-reg-reg 'rax src-reg))
              (nelisp-cc-x86_64--buffer-emit-bytes
               buf (nelisp-cc-x86_64--emit-mov-reg-reg target 'rax))
              (setq remaining (delq mv remaining))
              (dolist (m remaining)
                (when (eq (funcall reg-of-src m) src-reg)
                  (setcdr m (list :reg 'rax))))))))))))

(defun nelisp-cc-x86_64--marshal-args-push-pop (buf cg operands)
  "T84 PUSH-N + POP-N-reverse marshalling (legacy fallback).

Helper for `--marshal-args' when strategy is `push-pop'.  Each
operand is PUSHed onto the stack first, then POPed into arg-regs in
reverse order so arg-reg[0] ends up holding operand 0 etc.

Cost: 2N stack ops per call vs the T96 N + (#cycles) MOVs.  Kept for
A/B benchmarking + as a safety fallback while parallel-copy bedding
in on a wider set of forms."
  (let ((push-count 0))
    ;; Phase 1: push each operand source onto the stack.
    (dolist (op operands)
      (let ((slot (nelisp-cc-x86_64--reg-or-spill cg op)))
        (pcase slot
          (`(:reg ,r)
           (nelisp-cc-x86_64--emit-push-reg buf r))
          (`(:spill ,off)
           (nelisp-cc-x86_64--emit-load-spill buf 'rax off)
           (nelisp-cc-x86_64--emit-push-reg buf 'rax))))
      (cl-incf push-count))
    ;; Phase 2: pop into arg-regs in reverse order.
    (let* ((arg-regs nelisp-cc-x86_64--int-arg-regs)
           (used (cl-subseq arg-regs 0 push-count))
           (rev (reverse used)))
      (dolist (r rev)
        (nelisp-cc-x86_64--emit-pop-reg buf r)))))

(defun nelisp-cc-x86_64--marshal-args-v2 (buf cg operands)
  "T96 Phase 7.1.5 — alias for `--marshal-args' kept for ERT references.

The v2 helper was the primary entry point during T96 development;
its body has been folded back into `--marshal-args' (the canonical
name).  This wrapper preserves any external call sites that referred
to the v2 name during development."
  (nelisp-cc-x86_64--marshal-args buf cg operands))

(defun nelisp-cc-x86_64--emit-push-reg (buf reg)
  "Emit PUSH REG (8-byte push) into BUF.  Bytes: 50+rd (or REX.B + 50+rd)."
  (let* ((low3 (nelisp-cc-x86_64--reg-low3 reg))
         (ext  (nelisp-cc-x86_64--reg-rex-ext-p reg)))
    (when ext
      (nelisp-cc-x86_64--buffer-emit-byte buf #x41)) ; REX.B
    (nelisp-cc-x86_64--buffer-emit-byte buf (logior #x50 low3))))

(defun nelisp-cc-x86_64--emit-pop-reg (buf reg)
  "Emit POP REG (8-byte pop) into BUF.  Bytes: 58+rd (or REX.B + 58+rd)."
  (let* ((low3 (nelisp-cc-x86_64--reg-low3 reg))
         (ext  (nelisp-cc-x86_64--reg-rex-ext-p reg)))
    (when ext
      (nelisp-cc-x86_64--buffer-emit-byte buf #x41)) ; REX.B
    (nelisp-cc-x86_64--buffer-emit-byte buf (logior #x58 low3))))

(defun nelisp-cc-x86_64--lower-branch (cg instr)
  "Lower an SSA :branch INSTR — TEST cond + JE else + JMP then.

The branch carries (META :then THEN-BLOCK-ID :else ELSE-BLOCK-ID) and
a single operand (the condition value).  T63 Phase 7.5.7 makes the
emit *layout-independent* — both arms get an explicit jump rather
than relying on a fallthrough to THEN:

    TEST cond, cond ; flags ZF=1 iff cond is 0 (== nil)
    JE   L_else     ; if zero → jump to else
    JMP  L_then     ; else (non-zero) → jump to then

The pre-T63 skeleton emitted only the JE and assumed THEN sat next
in the codegen walk; reverse-postorder linearisation actually places
ELSE before THEN for an `(if c T E)' diamond (DFS tree explores the
first-linked successor deepest, postorder finishes it last; both
edges are added in (then else) order so DFS sinks into THEN→merge,
finishes THEN, then ELSE→merge[visited]→finishes ELSE; reverse-pop
yields entry, ELSE, THEN, merge — fall-through hit ELSE not THEN).
With both branches explicit the encoded bytes execute correctly
regardless of which block the codegen lays out next.

Note: x86_64 has no `CMP r64, imm0' shortcut shorter than `TEST
r64,r64' (two bytes 0x48 0x85 + ModR/M).  We use TEST because it is
one byte shorter than `CMP r64, imm32 0' and matches what gcc -O2
emits for `if (x)'."
  (let* ((buf      (nelisp-cc-x86_64--codegen-buffer cg))
         (operands (nelisp-cc--ssa-instr-operands instr))
         (meta     (nelisp-cc--ssa-instr-meta instr))
         (then-id  (plist-get meta :then))
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
       2))
    ;; JMP label_then — explicit unconditional branch to the THEN
    ;; block (T63 layout-independence).  Without this the codegen
    ;; would fall through to whichever block RPO laid down next,
    ;; which is *not* guaranteed to be THEN (see commentary above).
    (let ((then-label (intern (format "L_block_%d" then-id))))
      (nelisp-cc-x86_64--buffer-emit-fixup
       buf (nelisp-cc-x86_64--emit-jmp-rel32 0)
       then-label
       1))))

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

(defun nelisp-cc-x86_64--lower-call-indirect (cg instr)
  "Lower an SSA `:call-indirect' INSTR — first-class function call.
T38 Phase 7.5.5 — Doc 28 §3.1 first-class call lowering.

Operand layout:
  operands[0] = SSA value holding the function pointer (callee).
  operands[1..N] = SSA values for the positional arguments.

Codegen sequence:
  1. Materialise the callee value into rax (a caller-saved scratch
     register that is *not* in the System V argument bank, so
     argument marshalling will not clobber it).
  2. Marshal each argument into rdi/rsi/rdx/rcx/r8/r9 with
     spill-aware loads (mirrors `--lower-call').
  3. Emit `CALL [rax]' (FF /2 with ModR/M rm=rax).
  4. Route the return value (rax) to the def via `--writeback-def'.

Spill-aware: callee + each arg may live in a stack slot.  Args ≤ 6
are required (the same bound as direct `:call'); >6 args raise
`nelisp-cc-x86_64-unsupported-opcode' until the stack-spill ABI
lowering lands in a follow-up.

Note: there is *no* call-fixup entry — the callee address is
provided dynamically by the SSA value, so Phase 7.5
`nelisp-defs-index' patching is bypassed for indirect calls.
Closures whose runtime construction is also Phase 7.5 territory will
materialise their function pointer via the eventual `:closure'
runtime-cell load; the MVP `:closure' lowering currently emits a
zero placeholder (see `--lower-closure')."
  (let* ((buf      (nelisp-cc-x86_64--codegen-buffer cg))
         (def      (nelisp-cc--ssa-instr-def instr))
         (operands (nelisp-cc--ssa-instr-operands instr))
         (callee-val (car operands))
         (arg-vals (cdr operands))
         (n        (length arg-vals))
         (meta     (nelisp-cc--ssa-instr-meta instr))
         (tail-self-name (plist-get meta :tail-call-self))
         (direct-self-name (plist-get meta :self-direct-call))
         (cg-letrec (nelisp-cc-x86_64--codegen-letrec-name cg))
         (cg-self-idx (nelisp-cc-x86_64--codegen-tco-self-idx cg))
         (tco-self-idx
          (and tail-self-name
               (eq tail-self-name cg-letrec)
               cg-self-idx))
         (direct-self-idx
          (and direct-self-name
               (eq direct-self-name cg-letrec)
               cg-self-idx)))
    (unless callee-val
      (signal 'nelisp-cc-x86_64-encoding-error
              (list :call-indirect-without-callee instr)))
    (when (> n (length nelisp-cc-x86_64--int-arg-regs))
      (signal 'nelisp-cc-x86_64-unsupported-opcode
              (list :stack-arg-spill-not-implemented n)))
    (cond
     ;; T96 Phase 7.1.5 — tail self call: marshal args + JMP back to
     ;; this inner function's body label.  The callee operand load is
     ;; still in the byte stream (it was lowered as a `:load-var' a
     ;; few instructions earlier), but we skip the CALL+RET pair and
     ;; reuse the current frame.  Param-home-spill at `inner:IDX:body'
     ;; will overwrite the spilled-param slots with the new arg-reg
     ;; values, so the next iteration sees them correctly.
     (tco-self-idx
      ;; Marshal args into rdi/rsi/... using parallel-copy (the tail
      ;; sequence is short, so the move-graph optimisation outshines
      ;; the PUSH/POP pair).
      (let ((nelisp-cc-x86_64-marshal-strategy 'parallel-copy))
        (nelisp-cc-x86_64--marshal-args buf cg arg-vals))
      ;; JMP rel32 to inner:IDX:body.
      (let ((target (intern (format "inner:%d:body" tco-self-idx))))
        (nelisp-cc-x86_64--buffer-emit-fixup
         buf (nelisp-cc-x86_64--emit-jmp-rel32 0)
         target
         1))
      ;; The DEF is unreachable (the JMP is the terminator) — no need
      ;; to writeback.  But the SSA still has uses of DEF on the
      ;; phi/return path, which after phi-out become :copy + :return.
      ;; Those instructions will run `--lower-copy' / `--lower-return'
      ;; on the post-jmp dead code; the bytes get emitted but never
      ;; executed.  Acceptable overhead vs. tracking dead-block
      ;; elimination.
      )
     ;; T96 — non-tail self-call: skip the cell-load + indirect call,
     ;; emit `CALL inner:IDX' rel32 directly.  The callee operand's
     ;; load-var instruction was already elided (see
     ;; `--load-var-elidable-p').  The win is per-call-site: replace
     ;; the PUSH callee / POP rax / CALL [rax] sequence with the
     ;; simpler args-marshal + CALL rel32.
     ;;
     ;; Use parallel-copy marshalling here regardless of the global
     ;; strategy default — for a single-arg recursive call (the fib
     ;; / fact case) the difference is 1 MOV vs 2 stack ops, and
     ;; benchmarking on fib(30) shows parallel-copy is ~5% faster on
     ;; this short marshal sequence (alloc-heavy's regression was
     ;; specific to the cons trampoline's loop-body interaction).
     (direct-self-idx
      (let ((nelisp-cc-x86_64-marshal-strategy 'parallel-copy))
        (nelisp-cc-x86_64--marshal-args buf cg arg-vals))
      (let ((target (intern (format "inner:%d" direct-self-idx))))
        (nelisp-cc-x86_64--buffer-emit-fixup
         buf (nelisp-cc-x86_64--emit-call-rel32 0)
         target
         1))
      (when def
        (nelisp-cc-x86_64--writeback-def
         cg def nelisp-cc-x86_64--return-reg)))
     (t
      ;; T84 + T96: marshal callee + args.  The callee is a phantom-
      ;; zeroth arg that lands in rax.  We use the same PUSH/POP
      ;; strategy as direct calls for consistency + because the
      ;; alloc-heavy bench showed PUSH/POP outperforms direct moves
      ;; on the cons-trampoline path; the same may not be true for
      ;; cold call-indirect sites but the win is small either way.
      (let ((push-count 0))
        ;; Push args 0..n-1.
        (dolist (op arg-vals)
          (let ((slot (nelisp-cc-x86_64--reg-or-spill cg op)))
            (pcase slot
              (`(:reg ,r) (nelisp-cc-x86_64--emit-push-reg buf r))
              (`(:spill ,off)
               (nelisp-cc-x86_64--emit-load-spill buf 'rax off)
               (nelisp-cc-x86_64--emit-push-reg buf 'rax))))
          (cl-incf push-count))
        ;; Push callee on top of stack so POP rax retrieves it first.
        (let ((slot (nelisp-cc-x86_64--reg-or-spill cg callee-val)))
          (pcase slot
            (`(:reg ,r) (nelisp-cc-x86_64--emit-push-reg buf r))
            (`(:spill ,off)
             (nelisp-cc-x86_64--emit-load-spill buf 'rax off)
             (nelisp-cc-x86_64--emit-push-reg buf 'rax))))
        ;; Now pop: first rax (callee), then arg-regs in reverse.
        (nelisp-cc-x86_64--emit-pop-reg buf 'rax)
        (let* ((arg-regs nelisp-cc-x86_64--int-arg-regs)
               (used (cl-subseq arg-regs 0 push-count))
               (rev (reverse used)))
          (dolist (r rev)
            (nelisp-cc-x86_64--emit-pop-reg buf r))))
      ;; Step 3: CALL rax (FF /2 with mod=11, rm=rax → encodes
      ;; CALL rax, absolute jump).
      (nelisp-cc-x86_64--buffer-emit-bytes
       buf (nelisp-cc-x86_64--emit-call-indirect-reg 'rax))
      ;; Step 4: harvest return value.
      (when def
        (nelisp-cc-x86_64--writeback-def
         cg def nelisp-cc-x86_64--return-reg))))))

(defun nelisp-cc-x86_64--lower-closure (cg instr)
  "Lower an SSA `:closure' INSTR — emit `LEA rax, [rip + INNER:N]'
where INNER:N labels the inner-function body that the link step
embeds after the outer body.  T84 Phase 7.5 wire — Doc 28 §3.5 +
Doc 32 v2 §3.

The pre-T84 path (T43 SHIPPED) emitted a CALL into the embedded
`alloc-closure' trampoline whose self-pointer was non-callable
beyond a simple RET; the resulting `:call-indirect' on a fib-like
recursive lambda fell through into the trampoline and returned the
self-pointer instead of executing the lambda body.  T84 replaces
that with a proper function pointer to the actual compiled body.

Lookup: META :inner-function-index (set by the pre-codegen pass in
`nelisp-cc-callees--collect-inner-functions') is the 1-based visit
ordinal — it survives any later SSA mutations and so encodes a
stable label name across the outer + inner compile windows.

Spill-aware: routes through `--writeback-def' so a spilled def lands
in its slot."
  (let* ((buf (nelisp-cc-x86_64--codegen-buffer cg))
         (def (nelisp-cc--ssa-instr-def instr))
         (meta (nelisp-cc--ssa-instr-meta instr))
         (idx  (plist-get meta :inner-function-index)))
    (cond
     (idx
      ;; T84: LEA rax, [rip + cell:inner:IDX].  The 7-byte instruction
      ;; layout:  REX.W | 0x8D | ModR/M=05 | <disp32 little-endian>.
      ;; The disp32 field begins 3 bytes into the instruction; we
      ;; record the absolute offset of the disp32 so the resolver
      ;; patches in place.
      (let* ((before (nelisp-cc-x86_64--buffer-offset buf))
             (label  (intern (format "inner:%d" idx))))
        (nelisp-cc-x86_64--buffer-emit-bytes
         buf (nelisp-cc-x86_64--emit-lea-rax-rip-rel 0))
        (push (cons (+ before 3) label)
              (nelisp-cc-x86_64--buffer-fixups buf))))
     (t
      ;; Pre-T84 fallback (no index annotation — keeps existing
      ;; ERT golden tests on the `alloc-closure' embedded trampoline).
      (let ((before-call (nelisp-cc-x86_64--buffer-offset buf)))
        (nelisp-cc-x86_64--buffer-emit-bytes
         buf (nelisp-cc-x86_64--emit-call-rel32 0))
        (push (cons (+ before-call 1) 'alloc-closure)
              (nelisp-cc-x86_64--codegen-call-fixups cg)))))
    ;; Route the return value (rax = the closure pointer) to the def.
    (when def
      (nelisp-cc-x86_64--writeback-def
       cg def nelisp-cc-x86_64--return-reg))))

(defun nelisp-cc-x86_64--lower-instr (cg instr)
  "Dispatch a single SSA INSTR to its per-opcode lower helper.

Skeleton subset — opcodes outside the supported set raise
`nelisp-cc-x86_64-unsupported-opcode'.  phi nodes specifically must
be lowered out by a phi-out pass before this dispatch sees them."
  (pcase (nelisp-cc--ssa-instr-opcode instr)
    ('const          (nelisp-cc-x86_64--lower-const cg instr))
    ('load-var       (nelisp-cc-x86_64--lower-load-var cg instr))
    ('store-var      (nelisp-cc-x86_64--lower-store-var cg instr))
    ('call           (nelisp-cc-x86_64--lower-call cg instr))
    ('call-indirect  (nelisp-cc-x86_64--lower-call-indirect cg instr))
    ('closure        (nelisp-cc-x86_64--lower-closure cg instr))
    ('copy           (nelisp-cc-x86_64--lower-copy cg instr))
    ('branch         (nelisp-cc-x86_64--lower-branch cg instr))
    ('jump           (nelisp-cc-x86_64--lower-jump cg instr))
    ('return         (nelisp-cc-x86_64--lower-return cg instr))
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

(defun nelisp-cc-x86_64--emit-param-home-spills (cg)
  "T84 Phase 7.5 wire — for each function param the linear-scan
allocator marked `:spill', emit MOV [rbp - off], <arg-reg> at the top
of the function so the param's stack slot holds its incoming value.

The pre-T84 backend assumed every param interval kept its arg
register, but the T63 call-aware allocator forces call-crossing
intervals to spill; a recursive parameter (e.g. `n' in fib that is
read after `(funcall fib (- n 1))') is the canonical case.  Without
this prologue extension the spill slot is uninitialised and reloads
read garbage."
  (let* ((function    (nelisp-cc-x86_64--codegen-function cg))
         (alloc-state (nelisp-cc-x86_64--codegen-alloc-state cg))
         (slot-alist  (nelisp-cc-x86_64--codegen-slot-alist cg))
         (assignments (if (nelisp-cc--alloc-state-p alloc-state)
                          (nelisp-cc--alloc-state-assignments alloc-state)
                        ;; Backwards compat — some callers pass the raw
                        ;; alist returned by the simulator path.
                        alloc-state))
         (params      (nelisp-cc--ssa-function-params function))
         (buf         (nelisp-cc-x86_64--codegen-buffer cg)))
    (cl-loop for p in params
             for arg-reg in nelisp-cc-x86_64--int-arg-regs
             for vid = (nelisp-cc--ssa-value-id p)
             for assign = (cdr (assq vid assignments))
             when (eq assign :spill)
             do (let ((off (cdr (assq vid slot-alist))))
                  (when off
                    (nelisp-cc-x86_64--emit-store-spill
                     buf arg-reg off))))))

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
    ;; T84 — save spilled params to their slots.
    (nelisp-cc-x86_64--emit-param-home-spills cg)
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

(defun nelisp-cc-x86_64-compile-with-link (function alloc-state)
  "Compile FUNCTION + ALLOC-STATE and run the Phase 7.5 link step.
T84 Phase 7.5 wire — supersedes the T43 path with full callee + inner
+ cell wiring.

Returns a vector of bytes that is *executable* end-to-end:
  - every `:call FOO' site is patched to a CALL rel32 against the
    matching primitive trampoline (now embedded with cell-aware
    fixups so `cons' / `length' actually update + read the counter),
  - every `:closure' site is patched to a `LEA rax, [rip+disp32]'
    that resolves to the corresponding inner-function body
    (compiled into the same buffer),
  - every `:store-var' / `:load-var' on a letrec-bound name emits
    RIP-relative MOV against a dedicated `cell:NAME' slot at the
    JIT page tail (= recursive references reach across the
    inner-lambda boundary).

The pre-T84 path (T43 SHIPPED) emitted a self-pointer trampoline for
`:closure' so calls into the closure RET'd immediately; fib(30)
returned ~2x a pointer address rather than 832040.  T84 fixes that
by compiling the inner lambda into the shared buffer and resolving
the closure pointer to the actual body."
  (require 'nelisp-cc-callees)
  (nelisp-cc-x86_64--compile-and-link function alloc-state))

(defun nelisp-cc-x86_64--compile-and-link (function alloc-state)
  "T84 Phase 7.5 wire — full pipeline x86_64 entry.

Stages:
  1. Walk the SSA (outer + inner lambdas) collecting:
       - the unique cell-symbol set (`letrec' names + `cons-counter'),
       - the indexed list of inner functions (= `:closure' sites),
       - the unique callee set referenced by `:call' opcodes.
  2. Annotate each `:closure' instruction with `:inner-function-index'
     so `--lower-closure' emits a stable `inner:N' label fixup.
  3. Compile the outer body into a fresh buffer (cell symbols
     threaded so `--lower-load-var' / `--lower-store-var' emit
     RIP-relative MOV against `cell:NAME').
  4. Compile each inner function into the SAME buffer, binding the
     `inner:N' label at its entry and re-using a per-inner block-
     label namespace so the outer / inner block IDs do not collide.
  5. Embed each unique primitive trampoline, binding `callee:NAME'
     so the in-buffer fixup mechanism resolves the rel32 — note
     the `cons' / `length' trampolines themselves carry a fixup
     against `cell:cons-counter' so the counter-bump increment +
     load resolve at finalize time.
  6. Append 8-byte cell slots, binding `cell:NAME' at the start
     of each slot.
  7. Finalize: `--buffer-finalize' walks every label fixup and
     patches the rel32 / disp32 fields in place."
  ;; T84 — pre-codegen SSA rewrite: turn scope-bound setq'd variables
  ;; into cell-routed `:load-var' / `:store-var' so the post-setq
  ;; reads see the new value (fix for `(while ... (setq i (1+ i)))'
  ;; infinite loop in the alloc-heavy bench).
  (require 'nelisp-cc-callees)
  (nelisp-cc-callees--rewrite-setq-vars function)
  ;; Re-run linear-scan because the rewrite added load-var
  ;; instructions which need their own intervals + register
  ;; assignments.
  (setq alloc-state (nelisp-cc--linear-scan function))
  (let* ((cells   (nelisp-cc-callees--collect-cell-symbols function))
         (closure-inners (nelisp-cc-callees--collect-inner-functions function))
         ;; T162 fix Gap 1 part 2: merge lifted-inners (= recorded by
         ;; `nelisp-cc-lift-pass' before it dropped the `:closure'
         ;; instruction) into the inners list so Step 4 still compiles
         ;; their bodies + binds `callee:<NAME>' aliases.  Pre-T162 the
         ;; closure-inners walk was the *only* source — lambda-lift
         ;; orphaned the inner SSA function as soon as it dropped the
         ;; closure instr → inner body never emitted → CALL fell
         ;; through to address 0 → SIGSEGV.
         ;;
         ;; Synthesise plist entries with a fresh :index counter that
         ;; follows the closure-inners' last index so the `inner:N'
         ;; namespace stays unique.  :instr is nil (no closure to
         ;; annotate with `:inner-function-index' — the lifted inner
         ;; is reached via :call :fn NAME, not :closure).
         (lifted-inners-raw (nelisp-cc--ssa-function-lifted-inners function))
         (lifted-inner-entries
          (let ((next-idx (1+ (length closure-inners)))
                (acc nil))
            (dolist (cell lifted-inners-raw)
              (let ((name (car cell))
                    (inner (cdr cell)))
                ;; Stamp the inner's name (= what `:call :fn NAME'
                ;; references) so the Step 4 alias-label loop binds
                ;; `callee:NAME' correctly.  lift-pass already does
                ;; this via `--lift-name-and-record', but defensively
                ;; ensure it here in case future lift variants don't.
                (unless (nelisp-cc--ssa-function-name inner)
                  (setf (nelisp-cc--ssa-function-name inner) name))
                (push (list :index next-idx :instr nil :inner inner) acc)
                (setq next-idx (1+ next-idx))))
            (nreverse acc)))
         (inners (append closure-inners lifted-inner-entries))
         (callees (nelisp-cc-x86_64--collect-callees function)))
    ;; Step 2: annotate closures with their visit index.
    ;; T162 fix: skip lifted-inner entries (= `:instr nil') because
    ;; their `:closure' instruction was already dropped by lift-pass —
    ;; only annotate `:closure' entries from `closure-inners'.
    (dolist (entry inners)
      (let ((idx (plist-get entry :index))
            (instr (plist-get entry :instr)))
        (when instr
          (setf (nelisp-cc--ssa-instr-meta instr)
                (plist-put (nelisp-cc--ssa-instr-meta instr)
                           :inner-function-index idx)))))
    ;; Step 3: compile outer body.
    (let* ((cg (nelisp-cc-x86_64--compile-with-cells
                function alloc-state cells)))
      ;; Step 4: compile each inner function into the same buffer.
      ;; T162 fix (Gap 1 = synthetic callee resolution): when the inner
      ;; function carries a name (= letrec-name from frontend, lambda-
      ;; lift synthesised `<CALLER>$lift$N' name, or a name rewritten in
      ;; by T161 :call-indirect → :call pre-pass) AND that name appears
      ;; in the outer-collected callee set, also bind `callee:<NAME>' as
      ;; an alias at the inner's entry address.  This lets `:call :fn
      ;; NAME' fixups (recorded by `--lower-call' for non-primitive
      ;; callees) resolve to the inner body, the same way primitive
      ;; trampolines do — without a separate trampoline step.
      ;; Pre-T162: `embed-trampolines' only binds primitive callees, so
      ;; `callee:fib' / `callee:anon$lift$0' fall through to address 0
      ;; → SIGSEGV on exec when `nelisp-cc-enable-7.7-passes t'.
      (dolist (entry inners)
        (let* ((idx     (plist-get entry :index))
               (inner   (plist-get entry :inner))
               (label   (intern (format "inner:%d" idx)))
               (inner-name (and inner (nelisp-cc--ssa-function-name inner))))
          (nelisp-cc-x86_64--buffer-define-label
           (nelisp-cc-x86_64--codegen-buffer cg) label)
          (when (and inner-name (memq inner-name callees))
            ;; Alias label at the same byte offset — both `inner:<IDX>'
            ;; and `callee:<NAME>' resolve here, then compile-inner-into
            ;; -buffer emits the body.
            (nelisp-cc-x86_64--buffer-define-label
             (nelisp-cc-x86_64--codegen-buffer cg)
             (intern (format "callee:%s" inner-name))))
          (nelisp-cc-x86_64--compile-inner-into-buffer
           inner cg cells idx)))
      ;; Step 5: embed primitive trampolines.  Only primitive callees
      ;; get trampoline bodies here; synthetic callees were handled in
      ;; Step 4 via the alias label so embed-trampolines is a no-op for
      ;; them (the `nelisp-cc-callees-known-p' guard skips non-primitives).
      (nelisp-cc-x86_64--embed-trampolines cg callees)
      ;; Step 6: append cell slots.
      (nelisp-cc-x86_64--embed-cells cg cells)
      ;; Step 7: finalize.
      (nelisp-cc-x86_64--buffer-finalize
       (nelisp-cc-x86_64--codegen-buffer cg)))))

(defun nelisp-cc-x86_64--collect-callees (function)
  "Return the unique callee symbols referenced via `:call' anywhere in
FUNCTION + nested `:closure' inner-functions.  T84 Phase 7.5 wire."
  (require 'nelisp-cc-callees)
  (let ((seen nil))
    (nelisp-cc-callees--walk-calls
     function
     (lambda (fn)
       (when (and fn (not (memq fn seen)))
         (push fn seen))))
    (nreverse seen)))

(defun nelisp-cc-x86_64--compile-with-cells (function alloc-state cells)
  "Compile FUNCTION's outer body into a fresh codegen, threading CELLS.
T84 Phase 7.5 wire — splits out from `--compile-internal' so the
caller can re-enter for inner-function compilation against the same
buffer."
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
               :frame-size frame-size
               :cell-symbols cells)))
    (nelisp-cc-x86_64--emit-prologue buf frame-size)
    ;; T84 — save spilled params from their arg-regs to spill slots
    ;; before any body code reads them.
    (nelisp-cc-x86_64--emit-param-home-spills cg)
    (let ((rpo (nelisp-cc--ssa--reverse-postorder function)))
      (setf (nelisp-cc-x86_64--codegen-rpo cg) rpo)
      (dolist (blk rpo)
        (let ((lbl (intern (format "L_block_%d"
                                   (nelisp-cc--ssa-block-id blk)))))
          (nelisp-cc-x86_64--buffer-define-label buf lbl))
        (dolist (instr (nelisp-cc--ssa-block-instrs blk))
          (nelisp-cc-x86_64--lower-instr cg instr))))
    cg))

(defun nelisp-cc-x86_64--find-closure-instr-for-inner (function idx)
  "Walk FUNCTION (an outer-level SSA function) and return the
`:closure' instruction whose META `:inner-function-index' equals IDX.
Returns nil when no match.  T96 Phase 7.1.5 — used by
`--compile-inner-into-buffer' to discover the letrec-name attached
to the outer-level closure that produced the inner function being
compiled."
  (catch 'found
    (dolist (blk (nelisp-cc--ssa-function-blocks function))
      (dolist (instr (nelisp-cc--ssa-block-instrs blk))
        (when (and (eq (nelisp-cc--ssa-instr-opcode instr) 'closure)
                   (eq (plist-get
                        (nelisp-cc--ssa-instr-meta instr)
                        :inner-function-index)
                       idx))
          (throw 'found instr))))
    nil))

(defun nelisp-cc-x86_64--compile-inner-into-buffer (function outer-cg cells idx)
  "Compile inner FUNCTION into OUTER-CG's buffer.  T84 Phase 7.5 wire.

Each inner function gets its own linear-scan + slot allocation, but
shares the byte buffer + cell symbols + label namespace.  Block
labels are renamed `L_block_<IDX>_<BID>' so the inner basic block
IDs do not collide with the outer / sibling inner functions.

Note: the inner function's `:call' fixups are recorded in a fresh
codegen's `call-fixups' list — but we drop that list here because
the new pipeline embeds primitive trampolines via the buffer-fixup
label mechanism rather than the post-finalize rel32 patcher.  Each
`--lower-call' inside the inner body still PUSHes its callee onto
the OUTER-CG's call-fixups so the after-finalize patcher catches
both outer + inner fixups in one pass when callers go through the
legacy entry."
  (let* ((alloc      (nelisp-cc--linear-scan function))
         (buf        (nelisp-cc-x86_64--codegen-buffer outer-cg))
         (slots-pair (nelisp-cc--allocate-stack-slots alloc))
         (slot-alist (car slots-pair))
         (frame-size (cdr slots-pair))
         (cg  (nelisp-cc-x86_64--codegen-make
               :function function
               :alloc-state alloc
               :buffer buf  ; shared!
               :slot-alist slot-alist
               :frame-size frame-size
               :cell-symbols cells)))
    ;; T96 Phase 7.1.5 — mark tail-self-call sites BEFORE phi-out so
    ;; the pattern (call-indirect → phi → return) is still legible.
    ;; The mark sets META :tail-call-self LETREC-NAME so the backend
    ;; lowerer emits a JMP to `inner:IDX:body' instead of a CALL+RET.
    ;;
    ;; Phase 7.1.1 (2026-04-27) — fall back to FUNCTION's own `:name'
    ;; slot when the outer-level `:closure' instruction is no longer
    ;; reachable via index lookup.  Lambda-lift drops the `:closure'
    ;; from the outer body once it lifts the inner to a top-level
    ;; entry, so for lifted-inners `find-closure-instr-for-inner'
    ;; returns nil — but lift-pass stamps `nelisp-cc--ssa-function-name'
    ;; with the original letrec name, which is exactly the symbol the
    ;; T161 rewrite uses for `:fn' meta on direct `:call' sites.  This
    ;; lets `--load-var-elidable-p' Path B + C fire on the unrolled fib
    ;; body (= 14 dead `:load-var fib' instructions per Phase 7.1.1
    ;; analysis)."
    (let* ((outer-fn (nelisp-cc-x86_64--codegen-function outer-cg))
           (closure-instr
            (nelisp-cc-x86_64--find-closure-instr-for-inner outer-fn idx))
           (letrec-name
            (or (and closure-instr
                     (plist-get (nelisp-cc--ssa-instr-meta closure-instr)
                                :letrec-name))
                ;; Phase 7.1.1 lifted-inner fallback.
                (nelisp-cc--ssa-function-name function))))
      (when letrec-name
        (nelisp-cc-callees--mark-tail-self-calls function letrec-name)
        (setf (nelisp-cc-x86_64--codegen-letrec-name cg) letrec-name)
        (setf (nelisp-cc-x86_64--codegen-tco-self-idx cg) idx)))
    (nelisp-cc--resolve-phis function)
    (nelisp-cc-x86_64--emit-prologue buf frame-size)
    ;; T96 Phase 7.1.5 — define `inner:IDX:body' here so that tail
    ;; self-calls JMP back BEFORE param-home-spill runs.  This lets
    ;; the new arg-reg values land in their spill slots on every
    ;; iteration (= param-home-spill is idempotent given fresh
    ;; arg-regs).
    (let ((body-lbl (intern (format "inner:%d:body" idx))))
      (nelisp-cc-x86_64--buffer-define-label buf body-lbl))
    ;; T84 — save spilled params from arg-regs into spill slots.
    (nelisp-cc-x86_64--emit-param-home-spills cg)
    (let ((rpo (nelisp-cc--ssa--reverse-postorder function)))
      (setf (nelisp-cc-x86_64--codegen-rpo cg) rpo)
      (dolist (blk rpo)
        (let ((lbl (intern (format "L_block_%d_%d"
                                   idx
                                   (nelisp-cc--ssa-block-id blk)))))
          (nelisp-cc-x86_64--buffer-define-label buf lbl))
        (dolist (instr (nelisp-cc--ssa-block-instrs blk))
          (nelisp-cc-x86_64--lower-instr-with-prefix cg instr idx))))
    ;; Merge the inner cg's call-fixups into the outer cg so post-
    ;; finalize patching catches everything.
    (setf (nelisp-cc-x86_64--codegen-call-fixups outer-cg)
          (append (nelisp-cc-x86_64--codegen-call-fixups cg)
                  (nelisp-cc-x86_64--codegen-call-fixups outer-cg)))))

(defun nelisp-cc-x86_64--lower-instr-with-prefix (cg instr idx)
  "Like `--lower-instr' but rewrites block-relative branch targets to
the inner-function's prefixed label namespace.  T84 Phase 7.5 wire.

Currently only `:branch' / `:jump' carry block IDs we need to
prefix; every other opcode lowers identically."
  (pcase (nelisp-cc--ssa-instr-opcode instr)
    ('branch (nelisp-cc-x86_64--lower-branch-prefixed cg instr idx))
    ('jump   (nelisp-cc-x86_64--lower-jump-prefixed cg instr idx))
    (_       (nelisp-cc-x86_64--lower-instr cg instr))))

(defun nelisp-cc-x86_64--lower-branch-prefixed (cg instr idx)
  "Inner-function-aware variant of `--lower-branch' that targets
`L_block_<IDX>_<BID>' labels."
  (let* ((buf      (nelisp-cc-x86_64--codegen-buffer cg))
         (operands (nelisp-cc--ssa-instr-operands instr))
         (meta     (nelisp-cc--ssa-instr-meta instr))
         (then-id  (plist-get meta :then))
         (else-id  (plist-get meta :else))
         (cond-val (car operands))
         (cond-reg (nelisp-cc-x86_64--materialise-operand cg cond-val)))
    (nelisp-cc-x86_64--buffer-emit-bytes
     buf (nelisp-cc-x86_64--emit-binop-reg-reg #x85 cond-reg cond-reg))
    (let ((else-label (intern (format "L_block_%d_%d" idx else-id))))
      (nelisp-cc-x86_64--buffer-emit-fixup
       buf (nelisp-cc-x86_64--emit-jcc-rel32 'je 0)
       else-label
       2))
    (let ((then-label (intern (format "L_block_%d_%d" idx then-id))))
      (nelisp-cc-x86_64--buffer-emit-fixup
       buf (nelisp-cc-x86_64--emit-jmp-rel32 0)
       then-label
       1))))

(defun nelisp-cc-x86_64--lower-jump-prefixed (cg instr idx)
  "Inner-function-aware variant of `--lower-jump'."
  (let* ((buf     (nelisp-cc-x86_64--codegen-buffer cg))
         (block   (nelisp-cc--ssa-instr-block instr))
         (succs   (nelisp-cc--ssa-block-successors block))
         (succ    (car succs)))
    (unless succ
      (signal 'nelisp-cc-x86_64-encoding-error
              (list :jump-with-no-successor
                    (nelisp-cc--ssa-block-id block))))
    (let ((target-label
           (intern (format "L_block_%d_%d"
                           idx (nelisp-cc--ssa-block-id succ)))))
      (nelisp-cc-x86_64--buffer-emit-fixup
       buf (nelisp-cc-x86_64--emit-jmp-rel32 0)
       target-label
       1))))

(defun nelisp-cc-x86_64--embed-trampolines (cg callees)
  "Embed each primitive trampoline into CG's buffer + bind the
`callee:NAME' label so `:call' rel32 fixups (added to the codegen
call-fixups list) can be retro-patched after finalize.  T84 Phase
7.5 wire.

For the cell-aware trampolines (`cons' / `length' bumping the
`cons-counter' cell), this helper emits the carefully crafted byte
sequence inline + records buffer-level fixups against `cell:cons-
counter' so the link-time resolver patches the disp32 fields in
place.  Other primitives use the static byte sequence from
`nelisp-cc-callees--x86_64-trampolines'."
  (require 'nelisp-cc-callees)
  (let ((buf (nelisp-cc-x86_64--codegen-buffer cg)))
    (dolist (sym callees)
      (when (nelisp-cc-callees-known-p sym 'x86_64)
        (let ((label (intern (format "callee:%s" sym))))
          (nelisp-cc-x86_64--buffer-define-label buf label)
          (cond
           ((eq sym 'cons)
            (nelisp-cc-x86_64--emit-cons-trampoline buf))
           ((eq sym 'length)
            (nelisp-cc-x86_64--emit-length-trampoline buf))
           (t
            (dolist (b (nelisp-cc-callees-trampoline-bytes sym 'x86_64))
              (nelisp-cc-x86_64--buffer-emit-byte buf b)))))))
    ;; Convert the call-fixups (currently keyed by callee SYMBOL +
    ;; absolute byte offset of the rel32 field) into post-finalize
    ;; patching that walks `callee:NAME' labels.  We do this by
    ;; injecting a synthetic buffer-fixup against the callee label
    ;; for each call site — that way `--buffer-finalize' patches the
    ;; rel32 in the same pass as every other label fixup.
    ;;
    ;; T162 fix: also convert fixups for *synthetic* callees (= names
    ;; in `callees' that aren't primitive trampolines but ARE bound by
    ;; the Step 4 alias-label path = lambda-lift / call-indirect rewrite
    ;; products).  Pre-T162 this guard required `known-p' which is
    ;; primitive-only, so synthetic call sites kept their call-fixups
    ;; entry, were dropped by the `setf nil' below, and the rel32 stayed
    ;; at 0 → SIGSEGV on exec under `nelisp-cc-enable-7.7-passes t'.
    (let ((fixups (nelisp-cc-x86_64--codegen-call-fixups cg)))
      (dolist (fx fixups)
        (let ((abs-off (car fx))
              (callee  (cdr fx)))
          (when (and callee (memq callee callees))
            ;; Whether primitive (Step 5 embedded trampoline) or
            ;; synthetic (Step 4 alias label), the post-finalize
            ;; fixup against `callee:<NAME>' resolves identically.
            (push (cons abs-off (intern (format "callee:%s" callee)))
                  (nelisp-cc-x86_64--buffer-fixups buf)))))
      ;; Drop the call-fixups so `--link-unresolved-calls' is not
      ;; invoked on this buffer (the legacy entry calls it; we
      ;; bypass).
      (setf (nelisp-cc-x86_64--codegen-call-fixups cg) nil))))

(defun nelisp-cc-x86_64--emit-cons-trampoline (buf)
  "T84 Phase 7.5 wire — `cons' counter-bump trampoline.

Bumps the `cell:cons-counter' cell and returns the second arg
(rsi) — semantics: the bench harness's `(cons i acc)' chain only
needs *some* non-zero pointer to keep the loop alive and a counter
so `length' can return the right value.  Real cons-cell allocation
is Phase 7.6 (Doc 32 v2 §3 Stage 2 closure-pool / cons-pool).

Bytes:
  48 FF 05 <disp32>     INC qword [rip + cell:cons-counter]
  48 89 F0              MOV rax, rsi  (= return cdr ≈ tail of list)
  C3                    RET"
  (let* ((before (nelisp-cc-x86_64--buffer-offset buf)))
    (nelisp-cc-x86_64--buffer-emit-bytes
     buf (nelisp-cc-x86_64--emit-inc-mem-rip-rel 0))
    (push (cons (+ before 3) 'cell:cons-counter)
          (nelisp-cc-x86_64--buffer-fixups buf)))
  ;; MOV rax, rsi
  (nelisp-cc-x86_64--buffer-emit-bytes
   buf (nelisp-cc-x86_64--emit-mov-reg-reg 'rax 'rsi))
  (nelisp-cc-x86_64--buffer-emit-bytes
   buf (nelisp-cc-x86_64--emit-ret)))

(defun nelisp-cc-x86_64--emit-length-trampoline (buf)
  "T84 Phase 7.5 wire — `length' counter-load trampoline.

Loads the `cell:cons-counter' cell into rax + RET.  Pairs with
`--emit-cons-trampoline' so the bench-actual `(length acc)' returns
the count of `cons' calls (= 1000000 for the alloc-heavy axis).

Bytes:
  48 8B 05 <disp32>     MOV rax, [rip + cell:cons-counter]
  C3                    RET"
  (let* ((before (nelisp-cc-x86_64--buffer-offset buf)))
    (nelisp-cc-x86_64--buffer-emit-bytes
     buf (nelisp-cc-x86_64--emit-mov-rax-rip-rel 0))
    (push (cons (+ before 3) 'cell:cons-counter)
          (nelisp-cc-x86_64--buffer-fixups buf)))
  (nelisp-cc-x86_64--buffer-emit-bytes
   buf (nelisp-cc-x86_64--emit-ret)))

(defun nelisp-cc-x86_64--embed-cells (cg cells)
  "Append 8 zero bytes for each symbol in CELLS, binding `cell:NAME'
at the start of each slot.  T84 Phase 7.5 wire — final tail of the
JIT page; reachable RIP-relative from every embedded body / trampoline."
  (let ((buf (nelisp-cc-x86_64--codegen-buffer cg)))
    (dolist (sym cells)
      (let ((label (intern (format "cell:%s" sym))))
        (nelisp-cc-x86_64--buffer-define-label buf label)
        (dotimes (_ 8)
          (nelisp-cc-x86_64--buffer-emit-byte buf 0))))))

(provide 'nelisp-cc-x86_64)
;;; nelisp-cc-x86_64.el ends here
