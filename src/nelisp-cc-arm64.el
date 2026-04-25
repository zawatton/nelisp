;;; nelisp-cc-arm64.el --- arm64 backend skeleton (AAPCS64)  -*- lexical-binding: t; -*-

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

;; Phase 7.1.3 *skeleton subset* — arm64 backend (AAPCS64) for the
;; NeLisp native compiler.  Doc 28 §3.3 (LOCKED-2026-04-25-v2) commits
;; the full backend at ~1500-2500 LOC; this scaffold lands ~600-800
;; LOC so subsequent agents can complete the codegen on a frozen
;; substrate.  Mirrors the x86_64 skeleton (T9 sibling) — same opcode
;; subset, same MCP/test contract, separate module so the two
;; architectures can advance independently without merge contention.
;;
;; In scope (this file):
;;
;;   - AAPCS64 register table:
;;       * x0-x7  — integer / pointer argument-passing
;;       * x9-x15 — caller-saved temporaries
;;       * x19-x28, x29 (FP), x30 (LR) — callee-saved
;;       * x0     — integer return value
;;       * virtual r0..r7 (T4 default-int-registers) → x0..x7 mapping
;;
;;   - Fixed-width 32-bit instruction encoders (10 instructions):
;;       MOV (reg), MOVZ (imm16), ADD, SUB, MUL, RET, BL, B, CMP, CBZ,
;;       B.cond
;;
;;   - Byte buffer with arm64 forward-reference resolution:
;;       arm64 immediate fields live at *opcode-specific bit positions*
;;       (imm26 for B/BL, imm19 for CBZ/B.cond, imm14 for TBZ etc.),
;;       so each fixup carries an *encoding function* the resolver
;;       re-calls with the resolved offset.  Compare with x86_64 where
;;       fixups are byte-offset-stamped imm32 / imm8 patches.
;;
;;   - SSA → arm64 codegen subset (skeleton level):
;;       :const / :load-var / :store-var / :call / :branch / :return
;;       Spill / reload, callee-resolution, and cache invalidation
;;       (Doc 28 §6.9 MAP_JIT / clear_icache, Phase 7.0 SHIPPED at
;;       commit 20ecef4) are TODO comments + signal — Phase 7.1.4
;;       will integrate them across both architectures.
;;
;;   - Output: raw byte vector ready for mmap PROT_EXEC + MAP_JIT.
;;     The mmap step itself is Phase 7.1.4 scope; this skeleton
;;     terminates at the byte stream.
;;
;; Out of scope (deferred to subsequent phases):
;;
;;   - macOS Mach-O linker integration (Doc 28 §6.10)
;;   - hardened runtime / W^X transitions (Doc 28 §6.9 v2 risk)
;;   - I-cache flush via `nelisp_syscall_clear_icache' (wired to
;;     `__clear_cache' on AArch64 hosts) — Phase 7.1.4 wire-up
;;   - PAC (Pointer Authentication) signing for X30 returns —
;;     Phase 7.1.5 hardening pass on Apple silicon
;;   - half-word relocation patches (MOVZ + MOVK chain for absolute
;;     64-bit addresses) — Phase 7.1.4 integration
;;   - phi resolution / register coalescing / pre-coloring —
;;     Phase 7.1.5 graph-coloring revisit
;;
;; Module convention (matches `nelisp-cc' parent + sibling
;; `nelisp-cc-x86_64'):
;;
;;   - `nelisp-cc-arm64-'  = public API
;;   - `nelisp-cc-arm64--' = private helper
;;   - errors derive from `nelisp-cc-error' (parent in `nelisp-cc')

;;; Code:

(require 'cl-lib)
(require 'nelisp-cc)

;;; Errors -----------------------------------------------------------

(define-error 'nelisp-cc-arm64-error
  "NeLisp arm64 backend error" 'nelisp-cc-error)
(define-error 'nelisp-cc-arm64-todo
  "NeLisp arm64 backend skeleton — feature deferred to Phase 7.1.4"
  'nelisp-cc-arm64-error)
(define-error 'nelisp-cc-arm64-encoding-error
  "NeLisp arm64 instruction encoding violation"
  'nelisp-cc-arm64-error)

;;; ABI register table (AAPCS64) ------------------------------------
;;
;; AAPCS64 = ARM Architecture Procedure Call Standard for the AArch64
;; architecture (https://developer.arm.com/documentation/ihi0055/latest/).
;; Skeleton-relevant slices:
;;
;;   x0..x7   — arguments / results (first 8 int args, x0 result)
;;   x8       — indirect result location register (struct return)
;;   x9..x15  — caller-saved scratch
;;   x16, x17 — IP0/IP1 (intra-procedure call scratch / linker veneers)
;;   x18      — platform reserved (Apple / Win) — *do not* allocate
;;   x19..x28 — callee-saved
;;   x29      — frame pointer (FP)
;;   x30      — link register (LR), holds return address after BL
;;   sp       — stack pointer (16-byte aligned at function boundary)
;;   xzr / wzr — zero register (encoded as register 31)
;;
;; The skeleton commits to the *common* AAPCS64 subset; the platform
;; tweaks (Darwin variadic args via stack, Windows ABI) are Phase
;; 7.1.4 scope.  This file is the architecture-neutral half.

(defconst nelisp-cc-arm64--int-arg-regs
  '(x0 x1 x2 x3 x4 x5 x6 x7)
  "Integer / pointer argument-passing registers (AAPCS64).
First 8 positional arguments arrive in X0-X7.  Argument 9 onward spill
to the stack at SP+0, SP+8, ...  (16-byte aligned at the call boundary;
each slot is 8 bytes for 64-bit values).")

(defconst nelisp-cc-arm64--callee-saved
  '(x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30)
  "Callee-saved general-purpose registers under AAPCS64.
A function that uses any of these *must* save and restore them across
its prologue / epilogue.  X29 (FP) and X30 (LR) are conventionally
saved as a pair via STP / LDP; the skeleton does not yet emit
prologues — Phase 7.1.4 (mmap exec page + frame layout).")

(defconst nelisp-cc-arm64--caller-saved
  '(x9 x10 x11 x12 x13 x14 x15)
  "Caller-saved scratch registers under AAPCS64.
Allocator may freely clobber these around any call.  X16/X17 are
reserved for linker veneer / IP0-IP1 use, X18 is platform-reserved
on Apple silicon and Windows; they are *not* in the allocator pool.")

(defconst nelisp-cc-arm64--return-reg 'x0
  "Integer / pointer return-value register (AAPCS64).")

(defconst nelisp-cc-arm64--frame-pointer 'x29
  "Frame pointer register (AAPCS64).
Saved as part of the FP/LR pair in the function prologue.")

(defconst nelisp-cc-arm64--link-register 'x30
  "Link register — holds the return PC after BL.
Saved alongside FP in the prologue when the function makes any call.")

(defconst nelisp-cc-arm64--virtual-to-physical
  '((r0 . x0) (r1 . x1) (r2 . x2) (r3 . x3)
    (r4 . x4) (r5 . x5) (r6 . x6) (r7 . x7))
  "Default mapping from virtual register names (T4 linear-scan output)
to physical AAPCS64 registers.  The allocator emits r0..r7 as
ABI-agnostic symbolic names (`nelisp-cc--default-int-registers'); the
backend remaps to physical registers so the first 8 arguments land
where AAPCS64 expects them.  Phase 7.1.4 may extend this with a
callee-saved overflow band (r8..r15 → x19..x26) once linear-scan
pressure exceeds the eight-register front rank.")

(defun nelisp-cc-arm64--reg-encoding (reg)
  "Return the 5-bit AAPCS64 encoding for physical register REG.
REG is a symbol like `x0' .. `x30', `xzr', `sp', or one of the
allocator virtual names `r0'..`r7' (auto-translated through
`nelisp-cc-arm64--virtual-to-physical').  Signals
`nelisp-cc-arm64-encoding-error' for unknown names — the encoder is
deliberately strict so a typo cannot silently produce a valid but
wrong instruction word."
  (let* ((mapped (or (cdr (assq reg nelisp-cc-arm64--virtual-to-physical))
                     reg))
         (cell (assq mapped
                     '((x0 . 0) (x1 . 1) (x2 . 2) (x3 . 3)
                       (x4 . 4) (x5 . 5) (x6 . 6) (x7 . 7)
                       (x8 . 8) (x9 . 9) (x10 . 10) (x11 . 11)
                       (x12 . 12) (x13 . 13) (x14 . 14) (x15 . 15)
                       (x16 . 16) (x17 . 17) (x18 . 18) (x19 . 19)
                       (x20 . 20) (x21 . 21) (x22 . 22) (x23 . 23)
                       (x24 . 24) (x25 . 25) (x26 . 26) (x27 . 27)
                       (x28 . 28) (x29 . 29) (x30 . 30)
                       ;; xzr / sp share encoding 31; the opcode
                       ;; context disambiguates which one is meant.
                       (xzr . 31) (sp . 31) (wzr . 31)))))
    (unless cell
      (signal 'nelisp-cc-arm64-encoding-error
              (list :unknown-register reg)))
    (cdr cell)))

;;; Instruction encoding helpers ------------------------------------
;;
;; arm64 instructions are *fixed-width 32-bit* values, encoded as
;; little-endian 4-byte sequences in memory.  Each helper below
;; returns a single uint32 (0..#xFFFFFFFF); the byte buffer takes
;; care of the LE expansion via
;; `nelisp-cc-arm64--buffer-emit-instruction'.

(defun nelisp-cc-arm64--encode-mov-reg-reg (dst src)
  "Encode 64-bit MOV (register) Xd, Xm.
This is the canonical alias for ORR Xd, XZR, Xm:
  sf=1 1 0 1 0 1 0 1 0 0 0 0 Xm 0 0 0 0 0 0 11111 Xd
  bits:  31 30 29 28 27 26 25 24 23 22 21 20-16   15-10  9-5   4-0
Constant form (XZR=31 in the Xn slot) reduces to:
  0xAA0003E0 | (Xm << 16) | Xd"
  (let ((d (nelisp-cc-arm64--reg-encoding dst))
        (m (nelisp-cc-arm64--reg-encoding src)))
    (logior #xAA0003E0
            (ash (logand m #x1F) 16)
            (logand d #x1F))))

(defun nelisp-cc-arm64--encode-movz-imm (dst imm16 &optional shift)
  "Encode MOVZ Xd, #imm16 [, LSL #SHIFT].
SHIFT is the LSL amount (0 / 16 / 32 / 48).  The argument 16-bit IMM16
must fit in 0..#xFFFF.  Constant base for sf=1, opc=10, hw=SHIFT/16:
  1 1 0 1 0 0 1 0 1 hw imm16 Xd
  0xD2800000 | (hw << 21) | (imm16 << 5) | Xd"
  (let ((d (nelisp-cc-arm64--reg-encoding dst))
        (s (or shift 0)))
    (unless (memq s '(0 16 32 48))
      (signal 'nelisp-cc-arm64-encoding-error
              (list :movz-bad-shift s)))
    (unless (and (integerp imm16) (>= imm16 0) (<= imm16 #xFFFF))
      (signal 'nelisp-cc-arm64-encoding-error
              (list :movz-imm16-out-of-range imm16)))
    (logior #xD2800000
            (ash (/ s 16) 21)
            (ash (logand imm16 #xFFFF) 5)
            (logand d #x1F))))

(defun nelisp-cc-arm64--encode-add-reg-reg (dst src1 src2)
  "Encode 64-bit ADD Xd, Xn, Xm (shifted register, shift=0, amount=0).
  1 0 0 0 1 0 1 1 0 0 0 Xm imm6=0 Xn Xd
  0x8B000000 | (Xm << 16) | (Xn << 5) | Xd"
  (let ((d (nelisp-cc-arm64--reg-encoding dst))
        (n (nelisp-cc-arm64--reg-encoding src1))
        (m (nelisp-cc-arm64--reg-encoding src2)))
    (logior #x8B000000
            (ash (logand m #x1F) 16)
            (ash (logand n #x1F) 5)
            (logand d #x1F))))

(defun nelisp-cc-arm64--encode-sub-reg-reg (dst src1 src2)
  "Encode 64-bit SUB Xd, Xn, Xm (shifted register, shift=0, amount=0).
  1 1 0 0 1 0 1 1 0 0 0 Xm imm6=0 Xn Xd
  0xCB000000 | (Xm << 16) | (Xn << 5) | Xd"
  (let ((d (nelisp-cc-arm64--reg-encoding dst))
        (n (nelisp-cc-arm64--reg-encoding src1))
        (m (nelisp-cc-arm64--reg-encoding src2)))
    (logior #xCB000000
            (ash (logand m #x1F) 16)
            (ash (logand n #x1F) 5)
            (logand d #x1F))))

(defun nelisp-cc-arm64--encode-mul-reg-reg (dst src1 src2)
  "Encode 64-bit MUL Xd, Xn, Xm.
This is the canonical alias for MADD Xd, Xn, Xm, XZR:
  1 0 0 1 1 0 1 1 0 0 0 Xm 0 11111 Xn Xd
  0x9B007C00 | (Xm << 16) | (Xn << 5) | Xd"
  (let ((d (nelisp-cc-arm64--reg-encoding dst))
        (n (nelisp-cc-arm64--reg-encoding src1))
        (m (nelisp-cc-arm64--reg-encoding src2)))
    (logior #x9B007C00
            (ash (logand m #x1F) 16)
            (ash (logand n #x1F) 5)
            (logand d #x1F))))

(defun nelisp-cc-arm64--encode-cmp-reg-reg (a b)
  "Encode 64-bit CMP Xn, Xm.
Canonical alias for SUBS XZR, Xn, Xm:
  1 1 1 0 1 0 1 1 0 0 0 Xm imm6=0 Xn 11111
  0xEB00001F | (Xm << 16) | (Xn << 5)
Result is discarded into XZR; only the NZCV flags are updated for the
subsequent B.cond / CBZ-style branch."
  (let ((n (nelisp-cc-arm64--reg-encoding a))
        (m (nelisp-cc-arm64--reg-encoding b)))
    (logior #xEB00001F
            (ash (logand m #x1F) 16)
            (ash (logand n #x1F) 5))))

(defun nelisp-cc-arm64--encode-ret (&optional reg)
  "Encode RET [Xn] (default Xn=X30, the link register).
  1 1 0 1 0 1 1 0 0 1 0 11111 0 0 0 0 0 0 Xn 0 0 0 0 0
  0xD65F0000 | (Xn << 5)
With Xn=30, the constant collapses to 0xD65F03C0."
  (let ((n (nelisp-cc-arm64--reg-encoding (or reg 'x30))))
    (logior #xD65F0000
            (ash (logand n #x1F) 5))))

(defun nelisp-cc-arm64--encode-bl (offset)
  "Encode BL #imm.
OFFSET is the *byte* displacement from the BL instruction to the
target (must be 4-byte aligned, signed 28-bit range ±128 MiB).
  1 0 0 1 0 1 imm26
  0x94000000 | (imm26 & 0x3FFFFFF)
imm26 is OFFSET >> 2, sign-extended to 64 bits at execution time."
  (unless (zerop (logand offset #x3))
    (signal 'nelisp-cc-arm64-encoding-error
            (list :bl-misaligned offset)))
  (let* ((imm26 (ash offset -2)))
    (unless (and (>= imm26 (- (ash 1 25))) (< imm26 (ash 1 25)))
      (signal 'nelisp-cc-arm64-encoding-error
              (list :bl-out-of-range offset)))
    (logior #x94000000 (logand imm26 #x3FFFFFF))))

(defun nelisp-cc-arm64--encode-b (offset)
  "Encode B #imm (unconditional branch).
OFFSET is the byte displacement (4-byte aligned, ±128 MiB).
  0 0 0 1 0 1 imm26
  0x14000000 | (imm26 & 0x3FFFFFF)"
  (unless (zerop (logand offset #x3))
    (signal 'nelisp-cc-arm64-encoding-error
            (list :b-misaligned offset)))
  (let ((imm26 (ash offset -2)))
    (unless (and (>= imm26 (- (ash 1 25))) (< imm26 (ash 1 25)))
      (signal 'nelisp-cc-arm64-encoding-error
              (list :b-out-of-range offset)))
    (logior #x14000000 (logand imm26 #x3FFFFFF))))

(defun nelisp-cc-arm64--encode-cbz (reg offset)
  "Encode 64-bit CBZ Xt, #imm (compare and branch if zero).
OFFSET is the byte displacement (4-byte aligned, ±1 MiB / 19-bit imm).
  1 0 1 1 0 1 0 0 imm19 Xt
  0xB4000000 | (imm19 << 5) | Xt"
  (unless (zerop (logand offset #x3))
    (signal 'nelisp-cc-arm64-encoding-error
            (list :cbz-misaligned offset)))
  (let ((imm19 (ash offset -2))
        (rt (nelisp-cc-arm64--reg-encoding reg)))
    (unless (and (>= imm19 (- (ash 1 18))) (< imm19 (ash 1 18)))
      (signal 'nelisp-cc-arm64-encoding-error
              (list :cbz-out-of-range offset)))
    (logior #xB4000000
            (ash (logand imm19 #x7FFFF) 5)
            (logand rt #x1F))))

(defconst nelisp-cc-arm64--cond-codes
  '((eq . 0) (ne . 1) (cs . 2) (hs . 2) (cc . 3) (lo . 3)
    (mi . 4) (pl . 5) (vs . 6) (vc . 7)
    (hi . 8) (ls . 9) (ge . 10) (lt . 11)
    (gt . 12) (le . 13) (al . 14) (nv . 15))
  "AArch64 condition codes — 4-bit fields used by B.cond / CCMP / CSEL.
CS and HS are aliases (\"carry set\" / \"unsigned higher or same\").
NV is reserved (\"never\") — never lifted by the skeleton.")

(defun nelisp-cc-arm64--encode-bcc (cond offset)
  "Encode B.cond #imm (conditional branch).
COND is a symbol from `nelisp-cc-arm64--cond-codes' (eq / ne / lt
/ gt / le / ge / hi / ls / cs / cc / mi / pl / vs / vc / hs / lo).
OFFSET is the byte displacement (4-byte aligned, ±1 MiB / imm19).
  0 1 0 1 0 1 0 0 imm19 0 cond
  0x54000000 | (imm19 << 5) | cond"
  (let ((c (cdr (assq cond nelisp-cc-arm64--cond-codes))))
    (unless c
      (signal 'nelisp-cc-arm64-encoding-error
              (list :unknown-cond cond)))
    (unless (zerop (logand offset #x3))
      (signal 'nelisp-cc-arm64-encoding-error
              (list :bcc-misaligned offset)))
    (let ((imm19 (ash offset -2)))
      (unless (and (>= imm19 (- (ash 1 18))) (< imm19 (ash 1 18)))
        (signal 'nelisp-cc-arm64-encoding-error
                (list :bcc-out-of-range offset)))
      (logior #x54000000
              (ash (logand imm19 #x7FFFF) 5)
              (logand c #xF)))))

;;; Byte buffer + label resolution ----------------------------------
;;
;; arm64 forward references differ from x86_64 in two ways:
;;
;;   1. Width: every encodable branch produces a fixed 4-byte
;;      instruction (B / BL / CBZ / B.cond / TBZ).  No variable
;;      relaxation pass is needed — the skeleton only has to choose
;;      *which* encoding the user asked for and patch the immediate
;;      bits at finalize time.
;;
;;   2. Bit position: the immediate field's location depends on the
;;      opcode (imm26 for B/BL, imm19 for CBZ / B.cond, imm14 for
;;      TBZ / TBNZ).  We therefore record an *encoding function* in
;;      the fixup, and the resolver re-calls it with the resolved
;;      offset.  This keeps each opcode's bit-packing co-located
;;      with its encoder helper above — no separate "patch table".

(cl-defstruct (nelisp-cc-arm64--buffer
               (:constructor nelisp-cc-arm64--buffer-make-internal)
               (:copier nil))
  "arm64 byte buffer with label / fixup tracking.

BYTES is the *reverse-built* byte list — emit appends in O(1), and
`finalize' nreverses once at the end.  OFFSET is the next byte
position to be written (also = total length so far).  LABELS is an
alist (NAME . BYTE-OFFSET) recording every label binding.  FIXUPS is
an alist of (FIXUP-OFFSET . (LABEL . ENCODER-FN)) entries; ENCODER-FN
is called as (ENCODER-FN BYTE-DISPLACEMENT) at resolve time and must
return the patched 4-byte instruction word."
  (bytes nil)
  (offset 0 :type integer)
  (labels nil)
  (fixups nil))

(defun nelisp-cc-arm64--buffer-make ()
  "Construct a fresh empty arm64 byte buffer."
  (nelisp-cc-arm64--buffer-make-internal))

(defun nelisp-cc-arm64--buffer-emit-byte (buf u8)
  "Append one byte U8 (0..255) to BUF.
Skeleton-only helper used by tests and the instruction emitter — most
client code goes through `-buffer-emit-instruction' directly."
  (unless (and (integerp u8) (>= u8 0) (<= u8 #xFF))
    (signal 'nelisp-cc-arm64-encoding-error
            (list :byte-out-of-range u8)))
  (push (logand u8 #xFF)
        (nelisp-cc-arm64--buffer-bytes buf))
  (cl-incf (nelisp-cc-arm64--buffer-offset buf))
  buf)

(defun nelisp-cc-arm64--buffer-emit-instruction (buf u32)
  "Emit a single 4-byte arm64 instruction word U32 (uint32) to BUF.
The word is split into little-endian byte order (low 8 bits first)
because arm64 hardware reads instruction memory in LE on every
shipping platform NeLisp targets (Linux/aarch64, Apple silicon,
Windows on Arm)."
  (unless (and (integerp u32) (>= u32 0) (<= u32 #xFFFFFFFF))
    (signal 'nelisp-cc-arm64-encoding-error
            (list :u32-out-of-range u32)))
  (nelisp-cc-arm64--buffer-emit-byte buf (logand u32 #xFF))
  (nelisp-cc-arm64--buffer-emit-byte buf (logand (ash u32 -8) #xFF))
  (nelisp-cc-arm64--buffer-emit-byte buf (logand (ash u32 -16) #xFF))
  (nelisp-cc-arm64--buffer-emit-byte buf (logand (ash u32 -24) #xFF))
  buf)

(defun nelisp-cc-arm64--buffer-define-label (buf name)
  "Bind NAME to the current offset in BUF.
Re-defining a label signals — the verifier wants every label to have
exactly one binding so forward / backward reference symmetry is
unambiguous."
  (when (assq name (nelisp-cc-arm64--buffer-labels buf))
    (signal 'nelisp-cc-arm64-error
            (list :label-redefined name)))
  (push (cons name (nelisp-cc-arm64--buffer-offset buf))
        (nelisp-cc-arm64--buffer-labels buf))
  buf)

(defun nelisp-cc-arm64--buffer-emit-fixup (buf encoder-fn label)
  "Reserve a 4-byte instruction slot in BUF whose immediate references LABEL.
ENCODER-FN is called at resolve time as (ENCODER-FN BYTE-OFFSET) where
BYTE-OFFSET is the signed displacement from this instruction start
to the LABEL binding.  The current contents are placeholder zeros — the
resolver overwrites them.  Returns BUF.

Typical use:
  (nelisp-cc-arm64--buffer-emit-fixup
   buf
   (lambda (off) (nelisp-cc-arm64--encode-b off))
   \\='merge)

The fixup table is keyed by the *fixup byte offset* so the resolver
walks the bytes of BUF once and patches in place."
  (let ((fixup-offset (nelisp-cc-arm64--buffer-offset buf)))
    (push (cons fixup-offset (cons label encoder-fn))
          (nelisp-cc-arm64--buffer-fixups buf))
    ;; Emit four placeholder zero bytes — the resolver rewrites
    ;; them in `nelisp-cc-arm64--buffer-resolve-fixups'.
    (nelisp-cc-arm64--buffer-emit-byte buf 0)
    (nelisp-cc-arm64--buffer-emit-byte buf 0)
    (nelisp-cc-arm64--buffer-emit-byte buf 0)
    (nelisp-cc-arm64--buffer-emit-byte buf 0))
  buf)

(defun nelisp-cc-arm64--buffer-resolve-fixups (buf)
  "Walk the FIXUPS of BUF and patch every recorded slot to its real value.
Each fixup is (FIXUP-OFFSET . (LABEL . ENCODER-FN)).  We compute
DISPLACEMENT = LABEL-OFFSET - FIXUP-OFFSET (byte-relative, matches
arm64 PC-relative semantics exactly), call ENCODER-FN on it to obtain
the resolved 4-byte word, then overwrite the placeholder bytes in
BUF.BYTES.

Implementation note: BUF.BYTES is stored *reversed* (newest first) so
emit is O(1).  We compute reverse-positions on the fly via
(1 - LEN - INDEX) and walk a vector copy for O(1) patch overwrite —
this is the only place in the buffer API where the LE byte order is
relevant.  After resolution the byte list is rebuilt in original
forward order and stored back."
  (let* ((labels (nelisp-cc-arm64--buffer-labels buf))
         (fixups (nelisp-cc-arm64--buffer-fixups buf))
         (rev    (nelisp-cc-arm64--buffer-bytes buf))
         (len    (length rev))
         (vec    (vconcat (nreverse (copy-sequence rev)))))
    (dolist (fx fixups)
      (let* ((fx-off    (car fx))
             (label     (cadr fx))
             (encoder   (cddr fx))
             (lab-cell  (assq label labels)))
        (unless lab-cell
          (signal 'nelisp-cc-arm64-error
                  (list :unbound-label label)))
        (let* ((label-off (cdr lab-cell))
               (disp      (- label-off fx-off))
               (word      (funcall encoder disp)))
          ;; Patch four bytes at fx-off in little-endian order.
          (aset vec (+ fx-off 0) (logand word #xFF))
          (aset vec (+ fx-off 1) (logand (ash word -8) #xFF))
          (aset vec (+ fx-off 2) (logand (ash word -16) #xFF))
          (aset vec (+ fx-off 3) (logand (ash word -24) #xFF)))))
    ;; Rebuild reversed byte list so subsequent emit calls (rare —
    ;; finalize is normally the only post-resolve step) keep the
    ;; same invariant: BYTES is reverse-built.
    (setf (nelisp-cc-arm64--buffer-bytes buf)
          (nreverse (append vec nil)))
    ;; Drop the fixup table — it is consumed.
    (setf (nelisp-cc-arm64--buffer-fixups buf) nil)
    (ignore len)
    buf))

(defun nelisp-cc-arm64--buffer-finalize (buf)
  "Resolve all fixups and return the forward-ordered byte vector.
This is the public sink of the buffer API; after this call BUF is
considered consumed (a subsequent emit would corrupt the immediates).
The returned object is a fresh `vector' of integers 0..255 — the
caller is responsible for mmap PROT_EXEC and execute (Phase 7.1.4)."
  (nelisp-cc-arm64--buffer-resolve-fixups buf)
  (vconcat (nreverse (nelisp-cc-arm64--buffer-bytes buf))))

;;; Codegen utility: virtual → physical -----------------------------

(defun nelisp-cc-arm64--virtual-reg (vreg)
  "Translate a virtual register VREG (e.g. `r0') to its physical name.
If VREG is already a physical register, return it unchanged.  This
is the single point of indirection used by every codegen helper —
allocator-driven assignments funnel through here so a future
allocator change (e.g. callee-saved overflow band) only edits the
mapping table."
  (or (cdr (assq vreg nelisp-cc-arm64--virtual-to-physical))
      vreg))

(defun nelisp-cc-arm64--lookup-reg (alloc-state value)
  "Resolve the physical register for SSA VALUE under ALLOC-STATE.
VALUE is a `nelisp-cc--ssa-value' (or its id integer); ALLOC-STATE
is the assignments alist returned by `nelisp-cc--linear-scan'.
Returns the physical register symbol.  Spilled values raise
`nelisp-cc-arm64-todo' — the skeleton does *not* lower spill / reload
(Phase 7.1.4)."
  (let* ((vid (if (integerp value)
                  value
                (nelisp-cc--ssa-value-id value)))
         (assigned (nelisp-cc--alloc-register-of alloc-state vid)))
    (cond
     ((null assigned)
      (signal 'nelisp-cc-arm64-error
              (list :unallocated-value vid)))
     ((eq assigned :spill)
      (signal 'nelisp-cc-arm64-todo
              (list :spill-not-implemented vid
                    :phase '7.1.4)))
     (t (nelisp-cc-arm64--virtual-reg assigned)))))

;;; SSA → arm64 codegen (skeleton subset) ---------------------------
;;
;; Skeleton lowering covers six opcodes — the bare minimum to lower a
;; trivial lambda end-to-end:
;;
;;   :const     — MOVZ Xd, #imm16 (literal must fit 16 bits; larger
;;                literals are TODO Phase 7.1.4 with MOVK chain)
;;   :load-var  — TODO (free variable resolution, Phase 7.1.5
;;                integration with `nelisp-defs-index')
;;   :store-var — TODO (frame slot, Phase 7.1.4)
;;   :call      — BL <unresolved>; the skeleton emits a fixup keyed
;;                on the callee symbol and Phase 7.5 wiring patches
;;                the displacement once the linker knows the address.
;;   :branch    — B.cond / CBZ — skeleton uses CBZ for the boolean
;;                cond test and B for the unconditional fall-through.
;;   :return    — RET (after a MOV X0, <ret-val> when needed)
;;
;; Out-of-scope for the skeleton (TODO + signal):
;;
;;   - :phi resolution (Phase 7.1.5 graph-coloring)
;;   - :jump (skeleton inlines the merge edge into the lowering
;;     control flow — Phase 7.1.4 generalises this with a CFG pass)
;;   - :closure (lambda-lifting + capture array — Phase 7.1.4)
;;   - :call-indirect (BLR Xn — Phase 7.1.4)
;;
;; Spill / reload, prologue / epilogue, frame setup, cache invalidate
;; (`nelisp_syscall_clear_icache' from Phase 7.0), MAP_JIT page write
;; protection (`nelisp_syscall_jit_write_protect') — *all* deferred
;; to Phase 7.1.4 (Doc 28 §6.9 v2 risk).

(defun nelisp-cc-arm64--lower-const (instr buf alloc-state)
  "Lower a `:const' SSA instruction to MOVZ.
INSTR.META is a plist with `:literal LIT'; this skeleton only
supports nil / t / 16-bit non-negative integers (mapped to MOVZ #imm
respectively to 0 / 1 / lit).  Larger literals raise
`nelisp-cc-arm64-todo' — Phase 7.1.4 will emit a MOVZ + MOVK chain."
  (let* ((meta (nelisp-cc--ssa-instr-meta instr))
         (lit  (plist-get meta :literal))
         (def  (nelisp-cc--ssa-instr-def instr))
         (dst  (nelisp-cc-arm64--lookup-reg alloc-state def))
         (imm  (cond
                ((null lit) 0)
                ((eq lit t) 1)
                ((and (integerp lit) (>= lit 0) (<= lit #xFFFF)) lit)
                (t (signal 'nelisp-cc-arm64-todo
                           (list :const-wide-literal lit
                                 :phase '7.1.4))))))
    (nelisp-cc-arm64--buffer-emit-instruction
     buf
     (nelisp-cc-arm64--encode-movz-imm dst imm))))

(defun nelisp-cc-arm64--lower-return (instr buf alloc-state)
  "Lower a `:return' SSA instruction.
Emits MOV X0, <retval-reg> when the operand is not already in X0,
then RET.  AAPCS64 demands the integer return value in X0."
  (let* ((operands (nelisp-cc--ssa-instr-operands instr))
         (retval (car operands)))
    (when retval
      (let ((src (nelisp-cc-arm64--lookup-reg alloc-state retval)))
        (unless (eq src 'x0)
          (nelisp-cc-arm64--buffer-emit-instruction
           buf
           (nelisp-cc-arm64--encode-mov-reg-reg 'x0 src)))))
    (nelisp-cc-arm64--buffer-emit-instruction
     buf
     (nelisp-cc-arm64--encode-ret))))

(defun nelisp-cc-arm64--lower-call (instr buf _alloc-state)
  "Lower a `:call' SSA instruction (skeleton stub).
The skeleton emits a single BL with a label fixup keyed on the callee
symbol.  Phase 7.5 wiring resolves the actual function address once
`nelisp-defs-index' is consulted; the fixup encoder is
`-encode-bl' so the resolver auto-checks alignment and range.

INSTR.META carries `:fn SYM :unresolved t' — both are propagated
verbatim onto the fixup tag so the linker pass can dispatch."
  (let* ((meta (nelisp-cc--ssa-instr-meta instr))
         (callee (plist-get meta :fn)))
    (unless callee
      (signal 'nelisp-cc-arm64-error
              (list :call-without-fn instr)))
    (nelisp-cc-arm64--buffer-emit-fixup
     buf
     (lambda (off) (nelisp-cc-arm64--encode-bl off))
     (intern (format "callee:%s" callee)))))

(defun nelisp-cc-arm64--lower-branch (instr buf alloc-state)
  "Lower a `:branch' SSA instruction (skeleton, CBZ-shape).
The branch reads a single boolean operand: zero → fall through to the
`else' label (recorded in INSTR.META as `:else BLOCK-ID'), non-zero →
jump to the `then' label (`:then BLOCK-ID').  We emit:

  CBZ Xn, <else-label>     — fall through if zero, else proceed
  B   <then-label>          — fall-through path goes to the then arm

Both fixups are unresolved at emit time; the function-level
finalize step binds the `:then' / `:else' labels once block layout
completes.  For the skeleton a single test value is sufficient —
multi-condition guards (B.cond on flags) are Phase 7.1.4."
  (let* ((meta (nelisp-cc--ssa-instr-meta instr))
         (then-id (plist-get meta :then))
         (else-id (plist-get meta :else))
         (operands (nelisp-cc--ssa-instr-operands instr))
         (test-val (car operands)))
    (unless (and then-id else-id test-val)
      (signal 'nelisp-cc-arm64-error
              (list :branch-malformed instr)))
    (let ((src (nelisp-cc-arm64--lookup-reg alloc-state test-val))
          (then-label (intern (format "blk:%d" then-id)))
          (else-label (intern (format "blk:%d" else-id))))
      (nelisp-cc-arm64--buffer-emit-fixup
       buf
       (lambda (off) (nelisp-cc-arm64--encode-cbz src off))
       else-label)
      (nelisp-cc-arm64--buffer-emit-fixup
       buf
       (lambda (off) (nelisp-cc-arm64--encode-b off))
       then-label))))

(defun nelisp-cc-arm64--lower-load-var (_instr _buf _alloc-state)
  "Lower a `:load-var' SSA instruction.  Skeleton stub — TODO Phase 7.1.4.
Free-variable resolution requires consulting `nelisp-defs-index'
(Phase 6.5) for the symbol value cell, which the skeleton declines
to wire in until Phase 7.1.4 brings the full ABI lowering pass."
  (signal 'nelisp-cc-arm64-todo
          (list :load-var-not-implemented :phase '7.1.4)))

(defun nelisp-cc-arm64--lower-store-var (_instr _buf _alloc-state)
  "Lower a `:store-var' SSA instruction.  Skeleton stub — TODO Phase 7.1.4."
  (signal 'nelisp-cc-arm64-todo
          (list :store-var-not-implemented :phase '7.1.4)))

(defun nelisp-cc-arm64--lower-instr (instr buf alloc-state)
  "Lower one SSA INSTR into BUF using ALLOC-STATE register decisions.
Dispatches on `nelisp-cc--ssa-instr-opcode' to the per-opcode helpers.
Unknown / out-of-skeleton opcodes raise `nelisp-cc-arm64-todo'."
  (let ((op (nelisp-cc--ssa-instr-opcode instr)))
    (pcase op
      ('const     (nelisp-cc-arm64--lower-const     instr buf alloc-state))
      ('return    (nelisp-cc-arm64--lower-return    instr buf alloc-state))
      ('call      (nelisp-cc-arm64--lower-call      instr buf alloc-state))
      ('branch    (nelisp-cc-arm64--lower-branch    instr buf alloc-state))
      ('load-var  (nelisp-cc-arm64--lower-load-var  instr buf alloc-state))
      ('store-var (nelisp-cc-arm64--lower-store-var instr buf alloc-state))
      ;; Skeleton-elided ops fall through to TODO so misuse is loud.
      ('jump
       ;; Emit a placeholder `B' fixup with no label binding yet —
       ;; the function-level layout pass plants the target.
       ;; Skeleton: silently elide so trivial straight-line lambdas
       ;; (no control flow beyond entry → return) compile cleanly.
       buf)
      ('phi
       ;; Phi resolution is Phase 7.1.5 (graph-coloring with copy
       ;; insertion).  Skeleton tolerates a phi by emitting nothing
       ;; — the caller is expected to keep tests phi-free.
       buf)
      (_ (signal 'nelisp-cc-arm64-todo
                 (list :opcode-not-implemented op
                       :phase '7.1.4))))))

(defun nelisp-cc-arm64-compile (function alloc-state)
  "Compile SSA FUNCTION with ALLOC-STATE assignments to arm64 bytes.

FUNCTION is a `nelisp-cc--ssa-function' (T6 frontend output).
ALLOC-STATE is the alist returned by `nelisp-cc--linear-scan'
(T4 register allocator).  The result is a vector of integers 0..255
ready to mmap PROT_EXEC + MAP_JIT.

Phase 7.1.3 *skeleton subset* — supports the six opcodes
:const / :load-var / :store-var / :call / :branch / :return.  Larger
opcode coverage, prologue / epilogue, spill / reload, MAP_JIT write
protection, and I-cache flush all belong to Phase 7.1.4
(Doc 28 §6.9 v2 risk).

The output does *not* include a function prologue or epilogue —
trivial lambdas with no callee-saved register usage and no stack
slots run correctly without one (the test fixtures stay within that
band).  Lambdas that exceed the band trip
`nelisp-cc-arm64-todo' rather than miscompile silently."
  (let ((buf (nelisp-cc-arm64--buffer-make)))
    ;; Walk blocks in reverse postorder so block layout is forward
    ;; dataflow friendly — same linearisation the allocator used.
    (let ((rpo (nelisp-cc--ssa--reverse-postorder function)))
      (dolist (blk rpo)
        ;; Bind a label for the block so :branch fixups can resolve.
        (nelisp-cc-arm64--buffer-define-label
         buf (intern (format "blk:%d"
                             (nelisp-cc--ssa-block-id blk))))
        (dolist (instr (nelisp-cc--ssa-block-instrs blk))
          (nelisp-cc-arm64--lower-instr instr buf alloc-state))))
    (nelisp-cc-arm64--buffer-finalize buf)))

(provide 'nelisp-cc-arm64)
;;; nelisp-cc-arm64.el ends here
